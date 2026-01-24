# --- advancedStats.R ---------------------------------------------------------
# Builds per-team-per-game advanced stats + season-to-date (lagged) features.
# Produces:
#   - model_df             : per-team/per-game advanced stats
#   - model_df_sdt         : per-team/per-game with season-to-date (lag-1) features
#   - home_df_sdt          : home-team-only rows with SDT features (1 row per game)
#   - model_df_roll        : per-team/per-game with rolling averages (no leakage)
#   - home_df_roll         : home-team-only rolling averages
#   - games_combined       : one row per game, home & away features for modeling

suppressPackageStartupMessages({
  library(hoopR)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(slider)
})

#' Run advanced stats pipeline
#'
#' Pulls NBA team box scores, constructs rolling features, and saves
#' modeling objects to an RData file.
#'
#' @param seasons Integer vector of seasons to include.
#' @param output_file Output path for the saved RData file.
#' @export
run_advanced_stats <- function(seasons = 2002:2025,
                               output_file = "data/nba_games_modeling.RData") {
  # -------- PARAMETERS --------
  # Expand historical coverage (will increase download time/size; requires network).
  SEASONS <- seasons
  OUTPUT_FILE <- output_file

  # -------- LOAD RAW TEAM BOX --------
  team_box_raw <- load_nba_team_box(seasons = SEASONS) %>%
    filter(season_type == 2)   # 2 = Regular Season

  # -------- MINIMAL COLUMNS & RENAME --------
  tb <- team_box_raw %>%
    transmute(
      game_id, season,
      game_date = as.Date(game_date),
      team_id,
      team = team_abbreviation,
      team_name,
      side = if_else(tolower(team_home_away) == "home", "home", "away"),
      pts = team_score,
      fgm = field_goals_made, fga = field_goals_attempted,
      fg3m = three_point_field_goals_made, fg3a = three_point_field_goals_attempted,
      ftm = free_throws_made, fta = free_throws_attempted,
      orb = offensive_rebounds, drb = defensive_rebounds, trb = total_rebounds,
      ast = assists, stl = steals, blk = blocks, tov = turnovers
    ) %>%
    filter(!is.na(team_id) & team_id <= 30)

  # -------- ADD OPPONENT STATS (PAIR HOME/AWAY) --------
  opp <- tb %>%
    transmute(
      game_id, opp_side = side,
      opp_team_id = team_id, opp_team = team,
      opp_pts = pts, opp_fga = fga, opp_fta = fta,
      opp_orb = orb, opp_drb = drb, opp_trb = trb, opp_tov = tov,
      opp_fgm = fgm, opp_fg3m = fg3m, opp_fg3a = fg3a
    )

  df <- tb %>%
    mutate(desired_opp_side = if_else(side == "home", "away", "home")) %>%
    left_join(opp, by = c("game_id" = "game_id", "desired_opp_side" = "opp_side")) %>%
    # -------- ADVANCED STATS (same-game) --------
    mutate(
      efg  = (fgm + 0.5*fg3m) / pmax(fga, 1),
      ts   = pts / (2 * pmax(fga + 0.44*fta, 1)),
      ftr  = fta / pmax(fga, 1),
      x3par = fg3a / pmax(fga, 1),

      poss_team = fga + 0.4*fta - orb + tov,
      poss_opp  = opp_fga + 0.4*opp_fta - opp_orb + opp_tov,
      poss      = 0.5 * (poss_team + poss_opp),

      ortg = 100 * pts     / pmax(poss, 1),
      drtg = 100 * opp_pts / pmax(poss, 1),
      net_rating = ortg - drtg,

      orb_perc = orb / pmax(orb + opp_drb, 1),
      drb_perc = drb / pmax(drb + opp_orb, 1),
      tov_perc = tov / pmax(poss, 1),

      margin = pts - opp_pts,
      win = as.integer(margin > 0),
      is_home = as.integer(side == "home")
    )

  # -------- MODEL TABLE (same-game; useful for EDA only) --------
  model_df <- df %>%
    select(
      game_id, season, game_date, team_id, team, team_name, is_home, win, margin,
      pts, opp_pts,
      efg, ts, ftr, x3par,
      ortg, drtg, net_rating,
      orb_perc, drb_perc, tov_perc,
      ast, trb, stl, blk, tov,
      fgm, fga, fg3m, fg3a, ftm, fta
    )

  # -------- ROLLING MEANS (lagged, no leakage) --------
  roll_features <- c(
    "efg", "ts", "ftr", "x3par",
    "ortg", "drtg", "net_rating",
    "orb_perc", "drb_perc", "tov_perc",
    "margin", "pts", "opp_pts"
  )

  roll_windows <- c(3, 5, 10)
  roll_pattern <- paste0("roll(", paste(roll_windows, collapse = "|"), ")$")

  lagged_mean <- function(x) {
    x <- head(x, -1)
    if (!length(x)) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }

  df_roll <- df %>%
    arrange(season, team, game_date) %>%
    group_by(season, team) %>%
    mutate(games_played = dplyr::lag(row_number(), default = 0))

  for (window in roll_windows) {
    df_roll <- df_roll %>%
      mutate(
        across(
          all_of(roll_features),
          ~ slider::slide_dbl(
            .x,
            lagged_mean,
            .before = window,
            .complete = FALSE
          ),
          .names = paste0("{.col}_roll", window)
        )
      )
  }

  df_roll <- df_roll %>% ungroup()

  model_df_roll <- df_roll %>%
    select(
      game_id, season, game_date, team_id, team, team_name, is_home, win, margin,
      games_played,
      pts, opp_pts,
      efg, ts, ftr, x3par,
      ortg, drtg, net_rating,
      orb_perc, drb_perc, tov_perc,
      ast, trb, stl, blk, tov,
      fgm, fga, fg3m, fg3a, ftm, fta,
      matches(roll_pattern)
    )

  home_df_roll <- model_df_roll %>% filter(is_home == 1)
  away_df_roll <- model_df_roll %>% filter(is_home == 0)

  valid_team <- function(team_id) {
    !is.na(team_id) & team_id <= 30
  }

  compute_elo_core <- function(df, k = 20, baseline = 1500, hfa = 100, reset_each_season = FALSE) {
    required_cols <- c("game_id", "game_date", "season", "home_team", "away_team", "home_win")
    missing <- setdiff(required_cols, names(df))
    if (length(missing)) {
      stop("Missing columns for Elo: ", paste(missing, collapse = ", "))
    }

    df <- df %>%
      select(all_of(required_cols)) %>%
      arrange(game_date, game_id)

    split_list <- if (reset_each_season) split(df, df$season) else list(all = df)
    results <- vector("list", length(split_list))
    names(results) <- names(split_list)

    for (s in seq_along(split_list)) {
      chunk <- split_list[[s]]
      teams <- unique(c(chunk$home_team, chunk$away_team))
      ratings <- setNames(rep(baseline, length(teams)), teams)
      out <- chunk %>% mutate(Elo_H = NA_real_, Elo_A = NA_real_)

      for (i in seq_len(nrow(chunk))) {
        h <- chunk$home_team[i]
        a <- chunk$away_team[i]
        win_h <- as.logical(chunk$home_win[i])

        out$Elo_H[i] <- ratings[h]
        out$Elo_A[i] <- ratings[a]

        r_h <- ratings[h] + hfa
        r_a <- ratings[a]
        e_h <- 1 / (1 + 10^((r_a - r_h) / 400))
        s_h <- if (isTRUE(win_h)) 1 else 0

        ratings[h] <- ratings[h] + k * (s_h - e_h)
        ratings[a] <- ratings[a] + k * ((1 - s_h) - (1 - e_h))
      }

      results[[s]] <- out %>% mutate(Elo_Diff = Elo_H - Elo_A)
    }

    bind_rows(results)
  }

  home_roll_wide <- home_df_roll %>%
    filter(valid_team(team_id)) %>%
    select(-is_home) %>%
    rename_with(~ paste0("home_", .), -c(game_id, season, game_date, win, margin)) %>%
    rename(home_win = win, home_margin = margin)

  away_roll_wide <- away_df_roll %>%
    filter(valid_team(team_id)) %>%
    select(-is_home) %>%
    rename_with(~ paste0("away_", .), -c(game_id, season, game_date, win, margin)) %>%
    rename(away_win = win, away_margin = margin)

  games_combined_full <- home_roll_wide %>%
    inner_join(away_roll_wide, by = c("game_id", "season", "game_date"), suffix = c("", "")) %>%
    filter(valid_team(home_team_id), valid_team(away_team_id))

  safe_ratio <- function(num, denom) {
    ifelse(is.na(denom) | abs(denom) < 1e-6, NA_real_, num / denom)
  }

  games_combined <- games_combined_full %>%
    select(
      game_id, season, game_date,
      home_team_id, home_team, home_team_name,
      away_team_id, away_team, away_team_name,
      home_win,
      matches(paste0("^home_.*_", roll_pattern)),
      matches(paste0("^away_.*_", roll_pattern))
    )

  for (window in roll_windows) {
    suf <- paste0("_roll", window)
    games_combined[[paste0("matchup_ortg_drtg", suf)]] <- games_combined[[paste0("home_ortg", suf)]] - games_combined[[paste0("away_drtg", suf)]]
    games_combined[[paste0("matchup_net", suf)]] <- games_combined[[paste0("home_net_rating", suf)]] - games_combined[[paste0("away_net_rating", suf)]]

    games_combined[[paste0("orb", suf, "_diff")]] <- games_combined[[paste0("home_orb_perc", suf)]] - games_combined[[paste0("away_orb_perc", suf)]]
    games_combined[[paste0("orb", suf, "_ratio")]] <- safe_ratio(games_combined[[paste0("home_orb_perc", suf)]], games_combined[[paste0("away_orb_perc", suf)]])

    games_combined[[paste0("drb", suf, "_diff")]] <- games_combined[[paste0("home_drb_perc", suf)]] - games_combined[[paste0("away_drb_perc", suf)]]
    games_combined[[paste0("drb", suf, "_ratio")]] <- safe_ratio(games_combined[[paste0("home_drb_perc", suf)]], games_combined[[paste0("away_drb_perc", suf)]])

    games_combined[[paste0("tov", suf, "_diff")]] <- games_combined[[paste0("home_tov_perc", suf)]] - games_combined[[paste0("away_tov_perc", suf)]]
    games_combined[[paste0("tov", suf, "_ratio")]] <- safe_ratio(games_combined[[paste0("home_tov_perc", suf)]], games_combined[[paste0("away_tov_perc", suf)]])
  }

  # -------- ELO RATINGS (season-reset and all-time) --------
  elo_base <- games_combined %>%
    select(game_id, game_date, season, home_team, away_team, home_win)

  elo_season <- compute_elo_core(elo_base, reset_each_season = TRUE) %>%
    rename(Elo_H_season = Elo_H, Elo_A_season = Elo_A, Elo_Diff_season = Elo_Diff)

  elo_alltime <- compute_elo_core(elo_base, reset_each_season = FALSE) %>%
    rename(Elo_H_alltime = Elo_H, Elo_A_alltime = Elo_A, Elo_Diff_alltime = Elo_Diff)

  games_combined <- games_combined %>%
    left_join(elo_season %>% select(game_id, Elo_H_season, Elo_A_season, Elo_Diff_season), by = "game_id") %>%
    left_join(elo_alltime %>% select(game_id, Elo_H_alltime, Elo_A_alltime, Elo_Diff_alltime), by = "game_id")

  # Predictor-only version for downstream use without leakage targets
  games_combined_features <- games_combined %>%
    select(-home_win)

  # -------- SEASON-TO-DATE (lagged) FEATURES (NO LEAKAGE) --------
  model_df_sdt <- df %>%
    arrange(season, team, game_date) %>%
    group_by(season, team) %>%
    mutate(
      gp          = row_number(),
      efg_sdt     = lag(cummean(efg)),
      ts_sdt      = lag(cummean(ts)),
      ortg_sdt    = lag(cummean(ortg)),
      drtg_sdt    = lag(cummean(drtg)),
      net_sdt     = ortg_sdt - drtg_sdt,
      orb_sdt     = lag(cummean(orb_perc)),
      drb_sdt     = lag(cummean(drb_perc)),
      tov_sdt     = lag(cummean(tov_perc))
    ) %>%
    ungroup() %>%
    select(
      game_id, season, game_date, team_id, team, team_name, is_home, win,
      gp,
      efg_sdt, ts_sdt, ortg_sdt, drtg_sdt, net_sdt, orb_sdt, drb_sdt, tov_sdt
    )

  # -------- PERSIST MODELING OBJECTS --------
  model_objects <- c(
    "SEASONS",
    "team_box_raw",
    "games_combined",
    "games_combined_full",
    "games_combined_features",
    "model_df_sdt"
  )

  save(list = model_objects, file = OUTPUT_FILE, compress = "xz")
  message("Saved modeling data to ", OUTPUT_FILE)
}

if (identical(Sys.getenv("RUN_ADVANCED_STATS"), "true")) {
  run_advanced_stats()
}
