# Feature construction: per-team advanced stats, lagged rolling means,
# home/away matchup features, and pre-game Elo ratings.
#
# Every rolling feature for a game uses only that team's earlier games in the
# same season. tests/testthat/test-features.R checks this directly.

ROLL_FEATURES <- c(
  "efg", "ts", "ftr", "x3par",
  "ortg", "drtg", "net_rating",
  "orb_perc", "drb_perc", "tov_perc",
  "margin", "pts", "opp_pts"
)

ROLL_WINDOWS <- c(3, 5, 10)

#' Load regular-season team box scores through hoopR (requires network).
load_team_box <- function(seasons) {
  hoopR::load_nba_team_box(seasons = seasons) |>
    dplyr::filter(season_type == 2)
}

#' One row per team per game with same-game advanced stats.
build_team_games <- function(team_box_raw) {
  tb <- team_box_raw |>
    dplyr::transmute(
      game_id, season,
      game_date = as.Date(game_date),
      team_id,
      team = team_abbreviation,
      team_name,
      side = dplyr::if_else(tolower(team_home_away) == "home", "home", "away"),
      pts = team_score,
      fgm = field_goals_made, fga = field_goals_attempted,
      fg3m = three_point_field_goals_made, fg3a = three_point_field_goals_attempted,
      ftm = free_throws_made, fta = free_throws_attempted,
      orb = offensive_rebounds, drb = defensive_rebounds, trb = total_rebounds,
      ast = assists, stl = steals, blk = blocks, tov = turnovers
    ) |>
    dplyr::filter(!is.na(team_id) & team_id <= 30)

  opp <- tb |>
    dplyr::transmute(
      game_id, opp_side = side,
      opp_team_id = team_id, opp_team = team,
      opp_pts = pts, opp_fga = fga, opp_fta = fta,
      opp_orb = orb, opp_drb = drb, opp_trb = trb, opp_tov = tov,
      opp_fgm = fgm, opp_fg3m = fg3m, opp_fg3a = fg3a
    )

  tb |>
    dplyr::mutate(desired_opp_side = dplyr::if_else(side == "home", "away", "home")) |>
    dplyr::left_join(opp, by = c("game_id" = "game_id", "desired_opp_side" = "opp_side")) |>
    dplyr::mutate(
      efg = (fgm + 0.5 * fg3m) / pmax(fga, 1),
      ts = pts / (2 * pmax(fga + 0.44 * fta, 1)),
      ftr = fta / pmax(fga, 1),
      x3par = fg3a / pmax(fga, 1),
      poss_team = fga + 0.4 * fta - orb + tov,
      poss_opp = opp_fga + 0.4 * opp_fta - opp_orb + opp_tov,
      poss = 0.5 * (poss_team + poss_opp),
      ortg = 100 * pts / pmax(poss, 1),
      drtg = 100 * opp_pts / pmax(poss, 1),
      net_rating = ortg - drtg,
      orb_perc = orb / pmax(orb + opp_drb, 1),
      drb_perc = drb / pmax(drb + opp_orb, 1),
      tov_perc = tov / pmax(poss, 1),
      margin = pts - opp_pts,
      win = as.integer(margin > 0),
      is_home = as.integer(side == "home")
    )
}

# Mean of the previous values in the window, excluding the current game.
lagged_mean <- function(x) {
  x <- utils::head(x, -1)
  if (!length(x)) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

#' Add lagged rolling means within each team-season.
#'
#' For window w, the value at game t is the mean of games t-w .. t-1.
add_rolling_features <- function(team_games, features = ROLL_FEATURES, windows = ROLL_WINDOWS) {
  out <- team_games |>
    dplyr::arrange(season, team, game_date) |>
    dplyr::group_by(season, team) |>
    dplyr::mutate(games_played = dplyr::lag(dplyr::row_number(), default = 0))

  for (window in windows) {
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(features),
          ~ slider::slide_dbl(.x, lagged_mean, .before = window, .complete = FALSE),
          .names = paste0("{.col}_roll", window)
        )
      )
  }

  dplyr::ungroup(out)
}

safe_ratio <- function(num, denom) {
  ifelse(is.na(denom) | abs(denom) < 1e-6, NA_real_, num / denom)
}

#' Pre-game Elo ratings. The rating stored for a game is the rating before
#' that game's result is applied.
compute_elo <- function(games, k = 20, baseline = 1500, hfa = 100, reset_each_season = FALSE) {
  required_cols <- c("game_id", "game_date", "season", "home_team", "away_team", "home_win")
  missing <- setdiff(required_cols, names(games))
  if (length(missing)) {
    stop("Missing columns for Elo: ", paste(missing, collapse = ", "))
  }

  games <- games |>
    dplyr::select(dplyr::all_of(required_cols)) |>
    dplyr::arrange(game_date, game_id)

  split_list <- if (reset_each_season) split(games, games$season) else list(all = games)

  results <- lapply(split_list, function(chunk) {
    teams <- unique(c(chunk$home_team, chunk$away_team))
    ratings <- stats::setNames(rep(baseline, length(teams)), teams)
    chunk$Elo_H <- NA_real_
    chunk$Elo_A <- NA_real_

    for (i in seq_len(nrow(chunk))) {
      h <- chunk$home_team[i]
      a <- chunk$away_team[i]

      chunk$Elo_H[i] <- ratings[h]
      chunk$Elo_A[i] <- ratings[a]

      e_h <- 1 / (1 + 10^((ratings[a] - (ratings[h] + hfa)) / 400))
      s_h <- if (isTRUE(as.logical(chunk$home_win[i]))) 1 else 0

      ratings[h] <- ratings[h] + k * (s_h - e_h)
      ratings[a] <- ratings[a] + k * ((1 - s_h) - (1 - e_h))
    }
    chunk
  })

  dplyr::bind_rows(results) |>
    dplyr::mutate(Elo_Diff = Elo_H - Elo_A)
}

#' One row per game with home and away rolling features, matchup
#' differentials and ratios, and season-reset and all-time Elo.
build_games <- function(team_box_raw, windows = ROLL_WINDOWS) {
  roll <- team_box_raw |>
    build_team_games() |>
    add_rolling_features(windows = windows)

  roll_pattern <- paste0("roll(", paste(windows, collapse = "|"), ")$")
  keep <- c("game_id", "season", "game_date", "team_id", "team", "team_name", "win", "margin")

  side_wide <- function(side_value, prefix) {
    roll |>
      dplyr::filter(is_home == side_value) |>
      dplyr::select(dplyr::all_of(keep), games_played, dplyr::matches(roll_pattern)) |>
      dplyr::rename_with(~ paste0(prefix, .), -c(game_id, season, game_date, win, margin)) |>
      dplyr::rename(!!paste0(prefix, "win") := win, !!paste0(prefix, "margin") := margin)
  }

  games <- side_wide(1L, "home_") |>
    dplyr::inner_join(side_wide(0L, "away_"), by = c("game_id", "season", "game_date")) |>
    dplyr::select(
      game_id, season, game_date,
      home_team_id, home_team, home_team_name,
      away_team_id, away_team, away_team_name,
      home_win,
      dplyr::matches(paste0("^home_.*_", roll_pattern)),
      dplyr::matches(paste0("^away_.*_", roll_pattern))
    )

  for (window in windows) {
    suf <- paste0("_roll", window)
    col <- function(x) games[[paste0(x, suf)]]
    games[[paste0("matchup_ortg_drtg", suf)]] <- col("home_ortg") - col("away_drtg")
    games[[paste0("matchup_net", suf)]] <- col("home_net_rating") - col("away_net_rating")
    for (stat in c("orb", "drb", "tov")) {
      home <- col(paste0("home_", stat, "_perc"))
      away <- col(paste0("away_", stat, "_perc"))
      games[[paste0(stat, suf, "_diff")]] <- home - away
      games[[paste0(stat, suf, "_ratio")]] <- safe_ratio(home, away)
    }
  }
  games$efg_roll5_diff <- games$home_efg_roll5 - games$away_efg_roll5

  elo_base <- dplyr::select(games, game_id, game_date, season, home_team, away_team, home_win)
  elo_season <- compute_elo(elo_base, reset_each_season = TRUE) |>
    dplyr::select(game_id, Elo_H_season = Elo_H, Elo_A_season = Elo_A, Elo_Diff_season = Elo_Diff)
  elo_alltime <- compute_elo(elo_base, reset_each_season = FALSE) |>
    dplyr::select(game_id, Elo_H_alltime = Elo_H, Elo_A_alltime = Elo_A, Elo_Diff_alltime = Elo_Diff)

  games |>
    dplyr::left_join(elo_season, by = "game_id") |>
    dplyr::left_join(elo_alltime, by = "game_id")
}
