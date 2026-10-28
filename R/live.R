# Live predictions for the current season.
#
# History (completed seasons) comes from the hoopR snapshot. The current
# season comes from an NBAData database kept current with NBAData::nba_update().
# Games to predict are added as rows with no stats, so their features are
# built from earlier games only, exactly as in the backtest.
#
# Models are trained on every completed season before the current one. That
# training data does not change during the season, and fits use a fixed seed,
# so each daily run uses the same frozen models.

LIVE_SEED <- 478
LIVE_FALLBACK_MODEL <- "Logistic: Elo only"
LIVE_DIR <- "live"

#' Team box scores for history plus the current season, with placeholder rows
#' for scheduled games on `dates`.
live_team_box <- function(history, current, schedule, dates) {
  upcoming <- schedule |>
    dplyr::filter(game_date %in% as.Date(dates), !status_completed) |>
    dplyr::filter(!game_id %in% current$game_id)

  placeholders <- dplyr::bind_rows(
    dplyr::transmute(
      upcoming,
      game_id, season, season_type = 2L, game_date,
      team_id = home_team_id, team_abbreviation = home_team_abbreviation, team_home_away = "home"
    ),
    dplyr::transmute(
      upcoming,
      game_id, season, season_type = 2L, game_date,
      team_id = away_team_id, team_abbreviation = away_team_abbreviation, team_home_away = "away"
    )
  )
  placeholders$team_name <- placeholders$team_abbreviation

  history <- dplyr::filter(history, season < min(current$season, schedule$season))
  dplyr::bind_rows(history, current, placeholders)
}

#' Fit every model on all games in `frame` (frozen for the season).
fit_live_models <- function(frame, models) {
  set.seed(LIVE_SEED)
  stats::setNames(
    lapply(models, function(m) m$fit(frame, m$features)),
    vapply(models, `[[`, "", "name")
  )
}

#' Predict the games on `dates` with every fitted model.
#'
#' Games where a rolling feature is undefined (a team's first game of the
#' season) are predicted with the Elo-only model for every model, and
#' flagged with `fallback = TRUE`.
live_predict <- function(games, schedule, fitted, models, dates) {
  todo <- dplyr::filter(games, game_date %in% as.Date(dates), is.na(home_win))
  if (nrow(todo) == 0) {
    return(tibble::tibble())
  }

  all_features <- unique(unlist(lapply(models, `[[`, "features")))
  complete <- stats::complete.cases(todo[, all_features])
  elo_prob <- fitted[[LIVE_FALLBACK_MODEL]](todo)

  info <- dplyr::select(schedule, game_id, game_date_time, home_team_abbreviation, away_team_abbreviation)
  dplyr::bind_rows(lapply(models, function(m) {
    prob <- rep(NA_real_, nrow(todo))
    if (any(complete)) prob[complete] <- fitted[[m$name]](todo[complete, ])
    if (identical(m$features, character())) prob <- fitted[[m$name]](todo)
    use_fallback <- is.na(prob)
    prob[use_fallback] <- elo_prob[use_fallback]
    tibble::tibble(game_id = todo$game_id, model = m$name, prob_home = prob, fallback = use_fallback)
  })) |>
    dplyr::left_join(info, by = "game_id") |>
    dplyr::transmute(
      game_id,
      game_date = as.Date(todo$game_date[match(game_id, todo$game_id)]),
      tipoff = game_date_time,
      home = home_team_abbreviation,
      away = away_team_abbreviation,
      model, prob_home, fallback
    )
}

#' Append predictions for games that have not started and are not logged yet.
append_prediction_log <- function(preds, log_path, now = Sys.time()) {
  existing <- if (file.exists(log_path)) {
    readr::read_csv(log_path, show_col_types = FALSE, col_types = readr::cols(game_id = "i", .default = "?"))
  } else {
    NULL
  }

  tip <- as.POSIXct(preds$tipoff, tz = "America/New_York")
  preds <- preds[tip > now, , drop = FALSE]
  if (!is.null(existing)) {
    key <- paste(existing$game_id, existing$model)
    preds <- preds[!paste(preds$game_id, preds$model) %in% key, , drop = FALSE]
  }
  if (nrow(preds) == 0) {
    return(invisible(0L))
  }

  preds$tipoff <- format(as.POSIXct(preds$tipoff, tz = "America/New_York"), "%Y-%m-%dT%H:%M:%S%z")
  preds$predicted_at <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(preds, log_path, append = file.exists(log_path))
  invisible(nrow(preds))
}

#' Join logged predictions with final results.
score_predictions <- function(log, results) {
  log |>
    dplyr::inner_join(dplyr::select(results, game_id, home_win), by = "game_id") |>
    dplyr::mutate(correct = (prob_home >= 0.5) == (home_win == 1))
}

#' Accuracy, Brier score, and log loss by model for scored predictions.
live_summary <- function(scored) {
  if (nrow(scored) == 0) {
    return(tibble::tibble(model = character(), n = integer(), accuracy = numeric(), brier = numeric()))
  }
  scored |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      n = dplyr::n(),
      accuracy = mean(correct),
      brier = mean((prob_home - home_win)^2),
      log_loss = -mean(home_win * log(pmax(prob_home, 1e-12)) + (1 - home_win) * log(pmax(1 - prob_home, 1e-12))),
      fallback_games = sum(fallback),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(accuracy))
}

#' Final results for the current season from team box scores.
season_results <- function(current) {
  current |>
    dplyr::filter(team_home_away == "home", !is.na(team_score), !is.na(opponent_team_score)) |>
    dplyr::transmute(game_id, home_win = as.integer(team_score > opponent_team_score))
}
