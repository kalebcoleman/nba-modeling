test_that("prediction log skips started games and games already logged", {
  path <- file.path(withr::local_tempdir(), "log.csv")
  now <- as.POSIXct("2026-10-28 12:00:00", tz = "America/New_York")
  preds <- tibble::tibble(
    game_id = 1:3, game_date = as.Date("2026-10-28"),
    tipoff = as.POSIXct(
      c("2026-10-28 11:00:00", "2026-10-28 19:00:00", "2026-10-28 20:00:00"),
      tz = "America/New_York"
    ),
    home = c("A", "B", "C"), away = c("D", "E", "F"),
    model = "m", prob_home = c(0.6, 0.4, 0.7), fallback = FALSE
  )
  expect_equal(append_prediction_log(preds, path, now = now), 2L)
  expect_equal(append_prediction_log(preds, path, now = now), 0L)
  logged <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(sort(logged$game_id), 2:3)
})

test_that("live predictions fall back to Elo for a team's first game", {
  raw <- fake_team_box(seasons = 2001:2005, n_teams = 4, n_days = 12)
  # Turn the first day of 2005 into scheduled games with no stats.
  first_day <- min(raw$game_date[raw$season == 2005])
  sched_rows <- raw[raw$game_date == first_day, ]
  history <- raw[raw$season < 2005, ]
  schedule <- sched_rows |>
    dplyr::group_by(game_id, season, game_date) |>
    dplyr::summarise(
      home_team_id = team_id[team_home_away == "home"],
      away_team_id = team_id[team_home_away == "away"],
      home_team_abbreviation = team_abbreviation[team_home_away == "home"],
      away_team_abbreviation = team_abbreviation[team_home_away == "away"],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      status_completed = FALSE,
      game_date_time = as.POSIXct(paste(game_date, "19:00:00"), tz = "America/New_York")
    )
  current <- raw[0, ]
  current$season <- integer()

  tb <- live_team_box(history, current, schedule, dates = first_day)
  games <- build_games(tb)
  models <- all_models(games)
  fitted <- fit_live_models(model_frame(dplyr::filter(games, season < 2005)), models)
  preds <- live_predict(games, schedule, fitted, models, dates = first_day)

  expect_equal(nrow(preds), nrow(schedule) * length(models))
  expect_true(all(preds$fallback[preds$model != "Majority (home win rate)"]))
  elo <- preds$prob_home[preds$model == "Logistic: Elo only"]
  ridge <- preds$prob_home[preds$model == "Ridge: rolling + Elo"]
  expect_equal(ridge, elo)
})

test_that("scoring joins final results and summarises by model", {
  log <- tibble::tibble(game_id = c(1L, 2L, 3L), model = "m", prob_home = c(0.7, 0.3, 0.6), fallback = FALSE)
  results <- tibble::tibble(game_id = c(1L, 2L), home_win = c(1L, 1L))
  s <- live_summary(score_predictions(log, results))
  expect_equal(s$n, 2)
  expect_equal(s$accuracy, 0.5)
})

test_that("backfilled predictions equal predictions made with only earlier data", {
  raw <- fake_team_box(seasons = 2001:2005, n_teams = 4, n_days = 12)
  history <- raw[raw$season < 2005, ]
  current <- raw[raw$season == 2005, ]
  current$opponent_team_score <- NA_integer_
  schedule <- current |>
    dplyr::group_by(game_id, season, game_date) |>
    dplyr::summarise(
      home_team_id = team_id[team_home_away == "home"],
      away_team_id = team_id[team_home_away == "away"],
      home_team_abbreviation = team_abbreviation[team_home_away == "home"],
      away_team_abbreviation = team_abbreviation[team_home_away == "away"],
      .groups = "drop"
    ) |>
    dplyr::mutate(status_completed = TRUE, game_date_time = as.POSIXct(paste(game_date, "19:00:00")))

  # Backfill: the whole season is known; predict day 6 after the fact.
  target_day <- sort(unique(current$game_date))[6]
  games_full <- build_games(live_team_box(history, current, schedule, dates = as.Date(character())))
  models <- all_models(games_full)
  fitted <- fit_live_models(model_frame(dplyr::filter(games_full, season < 2005)), models)
  backfill <- live_predict(games_full, schedule, fitted, models, dates = target_day, pending_only = FALSE)

  # Pre-game: only games before target_day are known; target_day is scheduled.
  before <- current[current$game_date < target_day, ]
  sched_pre <- schedule
  sched_pre$status_completed <- sched_pre$game_date < target_day
  games_pre <- build_games(live_team_box(history, before, sched_pre, dates = target_day))
  pregame <- live_predict(games_pre, sched_pre, fitted, models, dates = target_day)

  expect_gt(nrow(backfill), 0)
  key <- function(d) d[order(d$game_id, d$model), c("game_id", "model", "prob_home", "fallback")]
  expect_equal(key(backfill), key(pregame))
})

test_that("backfill rows are only logged for games that have started", {
  path <- file.path(withr::local_tempdir(), "log.csv")
  now <- as.POSIXct("2026-10-28 12:00:00", tz = "America/New_York")
  preds <- tibble::tibble(
    game_id = 1:2, game_date = as.Date("2026-10-28"),
    tipoff = as.POSIXct(c("2026-10-27 19:00:00", "2026-10-28 19:00:00"), tz = "America/New_York"),
    home = c("A", "B"), away = c("C", "D"), model = "m", prob_home = 0.6, fallback = FALSE
  )
  expect_equal(append_prediction_log(preds, path, now = now, source = "backfill"), 1L)
  logged <- readr::read_csv(path, show_col_types = FALSE)
  expect_equal(logged$game_id, 1L)
  expect_equal(logged$source, "backfill")
})
