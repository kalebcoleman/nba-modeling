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
