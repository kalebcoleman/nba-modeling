test_that("predict_season trains only on earlier seasons", {
  frame <- tibble::tibble(
    game_id = 1:300,
    season = rep(2001:2003, each = 100),
    home_win = rep(c(0L, 1L), 150),
    x = stats::rnorm(300)
  )
  seen <- NULL
  spy <- function(train, features) {
    seen <<- unique(train$season)
    function(newdata) rep(0.5, nrow(newdata))
  }
  predict_season(frame, 2003, list(name = "spy", fit = spy, features = "x"))
  expect_equal(sort(seen), c(2001, 2002))
})

test_that("select_threshold uses validation data and handles constant predictions", {
  val <- tibble::tibble(prob = c(0.2, 0.4, 0.6, 0.8), home_win = c(0, 1, 1, 1))
  expect_true(select_threshold(val) <= 0.4)
  expect_equal(select_threshold(tibble::tibble(prob = rep(0.55, 4), home_win = c(0, 1, 1, 1))), 0.5)
})

test_that("classification_metrics matches hand calculation", {
  m <- classification_metrics(c(0.9, 0.2, 0.6, 0.4), c(1, 0, 0, 1))
  expect_equal(m$accuracy, 0.5)
  expect_equal(m$brier, mean((c(0.9, 0.2, 0.6, 0.4) - c(1, 0, 0, 1))^2))
  expect_equal(m$auc, 0.75)
})

test_that("calibration deciles are computed within each model", {
  preds <- tibble::tibble(
    model = rep(c("a", "b"), each = 20),
    prob = c(seq(0.01, 0.20, length.out = 20), seq(0.80, 0.99, length.out = 20)),
    home_win = rep(c(0L, 1L), 20)
  )
  bins <- calibration_bins(preds, bins = 10)
  expect_equal(nrow(bins), 20)
  expect_true(all(table(bins$model) == 10))
})

test_that("every registered model fits and returns probabilities", {
  games <- build_games(fake_team_box(seasons = 2001:2004, n_teams = 6, n_days = 20))
  frame <- model_frame(games)
  fns <- ls(envir = globalenv(), pattern = "^model_")
  fns <- setdiff(fns, c("model_frame"))
  for (fn in fns) {
    model <- get(fn)(games)
    p <- predict_season(frame, 2004, model)
    expect_true(all(p$prob >= 0 & p$prob <= 1), info = fn)
    expect_equal(nrow(p), sum(frame$season == 2004), info = fn)
  }
})
