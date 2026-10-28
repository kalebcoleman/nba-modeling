# Model fitting and metrics.
#
# A model is a list with `name`, `features`, and `fit`. `fit(train, features)`
# trains on `train` and returns a function that maps new data to home-win
# probabilities. Fitting never sees test outcomes. Model definitions live in
# R/model_registry.R.

#' Feature sets used by the models.
feature_sets <- function(games) {
  rolling <- c(
    grep("^(home|away)_.*_roll(3|5|10)$", names(games), value = TRUE),
    grep("^matchup_", names(games), value = TRUE),
    grep("_(diff|ratio)$", names(games), value = TRUE)
  )
  elo <- c("Elo_Diff_season", "Elo_Diff_alltime")
  list(
    lean = c("matchup_net_roll5", "matchup_ortg_drtg_roll5", "orb_roll5_diff", "tov_roll5_diff", "efg_roll5_diff"),
    elo = elo,
    rolling = sort(unique(rolling)),
    rolling_elo = sort(unique(c(rolling, elo)))
  )
}

#' Modeling frame: outcome, season, and every candidate feature, keeping only
#' games where all features are defined (drops each team's first game of a
#' season, where rolling means do not exist yet).
model_frame <- function(games) {
  feats <- feature_sets(games)$rolling_elo
  games |>
    dplyr::select(game_id, season, home_win, dplyr::all_of(feats)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(feats), as.numeric)) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(feats), is.finite))
}

fit_majority <- function(train, features = NULL) {
  rate <- mean(train$home_win)
  function(newdata) rep(rate, nrow(newdata))
}

fit_logit <- function(train, features) {
  fit <- stats::glm(
    stats::reformulate(features, "home_win"),
    data = train[, c("home_win", features)],
    family = stats::binomial()
  )
  # Keep only what predict() needs.
  fit[c("data", "model", "y", "residuals", "fitted.values", "linear.predictors", "weights", "prior.weights")] <- NULL
  function(newdata) as.numeric(stats::predict(fit, newdata = newdata, type = "response"))
}

# glmnet with the penalty chosen by cross-validation over whole seasons, so a
# season is never split between fitting and validation folds.
fit_glmnet <- function(train, features, alpha_grid = 0) {
  x <- as.matrix(train[, features])
  foldid <- as.integer(factor(train$season %% 5))

  best <- NULL
  for (a in alpha_grid) {
    cv_fit <- glmnet::cv.glmnet(x, train$home_win, family = "binomial", alpha = a, foldid = foldid)
    if (is.null(best) || min(cv_fit$cvm) < min(best$cvm)) best <- cv_fit
  }
  function(newdata) {
    as.numeric(stats::predict(best, as.matrix(newdata[, features]), s = "lambda.min", type = "response"))
  }
}

#' Wrap a parsnip model specification as a fit function.
#'
#' @param spec A parsnip model spec with mode "classification".
#' @param normalize Center and scale predictors (needed for neural nets).
fit_parsnip <- function(spec, normalize = FALSE) {
  function(train, features) {
    train <- train[, c("home_win", features)]
    train$home_win <- factor(train$home_win, levels = c(0, 1))
    rec <- recipes::recipe(home_win ~ ., data = train)
    if (normalize) rec <- recipes::step_normalize(rec, recipes::all_predictors())
    wf <- workflows::workflow(rec, spec)
    fitted <- parsnip::fit(wf, data = train)
    function(newdata) {
      as.numeric(stats::predict(fitted, new_data = newdata[, features], type = "prob")$.pred_1)
    }
  }
}

classification_metrics <- function(prob, truth, threshold = 0.5) {
  eps <- 1e-12
  p <- pmin(pmax(prob, eps), 1 - eps)
  auc <- if (length(unique(prob)) > 1) {
    as.numeric(pROC::auc(truth, prob, levels = c(0, 1), direction = "<", quiet = TRUE))
  } else {
    NA_real_
  }
  tibble::tibble(
    n = length(truth),
    accuracy = mean((prob >= threshold) == truth),
    auc = auc,
    brier = mean((prob - truth)^2),
    log_loss = -mean(truth * log(p) + (1 - truth) * log(1 - p))
  )
}

#' Fit one model on seasons before `test_season` and predict `test_season`.
predict_season <- function(frame, test_season, model) {
  train <- dplyr::filter(frame, season < test_season)
  test <- dplyr::filter(frame, season == test_season)
  stopifnot(nrow(train) > 0, nrow(test) > 0, max(train$season) < min(test$season))

  predictor <- model$fit(train, model$features)
  tibble::tibble(
    model = model$name,
    season = test_season,
    game_id = test$game_id,
    home_win = test$home_win,
    prob = predictor(test)
  )
}
