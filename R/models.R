# Model specifications, fitting, and metrics.
#
# Every fit function takes a training frame and a test frame and returns
# predicted home-win probabilities for the test frame. Nothing here looks at
# test outcomes.

#' Feature sets used in the paper.
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

fit_majority <- function(train, test, features = NULL) {
  rep(mean(train$home_win), nrow(test))
}

fit_logit <- function(train, test, features) {
  fit <- stats::glm(
    stats::reformulate(features, "home_win"),
    data = train,
    family = stats::binomial()
  )
  as.numeric(stats::predict(fit, newdata = test, type = "response"))
}

# glmnet with the penalty chosen by cross-validation over whole seasons, so a
# season is never split between fitting and validation folds.
fit_glmnet <- function(train, test, features, alpha_grid = 0) {
  xtr <- as.matrix(train[, features])
  xte <- as.matrix(test[, features])
  foldid <- as.integer(factor(train$season %% 5))

  best <- NULL
  for (a in alpha_grid) {
    cv_fit <- glmnet::cv.glmnet(xtr, train$home_win, family = "binomial", alpha = a, foldid = foldid)
    if (is.null(best) || min(cv_fit$cvm) < min(best$cvm)) best <- cv_fit
  }
  as.numeric(stats::predict(best, xte, s = "lambda.min", type = "response"))
}

#' Model specifications compared in the paper.
model_specs <- function(games) {
  fs <- feature_sets(games)
  list(
    "Majority (home win rate)" = list(fit = fit_majority, features = NULL),
    "Logistic: Elo only" = list(fit = fit_logit, features = fs$elo),
    "Logistic: lean rolling" = list(fit = fit_logit, features = fs$lean),
    "Ridge: rolling" = list(fit = fit_glmnet, features = fs$rolling),
    "Ridge: rolling + Elo" = list(fit = fit_glmnet, features = fs$rolling_elo),
    "Elastic net: rolling + Elo" = list(
      fit = function(train, test, features) fit_glmnet(train, test, features, alpha_grid = c(0.25, 0.5, 0.75)),
      features = fs$rolling_elo
    )
  )
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

#' Fit every model on seasons before `test_season` and predict `test_season`.
predict_season <- function(frame, test_season, specs) {
  train <- dplyr::filter(frame, season < test_season)
  test <- dplyr::filter(frame, season == test_season)
  stopifnot(nrow(train) > 0, nrow(test) > 0, max(train$season) < min(test$season))

  dplyr::bind_rows(lapply(names(specs), function(name) {
    spec <- specs[[name]]
    tibble::tibble(
      model = name,
      season = test_season,
      game_id = test$game_id,
      home_win = test$home_win,
      prob = spec$fit(train, test, spec$features)
    )
  }))
}
