# Model registry.
#
# To add a model:
#   1. Write a function here that takes `games` and returns
#      list(name = ..., features = ..., fit = ...). `fit` can be one of the
#      helpers in R/models.R, or fit_parsnip(<any parsnip spec>).
#   2. Add the function's name to MODELS in _targets.R.
# targets then fits and evaluates only the new model; the others stay cached.
#
# Hyperparameters for xgboost and the neural net are fixed, reasonable values,
# not tuned. Tuning would need nested time-ordered validation.

model_majority <- function(games) {
  list(name = "Majority (home win rate)", features = character(), fit = fit_majority)
}

model_elo_logit <- function(games) {
  list(name = "Logistic: Elo only", features = feature_sets(games)$elo, fit = fit_logit)
}

model_lean_logit <- function(games) {
  list(name = "Logistic: lean rolling", features = feature_sets(games)$lean, fit = fit_logit)
}

model_ridge_rolling <- function(games) {
  list(name = "Ridge: rolling", features = feature_sets(games)$rolling, fit = fit_glmnet)
}

model_ridge_rolling_elo <- function(games) {
  list(name = "Ridge: rolling + Elo", features = feature_sets(games)$rolling_elo, fit = fit_glmnet)
}

model_enet_rolling_elo <- function(games) {
  list(
    name = "Elastic net: rolling + Elo",
    features = feature_sets(games)$rolling_elo,
    fit = function(train, features) fit_glmnet(train, features, alpha_grid = c(0.25, 0.5, 0.75))
  )
}

model_xgboost <- function(games) {
  spec <- parsnip::boost_tree(
    trees = 400, tree_depth = 3, learn_rate = 0.03, min_n = 50, sample_size = 0.8, mtry = 0.5
  ) |>
    parsnip::set_engine("xgboost", counts = FALSE, nthread = 2) |>
    parsnip::set_mode("classification")
  list(name = "Gradient boosting: rolling + Elo", features = feature_sets(games)$rolling_elo, fit = fit_parsnip(spec))
}

model_mlp <- function(games) {
  spec <- parsnip::mlp(hidden_units = 8, penalty = 0.1, epochs = 300) |>
    parsnip::set_engine("nnet", MaxNWts = 5000, trace = FALSE) |>
    parsnip::set_mode("classification")
  list(
    name = "Neural net (MLP): rolling + Elo",
    features = feature_sets(games)$rolling_elo,
    fit = fit_parsnip(spec, normalize = TRUE)
  )
}
