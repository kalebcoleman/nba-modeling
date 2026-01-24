# --- PredictiveModels.R ------------------------------------------------------
# Predictive modeling entrypoint with optional CV and bootstrap.
# Outputs:
#   outputs/predictive_results.rds
#   outputs/predictive_metrics.csv
#   outputs/predictive_summary_table.csv
# Optional:
#   outputs/predictive_cv_metrics.csv
#   outputs/threshold_grid_accuracy.csv
#   outputs/threshold_bootstrap_summary.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(pROC)
  library(glmnet)
})

# Basic progress indicator without extra dependencies.
progress_init <- function(total_steps) {
  if (!interactive()) return(NULL)
  utils::txtProgressBar(min = 0, max = total_steps, style = 3)
}

progress_step <- function(pb, step) {
  if (!is.null(pb)) utils::setTxtProgressBar(pb, step)
}

progress_close <- function(pb) {
  if (!is.null(pb)) close(pb)
}

#' Run predictive models
#'
#' Fits predictive models, writes holdout metrics, and optionally runs CV
#' and bootstrap diagnostics.
#'
#' @param feature_mode "full" or "fast".
#' @param run_cv Logical; run k-fold CV on training seasons.
#' @param run_bootstrap Logical; bootstrap holdout accuracy for ridge.
#' @param include_rf Logical; include random forest benchmark.
#' @param cv_folds Number of CV folds.
#' @param alpha_grid Elastic-net alpha grid.
#' @export
run_predictive_models <- function(feature_mode = "full",
                                  run_cv = FALSE,
                                  run_bootstrap = FALSE,
                                  include_rf = FALSE,
                                  cv_folds = 5,
                                  alpha_grid = c(0.25, 0.5, 0.75)) {
  # ---- Controls ----
  FEATURE_MODE <- feature_mode   # "full" or "fast"
  RUN_CV <- run_cv
  RUN_BOOTSTRAP <- run_bootstrap
  INCLUDE_RF <- include_rf
  CV_FOLDS <- cv_folds
  ALPHA_GRID <- alpha_grid

if (!file.exists("data/nba_games_modeling.RData")) {
  stop("data/nba_games_modeling.RData not found. Run R/AdvancedStats.R first.")
}

set.seed(42)
load("data/nba_games_modeling.RData")

# Prefer Elo-augmented data if present
if (file.exists("outputs/games_with_elo.rds")) {
  games_df <- readRDS("outputs/games_with_elo.rds")
} else {
  games_df <- games_combined
}

games_model <- games_df %>%
  mutate(
    season_start = if (is.numeric(season)) as.integer(season) else as.integer(substr(season, 1, 4)),
    efg_roll5_diff = home_efg_roll5 - away_efg_roll5
  )

team_cols <- grep("^(home|away)_.*_roll(3|5|10)$", names(games_model), value = TRUE)
matchup_cols <- grep("^matchup_", names(games_model), value = TRUE)
diff_cols <- grep("_diff$", names(games_model), value = TRUE)
ratio_cols <- grep("_ratio$", names(games_model), value = TRUE)
elo_cols <- grep("^Elo_", names(games_model), value = TRUE)

feat_all <- sort(unique(c(team_cols, matchup_cols, diff_cols, ratio_cols, elo_cols, "efg_roll5_diff")))
feat_fast <- sort(unique(c(matchup_cols, diff_cols, ratio_cols, elo_cols, "efg_roll5_diff")))

feat_main <- if (FEATURE_MODE == "fast") feat_fast else feat_all

model_df <- games_model %>%
  select(home_win, season_start, all_of(feat_main)) %>%
  mutate(across(all_of(feat_main), as.numeric)) %>%
  filter(if_all(all_of(feat_main), ~ is.finite(.)))

holdout_season <- max(model_df$season_start, na.rm = TRUE)
train <- filter(model_df, season_start < holdout_season)
test  <- filter(model_df, season_start == holdout_season)

yte <- test$home_win
baseline_acc <- max(mean(yte == 1), mean(yte == 0))

infer_feats <- c("matchup_net_roll5", "matchup_ortg_drtg_roll5",
                 "orb_roll5_diff", "tov_roll5_diff",
                 "efg_roll5_diff")

calc_metrics <- function(prob, truth) {
  tibble(
    accuracy = mean((prob >= 0.5) == truth),
    auc = as.numeric(pROC::auc(truth, prob))
  )
}

fit_ridge <- function(train_df, test_df) {
  xtr <- model.matrix(home_win ~ ., data = train_df)[, -1, drop = FALSE]
  xte <- model.matrix(home_win ~ ., data = test_df)[, -1, drop = FALSE]
  cv_fit <- cv.glmnet(xtr, train_df$home_win, family = "binomial", alpha = 0)
  prob <- as.numeric(predict(cv_fit, xte, s = "lambda.min", type = "response"))
  list(prob = prob, model = cv_fit)
}

fit_elastic <- function(train_df, test_df, alpha_grid = ALPHA_GRID) {
  xtr <- model.matrix(home_win ~ ., data = train_df)[, -1, drop = FALSE]
  xte <- model.matrix(home_win ~ ., data = test_df)[, -1, drop = FALSE]
  best <- NULL
  best_cvm <- Inf
  best_alpha <- NA_real_
  for (a in alpha_grid) {
    cv_fit <- cv.glmnet(xtr, train_df$home_win, family = "binomial", alpha = a)
    m <- min(cv_fit$cvm, na.rm = TRUE)
    if (m < best_cvm) {
      best_cvm <- m
      best <- cv_fit
      best_alpha <- a
    }
  }
  prob <- as.numeric(predict(best, xte, s = "lambda.min", type = "response"))
  list(prob = prob, model = best, alpha = best_alpha)
}

fit_rf <- function(train_df, test_df) {
  if (!INCLUDE_RF) return(NULL)
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required when INCLUDE_RF = TRUE")
  }
  train_df <- train_df %>% mutate(home_win = factor(home_win, levels = c(0, 1)))
  test_df <- test_df %>% mutate(home_win = factor(home_win, levels = c(0, 1)))
  rf <- randomForest::randomForest(home_win ~ ., data = train_df)
  prob <- predict(rf, newdata = test_df, type = "prob")[, "1"]
  list(prob = as.numeric(prob), model = rf)
}

# ---- Fit models ----
total_steps <- 5L + as.integer(RUN_CV) + as.integer(RUN_BOOTSTRAP)
pb <- progress_init(total_steps)
step_id <- 0L

logit_fit <- glm(home_win ~ ., data = train %>% select(home_win, all_of(infer_feats)), family = binomial())
logit_prob <- predict(logit_fit, newdata = test %>% select(all_of(infer_feats)), type = "response")
step_id <- step_id + 1L; progress_step(pb, step_id)

full_train <- train %>% select(home_win, all_of(feat_main))
full_test  <- test %>% select(home_win, all_of(feat_main))

ridge_full <- fit_ridge(full_train, full_test)
step_id <- step_id + 1L; progress_step(pb, step_id)
elastic_full <- fit_elastic(full_train, full_test)
step_id <- step_id + 1L; progress_step(pb, step_id)
rf_full <- fit_rf(full_train, full_test)
step_id <- step_id + 1L; progress_step(pb, step_id)

metrics <- bind_rows(
  tibble(
    model = c("Majority baseline", "Logistic (lean)"),
    feature_set = c("none", "inferential"),
    accuracy = c(baseline_acc, mean((logit_prob >= 0.5) == yte)),
    auc = c(NA_real_, as.numeric(pROC::auc(yte, logit_prob)))
  ),
  tibble(
    model = c("Ridge logistic", "Elastic net"),
    feature_set = rep(FEATURE_MODE, 2),
    accuracy = c(mean((ridge_full$prob >= 0.5) == yte),
                 mean((elastic_full$prob >= 0.5) == yte)),
    auc = c(as.numeric(pROC::auc(yte, ridge_full$prob)),
            as.numeric(pROC::auc(yte, elastic_full$prob)))
  )
)

if (!is.null(rf_full)) {
  metrics <- bind_rows(
    metrics,
    tibble(
      model = "Random Forest",
      feature_set = FEATURE_MODE,
      accuracy = mean((rf_full$prob >= 0.5) == yte),
      auc = as.numeric(pROC::auc(yte, rf_full$prob))
    )
  )
}

calib <- tibble(
  pred = ridge_full$prob,
  truth = yte
) %>%
  mutate(bin = ntile(pred, 10)) %>%
  group_by(bin) %>%
  summarise(pred_mean = mean(pred), obs = mean(truth), .groups = "drop")

summary_table <- metrics %>%
  mutate(holdout_season = holdout_season) %>%
  arrange(desc(auc))

dir.create("outputs", showWarnings = FALSE)

saveRDS(
  list(
    metrics = metrics,
    calib = calib,
    yte = yte,
    ridge_prob = ridge_full$prob,
    ridge_prob_list = list(all_features = ridge_full$prob),
    elastic_prob = elastic_full$prob,
    holdout_season = holdout_season,
    elastic_alpha = elastic_full$alpha
  ),
  "outputs/predictive_results.rds"
)
write_csv(metrics, "outputs/predictive_metrics.csv")
write_csv(summary_table, "outputs/predictive_summary_table.csv")

message("Holdout season: ", holdout_season)
message("Wrote outputs/predictive_metrics.csv and outputs/predictive_summary_table.csv")
print(summary_table)
step_id <- step_id + 1L; progress_step(pb, step_id)

# ---- Optional: CV ----
if (RUN_CV) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("caret package is required when RUN_CV = TRUE")
  }
  folds <- caret::createFolds(train$home_win, k = CV_FOLDS, list = TRUE, returnTrain = FALSE)
  cv_sets <- vector("list", length(folds))
  f_id <- 1L
  for (va_idx in folds) {
    tr_idx <- setdiff(seq_len(nrow(train)), va_idx)
    tr_dat <- train[tr_idx, , drop = FALSE]
    va_dat <- train[va_idx, , drop = FALSE]

    logit_cv <- glm(home_win ~ ., data = tr_dat %>% select(home_win, all_of(infer_feats)), family = binomial())
    logit_cv_prob <- predict(logit_cv, newdata = va_dat %>% select(all_of(infer_feats)), type = "response")

    ridge_cv <- fit_ridge(tr_dat %>% select(home_win, all_of(feat_main)),
                          va_dat %>% select(home_win, all_of(feat_main)))
    elastic_cv <- fit_elastic(tr_dat %>% select(home_win, all_of(feat_main)),
                              va_dat %>% select(home_win, all_of(feat_main)))

    fold_metrics <- bind_rows(
      calc_metrics(logit_cv_prob, va_dat$home_win) %>% mutate(model = "Logistic (lean)"),
      calc_metrics(ridge_cv$prob, va_dat$home_win) %>% mutate(model = "Ridge logistic"),
      calc_metrics(elastic_cv$prob, va_dat$home_win) %>% mutate(model = "Elastic net")
    ) %>%
      mutate(feature_set = FEATURE_MODE, fold = f_id, .before = 1)

    if (INCLUDE_RF) {
      rf_cv <- fit_rf(tr_dat %>% select(home_win, all_of(feat_main)),
                      va_dat %>% select(home_win, all_of(feat_main)))
      fold_metrics <- bind_rows(
        fold_metrics,
        calc_metrics(rf_cv$prob, va_dat$home_win) %>%
          mutate(model = "Random Forest", feature_set = FEATURE_MODE, fold = f_id, .before = 1)
      )
    }

    cv_sets[[f_id]] <- fold_metrics
    f_id <- f_id + 1L
  }

  cv_metrics <- bind_rows(cv_sets) %>%
    group_by(model, feature_set) %>%
    summarise(cv_accuracy = mean(accuracy), cv_auc = mean(auc), .groups = "drop")

  write_csv(cv_metrics, "outputs/predictive_cv_metrics.csv")
  message("Wrote outputs/predictive_cv_metrics.csv (k = ", CV_FOLDS, ")")
  print(cv_metrics)
  step_id <- step_id + 1L; progress_step(pb, step_id)
}

# ---- Optional: bootstrap ----
if (RUN_BOOTSTRAP) {
  threshold_grid <- seq(0.3, 0.7, by = 0.01)
  acc_tbl <- tibble(
    threshold = threshold_grid,
    accuracy = vapply(threshold_grid, function(t) mean((ridge_full$prob >= t) == yte), numeric(1))
  )

  best_row <- acc_tbl %>% slice_max(accuracy, n = 1, with_ties = FALSE)
  best_thresh <- best_row$threshold

  boot_acc <- function(thresh, n_boot = 1000) {
    n <- length(yte)
    replicate(n_boot, {
      idx <- sample.int(n, n, replace = TRUE)
      mean((ridge_full$prob[idx] >= thresh) == yte[idx])
    })
  }

  boot_default <- boot_acc(0.5)
  boot_tuned   <- boot_acc(best_thresh)

  boot_summary <- tibble(
    model = "Ridge logistic",
    threshold = c(0.5, best_thresh),
    type = c("default", "tuned"),
    holdout_accuracy = c(mean((ridge_full$prob >= 0.5) == yte), best_row$accuracy),
    mean_accuracy = c(mean(boot_default), mean(boot_tuned)),
    ci_low = c(quantile(boot_default, 0.025), quantile(boot_tuned, 0.025)),
    ci_high = c(quantile(boot_default, 0.975), quantile(boot_tuned, 0.975))
  )

  write_csv(acc_tbl, "outputs/threshold_grid_accuracy.csv")
  write_csv(boot_summary, "outputs/threshold_bootstrap_summary.csv")
  message("Wrote outputs/threshold_grid_accuracy.csv and outputs/threshold_bootstrap_summary.csv")
  print(boot_summary)
  step_id <- step_id + 1L; progress_step(pb, step_id)
}

progress_close(pb)

}

if (identical(Sys.getenv("RUN_PREDICTIVE_MODELS"), "true")) {
  run_predictive_models()
}
