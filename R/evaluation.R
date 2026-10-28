# Time-ordered evaluation.
#
# - Rolling-origin validation: for each validation season s, train on all
#   seasons before s and predict s.
# - The classification threshold is picked on the last validation season
#   (2024), never on the holdout.
# - The holdout season (2025) is predicted once, by models trained on all
#   earlier seasons.

#' Out-of-sample predictions for each season in `seasons`.
rolling_origin_predictions <- function(frame, seasons, specs) {
  dplyr::bind_rows(lapply(seasons, function(s) predict_season(frame, s, specs)))
}

summarise_predictions <- function(preds, threshold = 0.5) {
  preds |>
    dplyr::group_by(model, season) |>
    dplyr::summarise(classification_metrics(prob, home_win, threshold), .groups = "drop")
}

#' Pick the threshold that maximizes accuracy on validation predictions.
select_threshold <- function(val_preds, grid = seq(0.30, 0.70, by = 0.01)) {
  # A constant prediction (majority baseline) has no meaningful threshold.
  if (length(unique(val_preds$prob)) == 1) {
    return(0.5)
  }
  acc <- vapply(grid, function(t) mean((val_preds$prob >= t) == val_preds$home_win), numeric(1))
  grid[which.max(acc)]
}

#' Percentile bootstrap over holdout games for each model's metrics, plus the
#' paired difference in AUC and accuracy relative to `reference`.
bootstrap_holdout <- function(holdout_preds, thresholds, reference, n_boot = 2000, seed = 478) {
  set.seed(seed)
  wide <- holdout_preds |>
    dplyr::select(model, game_id, home_win, prob) |>
    tidyr::pivot_wider(names_from = model, values_from = prob)
  models <- setdiff(names(wide), c("game_id", "home_win"))
  n <- nrow(wide)

  draws <- dplyr::bind_rows(lapply(seq_len(n_boot), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    y <- wide$home_win[idx]
    dplyr::bind_rows(lapply(models, function(m) {
      thr <- thresholds[[m]]
      p <- wide[[m]][idx]
      auc <- if (length(unique(p)) > 1) {
        as.numeric(pROC::auc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE))
      } else {
        NA_real_
      }
      tibble::tibble(draw = b, model = m, accuracy = mean((p >= thr) == y), auc = auc)
    }))
  }))

  ref <- dplyr::filter(draws, model == reference) |>
    dplyr::select(draw, ref_accuracy = accuracy, ref_auc = auc)

  draws |>
    dplyr::left_join(ref, by = "draw") |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      accuracy_low = stats::quantile(accuracy, 0.025),
      accuracy_high = stats::quantile(accuracy, 0.975),
      auc_low = stats::quantile(auc, 0.025, na.rm = TRUE),
      auc_high = stats::quantile(auc, 0.975, na.rm = TRUE),
      auc_diff_vs_ref_low = stats::quantile(auc - ref_auc, 0.025, na.rm = TRUE),
      auc_diff_vs_ref_high = stats::quantile(auc - ref_auc, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Holdout table: metrics at 0.5 and at the threshold chosen on validation.
holdout_table <- function(holdout_preds, val_preds) {
  models <- unique(holdout_preds$model)
  thresholds <- stats::setNames(lapply(models, function(m) {
    select_threshold(dplyr::filter(val_preds, model == m))
  }), models)

  dplyr::bind_rows(lapply(models, function(m) {
    p <- dplyr::filter(holdout_preds, model == m)
    at_half <- classification_metrics(p$prob, p$home_win, 0.5)
    tuned <- classification_metrics(p$prob, p$home_win, thresholds[[m]])
    dplyr::mutate(
      at_half,
      model = m,
      threshold_from_validation = thresholds[[m]],
      accuracy_at_validation_threshold = tuned$accuracy,
      .before = 1
    )
  }))
}

#' Calibration bins for one model's holdout predictions.
calibration_bins <- function(preds, bins = 10) {
  preds |>
    dplyr::mutate(bin = dplyr::ntile(prob, bins)) |>
    dplyr::group_by(model, bin) |>
    dplyr::summarise(pred_mean = mean(prob), obs_rate = mean(home_win), n = dplyr::n(), .groups = "drop")
}
