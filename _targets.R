# Pipeline: raw team box scores -> features -> rolling-origin validation ->
# holdout evaluation -> tables and figures -> paper.
#
# Run with targets::tar_make(). See README for setup.

library(targets)
library(tarchetypes)

tar_option_set(packages = c("dplyr", "tidyr", "ggplot2"))
tar_source("R")

SEASONS <- 2002:2026
VALIDATION_SEASONS <- 2015:2025
HOLDOUT_SEASON <- 2026
SNAPSHOT <- "data/team_box_raw.rds"

list(
  # Use the saved snapshot when present so results reproduce exactly; delete
  # it to pull fresh data through hoopR.
  tar_target(team_box_raw, {
    if (file.exists(SNAPSHOT)) readRDS(SNAPSHOT) else load_team_box(SEASONS)
  }),
  tar_target(games, build_games(team_box_raw)),
  tar_target(frame, model_frame(games)),
  tar_target(specs, model_specs(games)),
  tar_target(coverage, dplyr::count(games, season, name = "games")),

  tar_target(validation_preds, rolling_origin_predictions(frame, VALIDATION_SEASONS, specs)),
  tar_target(holdout_preds, predict_season(frame, HOLDOUT_SEASON, specs)),

  tar_target(validation_by_season, summarise_predictions(validation_preds)),
  tar_target(
    validation_summary,
    validation_by_season |>
      dplyr::group_by(model) |>
      dplyr::summarise(dplyr::across(c(accuracy, auc, brier, log_loss), mean), seasons = dplyr::n(), .groups = "drop")
  ),
  tar_target(
    holdout_metrics,
    holdout_table(holdout_preds, dplyr::filter(validation_preds, season == max(VALIDATION_SEASONS)))
  ),
  tar_target(
    holdout_bootstrap,
    bootstrap_holdout(
      holdout_preds,
      thresholds = stats::setNames(as.list(rep(0.5, length(specs))), names(specs)),
      reference = "Logistic: Elo only"
    )
  ),
  tar_target(odds_lean, fit_inferential(frame, feature_sets(games)$lean, HOLDOUT_SEASON)),
  tar_target(
    odds_lean_elo,
    fit_inferential(frame, c(feature_sets(games)$lean, feature_sets(games)$elo), HOLDOUT_SEASON)
  ),

  tar_target(
    tables,
    {
      dir.create("outputs", showWarnings = FALSE)
      out <- list(
        coverage = coverage, validation_by_season = validation_by_season,
        validation_summary = validation_summary, holdout_metrics = holdout_metrics,
        holdout_bootstrap = holdout_bootstrap, odds_lean = odds_lean, odds_lean_elo = odds_lean_elo
      )
      paths <- file.path("outputs", paste0(names(out), ".csv"))
      purrr::walk2(out, paths, readr::write_csv)
      paths
    },
    format = "file"
  ),

  tar_target(
    fig_validation,
    save_plot(
      plot_validation_by_season(
        validation_by_season,
        c("Logistic: Elo only", "Logistic: lean rolling", "Ridge: rolling", "Ridge: rolling + Elo")
      ),
      "figures/validation_auc_by_season.png"
    ),
    format = "file"
  ),
  tar_target(
    fig_calibration,
    save_plot(
      plot_calibration(calibration_bins(dplyr::filter(
        holdout_preds, model %in% c("Logistic: Elo only", "Ridge: rolling + Elo")
      ))),
      "figures/holdout_calibration.png", width = 5, height = 5
    ),
    format = "file"
  ),

  tar_quarto(paper, "paper/paper.qmd")
)
