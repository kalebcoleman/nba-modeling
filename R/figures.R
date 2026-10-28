# Figures used in the README and the paper. Each function returns a ggplot.

theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
}

plot_validation_by_season <- function(per_season, models) {
  per_season |>
    dplyr::filter(model %in% models) |>
    ggplot2::ggplot(ggplot2::aes(season, auc, colour = model)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_x_continuous(breaks = unique(per_season$season)) +
    ggplot2::labs(x = "Validation season (trained on all earlier seasons)", y = "AUC", colour = NULL) +
    theme_paper()
}

plot_calibration <- function(bins) {
  ggplot2::ggplot(bins, ggplot2::aes(pred_mean, obs_rate, colour = model)) +
    ggplot2::geom_abline(linetype = "dashed", colour = "grey60") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::coord_equal(xlim = c(0.1, 0.95), ylim = c(0.1, 0.95)) +
    ggplot2::labs(x = "Mean predicted home-win probability (decile)", y = "Observed home-win rate", colour = NULL) +
    theme_paper()
}

plot_odds_ratios <- function(or_table) {
  or_table |>
    dplyr::mutate(term = stats::reorder(term, odds_ratio)) |>
    ggplot2::ggplot(ggplot2::aes(odds_ratio, term)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_pointrange(ggplot2::aes(xmin = ci_low, xmax = ci_high), size = 0.3) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Odds ratio per unit (95% CI, log scale)", y = NULL) +
    theme_paper()
}

save_plot <- function(plot, path, width = 6.5, height = 4) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200)
  path
}
