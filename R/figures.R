# Figures for the paper, README, and site. Each plot function returns a ggplot.
#
# Colors follow the model, not its rank, so a model looks the same in every
# figure. Palette and chart chrome come from a validated reference palette
# (categorical slots in fixed order; light-surface values).

CHART <- list(
  surface = "#fcfcfb",
  ink = "#0b0b0b",
  ink_secondary = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  baseline = "#c3c2b7"
)

MODEL_COLORS <- c(
  "Logistic: Elo only" = "#2a78d6",
  "Ridge: rolling + Elo" = "#eb6834",
  "Ridge: rolling" = "#1baf7a",
  "Logistic: lean rolling" = "#eda100",
  "Elastic net: rolling + Elo" = "#e87ba4",
  "Gradient boosting: rolling + Elo" = "#6250d6",
  "Neural net (MLP): rolling + Elo" = "#e34948",
  "Majority (home win rate)" = "#898781"
)

theme_paper <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = CHART$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = CHART$surface, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = CHART$grid, linewidth = 0.3),
      axis.line.x = ggplot2::element_line(colour = CHART$baseline, linewidth = 0.4),
      axis.text = ggplot2::element_text(colour = CHART$muted),
      axis.title = ggplot2::element_text(colour = CHART$ink_secondary),
      plot.title = ggplot2::element_text(colour = CHART$ink, face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = CHART$ink_secondary),
      plot.caption = ggplot2::element_text(colour = CHART$muted, hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      legend.text = ggplot2::element_text(colour = CHART$ink_secondary),
      legend.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

# Label each line at its last point.
end_labels <- function(data, x, y) {
  last <- data |>
    dplyr::group_by(model) |>
    dplyr::slice_max({{ x }}, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  ggrepel::geom_text_repel(
    data = last,
    ggplot2::aes(label = model),
    colour = CHART$ink_secondary,
    size = 3,
    direction = "y",
    hjust = 0,
    nudge_x = 0.4,
    segment.colour = CHART$baseline,
    min.segment.length = 0,
    seed = 1
  )
}

plot_home_advantage <- function(games) {
  rates <- games |>
    dplyr::filter(!is.na(home_win)) |>
    dplyr::group_by(season) |>
    dplyr::summarise(home_win_rate = mean(home_win), .groups = "drop")

  ggplot2::ggplot(rates, ggplot2::aes(season, home_win_rate)) +
    ggplot2::geom_hline(yintercept = 0.5, colour = CHART$baseline, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_line(colour = MODEL_COLORS[["Logistic: Elo only"]], linewidth = 0.9) +
    ggplot2::geom_point(colour = MODEL_COLORS[["Logistic: Elo only"]], size = 2.2) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0.45, 0.65)) +
    ggplot2::scale_x_continuous(breaks = seq(2002, 2026, by = 4)) +
    ggplot2::labs(
      title = "Home teams win less often than they used to",
      subtitle = "Share of regular-season games won by the home team",
      x = "Season (end year)", y = NULL,
      caption = "2020 ended in a neutral-site bubble; most 2021 games had few or no fans."
    ) +
    theme_paper()
}

plot_validation_by_season <- function(per_season, models) {
  d <- dplyr::filter(per_season, model %in% models)
  ggplot2::ggplot(d, ggplot2::aes(season, auc, colour = model)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    end_labels(d, season, auc) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = unique(d$season),
      expand = ggplot2::expansion(mult = c(0.02, 0.45))
    ) +
    ggplot2::labs(
      title = "Out-of-sample AUC by season",
      subtitle = "Each season predicted by a model trained on all earlier seasons",
      x = NULL, y = "AUC"
    ) +
    theme_paper()
}

plot_model_comparison <- function(holdout_metrics, holdout_bootstrap, reference = "Logistic: Elo only") {
  d <- holdout_metrics |>
    dplyr::left_join(holdout_bootstrap, by = "model") |>
    dplyr::filter(!is.na(auc)) |>
    dplyr::mutate(model = stats::reorder(model, auc))
  ref <- d$auc[d$model == reference]

  ggplot2::ggplot(d, ggplot2::aes(auc, model, colour = model)) +
    ggplot2::geom_vline(xintercept = ref, colour = CHART$baseline, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = auc_low, xmax = auc_high), linewidth = 0.9) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_text(
      ggplot2::aes(x = auc_high, label = sprintf("%.3f", auc)),
      colour = CHART$ink_secondary, size = 3, hjust = -0.25
    ) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(
      title = "Holdout AUC, 2025-26 season",
      subtitle = "Points are AUC; bars are 95% bootstrap intervals. Dashed line: Elo only.",
      x = "AUC", y = NULL
    ) +
    theme_paper() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = CHART$grid, linewidth = 0.3)
    )
}

plot_calibration <- function(bins) {
  ggplot2::ggplot(bins, ggplot2::aes(pred_mean, obs_rate, colour = model)) +
    ggplot2::geom_abline(linetype = "dashed", colour = CHART$baseline, linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::coord_equal(xlim = c(0.1, 0.95), ylim = c(0.1, 0.95)) +
    ggplot2::labs(
      title = "Calibration, 2025-26 holdout",
      subtitle = "Deciles of predicted probability. Dashed line: perfect calibration.",
      x = "Predicted home-win probability", y = "Observed home-win rate"
    ) +
    theme_paper() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = CHART$grid, linewidth = 0.3))
}

plot_live_accuracy <- function(scored, models) {
  d <- scored |>
    dplyr::filter(model %in% models) |>
    dplyr::arrange(game_date) |>
    dplyr::group_by(model, game_date) |>
    dplyr::summarise(correct = sum(correct), n = dplyr::n(), .groups = "drop_last") |>
    dplyr::mutate(accuracy = cumsum(correct) / cumsum(n)) |>
    dplyr::ungroup()

  ggplot2::ggplot(d, ggplot2::aes(game_date, accuracy, colour = model)) +
    ggplot2::geom_hline(yintercept = 0.5, colour = CHART$baseline, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_step(linewidth = 0.9) +
    end_labels(d, game_date, accuracy) +
    ggplot2::scale_colour_manual(values = MODEL_COLORS, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0.3, 1)) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.02, 0.4))) +
    ggplot2::labs(
      title = "Live accuracy to date, 2026-27",
      subtitle = "Cumulative share of games picked correctly; predictions logged before tip-off",
      x = NULL, y = NULL
    ) +
    theme_paper()
}

save_plot <- function(plot, path, width = 6.5, height = 4) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, bg = CHART$surface)
  path
}
