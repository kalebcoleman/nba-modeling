# --- EloPlot.R --------------------------------------------------------------
# Quick Elo trajectory plot for selected teams.
# Reads nba_games_modeling.RData (with Elo columns from AdvancedStats) and
# saves outputs/elo_trends.png.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

#' Run Elo plot
#'
#' Generates a season-reset Elo trajectory plot for selected teams.
#'
#' @param plot_teams Character vector of team abbreviations.
#' @export
run_elo_plot <- function(plot_teams = c("PHX", "OKC", "LAL", "BOS", "GSW", "MIA")) {
  if (!file.exists("data/nba_games_modeling.RData")) {
    stop("data/nba_games_modeling.RData not found. Run R/AdvancedStats.R first.")
  }

  load("data/nba_games_modeling.RData")

  if (!all(c("Elo_H_season", "Elo_A_season") %in% names(games_combined))) {
    stop("Elo columns not found in games_combined. Re-run R/AdvancedStats.R to add Elo.")
  }

  elo_long <- games_combined %>%
    transmute(
      game_date = as.Date(game_date),
      season,
      team = as.character(home_team),
      elo = Elo_H_season
    ) %>%
    bind_rows(
      games_combined %>%
        transmute(
          game_date = as.Date(game_date),
          season,
          team = as.character(away_team),
          elo = Elo_A_season
        )
    ) %>%
    filter(!is.na(team), !is.na(elo))

  elo_plot <- elo_long %>%
    filter(team %in% plot_teams) %>%
    ggplot(aes(x = game_date, y = elo, group = interaction(team, season))) +
    geom_line(color = "steelblue", linewidth = 0.9, alpha = 0.8) +
    facet_wrap(~ team, ncol = 3, scales = "free_y") +
    labs(title = "Season-reset Elo trajectories (selected teams)", x = "Date", y = "Elo (season-reset)") +
    theme_minimal(base_size = 11)

  dir.create("outputs", showWarnings = FALSE)
  ggsave("outputs/elo_trends.png", elo_plot, width = 10, height = 6, dpi = 300)
  message("Saved outputs/elo_trends.png")
}

if (identical(Sys.getenv("RUN_ELO_PLOT"), "true")) {
  run_elo_plot()
}
