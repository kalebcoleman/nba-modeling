# Synthetic team box scores: `n_teams` teams playing a round robin over
# `n_days` days in each season, with random but valid box-score stats.
fake_team_box <- function(seasons = 2001:2002, n_teams = 4, n_days = 12, seed = 1) {
  set.seed(seed)
  teams <- seq_len(n_teams)
  rows <- list()
  gid <- 0L
  for (s in seasons) {
    start <- as.Date(sprintf("%d-11-01", s - 1))
    for (d in seq_len(n_days)) {
      pairing <- sample(teams)
      for (k in seq(1, n_teams, by = 2)) {
        gid <- gid + 1L
        for (side in c("home", "away")) {
          team <- if (side == "home") pairing[k] else pairing[k + 1]
          fga <- sample(80:95, 1)
          fgm <- sample(35:45, 1)
          fg3a <- sample(25:40, 1)
          fg3m <- sample(8:15, 1)
          fta <- sample(15:30, 1)
          ftm <- sample(10:fta, 1)
          rows[[length(rows) + 1]] <- tibble::tibble(
            game_id = gid, season = s, season_type = 2L,
            game_date = start + d, team_id = team,
            team_abbreviation = paste0("T", team), team_name = paste("Team", team),
            team_home_away = side, team_score = 2L * fgm + fg3m + ftm,
            field_goals_made = fgm, field_goals_attempted = fga,
            three_point_field_goals_made = fg3m, three_point_field_goals_attempted = fg3a,
            free_throws_made = ftm, free_throws_attempted = fta,
            offensive_rebounds = sample(8:14, 1), defensive_rebounds = sample(30:38, 1),
            total_rebounds = NA_integer_, assists = sample(20:30, 1), steals = sample(5:10, 1),
            blocks = sample(3:8, 1), turnovers = sample(10:18, 1)
          )
        }
      }
    }
  }
  out <- dplyr::bind_rows(rows)
  out$total_rebounds <- out$offensive_rebounds + out$defensive_rebounds
  # Avoid ties so every game has a winner.
  out$team_score[out$team_home_away == "home"] <- out$team_score[out$team_home_away == "home"] +
    ifelse(out$team_score[out$team_home_away == "home"] == out$team_score[out$team_home_away == "away"], 1L, 0L)
  out
}
