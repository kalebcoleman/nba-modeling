library(hoopR)
library(dplyr)
library(tidyr)

# Basic team box-score snapshot for reference; renamed to avoid clashes with modeling objects.
team_box_raw <- load_nba_team_box(seasons = 2020:2025) %>%
  filter(season_type == 2)

home_stats_raw <- filter(team_box_raw, team_home_away == "home") %>%
  select(game_id, season, home_team = team_name, home_abbrev = team_abbreviation,
         home_points = team_score, home_FG_pct = field_goal_pct,
         home_3P_pct = three_point_field_goal_pct, home_reb = total_rebounds,
         home_tov = turnovers)

away_stats_raw <- filter(team_box_raw, team_home_away == "away") %>%
  select(game_id, away_team = team_name, away_abbrev = team_abbreviation,
         away_points = team_score, away_FG_pct = field_goal_pct,
         away_3P_pct = three_point_field_goal_pct, away_reb = total_rebounds,
         away_tov = turnovers)

# Reference join; kept separate from advanced modeling tables.
games_combined_raw <- inner_join(home_stats_raw, away_stats_raw, by = "game_id")
