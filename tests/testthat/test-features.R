test_that("rolling features equal the mean of the previous w games", {
  tg <- add_rolling_features(build_team_games(fake_team_box()), windows = c(3, 5))
  one <- dplyr::filter(tg, team == "T1", season == 2001) |> dplyr::arrange(game_date)

  for (i in seq_len(nrow(one))) {
    prev <- one$net_rating[seq_len(i - 1)]
    expected3 <- if (i == 1) NA_real_ else mean(utils::tail(prev, 3))
    expect_equal(one$net_rating_roll3[i], expected3)
  }
})

test_that("first game of each team-season has no rolling value", {
  tg <- add_rolling_features(build_team_games(fake_team_box()), windows = 5)
  firsts <- tg |>
    dplyr::group_by(season, team) |>
    dplyr::slice_min(game_date, n = 1)
  expect_true(all(is.na(firsts$net_rating_roll5)))
  expect_true(all(firsts$games_played == 0))
})

test_that("changing a game's result does not change features for that game or earlier", {
  raw <- fake_team_box()
  base <- build_games(raw)

  # Rewrite one mid-season game: the home team now loses by 60.
  season_games <- dplyr::arrange(dplyr::filter(base, season == 2001), game_date)
  target <- season_games$game_id[nrow(season_games) %/% 2]
  target_date <- base$game_date[base$game_id == target]
  changed_raw <- raw
  rows <- changed_raw$game_id == target
  changed_raw$team_score[rows & changed_raw$team_home_away == "home"] <- 40L
  changed_raw$team_score[rows & changed_raw$team_home_away == "away"] <- 100L
  changed <- build_games(changed_raw)

  feature_cols <- grep("roll|Elo|matchup|_diff$|_ratio$", names(base), value = TRUE)
  up_to <- base$game_date <= target_date
  expect_equal(
    base[up_to, feature_cols],
    changed[match(base$game_id[up_to], changed$game_id), feature_cols]
  )

  # Sanity check that the change does reach later games.
  later <- base$game_date > target_date & base$season == 2001
  expect_false(isTRUE(all.equal(
    base[later, feature_cols],
    changed[match(base$game_id[later], changed$game_id), feature_cols]
  )))
})

test_that("rolling windows do not carry over between seasons", {
  tg <- add_rolling_features(build_team_games(fake_team_box()), windows = 3)
  first_2002 <- tg |>
    dplyr::filter(season == 2002) |>
    dplyr::group_by(team) |>
    dplyr::slice_min(game_date, n = 1)
  expect_true(all(is.na(first_2002$net_rating_roll3)))
})

test_that("Elo stores the pre-game rating", {
  games <- tibble::tibble(
    game_id = 1:2, game_date = as.Date(c("2020-01-01", "2020-01-02")), season = 2020,
    home_team = c("A", "A"), away_team = c("B", "B"), home_win = c(1L, 1L)
  )
  elo <- compute_elo(games)
  expect_equal(elo$Elo_H[1], 1500)
  expect_equal(elo$Elo_A[1], 1500)
  expect_gt(elo$Elo_H[2], 1500)
})

test_that("season-reset Elo starts every team at baseline each season", {
  games <- build_games(fake_team_box())
  firsts <- games |>
    dplyr::group_by(season) |>
    dplyr::slice_min(game_date, n = 1)
  expect_true(all(firsts$Elo_H_season == 1500 & firsts$Elo_A_season == 1500))
})

test_that("games without a result get features but do not update Elo", {
  raw <- fake_team_box()
  last_day <- max(raw$game_date)
  pending <- raw$game_date == last_day
  stat_cols <- c(
    "team_score", "field_goals_made", "field_goals_attempted", "three_point_field_goals_made",
    "three_point_field_goals_attempted", "free_throws_made", "free_throws_attempted",
    "offensive_rebounds", "defensive_rebounds", "total_rebounds", "assists", "steals", "blocks", "turnovers"
  )
  raw_pending <- raw
  raw_pending[pending, stat_cols] <- NA

  full <- build_games(raw)
  live <- build_games(raw_pending)
  feature_cols <- grep("roll|Elo|matchup|_diff$|_ratio$", names(full), value = TRUE)
  ids <- full$game_id[full$game_date == last_day]

  expect_true(all(is.na(live$home_win[live$game_id %in% ids])))
  expect_equal(
    full[full$game_id %in% ids, feature_cols],
    live[match(full$game_id[full$game_id %in% ids], live$game_id), feature_cols]
  )
})
