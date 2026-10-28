# Daily live update for the current season:
#   1. bring the NBAData database up to date,
#   2. log predictions for today's games that have not started,
#   3. score every logged prediction whose game is final.
#
# Run from the repository root: Rscript scripts/live_update.R
# NBA_DB and NBA_RAW_DIR override where the season's data is kept.

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

season <- NBAData::nba_current_season()
db_path <- Sys.getenv("NBA_DB", "data/live/nbadata.sqlite")
raw_dir <- Sys.getenv("NBA_RAW_DIR", "data/live/raw")
today <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = "America/New_York"))
log_path <- file.path(LIVE_DIR, sprintf("predictions_%d.csv", season))

NBAData::nba_update(season, db_path = db_path, raw_dir = raw_dir)

history <- readRDS("data/team_box_raw.rds")
current <- NBAData::nba_team_box(season, db_path = db_path)
schedule <- NBAData::nba_schedule(season, db_path = db_path)

games <- build_games(live_team_box(history, current, schedule, dates = today))
models <- all_models(games)
fitted <- fit_live_models(model_frame(dplyr::filter(games, season < !!season)), models)

preds <- live_predict(games, schedule, fitted, models, dates = today)
n_new <- append_prediction_log(preds, log_path, source = "pre-game")
message(sprintf("Logged %d new pre-game predictions for %s", n_new, today))

# Backfill finished games the log is missing (the start of the season, or a
# day the job did not run). Rows are labeled "backfill".
logged_ids <- if (file.exists(log_path)) readr::read_csv(log_path, show_col_types = FALSE)$game_id else integer()
missing_dates <- unique(games$game_date[games$season == season & !is.na(games$home_win) & !games$game_id %in% logged_ids])
if (length(missing_dates) > 0) {
  past <- live_predict(games, schedule, fitted, models, dates = missing_dates, pending_only = FALSE)
  past <- dplyr::filter(past, !game_id %in% logged_ids)
  n_back <- append_prediction_log(past, log_path, source = "backfill")
  message(sprintf("Backfilled %d predictions for %d dates", n_back, length(missing_dates)))
}

if (file.exists(log_path)) {
  log <- readr::read_csv(log_path, show_col_types = FALSE)
  scored <- score_predictions(log, season_results(current))
  readr::write_csv(scored, file.path(LIVE_DIR, sprintf("scored_%d.csv", season)))
  summary <- live_summary(scored)
  readr::write_csv(summary, file.path(LIVE_DIR, sprintf("summary_%d.csv", season)))
  print(summary, width = Inf)
}
