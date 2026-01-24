# --- InferentialModel.R ------------------------------------------------------
# Fits logistic regression on rolling matchup differentials and saves odds ratios.
# Requires nba_games_modeling.RData (created by R/AdvancedStats.R).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(broom)
})

#' Run inferential model
#'
#' Fits the logistic regression on matchup differentials and saves
#' the fitted model and odds-ratio table to outputs/.
#'
#' @export
run_inferential_model <- function() {

if (!file.exists("data/nba_games_modeling.RData")) {
  stop("data/nba_games_modeling.RData not found. Run R/AdvancedStats.R first.")
}

load("data/nba_games_modeling.RData")

# Prefer Elo-augmented data if present
if (file.exists("outputs/games_with_elo.rds")) {
  games_df <- readRDS("outputs/games_with_elo.rds")
} else {
  games_df <- games_combined
}

games_model <- games_df %>%
  mutate(season_start = if (is.numeric(season)) as.integer(season) else as.integer(substr(season, 1, 4)))

  feat <- c(
    "matchup_net_roll5", "matchup_ortg_drtg_roll5",
    "orb_roll5_diff", "tov_roll5_diff",
    "efg_roll5_diff"
  )
# Add an Elo differential if available for interpretability
elo_opts <- c("Elo_Diff_alltime", "Elo_Diff_season")
elo_to_use <- elo_opts[elo_opts %in% names(games_model)]
feat <- unique(c(feat, elo_to_use))

  model_df <- games_model %>%
    mutate(efg_roll5_diff = home_efg_roll5 - away_efg_roll5) %>%
    select(home_win, season_start, all_of(feat)) %>%
  mutate(across(all_of(feat), as.numeric)) %>%
  filter(if_all(all_of(feat), ~ is.finite(.)))

# Train on all seasons prior to the most recent (hold out latest for prediction/plots)
holdout_season <- max(model_df$season_start, na.rm = TRUE)
train <- filter(model_df, season_start < holdout_season)

logit_fit <- glm(home_win ~ . - season_start, data = train, family = binomial())

or_table <- broom::tidy(logit_fit, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    term,
    odds_ratio = estimate,
    ci_low = conf.low,
    ci_high = conf.high,
    p_value = p.value
  )

dir.create("outputs", showWarnings = FALSE)
saveRDS(logit_fit, "outputs/logit_fit.rds")
write_csv(or_table, "outputs/logit_odds_ratios.csv")

  message("Saved inferential results to outputs/logit_fit.rds and outputs/logit_odds_ratios.csv")
  print(or_table)

}

if (identical(Sys.getenv("RUN_INFERENTIAL_MODEL"), "true")) {
  run_inferential_model()
}
