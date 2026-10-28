# Data and helpers for paper.qmd. Sourced from the YAML header so the
# abstract can use computed numbers; the setup chunk also lists the targets
# it reads so tar_quarto() tracks them.

library(targets)
library(dplyr)

tar_load(c(
  coverage, validation_summary, validation_by_season, holdout_metrics,
  holdout_bootstrap, odds_lean, odds_lean_elo, frame
))

pct <- function(x) sprintf("%.1f%%", 100 * x)
f3 <- function(x) sprintf("%.3f", x)
h <- function(m) filter(holdout_metrics, model == m)
b <- function(m) filter(holdout_bootstrap, model == m)
v <- function(m) filter(validation_summary, model == m)
vs <- function(m, s) filter(validation_by_season, model == m, season == s)

model_order <- c(
  "Majority (home win rate)", "Logistic: Elo only", "Logistic: lean rolling",
  "Ridge: rolling", "Ridge: rolling + Elo", "Elastic net: rolling + Elo",
  "Gradient boosting: rolling + Elo", "Neural net (MLP): rolling + Elo"
)
holdout_n <- h("Ridge: rolling + Elo")$n
home_rate_train <- mean(frame$home_win[frame$season < 2026])

auc_wide <- validation_by_season |>
  select(model, season, auc) |>
  tidyr::pivot_wider(names_from = model, values_from = auc)
n_val <- nrow(auc_wide)
n_full_beats_elo <- sum(auc_wide[["Ridge: rolling + Elo"]] > auc_wide[["Logistic: Elo only"]])
n_elo_beats_roll <- sum(auc_wide[["Logistic: Elo only"]] > auc_wide[["Ridge: rolling"]])
