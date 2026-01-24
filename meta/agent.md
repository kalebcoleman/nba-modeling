Project: NBA rolling metrics for STA 478 final (predicting home wins from hoopR box scores, seasons 2020–2025 regular season).

Key files:
- `R/AdvancedStats.R`: pulls team box scores via `hoopR::load_nba_team_box`, computes advanced stats, rolling features (3/5/10-game), matchup differentials/ratios, and saves `nba_games_modeling.RData` with `games_combined` and related objects. Set `RUN_ADVANCED_STATS=true` to execute; `SEASONS <- 2002:2025`.
- `R/InferentialModel.R`: fits logistic regression on a lean set of matchup differentials (adds Elo diff if available); saves odds ratios to `outputs/logit_odds_ratios.csv` and model object to `outputs/logit_fit.rds`.
- `R/PredictiveModels.R`: lean/fast run with inferential logistic plus ridge and elastic net on all features (Elo included if available). Uses most recent season as holdout. Outputs metrics/calibration to `outputs/predictive_metrics.csv` and `outputs/predictive_results.rds`. (Cross-validation moved to `R/PredictiveModels_CV.R`; ensemble trees removed.)
- `R/ThresholdBootstrap.R`: post-process best available model (prefers fast Elo run if present) to tune the classification threshold and bootstrap accuracy (default vs tuned); writes `outputs/threshold_grid_accuracy.csv` and `outputs/threshold_bootstrap_summary.csv`.
- `R/EloPlot.R`: generates `outputs/elo_trends.png` showing all-time Elo trajectories for top teams (requires Elo columns from `R/AdvancedStats.R`).
- `R/testing.R`: earlier benchmarking script (train<=2024 vs test=2025) with ridge logistic, random forest, backward stepwise, and a simple ensemble; kept only as a legacy check, not used in the essay.
- `Proposal.Rmd`/`Proposal.pdf`: proposal write-up with initial EDA plots on matchup net rating, eFG%, and turnover differentials.
- `STA_478_Project_Details.pdf`: course requirements (proposal + 4-page essay + flash talk); figures/tables must be referenced in text; final essay includes intro, data evaluation, methods, analysis results with CV, discussion, conclusions, citations, annotated code appendix, and cleaned `.RData`.
- `hw/`: archived homework solutions (tree/forest/boosting examples live here only as references and are not part of the final pipeline).
  - `STA_478_F25_Assignment_7_Kaleb_Coleman.Rmd`: VIF helper (`compute_vif`, `stepwise_vif`), multicollinearity diagnostics, glmnet workflows for ridge/LASSO/elastic-net with CV and coefficient tables.
  - `STA_478_Assignment_8.Rmd`: tree/forest/boosting examples (classification/regression trees, pruning via `cv.tree` + `prune.misclass`, bagging/RF, boosting/BART); used as template for tree integration.
  - `STA_478_F25_Assignment_6_Solutions.pdf`: bootstrap utilities (inclusion fraction ~63% unique obs in bootstrap, `bootstrap.included`), Carseats regression with backward/forward bootstrap variable inclusion tables (1,000 reps), 10x10 CV vs bootstrap-selected models, 10k bootstrap coefficient histograms, and LOOCV classification via `boot::cv.glm`.
  - `STA_478_F25_Assignment_5_Solutions.pdf`: classification workflow on Auto data with cross-validated LDA/QDA/logistic/KNN over two feature sets and multiple k values; exploration of best-subset grids (8k+ models) with repeated 66/33 splits, showing QDA/LDA/logit comparisons and handling of factor-level sparsity.
  - `STA_478_F25_Assignment_4_Solutions.pdf`: curse-of-dimensionality intuition for KNN; logistic inversion example; fuel2001 linear-model CV loop (1,000 iterations comparing one/two-predictor models); reinforces hand-rolled CV patterns.
  - `STA_478_F25_Assignment_3_Solutions.pdf`: Beta distribution simulation; helper for summary stats; Monte Carlo comparisons of moments/quantiles vs analytic values.
  - `STA_478_F25_Assignment_2_Solutions.pdf`: inverse CDF sampler template and accept-reject sampler template (generic, parameterized functions).
  - `STA_478_F25_Assignment_1_Solutions.pdf`: tidyverse basics, factor recoding, filtering subsets (e.g., ChickWeight day filtering), simple data prep patterns.
  - Earlier assignments (1–6) as PDFs for reference (basic regression/EDA patterns if needed).

Professor feedback to address:
- Clarify terminology: define “rolling signal,” “lagged” metrics, and how windows avoid leakage.
- Tie all figures/tables into the narrative; add variable description/type columns to tables.
- Prefer dropping first-game NA rows; if imputing, explain carefully.
- Consider grouping by win/loss for additional tests beyond logistic smoothing.
- Keep EDA + modeling well linked (tables that show variables used).

Quick reproduction notes:
- From repo root: `Rscript R/AdvancedStats.R` writes `nba_games_modeling.RData`. Ensure internet/API access for hoopR.
- Then source the data in analysis scripts: `load("nba_games_modeling.RData")`.
- Run `Rscript R/testing.R` after loading data to refresh baseline model metrics.

Next actions for main project:
- Start final essay Rmd with required sections (Intro, Data eval, Methods, Results with CV, Discussion, Conclusions, Appendix). Include captions and in-text references for every figure/table.
- Expand variable tables with names, descriptions, and types; include note on dropped first-game rows.
- Add win/loss group comparisons (e.g., t-tests or boxplots) for key rolling features to complement logistic curves.
- Revisit rolling window choices (3/5/10) and document; confirm no leakage. (Implemented in data; document in writeup.)
- Prepare cleaned `.RData` bundle and ensure code chunks are annotated for the appendix and flash talk.

Current modeling plan (feature representations and testing):
- Representations to test:
  1. Team-only levels: home/away rolling metrics separately (e.g., `home_net_rating_roll5`, `away_net_rating_roll5`).
  2. Matchup differentials: home minus away for each stat (net rating, eFG%, TS%, ORB%, DRB%, TOV%).
  3. Ratios where meaningful: ORB%, DRB%, and TOV% ratios; avoid where denominators are near zero.
  4. Interaction candidates: net rating × TOV% (fast/sloppy), eFG% × ORB% (shooting + second chances), rest/games_played if available.
  5. Window sensitivity: roll3 vs roll5 vs roll10 to see which horizon predicts best.
- How to test “best” representation:
  - Inferential: small set of matchup differentials (net rating, eFG%, TOV%) with odds ratios and CIs.
  - Predictive: feature sets A (team-level), B (differentials), C (differentials + ratios), D (mixed + interactions); fit ridge/logistic and RF; compare AUC/accuracy/calibration on holdout and via CV.
- Quick checklist:
  - Add roll3/roll10 windows in `AdvancedStats.R` and generate diff/ratio features.
  - Extend `PredictiveModels.R` to loop over feature sets A–D, logging CV and holdout metrics.
  - Keep `InferentialModel.R` focused on a lean differential set for interpretability.
