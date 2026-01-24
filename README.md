# Leakage-Aware NBA Modeling

This project builds leakage-free rolling performance features from NBA team box scores and models home-win probability. It includes a reproducible data-engineering pipeline, an interpretable inferential model, and predictive baselines with holdout evaluation.

## Project Structure
- `R/` - Core scripts (exported functions)
- `reports/` - Final essay and generated PDF
- `proposal/` - Proposal and related files
- `presentation/` - Slide deck
- `data/` - Local modeling data (ignored by git)
- `outputs/` - Model outputs (ignored by git)

## Quick Start
Run from the project root:

```r
# Build modeling data
source("R/AdvancedStats.R")
run_advanced_stats()

# Fit inferential model
source("R/InferentialModel.R")
run_inferential_model()

# Fit predictive models (holdout + CV + bootstrap)
source("R/PredictiveModels.R")
run_predictive_models(run_cv = TRUE, run_bootstrap = TRUE)

# Optional: Elo plot
source("R/EloPlot.R")
run_elo_plot()
```

## Key Outputs
- `outputs/predictive_summary_table.csv` - holdout accuracy/AUC summary
- `outputs/predictive_cv_metrics.csv` - CV summary (if enabled)
- `outputs/threshold_bootstrap_summary.csv` - bootstrap summary (if enabled)
- `outputs/logit_fit.rds`, `outputs/logit_odds_ratios.csv` - inferential model outputs

## Notes
- The `data/` and `outputs/` folders are ignored in git to avoid large binary files.
- Models are evaluated using a final-season holdout (2025) to preserve time ordering.
