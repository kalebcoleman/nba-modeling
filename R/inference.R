# Inferential logistic regression on the training seasons (holdout excluded).
#
# Rate differentials (eFG%, ORB%, TOV%) are stored as proportions. They are
# multiplied by 100 here so odds ratios read "per percentage point".

PERCENT_TERMS <- c("orb_roll5_diff", "tov_roll5_diff", "efg_roll5_diff")

fit_inferential <- function(frame, features, holdout_season) {
  train <- frame |>
    dplyr::filter(season < holdout_season) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(PERCENT_TERMS), ~ .x * 100))
  fit <- stats::glm(stats::reformulate(features, "home_win"), data = train, family = stats::binomial())
  broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::transmute(
      term,
      unit = dplyr::case_when(
        term %in% PERCENT_TERMS ~ "percentage point",
        grepl("^Elo", term) ~ "Elo point",
        TRUE ~ "rating point per 100 possessions"
      ),
      odds_ratio = estimate, ci_low = conf.low, ci_high = conf.high, p_value = p.value
    )
}
