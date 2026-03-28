options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

project_root <- "."
model_root   <- file.path(project_root, "output", "model")
ppc_root     <- file.path(project_root, "output", "ppc")
clean_root   <- file.path(project_root, "data", "clean")
ensure_dir(ppc_root)

for (pkg in c("brms", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with install.packages('%s').", pkg, pkg))
  }
}

fit1 <- readRDS(file.path(model_root, "fit1_base.rds"))
fit2 <- readRDS(file.path(model_root, "fit2_supply.rds"))

# 'year' is not a predictor in the model formula so brms does not store it in
# fit$data. We reload the panel CSV and apply the same complete.cases filter
# used in 05_fit_models.R to guarantee row-for-row alignment with fit1$data.
panel_raw <- read.csv(file.path(clean_root, "panel_city_year.csv"),
                      stringsAsFactors = FALSE)
keep_cols <- c("log_price_index", "intl_students", "policy_rate",
               "housing_starts", "city", "year")
panel_cc  <- panel_raw[complete.cases(panel_raw[, keep_cols]), ]

y_obs    <- fit1$data$log_hpi
cma_vec  <- as.character(fit1$data$cma)
year_vec <- panel_cc$year          # aligned with fit1$data row order

# =============================================================================
# 1. SIMULATE FROM POSTERIOR PREDICTIVE DISTRIBUTION
# posterior_predict() draws S replicated datasets y_rep from the posterior
# predictive distribution p(y_rep | y, X). Each row is one full simulated
# dataset of the same size as the observed data. We draw 500 replicates —
# enough to estimate quantile bands stably without excessive memory use.
#
# This is the simulation step: rather than evaluating the fit at point
# estimates, we ask what range of log(HPI) trajectories would be plausible
# given the model's learned parameters. Comparing those simulated trajectories
# to the actual data is the core posterior predictive check.
# =============================================================================

cat("Simulating from posterior predictive distribution (Model 1)...\n")
yrep1 <- brms::posterior_predict(fit1, ndraws = 500)

cat("Simulating from posterior predictive distribution (Model 2)...\n")
yrep2 <- brms::posterior_predict(fit2, ndraws = 500)

# =============================================================================
# 2. GROUPED INTERVAL PLOTS — TIME SERIES VIEW
# For each CMA we compute the 50% and 95% posterior predictive intervals
# from the simulated draws and overlay the actual observed log(HPI) values.
#
# HOW TO READ THESE PLOTS:
#   - Each panel = one CMA.  X-axis = year (2016-2023).
#   - Red points       : observed log(HPI) for that city-year.
#   - Dark blue band   : 50% posterior predictive interval.
#   - Light blue band  : 95% posterior predictive interval.
#
# PASS    — Red points fall inside the 95% bands for all CMAs and most years:
#           the range of simulated trajectories is consistent with reality.
# CONCERN — Points consistently above or below the 95% band for a CMA:
#           systematic bias — possibly a missing city-specific trend or a
#           structural break (e.g. the 2022 rate spike) that the predictors
#           cannot anticipate.
#
# Comparing Model 1 vs. Model 2: narrower bands and better point coverage in
# Model 2 would confirm that adding house_supply_s improved the fit.
# =============================================================================

cat("Building interval data frames from simulations...\n")

build_interval_df <- function(yrep, y_obs, cma_vec, year_vec,
                               prob_inner = 0.50, prob_outer = 0.95) {
  lo_inner <- apply(yrep, 2, quantile, probs = (1 - prob_inner) / 2)
  hi_inner <- apply(yrep, 2, quantile, probs = 1 - (1 - prob_inner) / 2)
  lo_outer <- apply(yrep, 2, quantile, probs = (1 - prob_outer) / 2)
  hi_outer <- apply(yrep, 2, quantile, probs = 1 - (1 - prob_outer) / 2)
  data.frame(
    cma  = cma_vec,
    year = year_vec,
    y    = y_obs,
    lo50 = lo_inner,
    hi50 = hi_inner,
    lo95 = lo_outer,
    hi95 = hi_outer,
    stringsAsFactors = FALSE
  )
}

idf1 <- build_interval_df(yrep1, y_obs, cma_vec, year_vec)
idf2 <- build_interval_df(yrep2, y_obs, cma_vec, year_vec)

make_interval_plot <- function(idf, title, strip_fill) {
  ggplot2::ggplot(idf, ggplot2::aes(x = year)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo95, ymax = hi95),
      fill = "#A8C4DE", alpha = 0.5
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo50, ymax = hi50),
      fill = "#4E79A7", alpha = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = y),
      colour = "#C0392B", size = 2.2
    ) +
    ggplot2::facet_wrap(~ cma) +
    ggplot2::scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
    ggplot2::labs(
      title    = title,
      subtitle = "Red points = observed log(HPI).  Dark band = 50% PI.  Light band = 95% PI.",
      x        = "Year",
      y        = "log(HPI)"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = strip_fill),
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey40")
    )
}

p_ppc1 <- make_interval_plot(
  idf1,
  "Model 1 (Base): Posterior Predictive Intervals by CMA",
  "#E8EEF4"
)
p_ppc2 <- make_interval_plot(
  idf2,
  "Model 2 (Supply-Adjusted): Posterior Predictive Intervals by CMA",
  "#EAF2EA"
)

ggplot2::ggsave(
  file.path(ppc_root, "ppc_model1_intervals.png"),
  p_ppc1, width = 13, height = 7, dpi = 150
)
ggplot2::ggsave(
  file.path(ppc_root, "ppc_model2_intervals.png"),
  p_ppc2, width = 13, height = 7, dpi = 150
)
cat("Grouped interval plots saved.\n")

# =============================================================================
# 3. DENSITY OVERLAY BY CMA
# For each CMA we overlay the observed log(HPI) density against 50 simulated
# densities drawn from the posterior predictive distribution. If the observed
# density (red line) lies within the cloud of simulated densities (blue lines),
# the model captures the marginal distribution of that city well.
#
# This is a complementary view to Section 2:
#   - The interval plot (Section 2) checks year-by-year accuracy conditional
#     on time: does the model track the time trend for each city?
#   - The density overlay (this section) checks the overall shape of the
#     distribution unconditional on time: does the model get the spread,
#     skewness, and typical log(HPI) level right for each city?
#
# A model can pass the interval check but fail the density check if it
# consistently shifts the distribution or misrepresents the tails.
# =============================================================================

cat("Generating density overlay plots by CMA...\n")

n_dens_draws <- 50
draw_idx     <- sample(nrow(yrep1), n_dens_draws)

sim_long <- do.call(rbind, lapply(seq_along(draw_idx), function(i) {
  data.frame(
    log_hpi = yrep1[draw_idx[i], ],
    cma     = cma_vec,
    draw_id = i,
    stringsAsFactors = FALSE
  )
}))

obs_long <- data.frame(
  log_hpi = y_obs,
  cma     = cma_vec,
  stringsAsFactors = FALSE
)

p_dens <- ggplot2::ggplot() +
  ggplot2::geom_density(
    data = sim_long,
    ggplot2::aes(x = log_hpi, group = draw_id),
    colour = "#A8C4DE", linewidth = 0.3, alpha = 0.6
  ) +
  ggplot2::geom_density(
    data = obs_long,
    ggplot2::aes(x = log_hpi),
    colour = "#C0392B", linewidth = 0.9
  ) +
  ggplot2::facet_wrap(~ cma, scales = "free_y") +
  ggplot2::labs(
    title    = "Model 1 (Base): Observed vs Simulated log(HPI) Density by CMA",
    subtitle = "Red = observed distribution.  Blue lines = 50 posterior predictive draws.",
    x        = "log(HPI)",
    y        = "Density"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "#E8EEF4"),
    strip.text       = ggplot2::element_text(face = "bold", size = 9),
    plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey40")
  )

ggplot2::ggsave(
  file.path(ppc_root, "ppc_model1_density_by_cma.png"),
  p_dens, width = 11, height = 7, dpi = 150
)
cat("Density overlay plot saved to output/ppc/ppc_model1_density_by_cma.png\n")

# =============================================================================
# 4. COVERAGE STATISTICS
# Numerically validate what the plots show visually. For each CMA and each
# model, compute the empirical coverage: the fraction of actual log(HPI)
# observations that fall within the 50% and 95% posterior predictive intervals.
#
# Expected coverage under a well-calibrated model:
#   50% PI -> ~50% of observations should fall inside
#   95% PI -> ~95% of observations should fall inside
#
# Systematic over-coverage (e.g. 100% inside 95% PI) means the model is too
# uncertain — the intervals are wider than the data require.
# Systematic under-coverage (e.g. 70% inside 95% PI) means the model is
# overconfident — it misses too many real outcomes.
# With only 8 observations per CMA, individual city coverage will be noisy;
# focus on the overall pattern across all cities.
# =============================================================================

cat("Computing coverage statistics...\n")

compute_coverage <- function(yrep, y, cma_vec,
                             prob_inner = 0.50, prob_outer = 0.95) {
  lo_inner <- apply(yrep, 2, quantile, probs = (1 - prob_inner) / 2)
  hi_inner <- apply(yrep, 2, quantile, probs = 1 - (1 - prob_inner) / 2)
  lo_outer <- apply(yrep, 2, quantile, probs = (1 - prob_outer) / 2)
  hi_outer <- apply(yrep, 2, quantile, probs = 1 - (1 - prob_outer) / 2)

  in_inner <- y >= lo_inner & y <= hi_inner
  in_outer <- y >= lo_outer & y <= hi_outer

  do.call(rbind, lapply(unique(cma_vec), function(ct) {
    idx <- cma_vec == ct
    data.frame(
      cma         = ct,
      n_obs       = sum(idx),
      coverage_50 = round(mean(in_inner[idx]) * 100, 1),
      coverage_95 = round(mean(in_outer[idx]) * 100, 1),
      stringsAsFactors = FALSE
    )
  }))
}

cov1       <- compute_coverage(yrep1, y_obs, cma_vec)
cov2       <- compute_coverage(yrep2, y_obs, cma_vec)
cov1$model <- "Model 1 (Base)"
cov2$model <- "Model 2 (Supply-Adjusted)"
cov_all    <- rbind(cov1, cov2)

write.csv(cov_all, file.path(ppc_root, "ppc_coverage_stats.csv"), row.names = FALSE)
cat("Coverage statistics saved to output/ppc/ppc_coverage_stats.csv\n")

cat("\nCoverage summary (% of observations inside each PI):\n")
cat(sprintf("  %-35s  %5s  %10s  %10s  %s\n",
            "CMA", "N", "50% cov.", "95% cov.", "Model"))
for (i in seq_len(nrow(cov_all))) {
  cat(sprintf("  %-35s  %5d  %9.1f%%  %9.1f%%  %s\n",
              cov_all$cma[i], cov_all$n_obs[i],
              cov_all$coverage_50[i], cov_all$coverage_95[i],
              cov_all$model[i]))
}

# =============================================================================
# 5. WRITE REPORT
# =============================================================================

format_cov_table <- function(cov_df, model_label) {
  c(
    sprintf("  %s", model_label),
    sprintf("  %-35s  %5s  %10s  %10s",
            "CMA", "N", "50% cov.", "95% cov."),
    sprintf("  %-35s  %5s  %10s  %10s",
            "---", "-", "--------", "--------"),
    apply(cov_df, 1, function(r)
      sprintf("  %-35s  %5s  %9s%%  %9s%%",
              r["cma"], r["n_obs"], r["coverage_50"], r["coverage_95"])
    )
  )
}

overall_95_m1 <- round(mean(y_obs >= apply(yrep1, 2, quantile, 0.025) &
                              y_obs <= apply(yrep1, 2, quantile, 0.975)) * 100, 1)
overall_95_m2 <- round(mean(y_obs >= apply(yrep2, 2, quantile, 0.025) &
                              y_obs <= apply(yrep2, 2, quantile, 0.975)) * 100, 1)

report <- c(
  "Posterior Predictive Check Report",
  "==================================",
  "",
  "Models assessed:",
  "  Model 1 (Base):",
  "    log_hpi ~ intl_students_s + policy_rate + (1 + intl_students_s | cma)",
  "  Model 2 (Supply-Adjusted):",
  "    log_hpi ~ intl_students_s + policy_rate + house_supply_s",
  "    + (1 + intl_students_s + house_supply_s | cma)",
  "",
  "Simulation:",
  "  500 replicated datasets drawn from p(y_rep | y, X) via posterior_predict().",
  "  Interval plots use 50% and 95% posterior predictive intervals.",
  "  Density plots overlay 50 randomly selected draws against the observed",
  "  distribution for each CMA.",
  "",
  "--- Coverage Statistics ---",
  "  Expected: ~50% of observations inside the 50% PI,",
  "            ~95% of observations inside the 95% PI.",
  "  Systematic over-coverage  => model too uncertain (bands too wide).",
  "  Systematic under-coverage => model overconfident (misses real outcomes).",
  "  Note: with only 8 obs per CMA, city-level coverage is noisy — focus on",
  "  the overall across-CMA pattern.",
  "",
  sprintf("  Overall 95%% coverage — Model 1: %.1f%%  |  Model 2: %.1f%%",
          overall_95_m1, overall_95_m2),
  "",
  format_cov_table(cov1, "Model 1 (Base)"),
  "",
  format_cov_table(cov2, "Model 2 (Supply-Adjusted)"),
  "",
  "--- Interpretation Guide ---",
  "",
  "  Interval plots (Section 2 — time series view):",
  "    PASS    — Red points inside 95% bands for all CMAs and most years.",
  "    CONCERN — Points consistently outside the band for a specific CMA:",
  "              systematic bias, possibly a missing city-specific trend or a",
  "              structural break (e.g. the 2022 rate spike) the predictors",
  "              cannot anticipate.",
  "    MODEL 2 vs MODEL 1 — If Model 2's bands are narrower and observations",
  "              are better centred, adding house_supply_s improved the fit.",
  "",
  "  Density overlay (Section 3 — distributional view):",
  "    PASS    — Observed density (red) lies within the cloud of simulated",
  "              densities (blue) for all CMAs.",
  "    CONCERN — Observed density has a different shape, location, or heavier",
  "              tails: the model misrepresents the marginal distribution of",
  "              log(HPI) for that city.",
  "",
  "Output files:",
  "  output/ppc/ppc_model1_intervals.png       — grouped interval plot, Model 1",
  "  output/ppc/ppc_model2_intervals.png       — grouped interval plot, Model 2",
  "  output/ppc/ppc_model1_density_by_cma.png  — density overlay by CMA, Model 1",
  "  output/ppc/ppc_coverage_stats.csv         — empirical coverage table",
  "  output/ppc/ppc_report.txt                 — this report"
)

writeLines(report, file.path(ppc_root, "ppc_report.txt"))
cat("PPC report written to output/ppc/ppc_report.txt\n")
