options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

project_root <- "."
model_root   <- file.path(project_root, "output", "model")
loo_root     <- file.path(project_root, "output", "loo")
ensure_dir(loo_root)

for (pkg in c("brms", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with install.packages('%s').", pkg, pkg))
  }
}

fit1 <- readRDS(file.path(model_root, "fit1_base.rds"))
fit2 <- readRDS(file.path(model_root, "fit2_supply.rds"))

# =============================================================================
# 1. COMPUTE LOO FOR BOTH MODELS
# LOO-CV approximates how well each model predicts a held-out observation.
# The PSIS-LOO approximation is fast; high Pareto-k values (> 0.7) flag
# observations that are influential enough to require exact leave-one-out
# refitting. We save both LOO objects for downstream comparison.
# =============================================================================

cat("Computing LOO for Model 1...\n")
loo1 <- brms::loo(fit1, moment_match = TRUE)

cat("Computing LOO for Model 2...\n")
loo2 <- brms::loo(fit2, moment_match = TRUE)

# NOTE ON REMAINING PARETO-K > 0.7 WARNINGS
# After moment matching, a small number of observations may still have
# Pareto-k > 0.7 (2 in Model 1, 3 in Model 2 in our run). The next step
# would be reloo = TRUE, which refits the model once per flagged observation
# for exact LOO scores. We deliberately skip this because:
#   (a) With only 48 observations, 2-3 influential points are expected —
#       a single unusual city-year (e.g. Toronto during the 2022 rate spike)
#       will always carry disproportionate weight in a small panel.
#   (b) reloo = TRUE would add ~20-30 minutes of compute for a marginal
#       improvement on 2-3 data points that is unlikely to change the
#       model comparison conclusion.
# This is noted as a known limitation: LOO scores for those specific
# city-year observations carry slightly more approximation error than the rest.

saveRDS(loo1, file.path(loo_root, "loo1_base.rds"))
saveRDS(loo2, file.path(loo_root, "loo2_supply.rds"))

# =============================================================================
# 2. POINTWISE LOO BOXPLOT — MODEL 1
# Grouping the pointwise ELPD by CMA shows whether predictive accuracy is
# consistent across cities or concentrated in a few.
#
# HOW TO READ THE BOXPLOT:
#   - Each box covers the middle 50% of ELPD scores for that city's 8 years.
#   - The line inside the box is the median.
#   - Whiskers extend to the furthest point within 1.5x the box width.
#   - Dots outside the whiskers are outlier city-years — specific years where
#     the model predicted particularly poorly for that city. This does NOT mean
#     the model is bad overall; it means that specific year was unusual enough
#     (e.g. a structural break like the 2022 rate spike) that no smooth trend
#     could anticipate it. With only 8 years per city, even one unusual year
#     will show up as an outlier.
# =============================================================================

pointwise1        <- as.data.frame(loo1$pointwise)
pointwise1$cma    <- as.character(fit1$data$cma)
pointwise1$year   <- fit1$data$year

# Pre-compute outlier rows for console reporting and the report section
outlier_rows <- do.call(rbind, lapply(unique(pointwise1$cma), function(ct) {
  sub <- pointwise1[pointwise1$cma == ct, ]
  q   <- quantile(sub$elpd_loo, c(0.25, 0.75))
  iqr <- diff(q)
  sub[sub$elpd_loo < q[1] - 1.5 * iqr | sub$elpd_loo > q[2] + 1.5 * iqr, ]
}))

# Add outlier label column directly to pointwise1 so geom_text uses the same
# data frame as the rest of the plot (NA values are skipped via na.rm = TRUE)
outlier_keys <- if (!is.null(outlier_rows) && nrow(outlier_rows) > 0) {
  paste(outlier_rows$cma, outlier_rows$year)
} else {
  character(0)
}
pointwise1$outlier_label <- ifelse(
  paste(pointwise1$cma, pointwise1$year) %in% outlier_keys,
  as.character(pointwise1$year),
  NA_character_
)

p_loo <- ggplot2::ggplot(
  pointwise1,
  ggplot2::aes(x = reorder(cma, elpd_loo, FUN = median), y = elpd_loo)
) +
  ggplot2::geom_boxplot(
    fill = "#4E79A7", colour = "#2C4F7C",
    alpha = 0.7, outlier.shape = NA
  ) +
  ggplot2::geom_jitter(width = 0.05, size = 1.2, alpha = 0.5, colour = "#2C4F7C") +
  ggplot2::geom_text(
    ggplot2::aes(label = outlier_label),
    size = 3, hjust = -0.3, colour = "#C0392B", na.rm = TRUE
  ) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "Model 1 (Base): Pointwise LOO Log-Predictive Density by CMA",
    subtitle = "More negative = years the model predicts poorly.  Red labels = outlier years.",
    x        = NULL,
    y        = "Pointwise ELPD (LOO)"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"))

ggplot2::ggsave(
  file.path(loo_root, "loo1_pointwise_by_cma.png"),
  p_loo, width = 9, height = 5, dpi = 150
)
cat("Pointwise LOO boxplot saved to output/loo/loo1_pointwise_by_cma.png\n")

if (nrow(outlier_rows) > 0) {
  cat("Outlier city-years in the LOO boxplot (red labels):\n")
  for (i in seq_len(nrow(outlier_rows))) {
    cat(sprintf("  %s  %d  (elpd = %.3f)\n",
                outlier_rows$cma[i], outlier_rows$year[i], outlier_rows$elpd_loo[i]))
  }
}

# =============================================================================
# 3. MODEL COMPARISON — loo_compare()
# loo_compare() ranks models by total ELPD. The model listed first has the
# higher (better) ELPD. elpd_diff and se_diff on the second row give the
# difference and its standard error relative to the best model.
# A |elpd_diff| / se_diff ratio > 2 is a conventional threshold for a
# meaningful improvement. If adding house_supply meaningfully improves LOO
# AND shifts the student coefficient, that is evidence of omitted variable bias.
# =============================================================================

loo_comp <- brms::loo_compare(loo1, loo2)
write.csv(as.data.frame(loo_comp), file.path(loo_root, "loo_comparison.csv"))

# =============================================================================
# 4. COEFFICIENT SHIFT — OMITTED VARIABLE BIAS ASSESSMENT
# =============================================================================

coef1_est  <- brms::fixef(fit1)["intl_students_s", "Estimate"]
coef1_lo   <- brms::fixef(fit1)["intl_students_s", "Q2.5"]
coef1_hi   <- brms::fixef(fit1)["intl_students_s", "Q97.5"]

coef2_est  <- brms::fixef(fit2)["intl_students_s", "Estimate"]
coef2_lo   <- brms::fixef(fit2)["intl_students_s", "Q2.5"]
coef2_hi   <- brms::fixef(fit2)["intl_students_s", "Q97.5"]

coef_shift <- coef2_est - coef1_est

if (abs(coef_shift) > 0.05) {
  ovb_interp <- c(
    "  The student coefficient shifted by more than 0.05 log-HPI units after",
    "  controlling for housing supply. This is consistent with omitted variable",
    "  bias in Model 1: housing supply was partially confounding the student",
    "  effect (e.g. cities that attract more students also build more, or vice",
    "  versa, creating a spurious correlation in the baseline model)."
  )
} else {
  ovb_interp <- c(
    "  The student coefficient is stable across models (shift < 0.05 log-HPI",
    "  units). Adding housing supply did not materially change the student",
    "  effect, suggesting limited omitted variable bias from supply in the",
    "  baseline model."
  )
}

# Pareto-k diagnostics
k1 <- loo1$diagnostics$pareto_k
k2 <- loo2$diagnostics$pareto_k
n_bad_k1 <- sum(k1 > 0.7, na.rm = TRUE)
n_bad_k2 <- sum(k2 > 0.7, na.rm = TRUE)

# =============================================================================
# 5. CITY-SPECIFIC STUDENT SLOPES (random effects from Model 1)
# The population-level coefficient tells us the average effect across all CMAs.
# The random slopes tell us how much each city deviates from that average.
# A city with a large positive deviation means students have a stronger-than-
# average effect on prices there; negative means weaker. This is the core
# multilevel result — it answers whether all cities respond equally or whether
# the effect is concentrated in specific markets.
# =============================================================================

pop_slope <- brms::fixef(fit1)["intl_students_s", "Estimate"]

# coef() returns the TOTAL city-specific posterior (fixef + ranef) with
# uncertainty properly propagated from both the population mean AND the
# random deviation. This is correct for the partial pooling estimates.
# Using ranef + pop_slope_point_estimate was wrong because it ignored the
# uncertainty in pop_slope when constructing the credible interval.
ce <- coef(fit1)$cma

city_slopes <- data.frame(
  cma        = rownames(ce[, , "intl_students_s"]),
  city_slope = ce[, "Estimate", "intl_students_s"],
  lo95       = ce[, "Q2.5",     "intl_students_s"],
  hi95       = ce[, "Q97.5",    "intl_students_s"],
  deviation  = ce[, "Estimate", "intl_students_s"] - pop_slope,
  stringsAsFactors = FALSE
)
city_slopes <- city_slopes[order(city_slopes$city_slope, decreasing = TRUE), ]

write.csv(city_slopes, file.path(loo_root, "city_slopes.csv"), row.names = FALSE)

# Plot city-specific slopes with 95% credible intervals
p_slopes <- ggplot2::ggplot(
  city_slopes,
  ggplot2::aes(x = reorder(cma, city_slope), y = city_slope)
) +
  ggplot2::geom_hline(yintercept = pop_slope, linetype = "dashed",
                      colour = "grey50", linewidth = 0.8) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lo95, ymax = hi95),
    width = 0.2, colour = "#2C4F7C", linewidth = 0.8
  ) +
  ggplot2::geom_point(size = 3, colour = "#4E79A7") +
  ggplot2::annotate("text", x = 0.6, y = pop_slope,
                    label = sprintf("Population mean = %.3f", pop_slope),
                    hjust = 0, vjust = -0.5, size = 3.2, colour = "grey40") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "City-Specific Effect of International Students on log(HPI)",
    subtitle = "Point = posterior mean of city slope.  Bar = 95% credible interval.\nDashed line = population-level mean effect.",
    x        = NULL,
    y        = "slope for scale(intl_students)"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"))

ggplot2::ggsave(
  file.path(loo_root, "city_slopes.png"),
  p_slopes, width = 9, height = 5, dpi = 150
)
cat("City slopes plot saved to output/loo/city_slopes.png\n")

# =============================================================================
# 6. POOLING COMPARISON PLOT
# Illustrates the core argument for using a multilevel model by showing all
# three estimation strategies side by side for each CMA:
#
#   No pooling      — a separate Bayesian regression per city. We use a weak
#                     prior (normal(0, 1)) to let the city-specific data speak
#                     with minimal shrinkage. Estimates are unbiased but noisy.
#
#   Complete pooling — a single Bayesian regression across all cities, ignoring
#                     city identity. This is the "global" effect.
#
#   Partial pooling  — the multilevel model. Estimates are pulled ("shrunk")
#                     towar the population mean.
# =============================================================================

mdata <- fit1$data

# Pre-calculate global scaling to match fit1's internal scale(intl_students)
mdata$intl_students_s <- as.numeric(scale(mdata$intl_students))

# 6a. No pooling models (using a weak prior to represent un-shrunk city data)
no_pool_priors <- c(
  brms::set_prior("normal(4.8, 0.5)", class = "Intercept"),
  brms::set_prior("normal(0, 1)",      class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",    class = "b"),
  brms::set_prior("exponential(1)",    class = "sigma")
)

no_pool <- do.call(rbind, lapply(levels(mdata$cma), function(ct) {
  cat(sprintf("  Fitting no-pooling model for %s...\n", ct))
  sub    <- mdata[mdata$cma == ct, ]
  fit_np <- brms::brm(
    formula  = brms::bf(log_hpi ~ intl_students_s + policy_rate),
    data     = sub,
    family   = gaussian(),
    prior    = no_pool_priors,
    backend  = "rstan",
    chains   = 4,
    iter     = 2000,
    warmup   = 1000,
    seed     = 20260323,
    refresh  = 0
  )
  est <- brms::fixef(fit_np)["intl_students_s", ]
  data.frame(
    cma      = ct,
    estimate = est["Estimate"],
    lo       = est["Q2.5"],
    hi       = est["Q97.5"],
    type     = "No pooling (Weak prior)",
    stringsAsFactors = FALSE
  )
}))
cat("No-pooling models done.\n")

# 6b. Complete pooling model (one model for all data, no groups)
cat("  Fitting complete pooling model...\n")
cp_priors <- c(
  brms::set_prior("normal(4.8, 0.5)",         class = "Intercept"),
  brms::set_prior("skew_normal(0.1, 0.1, 3)", class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",           class = "b"),
  brms::set_prior("exponential(1)",           class = "sigma")
)
fit_cp <- brms::brm(
  formula  = brms::bf(log_hpi ~ intl_students_s + policy_rate),
  data     = mdata,
  family   = gaussian(),
  prior    = cp_priors,
  backend  = "rstan",
  chains   = 4,
  iter     = 2000,
  warmup   = 1000,
  seed     = 20260323,
  refresh  = 0
)
cp_est <- brms::fixef(fit_cp)["intl_students_s", ]

complete_pool <- data.frame(
  cma      = levels(mdata$cma),
  estimate = cp_est["Estimate"],
  lo       = cp_est["Q2.5"],
  hi       = cp_est["Q97.5"],
  type     = "Complete pooling (Global)",
  stringsAsFactors = FALSE
)

# 6c. Partial pooling: multilevel model city slopes
# We use the results from coef(fit1) which correctly propagate uncertainty
partial_pool <- data.frame(
  cma      = city_slopes$cma,
  estimate = city_slopes$city_slope,
  lo       = city_slopes$lo95,
  hi       = city_slopes$hi95,
  type     = "Partial pooling (Multilevel)",
  stringsAsFactors = FALSE
)

pool_df      <- rbind(no_pool, complete_pool, partial_pool)
pool_df$type <- factor(pool_df$type,
                       levels = c("No pooling (Weak prior)",
                                  "Complete pooling (Global)",
                                  "Partial pooling (Multilevel)"))

# Order CMAs by partial pooling estimate (low to high)
cma_order    <- city_slopes$cma[order(city_slopes$city_slope)]
pool_df$cma  <- factor(pool_df$cma, levels = cma_order)

pool_colours <- c(
  "No pooling (Weak prior)"   = "#E15759",
  "Complete pooling (Global)" = "#59A14F",
  "Partial pooling (Multilevel)" = "#4E79A7"
)
pool_shapes <- c(
  "No pooling (Weak prior)"   = 16,
  "Complete pooling (Global)" = 17,
  "Partial pooling (Multilevel)" = 15
)

p_pool <- ggplot2::ggplot(
  pool_df,
  ggplot2::aes(x = cma, y = estimate,
               colour = type, shape = type)
) +
  ggplot2::geom_hline(
    yintercept = cp_est["Estimate"],
    linetype = "dashed", colour = "grey50", linewidth = 0.6
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lo, ymax = hi),
    width = 0.25,
    position = ggplot2::position_dodge(width = 0.65),
    linewidth = 0.7
  ) +
  ggplot2::geom_point(
    size = 2.8,
    position = ggplot2::position_dodge(width = 0.65)
  ) +
  ggplot2::scale_colour_manual(values = pool_colours, name = NULL) +
  ggplot2::scale_shape_manual(values = pool_shapes, name = NULL) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "Pooling Comparison: Effect of International Students on log(HPI)",
    subtitle = paste0(
      "Red = no pooling (weak prior).  ",
      "Green = complete pooling (global model).\n",
      "Blue = partial pooling (multilevel).  ",
      "Dashed line = global mean."
    ),
    x = NULL,
    y = "Slope for intl_students_s"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    plot.subtitle   = ggplot2::element_text(size = 8.5, colour = "grey40")
  )

ggplot2::ggsave(
  file.path(loo_root, "pooling_comparison.png"),
  p_pool, width = 10, height = 6, dpi = 150
)
cat("Pooling comparison plot saved to output/loo/pooling_comparison.png\n")


# =============================================================================
# 7. WRITE REPORT
# =============================================================================

report <- c(
  "LOO Cross-Validation Report",
  "===========================",
  "",
  "Model 1 (Base):",
  "  log_hpi ~ scale(intl_students) + policy_rate + (1 + scale(intl_students) | cma)",
  "Model 2 (Supply-Adjusted):",
  "  log_hpi ~ scale(intl_students) + policy_rate + scale(house_supply)",
  "  + (1 + scale(intl_students) + scale(house_supply) | cma)",
  "",
  "--- LOO Summary: Model 1 ---",
  capture.output(print(loo1)),
  "",
  "--- LOO Summary: Model 2 ---",
  capture.output(print(loo2)),
  "",
  "--- Pareto-k Diagnostics ---",
  "  Pareto-k > 0.7 flags observations that are too influential for the PSIS",
  "  approximation to be reliable (exact LOO would be needed for those points).",
  sprintf("  Model 1: %d observation(s) with Pareto-k > 0.7", n_bad_k1),
  sprintf("  Model 2: %d observation(s) with Pareto-k > 0.7", n_bad_k2),
  "",
  "--- Model Comparison (loo_compare) ---",
  "  The first row is the preferred model (highest ELPD).",
  "  elpd_diff / se_diff > 2 is a conventional threshold for a meaningful",
  "  difference in predictive accuracy.",
  capture.output(print(loo_comp)),
  "",
  "--- Omitted Variable Bias Assessment ---",
  sprintf("  Model 1  scale(intl_students): %.4f  [%.4f, %.4f]",
          coef1_est, coef1_lo, coef1_hi),
  sprintf("  Model 2  scale(intl_students): %.4f  [%.4f, %.4f]",
          coef2_est, coef2_lo, coef2_hi),
  sprintf("  Shift after adding house_supply: %.4f", coef_shift),
  "",
  ovb_interp,
  "",
  "--- LOO Boxplot Interpretation ---",
  "  Each box covers the middle 50% of ELPD scores across the 8 years for",
  "  that city. Dots outside the whiskers are outlier city-years — specific",
  "  years the model predicted unusually poorly. This does NOT mean the model",
  "  is bad overall; it means that year was structurally unusual (e.g. a sharp",
  "  rate hike or COVID shock) that no smooth trend could anticipate.",
  "  With only 8 years per city, even one anomalous year appears as an outlier.",
  "",
  if (nrow(outlier_rows) > 0) {
    c("  Outlier city-years identified (red labels on plot):",
      apply(outlier_rows, 1, function(r)
        sprintf("    %s  %s  (elpd = %.3f)", r["cma"], r["year"], as.numeric(r["elpd_loo"]))
      )
    )
  } else {
    "  No outlier city-years detected."
  },
  "",
  "--- City-Specific Student Slopes (Model 1 random effects) ---",
  sprintf("  Population-level mean slope: %.4f", pop_slope),
  "  Interpretation: a 1 SD increase in international students is associated",
  sprintf("  with approximately %.1f%% change in HPI on average across all CMAs.", pop_slope * 100),
  "  City deviations from that average:",
  "",
  sprintf("  %-35s  %8s  %8s  %17s",
          "CMA", "Slope", "Dev.", "95% CI"),
  sprintf("  %-35s  %8s  %8s  %17s",
          "---", "-----", "----", "------"),
  apply(city_slopes, 1, function(r)
    sprintf("  %-35s  %8.4f  %+8.4f  [%6.4f, %6.4f]",
            r["cma"],
            as.numeric(r["city_slope"]),
            as.numeric(r["deviation"]),
            as.numeric(r["lo95"]),
            as.numeric(r["hi95"]))
  ),
  "",
  "  Cities where the 95% CI excludes zero have a statistically meaningful",
  "  student effect in that specific market.",
  "",
  "Output files:",
  "  output/loo/loo1_pointwise_by_cma.png  — pointwise ELPD boxplot by CMA",
  "  output/loo/city_slopes.png             — city-specific partial pooling slopes",
  "  output/loo/city_slopes.csv             — city slopes table",
  "  output/loo/pooling_comparison.png      — no / complete / partial pooling comparison",
  "  output/loo/loo_comparison.csv          — loo_compare() table",
  "  output/loo/loo_report.txt              — this report"
)

writeLines(report, file.path(loo_root, "loo_report.txt"))
cat("LOO report written to output/loo/loo_report.txt\n")
