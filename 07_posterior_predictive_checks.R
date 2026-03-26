options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

project_root <- "."
model_root   <- file.path(project_root, "output", "model")
ppc_root     <- file.path(project_root, "output", "ppc")
ensure_dir(ppc_root)

for (pkg in c("brms", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with install.packages('%s').", pkg, pkg))
  }
}

fit1 <- readRDS(file.path(model_root, "fit1_base.rds"))
fit2 <- readRDS(file.path(model_root, "fit2_supply.rds"))

# =============================================================================
# POSTERIOR PREDICTIVE CHECKS — GROUPED BY CMA
#
# pp_check(type = "intervals_grouped", group = "cma") draws 4000 replicated
# datasets from the posterior predictive distribution and, for each CMA panel,
# overlays the actual log(HPI) values onto the 50% and 95% predictive
# intervals across the observed years.
#
# How to interpret:
#   - If the observed points sit comfortably inside the 95% bands for all CMAs,
#     the model captures the data-generating process well.
#   - If a CMA's observed values are consistently above or below the bands,
#     the model has a systematic bias for that city (e.g. a missing
#     city-specific trend or structural break).
#   - Comparing Model 1 vs. Model 2: narrower bands and better coverage in
#     Model 2 would confirm that adding house_supply improved the fit.
#
# x = "year" places years on the horizontal axis so the plot reads as a
# time series of predictive accuracy rather than an arbitrary index.
# =============================================================================

p_ppc1 <- brms::pp_check(fit1, type = "intervals_grouped", group = "cma", x = "year") +
  ggplot2::scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
  ggplot2::labs(
    title    = "Model 1 (Base): Posterior Predictive Intervals by CMA",
    subtitle = "Points = observed log(HPI).  Dark band = 50% PI.  Light band = 95% PI.",
    x        = "Year",
    y        = "log(HPI)"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "#E8EEF4"),
    strip.text       = ggplot2::element_text(face = "bold", size = 9),
    axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
    plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey40")
  )

p_ppc2 <- brms::pp_check(fit2, type = "intervals_grouped", group = "cma", x = "year") +
  ggplot2::scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
  ggplot2::labs(
    title    = "Model 2 (Supply-Adjusted): Posterior Predictive Intervals by CMA",
    subtitle = "Points = observed log(HPI).  Dark band = 50% PI.  Light band = 95% PI.",
    x        = "Year",
    y        = "log(HPI)"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "#EAF2EA"),
    strip.text       = ggplot2::element_text(face = "bold", size = 9),
    axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
    plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey40")
  )

ggplot2::ggsave(
  file.path(ppc_root, "ppc_model1_base.png"),
  p_ppc1, width = 13, height = 7, dpi = 150
)
ggplot2::ggsave(
  file.path(ppc_root, "ppc_model2_supply.png"),
  p_ppc2, width = 13, height = 7, dpi = 150
)

cat("PPC plots saved to output/ppc/\n")

# =============================================================================
# WRITTEN INTERPRETATION GUIDE
# =============================================================================

interp_lines <- c(
  "Posterior Predictive Check Interpretation Guide",
  "================================================",
  "",
  "Files produced:",
  "  ppc_model1_base.png   — Model 1 (Base)",
  "  ppc_model2_supply.png — Model 2 (Supply-Adjusted)",
  "",
  "What the plots show:",
  "  Each panel = one CMA.  X-axis = year (2016-2023).",
  "  Points            : observed log(HPI) for that city-year.",
  "  Dark shaded band  : 50% posterior predictive interval.",
  "  Light shaded band : 95% posterior predictive interval.",
  "  The bands are constructed by drawing 4000 replicated datasets",
  "  from p(y_rep | y, X) and computing per-observation quantiles.",
  "",
  "Pass / Fail criteria:",
  "  PASS — Observed points fall inside the 95% bands for all cities",
  "          and most years: the model's assumed data-generating process",
  "          is consistent with reality.",
  "  CONCERN — Observed points consistently above/below the bands for",
  "          a specific CMA: systematic bias, possibly a missing",
  "          city-specific trend, structural break, or outlier year.",
  "  CONCERN — Bands are extremely wide: the model is uncertain, likely",
  "          due to limited data (only 8 observations per CMA) or",
  "          high residual variance.",
  "",
  "Comparing Model 1 vs. Model 2:",
  "  If Model 2's bands are narrower and observations are better centred,",
  "  adding house_supply improved predictive accuracy — consistent with",
  "  the LOO comparison in 06_loo_comparison.R.",
  "  If the two plots look nearly identical, housing supply adds little",
  "  predictive value beyond the student and rate effects already in",
  "  Model 1."
)

writeLines(interp_lines, file.path(ppc_root, "ppc_interpretation.txt"))
cat("Interpretation guide written to output/ppc/ppc_interpretation.txt\n")
