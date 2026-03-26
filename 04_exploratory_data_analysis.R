options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

project_root <- "."
clean_root   <- file.path(project_root, "data", "clean")
eda_root     <- file.path(project_root, "output", "eda")
ensure_dir(eda_root)

panel <- read.csv(file.path(clean_root, "panel_city_year.csv"), stringsAsFactors = FALSE)

fmt_stats <- function(x, label) {
  x <- x[is.finite(x)]
  sprintf("  %s:  n=%d  min=%.3f  median=%.3f  mean=%.3f  max=%.3f  sd=%.3f",
          label, length(x), min(x), median(x), mean(x), max(x), sd(x))
}

cities <- sort(unique(panel$city))
years  <- sort(unique(panel$year))
n_city <- length(cities)
n_year <- length(years)
n_obs  <- nrow(panel)

# =============================================================================
# SECTION 1 — Panel Structure
# Rationale: Verifying panel balance is the first step in any longitudinal
# analysis. An unbalanced panel can bias downstream estimates if missingness is
# systematic (e.g. smaller cities have fewer observations). We need to confirm
# that the intended 6-city × 8-year grid was fully constructed before trusting
# summary statistics or model results.
# =============================================================================

model_vars <- c("log_price_index", "policy_rate_z", "intl_students_z", "housing_starts_z")
n_complete <- sum(complete.cases(panel[, model_vars]))

section1 <- c(
  "============================================================",
  "SECTION 1 — Panel Structure",
  "============================================================",
  "Rationale: Verifying panel balance is the first step in any longitudinal",
  "analysis. An unbalanced panel can bias downstream estimates if missingness",
  "is systematic (e.g. smaller cities have fewer data points). We confirm that",
  "the intended 6-city x 8-year grid was fully constructed.",
  "",
  sprintf("Cities  : %d  (%s)", n_city, paste(cities, collapse = ", ")),
  sprintf("Years   : %d  (%d - %d)", n_year, min(years), max(years)),
  sprintf("Expected rows (balanced): %d", n_city * n_year),
  sprintf("Actual rows             : %d", n_obs),
  sprintf("Rows with all model variables complete: %d  (%.1f%%)",
          n_complete, 100 * n_complete / n_obs),
  "",
  "Observation count per city:"
)
for (ct in cities) {
  section1 <- c(section1, sprintf("  %-35s  %d rows", ct, sum(panel$city == ct)))
}
section1 <- c(section1, "", "Observation count per year:")
for (yr in years) {
  section1 <- c(section1, sprintf("  %d  %d rows", yr, sum(panel$year == yr)))
}

# =============================================================================
# SECTION 2 — Outcome Variable: Housing Price Index
# Rationale: Inspecting the outcome distribution before modelling is standard
# practice. The log transformation is motivated by (a) right-skew typical of
# nominal price indices, (b) the multiplicative dynamics of housing markets
# where percentage changes are more interpretable than absolute point changes,
# and (c) improved residual normality in linear models. City-level baseline
# differences motivate the random intercept per city in the Bayesian model.
# =============================================================================

pi_vals  <- panel$price_index[is.finite(panel$price_index)]
lpi_vals <- panel$log_price_index[is.finite(panel$log_price_index)]

section2 <- c(
  "",
  "============================================================",
  "SECTION 2 — Outcome Variable: Housing Price Index",
  "============================================================",
  "Rationale: Inspecting the outcome distribution before modelling is",
  "standard practice. The log transformation is motivated by (a) right-skew",
  "typical of nominal price indices, (b) multiplicative housing market",
  "dynamics where percentage changes are more interpretable, and (c) improved",
  "residual normality. City-level baseline differences motivate the random",
  "intercept per city in the Bayesian multilevel model.",
  "",
  "--- Raw price_index (base 100 = December 2016) ---",
  fmt_stats(pi_vals, "price_index"),
  "",
  "--- Log-transformed outcome (log_price_index) ---",
  fmt_stats(lpi_vals, "log_price_index"),
  "",
  "City-level mean of log_price_index (higher = higher overall price level):",
  "  (Cities with higher means require larger random intercepts in the model.)"
)
city_means <- sort(tapply(panel$log_price_index, panel$city, mean, na.rm = TRUE),
                   decreasing = TRUE)
for (ct in names(city_means)) {
  section2 <- c(section2, sprintf("  %-35s  %.4f", ct, city_means[ct]))
}

section2 <- c(section2, "",
  "Annual mean log_price_index across all cities (temporal trend):",
  "  (A sustained upward trend motivates the time component in the model.)"
)
year_means_lpi <- tapply(panel$log_price_index, panel$year, mean, na.rm = TRUE)
for (yr in sort(as.integer(names(year_means_lpi)))) {
  section2 <- c(section2, sprintf("  %d  %.4f", yr, year_means_lpi[as.character(yr)]))
}

city_year_lpi <- reshape(
  panel[, c("city", "year", "log_price_index")],
  idvar = "city", timevar = "year", direction = "wide"
)
names(city_year_lpi) <- sub("log_price_index\\.", "", names(city_year_lpi))
write.csv(city_year_lpi, file.path(eda_root, "log_price_index_city_year.csv"), row.names = FALSE)
section2 <- c(section2, "",
  "Full city x year table written to output/eda/log_price_index_city_year.csv"
)

# =============================================================================
# SECTION 3 — Policy Rate (Bank of Canada overnight-rate target)
# Rationale: The policy rate is the main macro-level predictor and the one
# most directly linked to the popular narrative about housing prices. Documenting
# its year-by-year values confirms the data captures both the prolonged low-rate
# era (2016-2021) and the sharp tightening cycle (2022-2023). A predictor with
# almost no variation over time would contribute little explanatory power; the
# contrast between the near-zero rates of 2020-2021 and the ~4.5% rates of
# 2023 gives the model substantial signal to work with.
# =============================================================================

pr_unique <- panel[!duplicated(panel$year) & is.finite(panel$policy_rate),
                   c("year", "policy_rate")]
pr_unique <- pr_unique[order(pr_unique$year), ]

section3 <- c(
  "",
  "============================================================",
  "SECTION 3 — Policy Rate (Bank of Canada overnight-rate target)",
  "============================================================",
  "Rationale: The policy rate is the main macro-level predictor and the one",
  "most directly linked to the housing-affordability narrative. Verifying the",
  "year-by-year values confirms the data captures the prolonged low-rate era",
  "(2016-2021) and the sharp tightening cycle (2022-2023). This range of",
  "variation gives the model strong signal to identify an interest rate effect.",
  "",
  fmt_stats(pr_unique$policy_rate, "policy_rate (annual mean, %)"),
  "",
  "Annual values (this is a national variable — identical for all cities):"
)
for (i in seq_len(nrow(pr_unique))) {
  section3 <- c(section3, sprintf("  %d  %.4f%%", pr_unique$year[i], pr_unique$policy_rate[i]))
}
section3 <- c(section3, "",
  "Standardised form (policy_rate_z) used in the model:",
  fmt_stats(
    panel$policy_rate_z[!duplicated(panel$year) & is.finite(panel$policy_rate_z)],
    "policy_rate_z"
  )
)

# =============================================================================
# SECTION 4 — International Student Enrolment by CMA
# Rationale: International students are the key demand-side predictor tied
# to the policy question (Canada's international student cap). We need to
# verify (a) which CMAs have the largest student populations and thus the
# strongest potential housing demand impact, (b) whether enrolment grew
# consistently over the panel or shows a distinct pre/post-cap trend, and
# (c) whether the cross-CMA spread is large enough to identify a student
# effect distinct from a Toronto-size effect. A predictor dominated by one
# city or showing no trend is a red flag for identification.
# =============================================================================

section4 <- c(
  "",
  "============================================================",
  "SECTION 4 — International Student Enrolment by CMA",
  "============================================================",
  "Rationale: This is the predictor most directly tied to the policy question",
  "(Canada's international student cap). Understanding the CMA-level",
  "distribution, growth trend, and relative magnitudes is essential for",
  "interpreting model coefficients. If enrolment barely varies within cities",
  "or is dominated by Toronto, the enrolment effect may be hard to identify.",
  "",
  fmt_stats(panel$intl_students[is.finite(panel$intl_students)], "intl_students (annual count)"),
  "",
  "City-level mean annual international student enrolment:"
)
city_intl_mean <- sort(tapply(panel$intl_students, panel$city, mean, na.rm = TRUE),
                       decreasing = TRUE)
for (ct in names(city_intl_mean)) {
  section4 <- c(section4, sprintf("  %-35s  %8.0f", ct, city_intl_mean[ct]))
}

section4 <- c(section4, "",
  "Panel-wide total across all CMAs by year (growth trend):"
)
year_intl <- tapply(panel$intl_students, panel$year, sum, na.rm = TRUE)
for (yr in sort(as.integer(names(year_intl)))) {
  section4 <- c(section4, sprintf("  %d  %d", yr, round(year_intl[as.character(yr)])))
}

city_year_intl <- reshape(
  panel[, c("city", "year", "intl_students")],
  idvar = "city", timevar = "year", direction = "wide"
)
names(city_year_intl) <- sub("intl_students\\.", "", names(city_year_intl))
write.csv(city_year_intl, file.path(eda_root, "intl_students_city_year.csv"), row.names = FALSE)
section4 <- c(section4, "",
  "Full city x year table written to output/eda/intl_students_city_year.csv"
)

# =============================================================================
# SECTION 5 — Housing Starts by CMA
# Rationale: Housing starts proxy the supply-side response to demand shocks.
# A city that builds quickly can dampen price growth even when demand rises.
# We document cross-city levels and trends to verify that starts vary enough
# to serve as an informative supply control and to flag cities where
# construction is chronically low (where supply constraints may amplify the
# effect of demand shocks on prices).
# =============================================================================

section5 <- c(
  "",
  "============================================================",
  "SECTION 5 — Housing Starts by CMA",
  "============================================================",
  "Rationale: Housing starts proxy the supply response to demand shocks.",
  "The model predicts that more starts moderate price growth. Documenting",
  "city-level construction activity confirms sufficient cross-city variation",
  "and flags cities where chronic supply constraints may amplify demand",
  "effects on prices — an important interaction context for results.",
  "",
  fmt_stats(panel$housing_starts[is.finite(panel$housing_starts)],
            "housing_starts (annual total units)"),
  "",
  "City-level mean annual housing starts over the panel:"
)
city_starts_mean <- sort(tapply(panel$housing_starts, panel$city, mean, na.rm = TRUE),
                         decreasing = TRUE)
for (ct in names(city_starts_mean)) {
  section5 <- c(section5, sprintf("  %-35s  %8.0f units/year", ct, city_starts_mean[ct]))
}

section5 <- c(section5, "",
  "Panel-wide annual total housing starts (supply trend check):"
)
year_starts <- tapply(panel$housing_starts, panel$year, sum, na.rm = TRUE)
for (yr in sort(as.integer(names(year_starts)))) {
  section5 <- c(section5, sprintf("  %d  %.0f", yr, year_starts[as.character(yr)]))
}

city_year_starts <- reshape(
  panel[, c("city", "year", "housing_starts")],
  idvar = "city", timevar = "year", direction = "wide"
)
names(city_year_starts) <- sub("housing_starts\\.", "", names(city_year_starts))
write.csv(city_year_starts, file.path(eda_root, "housing_starts_city_year.csv"), row.names = FALSE)
section5 <- c(section5, "",
  "Full city x year table written to output/eda/housing_starts_city_year.csv"
)

# =============================================================================
# SECTION 6 — Pairwise Correlations
# Rationale: Correlations provide a model-free first look at relationships.
# The outcome-predictor correlations give a preliminary signal of which
# variables carry predictive information. Predictor-predictor correlations
# flag multicollinearity risk: if two predictors are highly correlated
# (|r| > 0.7), their individual coefficients may be imprecise in the model
# even if their joint effect is well-estimated. This is especially relevant
# for intl_students and housing_starts, which may both track city size.
# =============================================================================

corr_vars <- c("log_price_index", "policy_rate", "intl_students", "housing_starts")
corr_data <- panel[complete.cases(panel[, corr_vars]), corr_vars]
corr_mat  <- round(cor(corr_data), 3)

section6 <- c(
  "",
  "============================================================",
  "SECTION 6 — Pairwise Correlations",
  "============================================================",
  "Rationale: Outcome-predictor correlations indicate which variables carry",
  "signal. Predictor-predictor correlations flag multicollinearity: high",
  "correlation (|r| > 0.7) between two predictors can inflate coefficient",
  "uncertainty and complicate interpretation of individual effects.",
  "",
  sprintf("Computed on %d complete-case rows.", nrow(corr_data)),
  ""
)
header_row <- sprintf("  %-20s", "")
for (v in corr_vars) header_row <- paste0(header_row, sprintf("  %16s", v))
section6 <- c(section6, header_row)
for (i in seq_len(nrow(corr_mat))) {
  row_str <- sprintf("  %-20s", rownames(corr_mat)[i])
  for (j in seq_len(ncol(corr_mat))) {
    row_str <- paste0(row_str, sprintf("  %16.3f", corr_mat[i, j]))
  }
  section6 <- c(section6, row_str)
}

write.csv(as.data.frame(corr_mat), file.path(eda_root, "correlation_matrix.csv"))
section6 <- c(section6, "",
  "Full correlation matrix written to output/eda/correlation_matrix.csv"
)

# =============================================================================
# SECTION 7 — Within-city Variation of Each Variable
# Rationale: The Bayesian multilevel model estimates effects from how variables
# change over time within the same city (within-city variation), not just
# from cross-city differences. If a predictor barely moves for any given city
# over the 8-year window, the data provide little leverage to estimate a
# within-city slope. Comparing within-city SD to between-city SD helps
# diagnose whether the panel design can identify the effects of interest.
# =============================================================================

within_sd <- function(x, grp) tapply(x, grp, function(v) sd(v, na.rm = TRUE))

lpi_w <- within_sd(panel$log_price_index, panel$city)
pr_w  <- within_sd(panel$policy_rate,     panel$city)
is_w  <- within_sd(panel$intl_students,   panel$city)
hs_w  <- within_sd(panel$housing_starts,  panel$city)

between_sd <- function(x, grp) {
  city_means_b <- tapply(x, grp, mean, na.rm = TRUE)
  sd(city_means_b, na.rm = TRUE)
}

section7 <- c(
  "",
  "============================================================",
  "SECTION 7 — Within-city Variation of Each Variable",
  "============================================================",
  "Rationale: Multilevel models identify slopes from within-unit time variation.",
  "A predictor that barely moves within any city over 8 years provides little",
  "leverage for slope estimation. Comparing within-city SD to between-city SD",
  "shows whether effects are identified primarily from cross-city differences",
  "(between variation) or from each city's own trend over time (within variation).",
  "",
  "log_price_index — within-city SD (how volatile is each city's price trend):"
)
for (ct in names(sort(lpi_w, decreasing = TRUE))) {
  section7 <- c(section7, sprintf("  %-35s  %.4f", ct, lpi_w[ct]))
}
section7 <- c(section7,
  sprintf("  Between-city SD of city means: %.4f", between_sd(panel$log_price_index, panel$city)),
  "",
  "policy_rate — within-city SD (national variable, identical across cities):",
  sprintf("  %.4f  (all cities share the same national rate by construction)", pr_w[[1]]),
  "",
  "intl_students — within-city SD (does enrolment move within each CMA?):"
)
for (ct in names(sort(is_w, decreasing = TRUE))) {
  section7 <- c(section7, sprintf("  %-35s  %8.1f students/year", ct, is_w[ct]))
}
section7 <- c(section7,
  sprintf("  Between-city SD of city means: %.1f", between_sd(panel$intl_students, panel$city)),
  "",
  "housing_starts — within-city SD (does construction pace vary within cities?):"
)
for (ct in names(sort(hs_w, decreasing = TRUE))) {
  section7 <- c(section7, sprintf("  %-35s  %8.1f units/year", ct, hs_w[ct]))
}
section7 <- c(section7,
  sprintf("  Between-city SD of city means: %.1f", between_sd(panel$housing_starts, panel$city))
)

# =============================================================================
# SECTION 8 — Missingness
# Rationale: Missing values reduce effective sample size and can bias results
# if the missingness is not random (e.g. StatCan suppresses small-cell counts
# with '..', which tends to happen for smaller cities — exactly the ones with
# lower housing activity). Locating which city-years are incomplete, and for
# which variables, allows an informed decision on imputation or exclusion.
# =============================================================================

miss_vars <- c("price_index", "log_price_index", "policy_rate",
               "intl_students", "housing_starts",
               "policy_rate_z", "intl_students_z", "housing_starts_z")

section8 <- c(
  "",
  "============================================================",
  "SECTION 8 — Missingness",
  "============================================================",
  "Rationale: Missing data in a panel can bias results if not missing",
  "completely at random (MCAR). StatCan suppresses small-cell counts with",
  "'..' which tends to affect smaller cities — exactly those with lower",
  "housing activity. Locating which city-years are incomplete, and for",
  "which variables, allows an informed decision on imputation or exclusion",
  "and prevents silent list-wise deletion inside the model.",
  "",
  "Missing value counts by variable:"
)
for (v in miss_vars) {
  if (v %in% names(panel)) {
    n_miss <- sum(is.na(panel[[v]]))
    pct    <- 100 * n_miss / nrow(panel)
    section8 <- c(section8, sprintf("  %-25s  %2d missing  (%.1f%%)", v, n_miss, pct))
  }
}

incomplete <- panel[
  is.na(panel$log_price_index) | is.na(panel$intl_students) | is.na(panel$housing_starts),
  c("city", "year", "log_price_index", "intl_students", "housing_starts")
]

if (nrow(incomplete) > 0) {
  section8 <- c(section8, "",
    sprintf("City-year cells with at least one missing model variable (%d rows):", nrow(incomplete))
  )
  for (i in seq_len(nrow(incomplete))) {
    r <- incomplete[i, ]
    flags <- c(
      if (is.na(r$log_price_index)) "log_price_index",
      if (is.na(r$intl_students))   "intl_students",
      if (is.na(r$housing_starts))  "housing_starts"
    )
    section8 <- c(section8,
      sprintf("  %-35s  %d  missing: %s", r$city, r$year, paste(flags, collapse = ", "))
    )
  }
  write.csv(incomplete, file.path(eda_root, "missing_cells.csv"), row.names = FALSE)
  section8 <- c(section8, "",
    "Incomplete rows written to output/eda/missing_cells.csv"
  )
} else {
  section8 <- c(section8, "",
    "No city-year cells are missing any model variable — panel is complete."
  )
}

# =============================================================================
# WRITE REPORT
# =============================================================================

report <- c(
  "Exploratory Data Analysis Report",
  "Housing Price Analysis in Major Ontario Cities",
  "===============================================",
  sprintf("Data file  : data/clean/panel_city_year.csv  (%d rows, %d columns)",
          nrow(panel), ncol(panel)),
  "",
  "This report documents the structure, distributions, trends, correlations,",
  "and data quality of the panel dataset used for Bayesian multilevel modelling.",
  "Each section states its rationale so that the diagnostic choices are",
  "transparent and reproducible.",
  "",
  section1,
  section2,
  section3,
  section4,
  section5,
  section6,
  section7,
  section8,
  "",
  "===============================================",
  "END OF REPORT"
)

writeLines(report, file.path(eda_root, "eda_report.txt"))
cat("EDA report written to output/eda/eda_report.txt\n")
cat("Supporting tables written to output/eda/\n")
