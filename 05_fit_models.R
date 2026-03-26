options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

project_root <- "."
clean_root   <- file.path(project_root, "data", "clean")
model_root   <- file.path(project_root, "output", "model")
ensure_dir(model_root)

if (!requireNamespace("brms", quietly = TRUE)) {
  stop("brms is required. Install it with install.packages('brms').")
}

# =============================================================================
# DATA PREPARATION
# Rename panel columns to the names used in the model formulas.
# =============================================================================

panel <- read.csv(file.path(clean_root, "panel_city_year.csv"), stringsAsFactors = FALSE)

model_data <- data.frame(
  log_hpi       = panel$log_price_index,
  intl_students = panel$intl_students,
  policy_rate   = panel$policy_rate,
  house_supply  = panel$housing_starts,
  cma           = panel$city,
  year          = panel$year,
  stringsAsFactors = FALSE
)
model_data <- model_data[complete.cases(model_data), ]
model_data$cma <- factor(model_data$cma)

# Pre-scale predictors to ensure consistency between fixed and random effects
# and to avoid issues when subsetting for no-pooling comparisons later.
model_data$intl_students_s <- as.numeric(scale(model_data$intl_students))
model_data$house_supply_s  <- as.numeric(scale(model_data$house_supply))

cat(sprintf("Rows used for modelling: %d across %d CMAs\n",
            nrow(model_data), nlevels(model_data$cma)))

# =============================================================================
# PRIORS
#
# Intercept ~ normal(4.8, 0.5)
#   log(HPI) spans 4.57-5.15 in this dataset; 4.8 is a weakly informative
#   anchor within that range without dominating the likelihood.
#
# intl_students_s ~ skew_normal(0.1, 0.1, 3)
#   Encodes a slight positive prior belief (more students -> more rental demand
#   -> upward pressure on HPI) while the skewness = 3 keeps most mass above
#   zero. The scale (0.1) is small enough that the data can pull the estimate
#   to zero or negative if warranted.
#
# policy_rate, house_supply_s ~ normal(0, 0.5)
#   Weakly informative, centred at zero; no strong directional prior assumed.
#
# sd (group-level intercept and slope) ~ exponential(1)
#   Controls how much the 6 CMAs are allowed to deviate from the population
#   mean for both intercept and student slope (partial pooling).
#
# cor ~ lkj(2)
#   Mildly regularises the correlation between the random intercept and slope.
# =============================================================================

priors_base <- c(
  brms::set_prior("normal(4.8, 0.5)",         class = "Intercept"),
  brms::set_prior("skew_normal(0.1, 0.1, 3)", class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",           class = "b"),
  brms::set_prior("exponential(1)",           class = "sd"),
  brms::set_prior("exponential(1)",           class = "sigma"),
  brms::set_prior("lkj(2)",                   class = "cor")
)

priors_supply <- c(
  brms::set_prior("normal(4.8, 0.5)",         class = "Intercept"),
  brms::set_prior("skew_normal(0.1, 0.1, 3)", class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",           class = "b"),
  brms::set_prior("exponential(1)",           class = "sd"),
  brms::set_prior("exponential(1)",           class = "sigma"),
  brms::set_prior("lkj(2)",                   class = "cor")
)

options(mc.cores = max(1L, min(4L, parallel::detectCores(logical = TRUE))))

# =============================================================================
# MODEL 1 — Base
# Tests the effect of international students on log(HPI) across CMAs,
# with a varying intercept and varying student slope per CMA.
# =============================================================================

fit1 <- brms::brm(
  formula    = brms::bf(
    log_hpi ~ intl_students_s + policy_rate + (1 + intl_students_s | cma)
  ),
  data       = model_data,
  family     = gaussian(),
  prior      = priors_base,
  backend    = "rstan",
  chains     = 4,
  iter       = 2000,
  warmup     = 1000,
  seed       = 20260323,
  save_pars  = brms::save_pars(all = TRUE),
  control    = list(adapt_delta = 0.99, max_treedepth = 15),
  file       = file.path(model_root, "fit1_base_cache"),
  file_refit = "always",
  refresh    = 0
)

saveRDS(fit1, file.path(model_root, "fit1_base.rds"))
cat("Model 1 saved to output/model/fit1_base.rds\n")

# =============================================================================
# MODEL 2 — Supply-Adjusted
# Adds house_supply_s to control for local housing supply conditions.
# Comparing the intl_students_s coefficient between Model 1 and Model 2
# is the key test for omitted variable bias: if the coefficient shifts
# substantially, housing supply was acting as a confound in Model 1.
# =============================================================================

fit2 <- brms::brm(
  formula    = brms::bf(
    log_hpi ~ intl_students_s + policy_rate + house_supply_s +
      (1 + intl_students_s | cma)
  ),
  data       = model_data,
  family     = gaussian(),
  prior      = priors_supply,
  backend    = "rstan",
  chains     = 4,
  iter       = 2000,
  warmup     = 1000,
  seed       = 20260323,
  save_pars  = brms::save_pars(all = TRUE),
  control    = list(adapt_delta = 0.99, max_treedepth = 15),
  file       = file.path(model_root, "fit2_supply_cache"),
  file_refit = "always",
  refresh    = 0
)

saveRDS(fit2, file.path(model_root, "fit2_supply.rds"))
cat("Model 2 saved to output/model/fit2_supply.rds\n")

# =============================================================================
# WRITE SPECIFICATION NOTE
# =============================================================================

coef1 <- brms::fixef(fit1)["intl_students_s", c("Estimate", "Q2.5", "Q97.5")]
coef2 <- brms::fixef(fit2)["intl_students_s", c("Estimate", "Q2.5", "Q97.5")]

spec_lines <- c(
  "Model Specifications",
  "====================",
  "",
  "Model 1 (Base):",
  "  Formula : log_hpi ~ intl_students_s + policy_rate",
  "            + (1 + intl_students_s | cma)",
  sprintf("  Observations: %d  |  CMAs: %d", nrow(model_data), nlevels(model_data$cma)),
  sprintf("  intl_students_s posterior: Estimate=%.4f, 95%% CI [%.4f, %.4f]",
          coef1["Estimate"], coef1["Q2.5"], coef1["Q97.5"]),
  "",
  "Model 2 (Supply-Adjusted):",
  "  Formula : log_hpi ~ intl_students_s + policy_rate + house_supply_s",
  "            + (1 + intl_students_s | cma)",
  sprintf("  Observations: %d  |  CMAs: %d", nrow(model_data), nlevels(model_data$cma)),
  sprintf("  intl_students_s posterior: Estimate=%.4f, 95%% CI [%.4f, %.4f]",
          coef2["Estimate"], coef2["Q2.5"], coef2["Q97.5"]),
  "",
  "Priors:",
  "  Intercept              : normal(4.8, 0.5)",
  "  scale(intl_students)   : skew_normal(0.1, 0.1, 3)",
  "  policy_rate            : normal(0, 0.5)  [default b prior]",
  "  scale(house_supply)    : normal(0, 0.5)  [default b prior]",
  "  sd (group-level)       : exponential(1)",
  "  sigma (residual SD)    : exponential(1)",
  "  cor (intercept-slope)  : lkj(2)",
  "",
  "Variable mapping from panel_city_year.csv:",
  "  log_hpi       <- log_price_index",
  "  intl_students <- intl_students",
  "  policy_rate   <- policy_rate",
  "  house_supply  <- housing_starts",
  "  cma           <- city"
)
writeLines(spec_lines, file.path(model_root, "model_specifications.txt"))
cat("Specification note written to output/model/model_specifications.txt\n")
