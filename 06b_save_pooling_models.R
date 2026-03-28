options(stringsAsFactors = FALSE)

# =============================================================================
# 06b_save_pooling_models.R
#
# Fits and saves the no-pooling (city-by-city) and complete-pooling (global)
# models used in the pooling comparison plot in 06_loo_comparison.R.
#
# Run this script ONCE after 05_fit_models.R. Afterwards, 06_loo_comparison.R
# will load these cached models instead of refitting them every run.
#
# To force a refit, delete the relevant RDS files from output/model/ and
# re-run this script.
# =============================================================================

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

project_root <- "."
model_root   <- file.path(project_root, "output", "model")
clean_root   <- file.path(project_root, "data", "clean")
ensure_dir(model_root)

if (!requireNamespace("brms", quietly = TRUE)) {
  stop("brms is required. Install with install.packages('brms').")
}

# Load model data from the already-fitted multilevel model to guarantee
# identical scaling and factor levels as used in 05_fit_models.R
fit1  <- readRDS(file.path(model_root, "fit1_base.rds"))
mdata <- fit1$data
mdata$intl_students_s <- as.numeric(scale(mdata$intl_students))

options(mc.cores = max(1L, min(4L, parallel::detectCores(logical = TRUE))))

# =============================================================================
# 1. NO-POOLING MODELS (one per CMA)
# Separate Bayesian regression per city using a weak prior on the student
# slope so the city-specific data drives the estimate with minimal shrinkage.
# =============================================================================

no_pool_priors <- c(
  brms::set_prior("normal(4.8, 0.5)", class = "Intercept"),
  brms::set_prior("normal(0, 1)",      class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",    class = "b"),
  brms::set_prior("exponential(1)",    class = "sigma")
)

for (ct in levels(mdata$cma)) {
  np_path <- file.path(model_root,
                       sprintf("no_pool_%s.rds", gsub("[^A-Za-z0-9]", "_", ct)))
  cat(sprintf("Fitting no-pooling model for %s...\n", ct))
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
  saveRDS(fit_np, np_path)
  cat(sprintf("  Saved to %s\n", np_path))
}
cat("All no-pooling models saved.\n\n")

# =============================================================================
# 2. COMPLETE POOLING MODEL (single global regression, no city groups)
# =============================================================================

cp_priors <- c(
  brms::set_prior("normal(4.8, 0.5)",         class = "Intercept"),
  brms::set_prior("skew_normal(0.1, 0.1, 3)", class = "b", coef = "intl_students_s"),
  brms::set_prior("normal(0, 0.5)",           class = "b"),
  brms::set_prior("exponential(1)",           class = "sigma")
)

cat("Fitting complete pooling model...\n")
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
saveRDS(fit_cp, file.path(model_root, "complete_pool.rds"))
cat(sprintf("Complete pooling model saved to %s\n",
            file.path(model_root, "complete_pool.rds")))
