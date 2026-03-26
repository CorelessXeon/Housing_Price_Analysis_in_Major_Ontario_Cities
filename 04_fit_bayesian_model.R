options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

project_root <- "."
clean_root <- file.path(project_root, "data", "clean")
model_root <- file.path(project_root, "output", "model")
ensure_dir(model_root)

if (!requireNamespace("brms", quietly = TRUE)) {
  stop("The brms package is not available. Install it before running 04_fit_bayesian_model.R.")
}

panel <- read.csv(file.path(clean_root, "panel_city_year.csv"), stringsAsFactors = FALSE)

predictors <- c("policy_rate_z", "intl_students_z", "housing_starts_z")
model_data <- panel[, c("year", "city", "log_price_index", predictors)]
model_data <- model_data[stats::complete.cases(model_data), ]
if (nrow(model_data) == 0) {
  stop("No complete rows were available for model fitting.")
}
model_data$city <- factor(model_data$city)

intercept_location <- round(mean(model_data$log_price_index), 3)

model_formula <- brms::bf(
  log_price_index ~ policy_rate_z + intl_students_z + housing_starts_z + (1 + housing_starts_z | city)
)

priors <- c(
  brms::set_prior(paste0("normal(", intercept_location, ", 1)"), class = "Intercept"),
  brms::set_prior("normal(0, 0.5)", class = "b"),
  brms::set_prior("exponential(1)", class = "sd"),
  brms::set_prior("exponential(1)", class = "sigma"),
  brms::set_prior("lkj(2)", class = "cor")
)

options(mc.cores = max(1L, min(4L, parallel::detectCores(logical = TRUE))))

fit <- brms::brm(
  formula = model_formula,
  data = model_data,
  family = gaussian(),
  prior = priors,
  backend = "rstan",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = max(1L, min(4L, parallel::detectCores(logical = TRUE))),
  seed = 20260323,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  file = file.path(model_root, "brms_fit_cache"),
  file_refit = "always",
  refresh = 0
)

saveRDS(fit, file.path(model_root, "brms_fit.rds"))

model_spec_lines <- c(
  "Model specification",
  "===================",
  "Package: brms",
  "Backend: rstan",
  "Formula: log_price_index ~ policy_rate_z + intl_students_z + housing_starts_z + (1 + housing_starts_z | city)",
  "Note: intl_students_z is the standardised annual international student enrolment count per CMA (StatCan 37-10-0232-01), merged by year onto the monthly panel.",
  sprintf("Observations used: %d", nrow(model_data)),
  sprintf("Cities used: %d", length(unique(model_data$city))),
  "Priors:",
  paste0("- ", c(
    paste0("Intercept ~ normal(", intercept_location, ", 1)"),
    "b ~ normal(0, 0.5)",
    "sd ~ exponential(1)",
    "sigma ~ exponential(1)",
    "cor ~ lkj(2)"
  )),
  "",
  "Notes:",
  "- Housing starts came from New_houses_built.csv.",
  "- The current local CMHC export contains city-specific housing-starts blocks for the six target geographies.",
  "- Because brms and rstan are installed locally, this script now uses brms instead of the earlier custom Gibbs sampler."
)
writeLines(model_spec_lines, file.path(model_root, "model_specification.txt"))

cat("brms model fit saved to output/model/brms_fit.rds.\n")
