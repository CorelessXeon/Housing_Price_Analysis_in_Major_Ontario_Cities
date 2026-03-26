options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

flatten_coef_summary <- function(coef_array) {
  cities <- dimnames(coef_array)[[1]]
  stats <- dimnames(coef_array)[[2]]
  terms <- dimnames(coef_array)[[3]]
  out <- list()
  idx <- 1
  for (city in cities) {
    for (term in terms) {
      row <- as.list(coef_array[city, , term])
      row$city <- city
      row$term <- term
      out[[idx]] <- row
      idx <- idx + 1
    }
  }
  df <- do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
  keep_cols <- c("city", "term", stats)
  df[, keep_cols]
}

project_root <- "."
model_root <- file.path(project_root, "output", "model")
fig_root <- file.path(project_root, "output", "figures")
table_root <- file.path(project_root, "output", "tables")
ensure_dir(fig_root)
ensure_dir(table_root)

if (!requireNamespace("brms", quietly = TRUE)) {
  stop("The brms package is not available. Install it before running 05_diagnostics_and_outputs.R.")
}
if (!requireNamespace("bayesplot", quietly = TRUE)) {
  stop("The bayesplot package is not available. Install it before running 05_diagnostics_and_outputs.R.")
}
if (!requireNamespace("posterior", quietly = TRUE)) {
  stop("The posterior package is not available. Install it before running 05_diagnostics_and_outputs.R.")
}

fit <- readRDS(file.path(model_root, "brms_fit.rds"))
fit_summary <- summary(fit)

fixed_df <- data.frame(parameter = rownames(fit_summary$fixed), fit_summary$fixed, row.names = NULL)
spec_df <- data.frame(parameter = rownames(fit_summary$spec_pars), fit_summary$spec_pars, row.names = NULL)

random_city <- fit_summary$random$city
random_df <- data.frame(
  parameter = paste0("city_", rownames(random_city)),
  random_city,
  row.names = NULL
)

main_summary_df <- rbind(fixed_df, spec_df, random_df)
write.csv(main_summary_df, file.path(table_root, "posterior_summary_main.csv"), row.names = FALSE)

city_effects_df <- flatten_coef_summary(coef(fit, summary = TRUE)$city)
write.csv(city_effects_df, file.path(table_root, "city_effects_summary.csv"), row.names = FALSE)

all_draws <- posterior::as_draws_df(fit)
draws_df <- as.data.frame(all_draws)
param_names <- setdiff(names(draws_df), c(".chain", ".iteration", ".draw"))
summary_df <- data.frame(
  variable = param_names,
  mean = vapply(draws_df[param_names], mean, numeric(1)),
  sd = vapply(draws_df[param_names], stats::sd, numeric(1)),
  q2_5 = vapply(draws_df[param_names], function(x) stats::quantile(x, 0.025), numeric(1)),
  q97_5 = vapply(draws_df[param_names], function(x) stats::quantile(x, 0.975), numeric(1)),
  row.names = NULL
)
convergence_df <- posterior::summarise_draws(
  posterior::as_draws_matrix(fit),
  posterior::rhat,
  posterior::ess_bulk,
  posterior::ess_tail
)
diagnostics_df <- merge(summary_df, convergence_df, by.x = "variable", by.y = "variable", all.x = TRUE, sort = FALSE)
write.csv(diagnostics_df, file.path(table_root, "all_parameter_diagnostics.csv"), row.names = FALSE)

trace_parameters <- c(
  "b_policy_rate_z",
  "b_intl_students_z",
  "b_housing_starts_z",
  "sigma"
)
trace_parameters <- trace_parameters[trace_parameters %in% colnames(all_draws)]

grDevices::png(file.path(fig_root, "trace_main_parameters.png"), width = 1400, height = 900)
print(bayesplot::mcmc_trace(posterior::as_draws_array(fit), pars = trace_parameters))
grDevices::dev.off()

grDevices::png(file.path(fig_root, "posterior_predictive_checks.png"), width = 1400, height = 900)
print(brms::pp_check(fit, ndraws = 50))
grDevices::dev.off()

city_plot_data <- city_effects_df[city_effects_df$term == "Intercept", ]
grDevices::png(file.path(fig_root, "city_random_intercepts.png"), width = 1000, height = 700)
par(mar = c(5, 12, 4, 2))
plot(
  city_plot_data$Estimate,
  seq_len(nrow(city_plot_data)),
  xlim = range(c(city_plot_data$Q2.5, city_plot_data$Q97.5), na.rm = TRUE),
  yaxt = "n",
  ylab = "",
  xlab = "Posterior city intercept",
  pch = 16,
  col = "steelblue",
  main = "Posterior City Intercepts"
)
segments(
  city_plot_data$Q2.5,
  seq_len(nrow(city_plot_data)),
  city_plot_data$Q97.5,
  seq_len(nrow(city_plot_data)),
  col = "grey50",
  lwd = 2
)
axis(2, at = seq_len(nrow(city_plot_data)), labels = city_plot_data$city, las = 2)
dev.off()

summary_lines <- c(
  "brms model summary",
  "==================",
  capture.output(print(fit_summary))
)
writeLines(summary_lines, file.path(model_root, "model_summary.txt"))

cat("Diagnostics, plots, and posterior summary tables were written to output/.\n")
