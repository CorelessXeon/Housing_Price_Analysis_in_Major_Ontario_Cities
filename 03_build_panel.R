options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

z_score <- function(x) {
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (sum(keep) < 2) {
    return(out)
  }
  out[keep] <- (x[keep] - mean(x[keep])) / stats::sd(x[keep])
  out
}

project_root <- "."
clean_root <- file.path(project_root, "data", "clean")
output_root <- file.path(project_root, "output")
ensure_dir(output_root)
ensure_dir(file.path(output_root, "tables"))
ensure_dir(file.path(output_root, "notes"))

target_cities <- c(
  "Toronto",
  "Ottawa-Gatineau",
  "Hamilton",
  "Kitchener-Cambridge-Waterloo",
  "London",
  "St. Catharines-Niagara"
)
panel_dates <- seq(as.Date("2016-01-01"), as.Date("2024-12-01"), by = "month")

panel <- expand.grid(
  date = panel_dates,
  city = target_cities,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

housing_prices <- read.csv(file.path(clean_root, "housing_prices_monthly.csv"), stringsAsFactors = FALSE)
student_inflows <- read.csv(file.path(clean_root, "student_inflows_monthly.csv"), stringsAsFactors = FALSE)
policy_rate <- read.csv(file.path(clean_root, "policy_rate_monthly.csv"), stringsAsFactors = FALSE)
housing_starts <- read.csv(file.path(clean_root, "housing_starts_monthly.csv"), stringsAsFactors = FALSE)

housing_prices$date <- as.Date(housing_prices$date)
student_inflows$date <- as.Date(student_inflows$date)
policy_rate$date <- as.Date(policy_rate$date)
housing_starts$date <- as.Date(housing_starts$date)

panel <- merge(
  panel,
  housing_prices[, c("date", "city", "price_index")],
  by = c("date", "city"),
  all.x = TRUE,
  sort = TRUE
)
panel <- merge(
  panel,
  student_inflows[, c("date", "city", "study_permit_inflow_proxy")],
  by = c("date", "city"),
  all.x = TRUE,
  sort = TRUE
)
panel <- merge(
  panel,
  housing_starts[, c("date", "city", "housing_starts")],
  by = c("date", "city"),
  all.x = TRUE,
  sort = TRUE
)
panel <- merge(
  panel,
  policy_rate[, c("date", "policy_rate")],
  by = "date",
  all.x = TRUE,
  sort = TRUE
)

panel$year <- as.integer(format(panel$date, "%Y"))
panel$month <- as.integer(format(panel$date, "%m"))
panel$log_price_index <- ifelse(
  is.na(panel$price_index) | panel$price_index <= 0,
  NA_real_,
  log(panel$price_index)
)
panel$policy_rate_z <- z_score(panel$policy_rate)
panel$study_permit_inflow_proxy_z <- z_score(panel$study_permit_inflow_proxy)
panel$housing_starts_z <- z_score(panel$housing_starts)
panel <- panel[order(panel$city, panel$date), ]

write.csv(panel, file.path(clean_root, "panel_city_month.csv"), row.names = FALSE)

missingness <- data.frame(
  variable = c("price_index", "log_price_index", "policy_rate", "study_permit_inflow_proxy", "housing_starts"),
  missing_count = c(
    sum(is.na(panel$price_index)),
    sum(is.na(panel$log_price_index)),
    sum(is.na(panel$policy_rate)),
    sum(is.na(panel$study_permit_inflow_proxy)),
    sum(is.na(panel$housing_starts))
  ),
  observed_count = c(
    sum(!is.na(panel$price_index)),
    sum(!is.na(panel$log_price_index)),
    sum(!is.na(panel$policy_rate)),
    sum(!is.na(panel$study_permit_inflow_proxy)),
    sum(!is.na(panel$housing_starts))
  )
)
write.csv(missingness, file.path(output_root, "tables", "panel_missingness.csv"), row.names = FALSE)

panel_note <- c(
  "Panel construction note",
  "=======================",
  "Unit of analysis: city-month.",
  "Date window: 2016-01 through 2024-12.",
  "Outcome: NHPI total (house and land), transformed to log(price_index).",
  "Policy rate: monthly mean of local daily series from lookup.csv.",
  "Student inflow: local prebuilt city proxy, plus St. Catharines-Niagara supplement from study permits.csv.",
  "Housing starts: city-specific total units from 3410015601-eng.csv.",
  "Consequence: housing supply is now available at the city-month level for all six target geographies."
)
writeLines(panel_note, file.path(output_root, "notes", "panel_note.txt"))

cat("Panel dataset written to data/clean/panel_city_month.csv.\n")
