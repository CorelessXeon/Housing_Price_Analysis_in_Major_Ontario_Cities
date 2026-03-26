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
# Panel is annual: 2016-2023 (upper bound set by international students data)
panel <- expand.grid(
  year = 2016:2023,
  city = target_cities,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

housing_prices <- read.csv(file.path(clean_root, "housing_prices_annual.csv"),  stringsAsFactors = FALSE)
intl_students  <- read.csv(file.path(clean_root, "intl_students_annual.csv"),   stringsAsFactors = FALSE)
policy_rate    <- read.csv(file.path(clean_root, "policy_rate_annual.csv"),     stringsAsFactors = FALSE)
housing_starts <- read.csv(file.path(clean_root, "housing_starts_annual.csv"),  stringsAsFactors = FALSE)

names(intl_students)[names(intl_students) == "cma"] <- "city"

panel <- merge(panel, housing_prices[, c("year", "city", "price_index")],    by = c("year", "city"), all.x = TRUE, sort = TRUE)
panel <- merge(panel, intl_students[,  c("year", "city", "intl_students")],  by = c("year", "city"), all.x = TRUE, sort = TRUE)
panel <- merge(panel, housing_starts[, c("year", "city", "housing_starts")], by = c("year", "city"), all.x = TRUE, sort = TRUE)
panel <- merge(panel, policy_rate[,    c("year", "policy_rate")],            by = "year",            all.x = TRUE, sort = TRUE)

panel$log_price_index <- ifelse(
  is.na(panel$price_index) | panel$price_index <= 0,
  NA_real_,
  log(panel$price_index)
)
panel$policy_rate_z    <- z_score(panel$policy_rate)
panel$intl_students_z  <- z_score(panel$intl_students)
panel$housing_starts_z <- z_score(panel$housing_starts)
panel <- panel[order(panel$city, panel$year), ]

write.csv(panel, file.path(clean_root, "panel_city_year.csv"), row.names = FALSE)

missingness <- data.frame(
  variable = c("price_index", "log_price_index", "policy_rate", "intl_students", "housing_starts"),
  missing_count = c(
    sum(is.na(panel$price_index)),
    sum(is.na(panel$log_price_index)),
    sum(is.na(panel$policy_rate)),
    sum(is.na(panel$intl_students)),
    sum(is.na(panel$housing_starts))
  ),
  observed_count = c(
    sum(!is.na(panel$price_index)),
    sum(!is.na(panel$log_price_index)),
    sum(!is.na(panel$policy_rate)),
    sum(!is.na(panel$intl_students)),
    sum(!is.na(panel$housing_starts))
  )
)
write.csv(missingness, file.path(output_root, "tables", "panel_missingness.csv"), row.names = FALSE)

panel_note <- c(
  "Panel construction note",
  "=======================",
  "Unit of analysis: city-year.",
  "Year window: 2016 through 2023 (upper bound set by international students data).",
  "Outcome: NHPI total (house and land) averaged annually, transformed to log(price_index).",
  "Policy rate: annual mean of daily series from Interest_rate.csv.",
  "International students: annual count of international students enrolled at postsecondary institutions in each CMA. Source: StatCan table 37-10-0232-01 (International_students.csv).",
  "Housing starts: annual sum of monthly starts per city from New_houses_built.csv."
)
writeLines(panel_note, file.path(output_root, "notes", "panel_note.txt"))

cat("Panel dataset written to data/clean/panel_city_year.csv.\n")




