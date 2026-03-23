options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

parse_numeric_flagged <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "..")] <- NA_character_
  x <- sub("E$", "", x)
  as.numeric(x)
}

sum_or_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

month_number <- function(x) {
  match(tolower(substr(trimws(x), 1, 3)), tolower(month.abb))
}

extract_lookup_daily <- function(csv_path) {
  lines <- readLines(csv_path, warn = FALSE)
  header_line <- grep("^Date,V39079,?\\s*$", trimws(lines))[1]
  if (is.na(header_line)) {
    stop("Could not find the Date,V39079 header in lookup.csv.")
  }
  data_lines <- trimws(lines[(header_line + 1):length(lines)])
  data_lines <- data_lines[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2},", data_lines)]
  pieces <- strsplit(data_lines, ",", fixed = TRUE)
  raw <- data.frame(
    Date = trimws(vapply(pieces, `[`, "", 1)),
    V39079 = trimws(vapply(pieces, `[`, "", 2)),
    stringsAsFactors = FALSE
  )
  raw$Date <- as.Date(raw$Date)
  raw$V39079 <- suppressWarnings(as.numeric(raw$V39079))
  raw <- raw[!is.na(raw$Date) & !is.na(raw$V39079), ]
  raw[order(raw$Date), ]
}

extract_nhpi_series <- function(raw, geography_pattern, city_name) {
  geography_row <- as.character(unlist(raw[1, ]))
  geography_row[is.na(geography_row)] <- ""
  geography_starts <- which(nzchar(trimws(geography_row)))
  start_col <- geography_starts[grepl(geography_pattern, geography_row[geography_starts], fixed = TRUE)]
  if (length(start_col) != 1) {
    stop(sprintf("Could not find NHPI geography block matching: %s", geography_pattern))
  }
  next_start <- geography_starts[match(start_col, geography_starts) + 1]
  end_col <- if (is.na(next_start)) ncol(raw) else next_start - 1

  date_labels <- as.character(unlist(raw[2, start_col:end_col]))
  values <- as.character(unlist(raw[4, start_col:end_col]))
  keep <- nzchar(trimws(date_labels)) & grepl("^[A-Za-z]+ [0-9]{4}$", date_labels)

  data.frame(
    date = as.Date(paste("01", date_labels[keep]), format = "%d %B %Y"),
    city = city_name,
    price_index = parse_numeric_flagged(values[keep]),
    source = "StatCan NHPI 18-10-0205-01",
    stringsAsFactors = FALSE
  )
}

extract_housing_starts_series <- function(raw, geography_pattern, city_name) {
  geography_row <- as.character(unlist(raw[1, ]))
  geography_row[is.na(geography_row)] <- ""
  geography_starts <- which(nzchar(trimws(geography_row)))
  start_col <- geography_starts[grepl(geography_pattern, geography_row[geography_starts], fixed = TRUE)]
  if (length(start_col) != 1) {
    stop(sprintf("Could not find housing-starts geography block matching: %s", geography_pattern))
  }
  next_start <- geography_starts[match(start_col, geography_starts) + 1]
  end_col <- if (is.na(next_start)) ncol(raw) else next_start - 1

  date_labels <- as.character(unlist(raw[2, start_col:end_col]))
  values <- as.character(unlist(raw[4, start_col:end_col]))
  keep <- nzchar(trimws(date_labels)) & grepl("^[A-Za-z]+ [0-9]{4}$", date_labels)

  data.frame(
    date = as.Date(paste("01", date_labels[keep]), format = "%d %B %Y"),
    city = city_name,
    housing_starts = parse_numeric_flagged(values[keep]),
    series_label = "Total units",
    source = "3410015601-eng.csv",
    stringsAsFactors = FALSE
  )
}

extract_housing_starts_data <- function(csv_path) {
  raw <- read.csv(
    csv_path,
    skip = 8,
    header = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  do.call(
    rbind,
    list(
      extract_housing_starts_series(raw, "Hamilton, Ontario", "Hamilton"),
      extract_housing_starts_series(raw, "Kitchener-Cambridge-Waterloo, Ontario", "Kitchener-Cambridge-Waterloo"),
      extract_housing_starts_series(raw, "London, Ontario", "London"),
      extract_housing_starts_series(raw, "Ottawa-Gatineau, Ontario part, Ontario", "Ottawa-Gatineau"),
      extract_housing_starts_series(raw, "St. Catharines-Niagara, Ontario", "St. Catharines-Niagara"),
      extract_housing_starts_series(raw, "Toronto, Ontario", "Toronto")
    )
  )
}

project_root <- "."
raw_root <- file.path(project_root, "data", "data_raw")
clean_root <- file.path(project_root, "data", "clean")
notes_root <- file.path(project_root, "output", "notes")
ensure_dir(clean_root)
ensure_dir(notes_root)

panel_start <- as.Date("2016-01-01")
panel_end <- as.Date("2024-12-01")
target_cities <- c(
  "Toronto",
  "Ottawa-Gatineau",
  "Hamilton",
  "Kitchener-Cambridge-Waterloo",
  "London",
  "St. Catharines-Niagara"
)

nhpi_raw <- read.csv(
  file.path(raw_root, "1810020501-eng.csv"),
  skip = 7,
  header = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

nhpi_clean <- do.call(
  rbind,
  list(
    extract_nhpi_series(nhpi_raw, "Toronto, Ontario", "Toronto"),
    extract_nhpi_series(nhpi_raw, "Ottawa-Gatineau, Ontario part", "Ottawa-Gatineau"),
    extract_nhpi_series(nhpi_raw, "Hamilton, Ontario", "Hamilton"),
    extract_nhpi_series(nhpi_raw, "Kitchener-Cambridge-Waterloo, Ontario", "Kitchener-Cambridge-Waterloo"),
    extract_nhpi_series(nhpi_raw, "London, Ontario", "London"),
    extract_nhpi_series(nhpi_raw, "St. Catharines-Niagara, Ontario", "St. Catharines-Niagara")
  )
)
nhpi_clean <- nhpi_clean[nhpi_clean$date >= panel_start & nhpi_clean$date <= panel_end, ]
nhpi_clean <- nhpi_clean[order(nhpi_clean$city, nhpi_clean$date), ]
write.csv(nhpi_clean, file.path(clean_root, "housing_prices_monthly.csv"), row.names = FALSE)

policy_daily <- extract_lookup_daily(file.path(raw_root, "lookup.csv"))
policy_daily$month <- as.Date(format(policy_daily$Date, "%Y-%m-01"))
policy_monthly <- aggregate(V39079 ~ month, data = policy_daily, FUN = mean)
names(policy_monthly) <- c("date", "policy_rate")
policy_monthly$source <- "lookup.csv"
policy_monthly <- policy_monthly[policy_monthly$date >= panel_start & policy_monthly$date <= panel_end, ]

write.csv(
  data.frame(date = policy_daily$Date, policy_rate = policy_daily$V39079),
  file.path(clean_root, "policy_rate_daily.csv"),
  row.names = FALSE
)
write.csv(policy_monthly, file.path(clean_root, "policy_rate_monthly.csv"), row.names = FALSE)

policy_note <- c(
  "Policy-rate note",
  "================",
  "Source: data/data_raw/lookup.csv",
  sprintf("Daily observations extracted: %d", nrow(policy_daily)),
  sprintf("Daily date range: %s to %s", format(min(policy_daily$Date), "%Y-%m-%d"), format(max(policy_daily$Date), "%Y-%m-%d")),
  "Monthly aggregation: arithmetic mean within each month."
)
writeLines(policy_note, file.path(notes_root, "policy_rate_note.txt"))

study_proxy <- read.csv(
  file.path(raw_root, "student_permits_city_proxy_2015_2025_monthly.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
study_proxy$date <- as.Date(study_proxy$date)
study_proxy <- study_proxy[study_proxy$city %in% c(
  "Toronto",
  "Ottawa",
  "Hamilton",
  "Kitchener-Cambridge-Waterloo",
  "London"
), ]
study_proxy$city[study_proxy$city == "Ottawa"] <- "Ottawa-Gatineau"
study_proxy$study_permit_inflow_proxy <- as.numeric(study_proxy$study_permit_inflow_proxy)
study_proxy$source <- "student_permits_city_proxy_2015_2025_monthly.csv"

study_raw <- read.csv(
  file.path(raw_root, "study permits.csv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
st_cath_institutions <- c(
  "Brock University",
  "Niagara College Canada",
  "University of Niagara Falls Canada"
)
st_cath_raw <- study_raw[
  study_raw$EN_DLI_PROVINCE_TERRITORY == "Ontario" &
    study_raw$EN_DESIGNATED_LEARNING_INSTITUTION %in% st_cath_institutions,
]
st_cath_raw$TOTAL_numeric <- suppressWarnings(as.numeric(st_cath_raw$TOTAL))
st_cath_raw$date <- as.Date(sprintf(
  "%04d-%02d-01",
  as.integer(st_cath_raw$EN_YEAR),
  month_number(st_cath_raw$EN_MONTH)
))
st_cath_clean <- aggregate(TOTAL_numeric ~ date, data = st_cath_raw, FUN = sum_or_na)
names(st_cath_clean)[2] <- "study_permit_inflow_proxy"
st_cath_clean$city <- "St. Catharines-Niagara"
st_cath_clean$source <- "study permits.csv supplement"

student_clean <- rbind(
  study_proxy[, c("date", "city", "study_permit_inflow_proxy", "source")],
  st_cath_clean[, c("date", "city", "study_permit_inflow_proxy", "source")]
)
student_clean <- student_clean[
  student_clean$date >= panel_start & student_clean$date <= panel_end &
    student_clean$city %in% target_cities,
]
student_clean <- student_clean[order(student_clean$city, student_clean$date), ]
write.csv(student_clean, file.path(clean_root, "student_inflows_monthly.csv"), row.names = FALSE)

student_note <- c(
  "Student inflow note",
  "===================",
  "Main source: student_permits_city_proxy_2015_2025_monthly.csv",
  "Supplement: study permits.csv for St. Catharines-Niagara only.",
  paste("Supplement institutions:", paste(st_cath_institutions, collapse = ", ")),
  "TODO: some Niagara-named institutions are geographically ambiguous and were not added automatically."
)
writeLines(student_note, file.path(notes_root, "student_inflow_note.txt"))

housing_starts_clean <- extract_housing_starts_data(file.path(raw_root, "3410015601-eng.csv"))
housing_starts_clean <- housing_starts_clean[
  housing_starts_clean$date >= panel_start & housing_starts_clean$date <= panel_end,
]
housing_starts_clean <- housing_starts_clean[order(housing_starts_clean$city, housing_starts_clean$date), ]
write.csv(housing_starts_clean, file.path(clean_root, "housing_starts_monthly.csv"), row.names = FALSE)

housing_note <- c(
  "Housing starts note",
  "===================",
  "Source: data/data_raw/3410015601-eng.csv",
  "Series used: Total units",
  "Geography available in the local export: Hamilton, Kitchener-Cambridge-Waterloo, London, Ottawa-Gatineau, St. Catharines-Niagara, Toronto",
  "The CMHC file now contains city-specific housing starts for the six target geographies, so housing_starts is merged directly at the city-month level."
)
writeLines(housing_note, file.path(notes_root, "housing_starts_note.txt"))

cat("Cleaned source files written to data/clean.\n")
