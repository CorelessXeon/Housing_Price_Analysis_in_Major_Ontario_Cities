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
    stop("Could not find the Date,V39079 header in Interest_rate.csv.")
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
    source = "New_houses_built.csv",
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

# =============================================================================
# SETUP
# =============================================================================

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

# =============================================================================
# 1. HOUSING PRICE INDEX  (Housing_price_index.csv -> housing_prices_annual.csv)
# Aggregation: annual mean of monthly price index values per city.
# =============================================================================

nhpi_raw <- read.csv(
  file.path(raw_root, "Housing_price_index.csv"),
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
nhpi_clean$year <- as.integer(format(nhpi_clean$date, "%Y"))

housing_prices_annual <- aggregate(price_index ~ year + city, data = nhpi_clean, FUN = mean)
housing_prices_annual <- housing_prices_annual[order(housing_prices_annual$city, housing_prices_annual$year), ]
write.csv(housing_prices_annual, file.path(clean_root, "housing_prices_annual.csv"), row.names = FALSE)

# =============================================================================
# 2. INTEREST RATE  (Interest_rate.csv -> policy_rate_annual.csv)
# Aggregation: annual mean of daily values.
# =============================================================================

policy_daily <- extract_lookup_daily(file.path(raw_root, "Interest_rate.csv"))
policy_daily$year <- as.integer(format(policy_daily$Date, "%Y"))
policy_daily <- policy_daily[policy_daily$year >= as.integer(format(panel_start, "%Y")) &
                               policy_daily$year <= as.integer(format(panel_end,   "%Y")), ]

policy_rate_annual <- aggregate(V39079 ~ year, data = policy_daily, FUN = mean)
names(policy_rate_annual) <- c("year", "policy_rate")
write.csv(policy_rate_annual, file.path(clean_root, "policy_rate_annual.csv"), row.names = FALSE)

policy_note <- c(
  "Policy-rate note",
  "================",
  "Source: data/data_raw/Interest_rate.csv",
  sprintf("Daily observations extracted: %d", nrow(policy_daily)),
  sprintf("Daily date range: %s to %s", format(min(policy_daily$Date), "%Y-%m-%d"), format(max(policy_daily$Date), "%Y-%m-%d")),
  "Annual aggregation: arithmetic mean of all daily values within each calendar year."
)
writeLines(policy_note, file.path(notes_root, "policy_rate_note.txt"))

# =============================================================================
# 3. INTERNATIONAL STUDENTS  (International_students.csv -> intl_students_annual.csv)
# Source: StatCan table 37-10-0232-01 — Postsecondary enrolments by institution.
# Each row in the raw file = one institution with its total international student enrolment.
# Institution names are stripped of ", Ontario [footnote]" suffix before CMA matching.
# For academic years (e.g. "2023 / 2024") the lower year (2023) is used as the year value.
# =============================================================================

intl_raw <- read.csv(
  file.path(raw_root, "International_students.csv"),
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
names(intl_raw)[1] <- "institution"

# Filter to Ontario institutions and clean names
intl_raw <- intl_raw[grepl(",\\s*Ontario", intl_raw$institution), ]
intl_raw$institution <- trimws(sub("\\s+[0-9]+(\\s+[0-9]+)*\\s*$", "",
                                   sub(",\\s*Ontario.*$", "", intl_raw$institution)))

# Assign CMA
cma_overrides_intl <- c(
  "Brock University"                                               = "St. Catharines-Niagara",
  "Carleton University"                                            = "Ottawa-Gatineau",
  "Collège dominicain de philosophie et de théologie"              = "Ottawa-Gatineau",
  "McMaster University"                                            = "Hamilton",
  "University of Ottawa-Université d'Ottawa"                       = "Ottawa-Gatineau",
  "Toronto Metropolitan University"                                = "Toronto",
  "University of Toronto"                                          = "Toronto",
  "University of Waterloo"                                         = "Kitchener-Cambridge-Waterloo",
  "Western University"                                             = "London",
  "Wilfrid Laurier University"                                     = "Kitchener-Cambridge-Waterloo",
  "York University"                                                = "Toronto",
  "OCAD University"                                                = "Toronto",
  "Ontario Tech University"                                        = "Toronto",
  "Université de l'Ontario français"                               = "Toronto",
  "La Cité collégiale d'arts appliqués et de technologie"          = "Ottawa-Gatineau",
  "Algonquin College of Applied Arts and Technology"               = "Ottawa-Gatineau",
  "Centennial College of Applied Arts and Technology"              = "Toronto",
  "Conestoga College Institute of Technology and Advanced Learning" = "Kitchener-Cambridge-Waterloo",
  "Fanshawe College of Applied Arts and Technology"                = "London",
  "George Brown College of Applied Arts and Technology"            = "Toronto",
  "Mohawk College of Applied Arts and Technology"                  = "Hamilton",
  "Niagara College of Applied Arts and Technology"                 = "St. Catharines-Niagara",
  "Humber College Institute of Technology and Advanced Learning"   = "Toronto",
  "Seneca College of Applied Arts and Technology"                  = "Toronto",
  "Sheridan College Institute of Technology and Advanced Learning"  = "Toronto"
)
intl_raw$cma <- cma_overrides_intl[intl_raw$institution]
intl_raw$cma[is.na(intl_raw$cma)] <- "Other"
intl_raw <- intl_raw[intl_raw$cma != "Other", ]

# Year columns: names like "2015 / 2016"; lower year used as the integer year value
year_cols <- setdiff(names(intl_raw), c("institution", "cma"))

# Parse all year columns: remove thousand commas, ".." -> NA
for (col in year_cols) {
  intl_raw[[col]] <- suppressWarnings(
    as.numeric(gsub(",", "", ifelse(trimws(intl_raw[[col]]) == "..", NA_character_, intl_raw[[col]])))
  )
}

# Impute ".." values:
#   Leading NAs (before the first observed value) -> 0  (institution did not yet exist)
#   Interior / trailing NAs                        -> institution mean (StatCan suppression)
for (i in seq_len(nrow(intl_raw))) {
  vals      <- as.numeric(intl_raw[i, year_cols])
  first_obs <- which(!is.na(vals))[1]
  row_mean  <- mean(vals, na.rm = TRUE)
  na_idx    <- which(is.na(vals))

  leading  <- if (is.na(first_obs)) na_idx else na_idx[na_idx < first_obs]
  interior <- if (is.na(first_obs)) integer(0) else na_idx[na_idx >= first_obs]

  intl_raw[i, year_cols[leading]]  <- 0
  intl_raw[i, year_cols[interior]] <- row_mean
}

# Reshape to long and extract integer year from "YYYY / YYYY" label
intl_long <- do.call(rbind, lapply(year_cols, function(col) {
  data.frame(
    cma     = intl_raw$cma,
    year    = as.integer(trimws(sub("\\s*/.*", "", col))),
    total_n = intl_raw[[col]],
    stringsAsFactors = FALSE
  )
}))

# Drop rows still NA (institution had ".." in every year — nothing to impute from)
intl_long <- intl_long[!is.na(intl_long$total_n), ]

# Filter to panel years and aggregate by CMA + year
intl_long <- intl_long[intl_long$year >= as.integer(format(panel_start, "%Y")) &
                        intl_long$year <= as.integer(format(panel_end,   "%Y")), ]

intl_students_clean <- aggregate(total_n ~ cma + year, data = intl_long, FUN = sum)
names(intl_students_clean)[3] <- "intl_students"
intl_students_clean <- intl_students_clean[order(intl_students_clean$cma, intl_students_clean$year), ]

write.csv(intl_students_clean, file.path(clean_root, "intl_students_annual.csv"), row.names = FALSE)

# =============================================================================
# 4. HOUSING STARTS  (New_houses_built.csv -> housing_starts_annual.csv)
# Aggregation: annual sum of monthly housing starts per city (starts are a flow variable).
# =============================================================================

housing_starts_clean <- extract_housing_starts_data(file.path(raw_root, "New_houses_built.csv"))
housing_starts_clean <- housing_starts_clean[
  housing_starts_clean$date >= panel_start & housing_starts_clean$date <= panel_end,
]
housing_starts_clean$year <- as.integer(format(housing_starts_clean$date, "%Y"))

housing_starts_annual <- aggregate(housing_starts ~ year + city, data = housing_starts_clean, FUN = sum)
housing_starts_annual <- housing_starts_annual[order(housing_starts_annual$city, housing_starts_annual$year), ]
write.csv(housing_starts_annual, file.path(clean_root, "housing_starts_annual.csv"), row.names = FALSE)

housing_note <- c(
  "Housing starts note",
  "===================",
  "Source: data/data_raw/New_houses_built.csv",
  "Series used: Total units",
  "Geography: Hamilton, Kitchener-Cambridge-Waterloo, London, Ottawa-Gatineau, St. Catharines-Niagara, Toronto",
  "Annual aggregation: sum of monthly starts within each calendar year."
)
writeLines(housing_note, file.path(notes_root, "housing_starts_note.txt"))

cat("Cleaned source files written to data/clean.\n")

