options(stringsAsFactors = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
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

inspect_housing_starts_source <- function(csv_path) {
  raw <- read.csv(
    csv_path,
    skip = 8,
    header = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  geography_row <- as.character(unlist(raw[1, ]))
  geography_row[is.na(geography_row)] <- ""
  non_empty_geographies <- geography_row[nzchar(trimws(geography_row))]
  date_labels <- as.character(unlist(raw[2, -1]))
  data.frame(
    geography_labels = paste(non_empty_geographies, collapse = ", "),
    first_date = date_labels[1],
    last_date = date_labels[length(date_labels)],
    series_rows = paste(trimws(raw[4:9, 1]), collapse = ", "),
    stringsAsFactors = FALSE
  )
}

project_root <- "."
raw_root <- file.path(project_root, "data", "data_raw")
output_root <- file.path(project_root, "output", "inspection")
ensure_dir(output_root)

raw_files <- list.files(raw_root, recursive = TRUE, full.names = TRUE)
docs_files <- list.files(file.path(project_root, "docs"), recursive = TRUE, full.names = TRUE)

nhpi_path <- file.path(raw_root, "Housing_price_index.csv")
teranet_path <- file.path(raw_root, "House_Price_Index.csv")
lookup_path <- file.path(raw_root, "Interest_rate.csv")
housing_starts_path <- file.path(raw_root, "New_houses_built.csv")
intl_students_path <- file.path(raw_root, "International_students.csv")

nhpi_raw <- read.csv(
  nhpi_path,
  skip = 7,
  header = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
nhpi_target_geographies <- c(
  "Ottawa-Gatineau, Ontario part, Ontario/Quebec 7",
  "Toronto, Ontario 9",
  "Hamilton, Ontario",
  "St. Catharines-Niagara, Ontario",
  "Kitchener-Cambridge-Waterloo, Ontario",
  "London, Ontario"
)
nhpi_measure_rows <- trimws(as.character(nhpi_raw[4:6, 1]))

teranet_groups <- character(0)
if (file.exists(teranet_path)) {
  teranet_header_line <- readLines(teranet_path, n = 1, warn = FALSE)
  teranet_groups <- gsub("\"", "", strsplit(teranet_header_line, ",", fixed = TRUE)[[1]])
  teranet_groups <- unique(teranet_groups[grepl("^on_", teranet_groups)])
}

intl_raw <- read.csv(intl_students_path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
lookup_daily <- extract_lookup_daily(lookup_path)
housing_starts_info <- inspect_housing_starts_source(housing_starts_path)

inventory <- data.frame(
  file = c(
    nhpi_path,
    lookup_path,
    housing_starts_path,
    intl_students_path
  ),
  likely_contents = c(
    "Statistics Canada New Housing Price Index with multiple geography blocks and monthly values.",
    "Daily Bank of Canada overnight-rate target data.",
    "CMHC housing starts file with city-specific series for the six target CMAs.",
    "StatCan table 37-10-0232-01: annual international student enrolment by institution (2015/2016-2023/2024)."
  )
)
if (file.exists(teranet_path)) {
  inventory <- rbind(
    data.frame(
      file = teranet_path,
      likely_contents = "Repeat-sales house price index with only a subset of Ontario cities.",
      stringsAsFactors = FALSE
    ),
    inventory
  )
}
write.csv(inventory, file.path(output_root, "source_inventory.csv"), row.names = FALSE)

summary_lines <- c(
  "File inspection summary",
  "======================",
  "",
  "Raw files found:",
  paste0("- ", gsub("^\\./", "", c(raw_files, docs_files))),
  "",
  "Most usable outcome source:",
  paste0(
    "- Housing_price_index.csv is the best housing-price source because its geography blocks include all six target CMAs: ",
    paste(nhpi_target_geographies, collapse = "; "),
    "."
  ),
  paste0("- Usable NHPI measure rows in the first geography block: ", paste(nhpi_measure_rows, collapse = ", "), "."),
  "- The planned outcome row is `Total (house and land)` and the outcome transform will be log(price_index).",
  "Policy-rate source:",
  paste0(
    "- Interest_rate.csv contains ",
    nrow(lookup_daily),
    " daily observations with date range ",
    format(min(lookup_daily$Date), "%Y-%m-%d"),
    " to ",
    format(max(lookup_daily$Date), "%Y-%m-%d"),
    "."
  ),
  "- The cleaning script will aggregate Interest_rate.csv from daily to monthly means.",
  "",
  if (file.exists(teranet_path)) {
    paste0(
      "Alternative housing-price file:\n- House_Price_Index.csv contains Ontario series blocks ",
      paste(teranet_groups, collapse = ", "),
      ", but it does not cover all six required CMAs, so it is not the preferred panel outcome.\n"
    )
  } else {
    "Alternative housing-price file:\n- House_Price_Index.csv is not present in the current project folder, so it is ignored.\n"
  },
  "",
  "Housing-starts source:",
  paste0("- New_houses_built.csv runs from ", housing_starts_info$first_date, " to ", housing_starts_info$last_date, "."),
  paste0("- Series rows available in the local export: ", housing_starts_info$series_rows, "."),
  paste0("- Geography labels present in the local export: ", paste(housing_starts_info$geography_labels, collapse = ", "), "."),
  "- The current local file contains the six target geographies directly, so housing starts can now be merged at the city-month level.",
  "",
  "International student enrolment source:",
  paste0("- International_students.csv: ", nrow(intl_raw), " institutions x ", ncol(intl_raw) - 1, " academic years."),
  "- Source: StatCan table 37-10-0232-01 — annual postsecondary enrolment of international students by institution.",
  "- Each column is an academic year (e.g. '2023 / 2024'); lower year used as the integer year value.",
  "- '..' (suppressed values) are imputed: leading '..' (before institution existed) set to 0; interior '..' set to institution mean across available years.",
  "- Institution-to-CMA mapping applied in 02_clean_each_source.R; institutions outside the 6 target CMAs are dropped.",
  "",
  "Expected harmonization steps:",
  "- Standardize city labels to the six target CMAs.",
  "- Restrict all sources to 2016-01 through 2024-12.",
  "- Convert Interest_rate.csv from daily to monthly frequency.",
  "- Strip NHPI quality flag suffixes such as trailing `E` before numeric conversion.",
  "- Merge city-specific housing-starts blocks from New_houses_built.csv into the panel.",
  "- Merge to a city-by-month panel."
)

writeLines(summary_lines, file.path(output_root, "source_summary.txt"))
