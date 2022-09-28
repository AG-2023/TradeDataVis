library("RPostgreSQL")
library("readr")
library("rvest")
library("dplyr")
library("data.table")
library("tidyr")
library("tibble")

# Set the working directory.
setwd(
  paste0(
     "C:/Users/901538/OneDrive - Food Standards Agency/Documents/",
    "Trade database update/Credentials Write"
  )
)

# Constants.
DATA_NAMES <- c("smka12", "bdsexp", "bdsimp")
EXPORTS_IMPORTS_FIELDS <- c("period_reference", "type", "month_of_account",
  "comcode", "sitc", "cod_sequence", "cod_alpha", "port_sequence",
  "port_alpha", "coo_sequence", "coo_alpha", "mode_of_transport",
  "statistical_value", "net_mass", "supplementary_unit", "suppresion_indicator",
  "flow", "record_type")
EXPORTS_IMPORTS_SPLITS <- c(6, 7, 13, 21, 26, 29, 31, 34, 37, 40, 42, 44, 56,
  68, 80, 81, 84)
CONTROL_FIELDS <- c("MK-COMCODE", "MK-INTRA-EXTRA-IND", "MK-INTRA-MMYY-ON",
  "MK-INTRA-MMYY-OFF", "MK-EXTRA-MMYY-ON", "MK-EXTRA-MMYY-OFF",
  "MK-NON-TRADE-ID", "MK-SITC-NO", "MK-SITC-IND", "MK-SITC-CONV-A",
  "MK-SITC-CONV-B", "MK-CN-Q2", "MK-SUPP-ARRIVALS", "MK-SUPP-DESPATCHES",
  "MK-SUPP-IMPORTS", "MK-SUPP-EXPORTS", "MK-SUB-GROUP-ARR", "MK-ITEM-ARR",
  "MK-SUB-GROUP-DESP", "MK-ITEM-DESP", "MK-SUB-GROUP-IMP", "MK-ITEM-IMP",
  "MK-SUB-GROUP-EXP", "MK-ITEM-EXP", "MK-QTY1-ALPHA", "MK-QTY2-ALPHA",
  "MK-COMMODITY-ALPHA-1")
errors <- character()

# Functions.
db_safe_names <- function(names) {
  names <- gsub("[^a-z0-9]+", "_", tolower(names))
  names <- make.names(names, unique = TRUE, allow_ = TRUE)
  names <- gsub(".", "_", names, fixed = TRUE)
  print("Names made suitable for the database.")
  return(names)
}

data_location <- function(link_df, data_name) {
  location <- link_df %>%
    filter(grepl(data_name, domain))
  location <- as.vector(location$domain[1])
  print("Website data locations identified.")
  return(location)
}

latest_file_time <- function(smka_location, month_or_year) {
  if (month_or_year == "year") {
    positions <- c(6, 7)
  }
  else if (month_or_year == "month") {
    positions <- c(4, 5)
  }
  else {
    print("Please set month_or_year to 'month' or 'year'.")
    break
  }
  time_stamp <- substr(smka_location,
    nchar(smka_location) - positions[2],
    nchar(smka_location) - positions[1]
  )
  print("Time stamp extracted from files.")
  return(time_stamp)
}

download_hmrc_file <- function(data_name, data_location, latest_year,
  latest_month) {
  download.file(paste0("https://www.uktradeinfo.com/", data_location),
    paste0(data_name, latest_year, latest_month, ".zip")
  )
  print("File downlaoded from HMRC.")
}

read_imports_exports_file <- function(file_type, year, month, fields, splits,
  old_fields) {
  df <- fread(paste(file_type, year, month, sep = ""), sep = "",
    header = FALSE, fill = TRUE, colClasses = "character", skip = 1) %>%
  separate(1, fields, sep = splits) %>%
  rename(account_date = period_reference) %>%
  mutate(account_date = paste(
    substr(account_date, 5, 6), "/", substr(account_date, 1, 4), sep = ""
  )) %>%
  rename(value = statistical_value) %>%
  rename(quantity_1 = net_mass) %>%
  rename(quantity_2 = supplementary_unit) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(quantity_1 = as.numeric(quantity_1)) %>%
  mutate(quantity_2 = as.numeric(quantity_2))
  for (name in all_of(old_fields)) {
    if (!name %in% names(df)) {
      # Use !!() to specify use the name and := to assign values.
      df <- df %>% add_column(!!(name) := "")
    }
  }
  df <- df %>% select(all_of((old_fields)))
  print("File imported and wrangled.")
  return(df)
}

# Connect to the database.
pg <- dbDriver("PostgreSQL")
db_env <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
trade_data <- dbConnect(pg, user = db_env[1, 2], password = db_env[2, 2],
  host = db_env[3, 2], port = db_env[4, 2], dbname = db_env[5, 2])

# Get a list of country codes.
country_code <- dbGetQuery(trade_data, "select * from country")
country_code <- paste(country_code$countrycode, collapse = "','")

# Webscrape the data locations.
links <- read_html(
  "https://www.uktradeinfo.com/trade-data/latest-bulk-datasets") %>%
  html_nodes("ul a")
domain_values <- links %>%
  html_attr("href")
link_df <- data.frame(domain = domain_values) %>%
  filter(grepl(paste0(DATA_NAMES[1], "|", DATA_NAMES[2], "|", DATA_NAMES[3]),
    domain) & !grepl("archive", domain)
  )

# Download and unzip the data.
dir.create("temporary_data_file")
setwd("temporary_data_file")
for (name in DATA_NAMES) {
  # Select the relevant location in the website.
  location_name <- paste0(name, "_location")
  assign(location_name, data_location(link_df, name))
  location_name_eval <- eval(parse(text = paste0(name, "_location")))
  # Find the month and year of the latest file.
  latest_month <- latest_file_time(location_name_eval, "month")
  latest_year <- latest_file_time(location_name_eval, "year")
  # Get the file and extract.
  download_hmrc_file(name, location_name_eval,
    latest_year, latest_month)
  unzip(paste0(name, latest_year, latest_month, ".zip"), exdir = getwd())
}

# Identify the data files (non-.zip) by inverting pattern matching.
data_files <- grep(list.files(), pattern = ".zip", invert = TRUE, value = TRUE)
# Make all the file names lower case to fit with DATA_NAMES.
data_files <- sapply(data_files, FUN = function(file_name) {
  file.rename(from = file_name, to = tolower(file_name))
})

# Remove .txt suffixes.
text_files <- list.files(pattern = ".txt")
text_files <- sapply(text_files, FUN = function(txt_file) {
  file.rename(from = txt_file,
    to = sub(pattern = ".txt",
      replacement = "", txt_file
    )
  )
})

# Database writing should be made into a function in future.
# Write exports data into the database.
exports_date_query <- paste0(
  "SELECT COUNT(account_date) FROM exports_and_dispatches ",
  "WHERE account_date= '",
  latest_month,
  "/20",
  latest_year,
  "'"
)
exports_date_count <- dbGetQuery(trade_data, exports_date_query)

if (exports_date_count[1, 1] <= 50) {  # Consider a better criterion in future.
  print(paste("Loading exports:", latest_year, latest_month))
  existing_exports_query <- paste(
    "SELECT * FROM exports_and_dispatches LIMIT 1")
  existing_exports <- dbGetQuery(trade_data, existing_exports_query)
  exports <- read_imports_exports_file(DATA_NAMES[2], latest_year, latest_month,
    EXPORTS_IMPORTS_FIELDS, EXPORTS_IMPORTS_SPLITS, names(existing_exports))
  colnames(exports) <- db_safe_names(colnames(exports))
  # Final line trailer record not in technical specifications.
  # exports <- exports[-.N]
  exports <- exports[, comcode := substr(comcode, 1, 8)]
  exports <- exports[, c("number_of_consignments", "coo_sequence", "coo_alpha",
    "nature_of_transaction") := rep(list(""), 4)]
  exports <- exports[, number_of_consignments := 0]
  exports <- exports[, trade_indicator := 0]
  tryCatch({
    dbWriteTable(trade_data, "exports_and_dispatches", exports,
      row.names = FALSE, append = TRUE)
  }, error = function(e) {
    errors <<- c(errors, paste0(DATA_NAMES[2], latest_year, latest_month))
    }
  )
} else {
  print(
    paste0(
      "Exports 20", latest_year, "/", latest_month, " data exists in database"
    )
  )
}

# Write imports data into the database.
imports_date_query <- paste0(
  "SELECT COUNT(account_date) FROM imports_and_arrivals ",
  "WHERE account_date= '",
  latest_month,
  "/20",
  latest_year,
  "'"
)
imports_date_count <- dbGetQuery(trade_data, imports_date_query)

if (imports_date_count[1, 1] <= 50) {  # As above for exports.
  print(paste("Loading imports:", latest_year, latest_month))
  existing_imports_query <- paste("SELECT * FROM imports_and_arrivals LIMIT 1")
  existing_imports <- dbGetQuery(trade_data, existing_imports_query)
  imports <- read_imports_exports_file(DATA_NAMES[3], latest_year, latest_month,
    EXPORTS_IMPORTS_FIELDS, EXPORTS_IMPORTS_SPLITS, names(existing_imports))
  colnames(imports) <- db_safe_names(colnames(imports))
  # Final line trailer record not in technical specifications.
  #imports <- imports[-.N]
  imports <- imports[, comcode := substr(comcode, 1, 8)]
  imports <- imports[, c("number_of_consignments", "nature_of_transaction",
    "ip_comcode") := rep(list(""), 3)]
  imports <- imports[, number_of_consignments := 0]
  imports <- imports[, trade_indicator := 0]
  tryCatch({
    dbWriteTable(trade_data, "imports_and_arrivals", imports, row.names = FALSE,
      append = TRUE)
  }, error = function(e) {
    errors <<- c(errors, paste0(DATA_NAMES[3], latest_year, latest_month))
    }
  )
} else {
  print(
    paste0(
      "Imports 20", latest_year, "/", latest_month, " data exists in database"
    )
  )
}

# Write control file data to the database.
# Note from the past: There is no need to check for existing data, as this is
# already UPSERTing line by line.
print(paste("Loading commodity codes: ", latest_year, latest_month))
tryCatch({
  control <- read.table(paste(DATA_NAMES[1], latest_year, latest_month,
    sep = ""), sep = "|", skip = 1, quote = NULL, fill = TRUE,
    strip.white = TRUE, colClasses = "character")
  fixed_columns <- length(CONTROL_FILEDS)
  actual_columns <- ncol(control)
  if (fixed_columns != actual_columns) {
    control <- unite(control, "newcol", "V27", "V28", sep = "")
  }
  safe_control_file_columns <- db_safe_names(CONTROL_FIELDS)
  colnames(control) <- safe_control_file_columns
  control <- control[-length(control$mk_comcode), 1:fixedCols]
  control$mk_comcode <- substr(control$mk_comcode, 1, 8)
  control$mk_commodity_alpha_1 <- trimws(control$mk_commodity_alpha_1, "both")
  control$mk_commodity_alpha_1 <- gsub("\'", "\'\'",
    control$mk_commodity_alpha_1)
  control$mk_commodity_alpha_1 <- iconv(control$mk_commodity_alpha_1,
    from = "UTF-8", to = "LATIN1", sub = "/")
  for (k in 1:length(control$mk_comcode)) {
    valstring1 <- paste(
      sapply(control[k, ], paste, ", \'", sep = "\'"), sep = "", collapse = "")
    valstring2 <- paste("\'", substr(valstring1, 1, nchar(valstring1) - 3),
      sep = "")
    control_sql_query <- paste(
      "INSERT INTO control VALUES (",
      valstring2,
      ") ON CONFLICT (mk_comcode) DO UPDATE SET mk_commodity_alpha_1 = ",
      paste("\'", control$mk_commodity_alpha_1[k], "\'", sep = ""),
      ";", sep = "")
    dbGetQuery(tradedata, control_sql_query)
  }
}, error = function(e) {
  errors <<- c(errors,
    paste(DATA_NAMES[1], latest_year, latest_month, sep = "")
    )
  }
)

# Delete the temporary data file.
setwd("..")
unlink("./temporary_data_file", recursive = TRUE)
print("Temporary data file has been deleted.")
print("Process end.")
