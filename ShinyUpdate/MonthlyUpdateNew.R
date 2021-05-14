#MonthlyUpdate_v3




# SCRIPT START ###############################################################

library('RPostgreSQL')
library('tidyr')
library('readr')
library('rvest')
library('dplyr')
library('data.table')

# Constant setup =============================================================

start <- Sys.time()
errors <- character()


# Set working directory
setwd("C:/Users/901280/OneDrive - Food Standards Agency/Documents/Trade database update/Credentials Write")
suppressWarnings(dir.create(paste0(getwd(), "/datafiles")))
setwd("datafiles")

# Connect to Database
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])

# dbSafeNames function definition
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}


#Countrycode lists
countrycode <- dbGetQuery(tradedata, "select * from country")
eucountrycode <- countrycode %>% filter(eu == TRUE)
eucountrycode <- paste(eucountrycode$countrycode, collapse = "','")
noneucountrycode <- countrycode %>% filter(eu != TRUE)
noneucountrycode <- paste(noneucountrycode$countrycode, collapse = "','")


# Data Structure constants
files <- c("SMKA12", "SMKE19", "SMKI19", "SMKX46", "SMKM46", "SESX16", "SESM16")
names(files) <- c("control", "exp", "imp", "disp", "arr", "dispest", "arrest")

arrivalcols <- c("COMCODE", "RECORD-TYPE", "COD-SEQUENCE", "COD-ALPHA", "TRADE-INDICATOR", "COO-SEQUENCE", "COO-ALPHA", "NATURE-OF-TRANSACTION", "MODE-OF-TRANSPORT", "ACCOUNT-DATE", "SUITE-INDICATOR", "SITC", "IP-COMCODE", "NUMBER-OF-CONSIGNMENTS", "VALUE", "QUANTITY-1", "QUANTITY-2")
dispatchcols <- c("COMCODE", "RECORD-TYPE", "COD-SEQUENCE", "COD-ALPHA", "TRADE-INDICATOR", "COO-SEQUENCE", "COO-ALPHA", "NATURE-OF-TRANSACTION", "MODE-OF-TRANSPORT", "ACCOUNT-DATE", "SUITE-INDICATOR", "SITC", "INDUSTRIAL-PLANT-COMCODE", "NUMBER-OF-CONSIGNMENTS", "VALUE", "QUANTITY-1", "QUANTITY-2")
exportcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","VALUE","QUANTITY-1","QUANTITY-2","INDUSTRIAL-PLANT-COMCODE")
importcols <- c("COMCODE","SITC","RECORD-TYPE","COD-SEQUENCE","COD-ALPHA","COO-SEQUENCE","COO-ALPHA","ACCOUNT-DATE","PORT-SEQUENCE","PORT-ALPHA","FLAG-SEQUENCE","FLAG-ALPHA","COUNTRY-SEQUENCE-COO-IMP","COUNTRY-ALPHA-COO-IMP","TRADE-INDICATOR","CONTAINER","MODE-OF-TRANSPORT","INLAND-MOT","GOLO-SEQUENCE","GOLO-ALPHA","SUITE-INDICATOR","PROCEDURE-CODE","CB-CODE","VALUE","QUANTITY-1","QUANTITY-2")
controlfilecols <- c("MK-COMCODE","MK-INTRA-EXTRA-IND","MK-INTRA-MMYY-ON","MK-INTRA-MMYY-OFF","MK-EXTRA-MMYY-ON","MK-EXTRA-MMYY-OFF","MK-NON-TRADE-ID","MK-SITC-NO","MK-SITC-IND","MK-SITC-CONV-A","MK-SITC-CONV-B","MK-CN-Q2","MK-SUPP-ARRIVALS","MK-SUPP-DESPATCHES","MK-SUPP-IMPORTS","MK-SUPP-EXPORTS","MK-SUB-GROUP-ARR","MK-ITEM-ARR","MK-SUB-GROUP-DESP","MK-ITEM-DESP","MK-SUB-GROUP-IMP","MK-ITEM-IMP","MK-SUB-GROUP-EXP","MK-ITEM-EXP","MK-QTY1-ALPHA","MK-QTY2-ALPHA","MK-COMMODITY-ALPHA-1")


# Webscrape ==============================================================================
# Pulls all the hyperlinks from the uktradeinfo 'bulk datasets' webpage and saves them in a dataframe called link_df,
# then filters that dataframe to only include the 5 relevant links.
# If this code throws an error then it's possible that the uktradeinfo website has been changed.  To get around this problem, you can find the 
# specific links to the most recent data on the uktradeinfo website and plug them into the 'File Downloads' Code (line ~104 - line ~123).
# Alternatively, you can download the zip files manually, save them in the wd and run the code from line ~126.

links <- read_html("https://www.uktradeinfo.com/trade-data/latest-bulk-datasets") %>%
  html_nodes('ul a') 
domain_value = links %>% html_attr('href')
link_df <- data.frame(domain = domain_value) 
link_df_filt <- link_df %>%
  filter(grepl('smki|smka|smke|smkx|smkm', domain) & !grepl('archive', domain))


# Format Links ---------------------------------------------
# Saves each link separately as a vector
smke <- link_df_filt %>% filter(grepl('smke', domain)) 
smke <- as.vector(smke$domain[1])

smki <- link_df_filt %>% filter(grepl('smki', domain))
smki <- as.vector(smki$domain[1])

smkx <- link_df_filt %>% filter(grepl('smkx', domain))
smkx <- as.vector(smkx$domain[1])

smkm <- link_df_filt %>% filter(grepl('smkm', domain))
smkm <- as.vector(smkm$domain[1])

smka <- link_df_filt %>% filter(grepl('smka', domain))
smka <- as.vector(smka$domain[1])


# Set Month and Year ======================================================
# Sets the month and year to the month and year of the most recent file on the uktradeinfo 'bulk datasets' webpage
year <- function(x){
  substr(x, nchar(x)-7, nchar(x)-6)
}
month <- function(x){
  substr(x, nchar(x)-5, nchar(x)-4)
}
syr <- year(smke)
smth <- month(smke)


## File Downloads =============================================================
tryCatch({ suppressWarnings(
  download.file(paste0("https://www.uktradeinfo.com", smke), paste0("smke19", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smke19", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
  download.file(paste0("https://www.uktradeinfo.com", smki), paste0("smki19", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smki19", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
  download.file(paste0("https://www.uktradeinfo.com", smkx), paste0("smkx46", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smkx46", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
  download.file(paste0("https://www.uktradeinfo.com", smkm), paste0("smkm46", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smkm46", syr, smth, ".zip"))})

tryCatch({ suppressWarnings(
  download.file(paste0("https://www.uktradeinfo.com", smka), paste0("smka12", syr, smth, ".zip"))
)}, error = function(e){errors <<- c(errors, paste0("smka12", syr, smth, ".zip"))})

# Unzip monthly files --------------------------------------------------------
unzip(paste0("smke19", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smki19", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smkx46", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smkm46", syr, smth, ".zip"), exdir = getwd())
unzip(paste0("smka12", syr, smth, ".zip"), exdir = getwd())



# Cleanup --------------------------------------------------------------------

# Delete the zipfiles
zipfiles <- list.files(getwd(), pattern = ".zip")
sapply(zipfiles, unlink)

# Make all uppercase
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(up) {
  file.rename(from = up, to = toupper(up))
})

# Remove .txt suffix
datafiles <- list.files(getwd())
datafiles <- sapply(datafiles, FUN = function(txt) {
  file.rename(from = txt, to = sub(pattern = ".TXT", replacement = "", txt))
})




 
  # Load to Database =============================================================
  
  # Check if Data Present in Databases, load if not. -----------------------------
  # This is done by checking whether there are 50+ records for that month already present in the database.
  

#EXPORTS
expQuery <- paste0("SELECT COUNT(account_date) FROM exports_and_dispatches WHERE account_date= '", smth,"/20", syr, "'")
expexisting <- dbGetQuery(tradedata, expQuery)

if (expexisting[1,1] <= 50) {
  
  print(paste("Loading exports:", syr, smth))
  
  exports <- fread(paste(files["exp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = exportcols, fill = TRUE, colClasses = "character")
  colnames(exports) = dbSafeNames(colnames(exports))
  exports <- exports[-.N]
  exports <- exports[, comcode := substr(comcode, 1, 8)]
  exports <- exports[, c("number_of_consignments", "coo_sequence", "coo_alpha", "nature_of_transaction") := rep(list(""),4)]
  exports <- exports[, number_of_consignments := 0]
  
  tryCatch({
    dbWriteTable(tradedata,'exports_and_dispatches', exports, row.names=FALSE, append = TRUE)
  }, error = function(e){errors <<- c(errors, paste(files["exp"], syr, smth, sep = ""))})
  
} else {
  print(paste0("Exports 20", syr, "/", smth, " data exists in database"))
}




# IMPORTS
impQuery <- paste0("SELECT COUNT(account_date) FROM imports_and_arrivals WHERE account_date= '", smth,"/20", syr, "' AND cod_alpha IN ('", noneucountrycode,  "')")
impexisting <- dbGetQuery(tradedata, impQuery)

if (impexisting[1,1] <= 50) {
  
  print(paste("Loading imports:", syr, smth))
  
  imports <- fread(paste(files["imp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = importcols, fill = TRUE, colClasses = "character")
  colnames(imports) = dbSafeNames(colnames(imports))
  imports <- imports[-.N]
  imports <- imports[, comcode := substr(comcode, 1, 8)]
  imports <- imports[, c("number_of_consignments","nature_of_transaction", "ip_comcode") := rep(list(""),3)]
  imports <- imports[, number_of_consignments := 0]
  
  tryCatch({
    dbWriteTable(tradedata,'imports_and_arrivals', imports, row.names=FALSE, append = TRUE)
  }, error = function(e){errors <<- c(errors, paste(files["imp"], syr, smth, sep = ""))})
  
} else {
  print(paste0("Imports 20", syr, "/", smth, " data exists in database"))
}




#ARRIVALS
arrQuery <- paste0("SELECT COUNT(account_date) FROM imports_and_arrivals WHERE account_date= '", smth,"/20", syr, "' AND cod_alpha IN ('", eucountrycode,  "')")
arrexisting <- dbGetQuery(tradedata, arrQuery)

if (arrexisting[1,1] <= 50) {
  
  print(paste("Loading arrivals:", syr, smth))
  
  arrivals <- fread(paste(files["arr"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = arrivalcols, fill = TRUE, colClasses = "character")
  colnames(arrivals) = dbSafeNames(colnames(arrivals))
  arrivals <- arrivals[-.N]
  arrivals <- arrivals[, comcode := substr(comcode, 1, 8)]
  arrivals <- arrivals[, account_date := paste0(substr(account_date,6,7), "/", substr(account_date,2,5))]
  arrivals <- arrivals[, coo_alpha := "UNK EU"]
  arrivals <- arrivals[, number_of_consignments := 0]
  arrivals <- arrivals[, c("port_sequence", "port_alpha", "flag_sequence", "flag_alpha", "country_sequence_coo_imp", "country_alpha_coo_imp", "container", "inland_mot", "golo_sequence", "golo_alpha", "procedure_code", "cb_code") := rep(list(""),12)]
  arrivals <- arrivals[, port_alpha := "EU UNK"]
  setcolorder(arrivals, c("comcode", "sitc", "record_type", "cod_sequence", "cod_alpha", "coo_sequence", "coo_alpha", "account_date", "port_sequence", "port_alpha", "flag_sequence", "flag_alpha", "country_sequence_coo_imp", "country_alpha_coo_imp", "trade_indicator", "container", "mode_of_transport", "inland_mot", "golo_sequence", "golo_alpha", "suite_indicator", "procedure_code", "cb_code", "value", "quantity_1", "quantity_2", "number_of_consignments", "nature_of_transaction", "ip_comcode"))  
  
  tryCatch({
    dbWriteTable(tradedata,'imports_and_arrivals', arrivals, row.names=FALSE, append = TRUE)
  }, error = function(e){errors <<- c(errors, paste(files["arr"], syr, smth, sep = ""))})
  
} else {
  print(paste0("Arrivals 20", syr, "/", smth, " data exists in database"))
}



#DISPATCHES
# dispQuery <- paste0("SELECT COUNT(account_date) FROM exports_and_dispatches WHERE account_date= '", smth,"/20", syr, "'")
# dispexisting <- dbGetQuery(tradedata, dispQuery)

# if (dispexisting[1,1] <= 50) {
  
  print(paste("Loading dispatches:", syr, smth))
  
  dispatches <- fread(paste(files["disp"], syr, smth, sep = ""), sep = "|", skip = 1, col.names = dispatchcols, fill = TRUE, colClasses = "character")
  #dispatches <- fread("SMKX462101_NEW", sep = "|", skip = 1, col.names = dispatchcols, fill = TRUE, colClasses = "character")
  colnames(dispatches) = dbSafeNames(colnames(dispatches))
  dispatches <- dispatches[-.N]
  dispatches <- dispatches[, comcode := substr(comcode, 1, 8)]
  dispatches <- dispatches[, account_date := paste0(substr(account_date,6,7), "/", substr(account_date,2,5))]
  dispatches <- dispatches[, number_of_consignments := 0]
  dispatches <- dispatches[, c("port_sequence", "port_alpha", "flag_sequence", "flag_alpha", "container", "inland_mot", "golo_sequence", "golo_alpha", "procedure_code") := rep(list(""),9)]
  dispatches <- dispatches[, port_alpha := "EU UNK"]
  setcolorder(dispatches, c("comcode", "sitc", "record_type", "cod_sequence", "cod_alpha", "account_date", "port_sequence", "port_alpha", "flag_sequence", "flag_alpha", "trade_indicator", "container", "mode_of_transport", "inland_mot", "golo_sequence", "golo_alpha", "suite_indicator", "procedure_code", "value", "quantity_1", "quantity_2", "industrial_plant_comcode", "number_of_consignments", "coo_sequence", "coo_alpha", "nature_of_transaction"))  
  
  tryCatch({
    dbWriteTable(tradedata,'exports_and_dispatches', dispatches, row.names=FALSE, append = TRUE)
  }, error = function(e){errors <<- c(errors, paste(files["disp"], syr, smth, sep = ""))})
  
# } else {
#   print(paste0("Dispatches 20", syr, "/", smth, " data exists in database"))
# }




# Note that there is no need to check for existing data, as this is already UPSERTing line by line.

print(paste("Loading commodity codes: ", syr, smth))

tryCatch({
  control <- read.table(paste(files["control"], syr, smth, sep = ""), sep = "|", skip = 1, quote = NULL, fill = TRUE, strip.white = TRUE, colClasses = "character")
  fixedCols <- length(controlfilecols)
  actualCols <- ncol(control)
  if (fixedCols != actualCols) {
    control <- unite(control, "newcol", "V27", "V28", sep = "")}
  safecontrolfilecols <- dbSafeNames(controlfilecols)
  colnames(control) <- safecontrolfilecols
  control <- control[-length(control$mk_comcode), 1:fixedCols]
  control$mk_comcode <- substr(control$mk_comcode, 1, 8)
  control$mk_commodity_alpha_1 <- trimws(control$mk_commodity_alpha_1, "both")
  control$mk_commodity_alpha_1 <- gsub("\'", "\'\'", control$mk_commodity_alpha_1)
  control$mk_commodity_alpha_1 <- iconv(control$mk_commodity_alpha_1, from = "UTF-8", to = "LATIN1", sub = "/")
  for (k in 1:length(control$mk_comcode)) {
    valstring1 <- paste(sapply(control[k,], paste, ", \'", sep = "\'"), sep = "", collapse = "")
    valstring2 <- paste("\'", substr(valstring1, 1, nchar(valstring1)-3), sep = "")
    sqlQuery <- paste(
      "INSERT INTO control VALUES (",
      valstring2,
      ") ON CONFLICT (mk_comcode) DO UPDATE SET mk_commodity_alpha_1 = ", 
      paste("\'", control$mk_commodity_alpha_1[k], "\'", sep = ""),
      ";", sep = "")
    dbGetQuery(tradedata,sqlQuery)
  }
}, error = function(e){errors <<- c(errors, paste(files["control"], syr, smth, sep = ""))}
)   

# Now clear out datafiles/ ---------------------------------------------------
datafiles <- list.files(getwd())
sapply(datafiles, unlink)

# Print info =================================================================
end <- Sys.time()

print(paste("Time taken:", end - start))
print("The following files were not found:")
print(errors)

# END SCRIPT #################################################################


#  dbGetQuery(tradedata, "update exports_and_dispatches set port_alpha = 'EU UNK' where port_alpha = ''")
#  dbGetQuery(tradedata, "select count(*) from exports_and_dispatches where port_alpha = ''")







