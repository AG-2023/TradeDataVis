# SCRIPT START ################################################################

# LIBRARY IMPORT AND CONSTANTS ================================================

if(require("readr") == FALSE) {install.packages("readr")}
library("readr")

if(require("dplyr") == FALSE) {install.packages("dplyr")}
library("dplyr")

if(require("stringr") == FALSE) {install.packages("stringr")}
library("stringr")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

if(require("readxl") == FALSE) {install.packages("readxl")}
library("readxl")

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

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

# Connect to the database.

pg <- dbDriver("PostgreSQL")
db_env <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
trade_data <- dbConnect(pg, user = db_env[1, 2], password = db_env[2, 2],
                        host = db_env[3, 2], port = db_env[4, 2], dbname = db_env[5, 2])

# COMCODE TABLE BUILD =========================================================

# Read Data using readr and tidy as appropriate -------------------------------

CNURL <- "http://ec.europa.eu/eurostat/ramon/nomenclatures/index.cfm?TargetUrl=ACT_OTH_CLS_DLD&StrNom=CN_2022&StrFormat=CSV&StrLanguageCode=EN&IntKey=&IntLevel=&bExport="
CN <- read_csv2(CNURL)
CN <- tibble(CommodityCode = CN$Code...5,Parent = CN$Parent...6,Description = CN[[8]])
colnames(CN) <- c("CommodityCode", "Parent", "Description")
CN$CommodityCode <- gsub(" ", "", CN$CommodityCode)
CN$Parent <- gsub(" ", "", CN$Parent)
CN <- CN[is.na(CN$CommodityCode) == FALSE,]

# store indices for Section Characters (roman numerals)
Sections <- tibble(Section = CN$CommodityCode[grep("^.*(I|V|X).*$",CN$CommodityCode)],
                   Code = CN$CommodityCode[grep("^.*(I|V|X).*$",CN$CommodityCode)+1],
                   Description = CN$Description[grep("^.*(I|V|X).*$",CN$CommodityCode)])

control <- dbGetQuery(trade_data, "SELECT mk_comcode,mk_commodity_alpha_1 FROM public.control")
control <- tibble(commoditycode = control$mk_comcode, description = control$mk_commodity_alpha_1)
control <- arrange(control,commoditycode)

# Create a complete Codes and Parents column ----------------------------------

# Extracts numeric comcodes (IE: not section level).
codesonly <- CN$CommodityCode[grepl("^.*(I|V|X).*$",CN$CommodityCode) == FALSE]
codesonly <- unique(c(codesonly, control$commoditycode))

# Commodity Nomenclature codes contain a ragged hierarchy. This means that
# a child-parent relationship can span levels in the hierarchy. Therefore it
# isn't simply a case of chopping off the last two digits of a code gives the
# parent's code - it may not exist! The code below generates a parent through
# recursive removal of last two digits in the commodity code.
# Once all the parents can be found within the commodity code vector, create tibble.
parents <- substr(codesonly,1, nchar(codesonly)-2)
thisrecur <- parents

thisrecur <- vapply(thisrecur,function(x){
  if (x %in% codesonly == FALSE){
    x <- substr(x,1,nchar(x)-2)
  } else {
    return(x)
  }
}, character(1), USE.NAMES = FALSE)

#names(thisrecur) = NULL # for some reason lastrecur becomes names in thisrecur...
parents <- thisrecur

codeandparent <- tibble(commoditycode = codesonly, parent = parents)

# Joining data ----------------------------------------------------------------

colnames(CN) <- dbSafeNames(colnames(CN))
colnames(codeandparent) <- dbSafeNames(colnames(codeandparent))
colnames(control) <- dbSafeNames(colnames(control))

# joins new parent vector to tibble, merges cols.
CN <- full_join(CN, codeandparent, by = "commoditycode")
CN$parent.z <- ifelse(is.na(CN$parent.x), CN$parent.y, CN$parent.x)
CN <- tibble(commoditycode = CN$commoditycode, parent = CN$parent.z, description = CN$description)

# joins descriptions from control file to tibble, merges cols
CN <- full_join(CN, control, by = "commoditycode")
CN <- arrange(CN, commoditycode)
CN$description.z <- ifelse(is.na(CN$description.x), CN$description.y, CN$description.x)
CN <- tibble(commoditycode = CN$commoditycode, parent = CN$parent, description = CN$description.z)

# remember the indices object? we can now put section numbers back in the right place
# using the data stored in that tibble! In reverse order so add_row doesn't get confused...

for (i in length(Sections$Section):1){
  CN <- add_row(CN, commoditycode = Sections$Section[i], 
                parent = "", 
                description = Sections$Description[i],
                .before = grep(paste("^", Sections$Code[i], "$", sep=""),CN$commoditycode))
}

CN <- CN[!duplicated(CN$commoditycode),]

#Join new list of commodity codes (CN) to old list of commodity codes (comcode)

comcode <- dbGetQuery(trade_data, "SELECT * FROM comcode")
newcomcode <- full_join(CN, comcode, by = "commoditycode")
newcomcode$parent.z <- ifelse(is.na(newcomcode$parent.x), newcomcode$parent.y, newcomcode$parent.x)
newcomcode$description.z <- ifelse(is.na(newcomcode$description.x), newcomcode$description.y, newcomcode$description.x)
newcomcode <- tibble(commoditycode = newcomcode$commoditycode, parent = newcomcode$parent.z, description = newcomcode$description.z)

# creates new table, adds to db -----------------------------------------------

dbWriteTable(trade_data, 'comcode', newcomcode, row.names=FALSE)
dbSendQuery(trade_data, "delete from comcode")
dbSendQuery(trade_data, "SET client_encoding = 'LATIN1'")
try(dbSendQuery(trade_data, "alter table comcode add constraint control_pkey PRIMARY KEY (CommodityCode)"))

dbWriteTable(trade_data,'comcode', newcomcode, row.names=FALSE, append = TRUE)