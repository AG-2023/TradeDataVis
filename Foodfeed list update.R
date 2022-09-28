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

# Load foodfeed list
allfoodfeed <- dbGetQuery(trade_data, "SELECT * FROM allfoodfeed")

# Load list of commodity codes
comcode <- dbGetQuery(trade_data, "SELECT * FROM comcode")

#Obtain list of commodity codes which don't appear in the foodfeed list
uniquecomcode <- anti_join(comcode, allfoodfeed, by = c("commoditycode" = "comcodeff"))

#Once you have identified the new commodity codes which should be added into the foodfeed list, add them here
newfoodfeedcodes <- c("03072110", "03072190", "03072295", "03083080", "03091000", "03099000",  "04032011", "04032013", "04032019", "04032031", "04032033", "04032039", "04032041", "04032049", "04032051", "04032053", "04032059", "04032091", "04032093", "04032099", "04101010", "04101091", "04101099", "04109000", "07041010", "07041090", "07095200", "07095300", "07095400", "07095500", "07095600", "07095900", "07123400", "08029100", "08029200", "08029910", "08029990", "15092000", "15093000", "15094000", "15101000", "15109000", "15155019", "15156051", "15156059", "15156091", "15156099", "15163091", "15163098", "19019095", "22082016", "22082018", "22082019", "22082028", "22082066", "22082069", "22082088")
uniquecodesv2 <- uniquecomcode %>% filter(commoditycode %in% newfoodfeedcodes)
uniquecodesv2["poao"] <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "Yes", "No", "No", "No", "No", "No", "No", "No")
uniquecodesv2["fnao"] <- c("No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
uniquecodesv2["food"] <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
uniquecodesv2["feed"] <- c("No", "No", "No", "No", "Yes", "Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "Yes", "No", "No", "No", "No", "No", "No", "No")

#Drop unnecessary columns and rename columns to match the existing foodfeed table
uniquecodesv2 <- uniquecodesv2 %>%
  select(commoditycode, poao, fnao, food, feed) %>%
  rename(comcodeff = commoditycode)

#Join the two tables together (by stacking on top of each other) and arrange in order of commodity code
newallfoodfeed <- allfoodfeed %>%
  bind_rows(uniquecodesv2) %>%
  arrange(comcodeff)


# WRITE TO DATABASE ------------------------------------------------------------
dbWriteTable(trade_data, 'allfoodfeed', newallfoodfeed, row.names=FALSE)
dbSendQuery(trade_data, "delete from allfoodfeed")
dbWriteTable(trade_data, 'allfoodfeed', newallfoodfeed, row.names=FALSE, append = TRUE)