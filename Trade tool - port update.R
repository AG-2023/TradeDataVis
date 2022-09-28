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

# Load list of ports
portcode <- dbGetQuery(trade_data, "SELECT * FROM port")
# Add ports to the port table
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Beccles Airport', 'BCC', 'Airport', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Sywell Aerodrome', 'SYW', 'Airport', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Wick Airport', 'WIK', 'Airport', 'Scotland')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Northolt', 'NHT', 'Airport', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Waddington', 'WAD', 'Airport', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Ashford', 'ASD', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Barking', 'LBK', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Dagenham', 'DAG', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Daventry', 'DVY', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Dollands Moor', 'NGO', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Doncaster', 'DON', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Irvine', 'KMK', 'Rail', 'Scotland')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Scunthorpe', 'SCP', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Widnes', 'WDN', 'Rail', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Dover / Eurotunnel', 'DEU', 'Roll on Roll off', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Eurotunnel', 'EUT', 'Roll on Roll off', 'Unknown Region')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Killingholme', 'KIL', 'Roll on Roll off', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Purfleet', 'PUF', 'Roll on Roll off', 'England')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Not collected', 'ZZZ','Unknown','Unknown Region')")
dbGetQuery(trade_data, "INSERT INTO port (portname, portcode, type, region) VALUES ('Not collected', 'zzz','Unknown','Unknown Region')")
