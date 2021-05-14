# 20180306
# Script to check a detabase has been loaded with all monthly HMRC data. Then plot the result. 

# Written by Ethan Linke for TradeDataVis application
# Github: 

# SCRIPT START ################################################################

# Library Import --------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(ggplot2)
library(RPostgreSQL)
library(patchwork)
#setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/")
setwd("C:/Users/901280/OneDrive - Food Standards Agency/Documents/Trade database update/Credentials Write")

# Connect to DB ---------------------------------------------------------------
pg <- dbDriver("PostgreSQL")
dbenv <- read_delim(".env", delim = "=", col_names = FALSE, trim_ws = TRUE)
tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2],
                       host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])


# Get country data ------------------------------------------------------------

countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")
eucountrycode <- countrycode %>% filter(eu == TRUE)
noneucountrycode <- countrycode %>% filter(eu != TRUE)

# Import Data into R ----------------------------------------------------------

# Imports
sqlquery <- "SELECT DISTINCT account_date, cod_alpha, COUNT(account_date) AS records FROM imports_and_arrivals GROUP BY account_date, cod_alpha"
imports <- dbGetQuery(tradedata, sqlquery)
imports$account_date <- paste0(substr(imports$account_date, 4, 7),substr(imports$account_date, 1, 2))
imports <- imports %>%
  rename(period = account_date) %>%
  arrange(period) %>% 
  filter(str_detect(period, "^99|13$|000000", negate = TRUE))
euimports <- imports %>% filter(cod_alpha %in% eucountrycode$countrycode) %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "EU Imports")
noneuimports <- imports %>% filter(cod_alpha %in% noneucountrycode$countrycode) %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "Non-EU Imports")
imports <- imports %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "Imports")

# Exports
sqlquery <- "SELECT DISTINCT account_date, cod_alpha, COUNT(account_date) AS records FROM exports_and_dispatches GROUP BY account_date, cod_alpha"
exports <- dbGetQuery(tradedata, sqlquery)
exports$account_date <- paste0(substr(exports$account_date, 4, 7),substr(exports$account_date, 1, 2))
exports <- exports %>%
  rename(period = account_date) %>%
  arrange(period) %>% 
  filter(str_detect(period, "^99|13$|000000", negate = TRUE))
euexports <- exports %>% filter(cod_alpha %in% eucountrycode$countrycode) %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "EU Exports")
noneuexports <- exports %>% filter(cod_alpha %in% noneucountrycode$countrycode) %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "Non-EU Exports")
exports <- exports %>% group_by(period) %>% summarise(records = sum(records)) %>% mutate(type = "Exports")


# Join Data --------------------------------------------------------------------

UKTradeRecords <- rbind(euimports,noneuimports,imports,euexports,noneuexports,exports)
rm(euimports,noneuimports,imports,euexports,noneuexports,exports,countrycode,eucountrycode,noneucountrycode)


# Plot Data --------------------------------------------------------------------

xscale <- unique(UKTradeRecords$period)[seq(1, length(unique(UKTradeRecords$period)) + 1, 4)]
ryears <- paste(c(as.numeric(substr(Sys.Date(), 1,4)) - 1, substr(Sys.Date(), 1,4)), collapse = "|")

exp_imp_plot1 <- UKTradeRecords %>% 
  filter(type == "Imports"|type == "Exports", str_detect(period, "^2008|^2009|^2010", negate=TRUE) ) %>%
  ggplot(aes(x=period, 
             y=records, 
             color=type, 
             group=type)) +
  geom_line(size = 1) +
  scale_x_discrete(breaks = xscale) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid = element_line(color="light gray")) +
  #scale_color_manual(palette = "Set1") +
  labs(x="", y = "Number of Records", title = "UK Trade Activity over time by Record Count in HMRC Data")

eu_noneu_plot1 <- UKTradeRecords %>% 
  filter(type != "Imports" & type != "Exports", str_detect(period, "^2008|^2009|^2010", negate=TRUE) ) %>%
  ggplot(aes(x=period, 
             y=records, 
             color=type, 
             group=type)) +
  geom_line(size = 1) +
  scale_x_discrete(breaks = xscale) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid = element_line(color="light gray")) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y = "Number of Records")


exp_imp_plot2 <- UKTradeRecords %>% 
  filter(type == "Imports"|type == "Exports", str_detect(period, ryears)) %>%
  ggplot(aes(x=period, 
             y=records, 
             color=type, 
             group=type)) +
  geom_line(size = 1.5) +
 # scale_x_discrete(breaks = xscale) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_line(color="light gray")) +
  labs(x="", y = "Number of Records", title = "Recent Years Only")

eu_noneu_plot2 <- UKTradeRecords %>% 
  filter(type != "Imports" & type != "Exports", str_detect(period, ryears)) %>%
  ggplot(aes(x=period, 
             y=records, 
             color=type, 
             group=type)) +
  geom_line(size = 1.5) +
 # scale_x_discrete(breaks = xscale) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        panel.grid = element_line(color="light gray")) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y = "Number of Records")



# exp_imp_plot1 + eu_noneu_plot1 + plot_layout(ncol=1)
# exp_imp_plot2 + eu_noneu_plot2 + plot_layout(ncol=1)


exp_imp_plot1 + exp_imp_plot2 + eu_noneu_plot1 + eu_noneu_plot2 + plot_layout(widths=c(2,1),ncol=2)









