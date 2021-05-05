# 20171004
# Updated 2021xxxx by Ethan Linke
# R Shiny script for web based user interface with PGSQL database.
# Written by Louis Tsiattalou for TradeDataVis project.
# Github: https://github.com/FoodStandardsAgency/TradeDataVis

# SCRIPT START ###############################################################

# Load Packages --------------------------------------------------------------

if(require("shiny") == FALSE) {install.packages("shiny")}
library("shiny")

if(require("shinyjs") == FALSE) {install.packages("shinyjs")}
library("shinyjs")

if(require("shinyWidgets") == FALSE) {install.packages("shinyWidgets")}
library("shinyWidgets")

if(require("shinycssloaders") == FALSE) {install.packages("shinycssloaders")}
library("shinycssloaders")

if(require("tidyverse") == FALSE) {install.packages("tidyverse")}
library("tidyverse")

 if(require("devtools") == FALSE) {install.packages("devtools")}
library("devtools")

if(require("RPostgreSQL") == FALSE) {install.packages("RPostgreSQL")}
library("RPostgreSQL")

# The development version of ggplot2 is necessary for the plotly time series plots to render correctly
# Connect to mobile tether to install from github - FSA Firewall blocks it!
# Comment out install_github lines when installed on your machine
#install_github("tidyverse/ggplot2",force=TRUE)
library("ggplot2")

# Same for pool - otherwise it's all over the place with my app
#install_github("rstudio/pool", force = TRUE)
 library("pool")

# NetworkD3 conflicts with many shiny elements. Created an FSA version which fixes issues.
#install_github("fsa-analytics/networkD3")
library("networkD3")

# Leaflet had some crashing issues in QA...
#install_github("rstudio/leaflet")
library("leaflet")

if(require("rgeos") == FALSE) {install.packages("rgeos")}
library("rgeos")

if(require("maptools") == FALSE) {install.packages("maptools")}
library("maptools")

if(require("maps") == FALSE) {install.packages("maps")}
library("maps")

if(require("DT") == FALSE) {install.packages("DT")}
library("DT")

if(require("ggmap") == FALSE) {install.packages("ggmap")}
library("ggmap")

if(require("plotly") == FALSE) {install.packages("plotly")}
library("plotly")

if(require("scales") == FALSE) {install.packages("scales")}
library("scales")

if(require("shinythemes") == FALSE) {install.packages("shinythemes")}
library("shinythemes")

# if(require("xlsx") == FALSE) {install.packages("xlsx")}
# library("xlsx")
# 
# if(require("writexl") == FALSE) {install.packages("writexl")}
# library("writexl")


# Function Definitions -------------------------------------------------------

# Descendants - obtain all descendants of a vector of commodity codes.
# Tested on "01" and "02", takes 0.25 secs for 500 descendants. Quick!
descendants <- function(data, codes) {
  # Strip 8-digit codes (these are leaf nodes)
  codes <- codes[nchar(codes) < 8]
  if (length(codes) == 0) {
    return()
  } else {
    # get all the children's indices
    f <- data$parent %in% codes
    # get current children
    children <- data$commoditycode[f]
    return(c(children, descendants(data, children)))
  }
}

# comcodeshort - Gets comcode from comcode - description format
comcodeshort <- function(long) {
  # Handle "All" elements
  long <- vapply(long, function(x) {if (x == "All") {return(x <- "All - ")} else return(x)}, character(1))
  # Handle " All Food & Feed elements
  long <- vapply(long, function(x) {if (x == "All Food & Feed") {return(x <- "All Food & Feed - ")} else return(x)}, character(1))
  short <- substr(long, 1, str_locate(long, " - ")[,1] - 1)
  return(short)
}

# comcodelong - Gets comcode - description from comcode
comcodelong <- function(short) {
  # Handle "All - " elements; probably not necessary but will keep commented just in case.
  # short <- vapply(short, function(x) {if (x == "All - ") {return(x <- "All")} else return(x)}, character(1))
  long <- paste(short, comcode$description[match(short, comcode$commoditycode)], sep = " - ")
  return(long)
}

# Load Prerequisite Static data - Ports, Comcodes, etc. ======================
# Use pool instead of dbConnect
# setwd("C:/Users/ltsiattalou/Documents/R/ImportTool/ShinyMain/")
# setwd("C:/Users/901280/OneDrive - Food Standards Agency/Documents/trade data vis app/New New version")

dbenv <- read_delim("ATT52349.env", delim = "=", col_names = FALSE, trim_ws = TRUE)

# pg <- dbDriver("PostgreSQL")
# tradedata <- dbConnect(pg, user=dbenv[1,2], password=dbenv[2,2], host=dbenv[3,2], port=dbenv[4,2], dbname=dbenv[5,2])
tradedata <- dbPool(
    drv = RPostgreSQL::PostgreSQL(max.con=40),
    user = dbenv[1,2],
    password = dbenv[2,2],
    host = dbenv[3,2],
    port = dbenv[4,2],
    dbname = dbenv[5,2],
    minSize = 3,
    idleTimeout = 1200000  # 20 minutes
)

onStop(function() {
    poolClose(tradedata)
})



# Load Metadata
portcode <- dbGetQuery(tradedata, "SELECT * FROM port")
comcode <- dbGetQuery(tradedata, "SELECT * FROM comcode")
countrycode <- dbGetQuery(tradedata, "SELECT * FROM country")
allfoodfeed <- dbGetQuery(tradedata, "SELECT * FROM allfoodfeed")


#Adding a row which contains the blank ports so these also get queried from the database
portcode<-rbind(portcode,c("Unknown Port", "   ",NA,NA,NA, "Unknown Region"))
portcode <- rbind(portcode, c("EU Trade - Port Unknown", "EU UNK",NA,NA,NA, "Unknown Region"))
countrycode <- rbind(countrycode, c("EU Dispatch - Unknown Origin", "UNK EU", TRUE, TRUE))

# Order Ascending
portcode <- portcode %>% arrange(portname)
comcode <- comcode %>% arrange(commoditycode)
countrycode <- countrycode %>% arrange(countryname)


# Remove Duplicate Countrycodes
countrycode <- countrycode[!duplicated(countrycode$countrycode),]

# Define EU/Non-EU subsets
eucountrycode <- countrycode %>% filter(eu == TRUE)
noneucountrycode <- countrycode %>% filter(non_eu == TRUE) %>% filter(!countrycode %in% eucountrycode$countrycode)

# Factor enables multiple search terms in comcode lookup tab
comcodelookup <- tibble(commoditycode = as.factor(comcode$commoditycode), description = comcode$description)

#desclookup <- c(portcode$portname,countrycode$countryname)
#names(desclookup) <- c(portcode$portcode,countrycode$countrycode)

desclookup <- c(portcode$portname,countrycode$countryname,comcodelong(comcode$commoditycode))
names(desclookup) <- c(portcode$portcode,countrycode$countrycode,comcode$commoditycode)

desclookup <- data.frame(keyName=names(desclookup), value=desclookup, row.names=NULL, stringsAsFactors = FALSE)
desclookup <- desclookup[desclookup$value != "",]
#desclookup$keyName[desclookup$value == "EU Dispatch - Unknown Origin"] <- "UNK EU"


#desclookup <-rbind(desclookup,data.frame(keyName="04090000",value="04090000 - description"))

itemCount <- 5

# Note - no data loaded at this stage - we query what we need when we need it

# Create list of 2, 4, 6, 8 digit commodity codes. Sort by string ascending.
comcode_2 <- comcode[nchar(comcode$commoditycode) == 2,]
comcode_4 <- comcode[nchar(comcode$commoditycode) == 4,]
comcode_6 <- comcode[nchar(comcode$commoditycode) == 6,]
comcode_8 <- comcode[nchar(comcode$commoditycode) == 8,] 




# Create month list
# Two and a half month gap between current date and data contained in App.
curyr <- as.numeric(substr(as.character(Sys.Date()),3,4))
curmth <- as.numeric(substr(as.character(Sys.Date()),6,7))
curday <- as.numeric(substr(as.character(Sys.Date()),9,10))
dates <- character(0)

# Current Year
for (j in curmth:1) {
  dates <- c(dates, paste0(20, as.character(curyr), "-", as.character(sprintf("%02d", j)) ))
}

# All Other Years
syrs <- as.character(sprintf("%02d",c(curyr-1:09)))
smths <- as.character(sprintf("%02d",c(12:1)))
for (i in syrs){
  for (j in smths){
    dates <- c(dates, paste0("20", i, "-", j))
  }
}
# 2 and a bit months time lag: pop off first two/three elements depending on day.
dates <- if (curday > 15) {
              dates[3:length(dates)]
          } else {
              dates[4:length(dates)]    
          }

# Defining messages
noneusuccess <- NULL
eusuccess <- NULL
noneunosuccess <- NULL
eunosuccess <- NULL




# UI ==========================================================================
# Set plot animation colour
options(spinner.color="#0dc5c1")

ui <- navbarPage(theme = shinytheme("flatly"), inverse = TRUE, 
                 
      # Navbar Title           
      title = ( "UK Trade Data Visualisation"),           
      

  # WELCOME PAGE --------------------------------------------------------------
  tabPanel("Welcome",
tags$head(HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
               <script async src='https://www.googletagmanager.com/gtag/js?id=UA-147223602-1'></script>
               <script>
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', 'UA-147223602-1');
               </script> ")),
#div(img(src = "FSA logo.png", height = 80, width = 160), style="float: right;"),
  #fluidRow(column(10,
           tags$h1("Welcome to the Trade Data Visualisation Application!"),
           # column(6,
           #        div(img(src = "FSA logo.png", height = 80, width = 160), style="float: right;")),
           tags$hr(),
           HTML("<div class=\"alert alert-dismissible alert-success\">
                    <strong>Welcome!</strong> This is the Trade Data Visualisation app. Please take the time to read through this page to familiarise yourself with the functionality of the app. If you encounter any bugs/crashes, please let us know so we can fix them by emailing Tim.Johnston@food.gov.uk.<br>
                Please only use in Chrome. Internet Explorer and Microsoft Edge are unsupported.
                </div>"),
           tags$hr(),
           column(8,
           tags$h3("Introduction"),
           tags$p("This application allows you to access and analyse HMRC trade data downloaded from ", tags$a(href = "https://www.uktradeinfo.com/trade-data/latest-bulk-datasets/bulk-datasets-guidance-and-technical-specifications/", "UKTradeInfo"), "."),
           tags$p("The application provides three key visualisations: ",
                  tags$ul(
                           tags$li("Sankey Diagram - a network diagram describing the flow of absolute quantities (including Price and Weight) between countries, commodities and ports. You can hover over a connection to find the value."),
                           tags$li("Interactive World Map - a coloured map of the world showing where our imports/exports are coming from/to; the colours of each country tells you the value for your selected unit, and you can click a country for more information."),
                           tags$li("Bar Charts by Comcode/Country/Port - these show the proportion of the imports/exports contributed by each country, commodity and port.")
                       )
                  ),
           tags$p("Data can also be downloaded for further analysis using this application.  Please note that port data is not available for EU imports or for EU exports before 2021.  Additionally, number of consignments data is only available for EU trade before 2021."),
           tags$hr()),
           column(4,
                  HTML("<div class=\"alert alert-dismissible alert-success\">
                    <h5> <strong> Update Information: </strong> </h5> 
                    The data used by this application is normally updated by the <strong> 13th of every month </strong>. <br>
                    However, on occasions when the publishing date of HMRC trade data is delayed, the update will occur up to a few days after the 13th. <br>
                    Please note, there is a two month time lag between the current date and the available HMRC data. For instance, January's trade data is updated on March 13th, February's on April 13th, and so on.
                    
                </div>")),
           column(12,
           tags$h3("How to Use the Application"),
           tags$p("To use the application, first navigate to the query panel tab - this is where you make your selections. There are two options for querying data: either selecting by commodity code or selecting a filter for food/feed. To search by commodity code, use the boxes to input commodity codes at the level of detail you require (2 - 8 digits). You can search for a product in the commodity code search table at the bottom of the page. If you would prefer to search using a food/feed filter please select the 'By Commodity Type' tab. This will allow you to use the buttons to select: all food & feed, just food, or just feed, as well as all food of non-animal origin (FNAO) & all products of animal origin (POAO), just FNAO or just POAO.  Please note that these lists have been compiled by the FSA and are not official data sources, and some commodity codes are classified as both food/feed and POAO/FNAO. To see this list of food/feed commodity codes, please download using the button below."),
           tags$p("Once you know what you're looking for, make your selections in the dropdown boxes and hit the ", tags$i("Run Query"), " button. This will load all of the matching data from the database into the app. You will need to open the ", tags$i("Results Panel"), "tab to see the visualisations and download the query data. Little notifications in the bottom right will alert you to progress, and let you know if data was found for your selections."),
           tags$p("Once the visualisations show up, you will see:",
                  tags$ul(
                           tags$li("A mini Commodity Code Lookup table, showing", tags$i("only the commodity codes currently in play.")),
                           tags$li("A row containing a Date Slider, Unit Selector and an option to choose between EU and non-EU data. This will enable you to filter the query you've already made to show the appropriate unit and enable you to see the evolution of relevant trade data activity in time by clicking and dragging the date slider."),
                           tags$li("A tab selection panel enabling you to switch between the interactive visualisations."),
                           tags$li("A Download Query Data Button, which downloads the results of the unfiltered query you just made by clicking \"Run Query\"."),
                           tags$li("A Download Commodity Codes Button, which allows you to download a list of all the commodity codes currently in play. ")
                           ),
                  "There is also an ", tags$i("advanced download"), " tab which allows you to change how you download the data. For example, you can choose to download the data at a 2 digit commodity code level and sum over all different countries. "),
           tags$p("You can also run another query by changing the selectors (or clicking the Clear Selections button and starting again), then clicking Run Query again."),
           downloadButton("foodfeeddownload", "Download food/feed commodity code list"),
           tags$hr(),
           tags$h3("Known Bugs"),
                  tags$p(tags$u("Small Visualisations/Data Tables not loading"),
                  tags$br(),
                  tags$b("Solution: "), "Use Chrome.",
                  tags$br(),
                  tags$br(),
                  tags$u("Sankey Diagram only shows lines, not nodes"),
                  tags$br(),
                  tags$b("Solution: "), "Too many countries/comcodes/ports. Narrow your query to a smaller Date Range or fewer Commodity Codes.",
                  tags$br(),
                  tags$br(),
                  tags$u("Commodity codes missing at the 4 or 6 digit level"),                  
                  tags$br(),
                  tags$b("Solution: "), "Go straight to the 8 digit level. You can use the search function to find the code you are looking for.",
                  tags$br(),
                  tags$br(),
                  tags$u("Namibia data doesn't appear"),  
                  tags$br(),
                  tags$b("Solution: "), "If you require data for Namibia, contact the tool owner."),
                  tags$br(),
           tags$hr(),
           tags$h3("About"),
           tags$p("Github:", tags$a(href = "https://github.com/FoodStandardsAgency/TradeDataVis", "FSA Analytics Github"),
                  #tags$br(),
                  #"Version Number:", "1.0.2",
                  tags$br(),
                  "Release Date:", "02/09/2019",
                  tags$br(),
                  "Contact:", tags$a(href = "mailto:tim.johnston@food.gov.uk", "Tim Johnston")
                  ),
           tags$p("This application was developed by Louis Tsiattalou (Operational Research Fast Stream) at the Food Standards Agency; with infrastructure support from Arthur Lugtigheid (Data Science) and Tim Johnston (Operational Research). QA was performed by Harry Grantham-Hill (Operational Research). Updates were implemented by Anna Webb (Operational Research Fast Stream) and Ethan Linke (Operational Research Fast Stream).")
           )),
  
  # QUERY PARAMETERS-----------------------------------------------------
  
  tabPanel("Query Panel",
      # Query Options
    fluidRow(      
        
    # Define date selectors and four cascading inputs - don't allow "All" on 2-digit comcode
      column(3,
        selectizeInput("datestart", "Period Start:",
                       choices=dates),
        selectizeInput("dateend", "Period End:",
                       choices=dates),
        selectizeInput("countryselect", "Country:",
                       selected = "All",
                       choices=list("All", "All EU",  "All Non-EU",
                                    `Non-EU` = noneucountrycode$countryname,
                                    `EU` = eucountrycode$countryname),
                       options = list(maxItems = 20)),
        selectizeInput("portselect", "Port:",
                       selected = "All",
                       choices=list(`All` = "All",
                                 `Region` = c("England", "Scotland", "Wales", "Northern Ireland", "Channel Islands and Isle of Man", "Unknown Region"),
                                 `Specific` = portcode$portname),
                       options = list(maxItems = 20)),
        br(),
        
        radioButtons("impexpSelect", label = NULL, inline = TRUE,
                     choices = c("Imports","Exports")),
        #br(),
        
        br(),
        br(),
        br(),
        
        actionButton("queryButton", "Run Query"),
        br(),
        br(),
        actionButton("queryClear", "Clear Selections"),
        #actionButton("removenotification", "remove test")
        br()
        
        
        ),
      column(9,
             tabsetPanel(id="byCommodityTab",
               tabPanel("By Commodity Code",
                        br(),
        #checkboxInput("comcodefilter","Please select this box to query by commodity code.",width = "900px", value=TRUE),
        selectizeInput("comcode2", "2-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_2$commoditycode)),
                       options = list(maxItems = 20)),
        selectizeInput("comcode4", "4-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_4$commoditycode)),
                       options = list(maxItems = 20)),
        selectizeInput("comcode6", "6-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_6$commoditycode)),
                       options = list(maxItems = 20)),
        selectizeInput("comcode8", "8-digit Commodity Code:",
                       selected = "All",
                       width = "100%",
                       choices=c("All", comcodelong(comcode_8$commoditycode)),
                       options = list(maxItems = 20)),
        hr()),
              tabPanel("By Commodity Type",
                       br(),
        #tags$h4("Filter query"),
       # checkboxInput("foodfeedfilter","Please select this box to query by commodity type.",width = "900px"),
        radioButtons("foodorfeed", label = "Select Food and/or Feed:", inline = TRUE,
                     choices = c("All Food & Feed","Food", "Feed")),
        radioButtons("poaoorfnao", label = "Select FNAO Products and/or POAO Products:", inline = TRUE,
                     choices = c("All FNAO & POAO","FNAO", "POAO")),
        hr())),
       br(),
        tags$h4("Advanced options"),
        "These are set to default values and should be used only by those familiar with trade data.",
        radioButtons("tradeselect",label = "Select which type of trade data you would like. Note: Special trade includes all imports that enter the UK excluding re-exports. This matches Defra statistics.", inline= TRUE,
                     choices = c("All data", "Special trade", "General trade"), selected = "Special trade"),
        radioButtons("codorcooselect", label = "Select whether you would like to use the country of dispatch or the country of origin for imports. Please note that country of origin data is not available for consignments imported from the EU.", inline = TRUE,
                     choices = c("Country of Origin", "Country of Dispatch"), selected = "Country of Dispatch")
      
      )
    ),
    hr(),
    dataTableOutput("ComcodeLookup") %>% withSpinner(type=6)
    ),
  
  # QUERY RESULTS ---------------------------------------------------------

  tabPanel("Results Panel",
               # Head Styles
               tags$head(tags$style(HTML("
                 .progress-striped .bar {
                                         background-color: #149bdf;
                                         background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                                         background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                         background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                         background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                         background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                         -webkit-background-size: 40px 40px;
                                         -moz-background-size: 40px 40px;
                                         -o-background-size: 40px 40px;
                                         background-size: 40px 40px;
                                         }
                                         "))),
              uiOutput("impexpHeader"),

               # Create comcode legend
               fluidRow(
                 column(12,
                   dataTableOutput("ComcodeLegend")
                 ),
                 hr()
               ),

               # Create slider/unit bar
               fluidRow(
  
                 
                 tags$style("
                              .irs-bar-edge,
                              .irs-bar
                             {
                                background: #e6e6e6;
                                border-color: #cfcfcf;
                              }
                              "),
                 
                 column(2,
                   tags$label("Select Month:"),
                   checkboxInput("dateSliderAll", label = "All", value = TRUE)
                        ),
                 column(4,
                   shinyjs::disabled(sliderTextInput("dateSlider", label = NULL, grid = TRUE, force_edges = TRUE,
                                              choices = c(dates)))
                        ),
                 column(6,
                   tags$label("Select EU or Non-EU:"),
                   radioButtons("EUNonEUSelect", label = NULL, inline = TRUE,
                                choices = c("All", "EU", "Non-EU"), selected = "All"))
               ),
  
           
              fluidRow(
                column(6, 
                       br(),
                  textOutput("NoDataMessage"),
                       tags$head(tags$style("#NoDataMessage{
                                            font-size: 18px;
                                            color: red;}"))),
                column(6,
                  tags$label("Select Units:"),
                  radioButtons("unitSelect", label = NULL, inline = TRUE,
                                choices = c("Price (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)", "Number of Consignments")))
              ),
                
              fluidRow(
                column(6),
                column(6, 
                       textOutput("ConsignmentsMessage"),
                       tags$head(tags$style("#ConsignmentsMessage{
                                            font-size: 13px;}")))
              ),
               # Create a spot for the plots
               fluidRow(
                 column(12,
                   tabsetPanel(
                     tabPanel("FLOW",
                       tabsetPanel(
                         tabPanel("Full", 
                                  # br(),
                                  # fluidRow(
                                  # column(9),column(3,
                                  # sliderInput("fontsizeSelect", "Select Font Size:", min = 1, max = 10, value = 3, width = 200))),
                                  sankeyNetworkOutput(outputId = "sankeyTrade") %>% withSpinner(type=6)),
                         tabPanel("Country & Port Only", sankeyNetworkOutput(outputId = "pcSankeyTrade") %>% withSpinner(type=6))
                       )
                     ),
                     tabPanel("MAP", leafletOutput(outputId = "worldMap", height = 600) %>% withSpinner(type=6)),
                     tabPanel("TIME SERIES",
                       tabsetPanel(
                         tabPanel("By Commodity Code", plotlyOutput(outputId = "tsByComcode") %>% withSpinner(type=6),#Test image download button
                                  fluidRow(
                                    column(10)#,
                                    #column(2, downloadButton("downloadplotnoneu", "Download plot"))
                                  )),
                         tabPanel("By Country", plotlyOutput(outputId = "tsByCountry") %>% withSpinner(type=6)),
                         tabPanel("By Port", plotlyOutput(outputId = "tsByPort") %>% withSpinner(type=6))
                       )
                     )
                   )
                 )
               ),

               # Create a spot for the download button
               fluidRow(
                 column(2, downloadButton("dataDownload", "Download Query Data"))
               ),
               br(),

               # Create a spot for the commodity code download button
               fluidRow(
                 column(2, downloadButton("noneucomcodedownload", "Download Commodity Codes"))
               ),
               br()


             ),

  # Advanced Download   --------------------------------------------------------------


tabPanel("Advanced Download",
         # Head Styles
         tags$head(tags$style(HTML("
                                   .progress-striped .bar {
                                   background-color: #149bdf;
                                   background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                                   background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                   background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                   background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                   background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                   -webkit-background-size: 40px 40px;
                                   -moz-background-size: 40px 40px;
                                   -o-background-size: 40px 40px;
                                   background-size: 40px 40px;
                                   }
                                   "))),
         "This tab allows you to choose more advanced options for how you download the data from your query.",
       #  tags$h3("Non-EU Trade"),
         "Please choose the options you would like:",
         checkboxInput("noneumonthsum", "Sum over months"),
         checkboxInput("noneucountrysum", "Sum over countries"),
         checkboxInput("noneuportsum", "Sum over ports"),
         radioButtons("noneucnlevel", "Choose which commodity code digit level you would like to sum over", inline = TRUE,
                      selected = "8-digit",
                      choices = c("2-digit","4-digit","6-digit","8-digit")),
         fluidRow(
         #  column(10),
           column(2, downloadButton("noneuadvancedownload", "Download data"))
         ),
         
         # tags$h3("EU Trade"),
         # "Please choose the options you would like:",
         # checkboxInput("eumonthsum", "Sum over months"),
         # checkboxInput("eucountrysum", "Sum over countries"),
         # radioButtons("eucnlevel", "Choose which commodity code digit level you would like to sum over", inline = TRUE,
         #              selected = "8-digit",
         #              choices = c("2-digit","4-digit","6-digit","8-digit")),
         # fluidRow(
         #   column(10),
         #   column(2, downloadButton("euadvancedownload", "Download data"))
         # ),
         tags$br(),
         "Please note that not all commodity codes at the 4 and 6 digit level exist in the official commodity code hierarchy. This is indicated in downloaded data when the description box is blank."
         
         
        ),

  # Feedback Form (I suggest you leave this part folded...)
  # tabPanel("Feedback",
  #          HTML("<p>Feedback form not showing up? <a href=https://fsaanalytics.wufoo.co.uk/forms/mmr67cc1bqdty6/>Fill it out here!</a><hr>
  #              <div id=\"wufoo-mmr67cc1bqdty6\">
  #                       Fill out my <a href=\"https://fsaanalytics.wufoo.co.uk/forms/mmr67cc1bqdty6\">online form</a>.
  #              </div>
  #                <div id=\"wuf-adv\" style=\"font-family:inherit;font-size: small;color:#a7a7a7;text-align:center;display:block;\">The easy to use <a href=\"http://wufoo.co.uk/form-builder/\">Wufoo form builder</a> helps you make forms easy, fast, and fun.</div>
  #                                                                                                                                                                                                                                                           <script type=\"text/javascript\">var mmr67cc1bqdty6;(function(d, t) {
  #                                                                                                                                                                                                                                                               var s = d.createElement(t), options = {
  #                                                                                                                                                                                                                                                                   'userName':'fsaanalytics',
  #                                                                                                                                                                                                                                                                   'formHash':'mmr67cc1bqdty6',
  #                                                                                                                                                                                                                                                                   'autoResize':true,
  #                                                                                                                                                                                                                                                                   'height':'1262',
  #                                                                                                                                                                                                                                                                   'async':true,
  #                                                                                                                                                                                                                                                                   'host':'wufoo.co.uk',
  #                                                                                                                                                                                                                                                                   'header':'show',
  #                                                                                                                                                                                                                                                                   'ssl':true};
  #                                                                                                                                                                                                                                                               s.src = ('https:' == d.location.protocol ? 'https://' : 'http://') + 'www.wufoo.co.uk/scripts/embed/form.js';
  #                                                                                                                                                                                                                                                               s.onload = s.onreadystatechange = function() {
  #                                                                                                                                                                                                                                                                   var rs = this.readyState; if (rs) if (rs != 'complete') if (rs != 'loaded') return;
  #                                                                                                                                                                                                                                                                   try { mmr67cc1bqdty6 = new WufooForm();mmr67cc1bqdty6.initialize(options);mmr67cc1bqdty6.display(); } catch (e) {}};
  #                                                                                                                                                                                                                                                               var scr = d.getElementsByTagName(t)[0], par = scr.parentNode; par.insertBefore(s, scr);
  #                                                                                                                                                                                                                                                           })(document, 'script');</script>"
  #          )
  #       ),
  # Enable ShinyJS support for cleaner on-click and disable features.
  shinyjs::useShinyjs()
)




# SERVER ======================================================================

server <- function(input, output, session) {

  queryData <- reactiveValues(dataraw = NULL, dataraw_EU = NULL, dataraw_NonEU = NULL,
                              portsumraw = NULL, portsumraw_EU = NULL, portsumraw_NonEU = NULL,
                              countrysumraw = NULL, countrysumraw_EU = NULL, countrysumraw_NonEU = NULL,
                              comcodesumraw = NULL, comcodesumraw_EU = NULL, comcodesumraw_NonEU = NULL)
  comcodeLegendData <- reactiveValues(comcodelegend = NULL)
  sankeyData <- reactiveValues(links = NULL, nodes = NULL, pclinks = NULL, pcnodes = NULL)
  mapData <- reactiveValues(mapWorld = NULL)
  timeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL, byPort = NULL)

  nullDataframe <- reactiveValues(nullDataframe = NULL, eunullDataframe = NULL, comcodequery = NULL)

  # euQueryData <- reactiveValues(euDataRaw = NULL)
  # euComcodeLegendData <- reactiveValues(comcodelegend = NULL)
  # euSankeyData <- reactiveValues(links = NULL, nodes = NULL)
  # euMapData <- reactiveValues(mapWorld = NULL)
  # euTimeseriesData <- reactiveValues(byComcode = NULL, byCountry = NULL)




  # SERVER SIDE COMMODITY CODE LOOKUP -----------------------------------------
   output$ComcodeLookup = renderDataTable(comcodelookup,
                                          filter = "top",
                                          rownames = FALSE,
                                          colnames = c("Commodity Code", "Description"),
                                          class = "cell-border stripe",
                                          options = list(
                                            #  dom = "t", # disable search bar at top
                                            pageLength = 25, # set number of elements on page
                                            language = list(search = "Search Comcodes and Descriptions:"), # Change Search Text.
                                            columnDefs = list(list(width = "150px", targets = 0)) # Set width of Commodity Code column to be 150px wide
                                            )
                                          )

  # SHINYJS ONCLICK STATEMENTS -----------------------------------------------
  shinyjs::onclick("countryselect", {updateSelectizeInput(session, "countryselect", selected = input$countryselect[which(!input$countryselect == "All")])})
  shinyjs::onclick("portselect", {updateSelectizeInput(session, "portselect", selected = input$portselect[which(!input$portselect == "All")])})
  shinyjs::onclick("comcode2", {updateSelectizeInput(session, "comcode2", selected = input$comcode2[which(!input$comcode2 == "All")])})
  shinyjs::onclick("comcode4", {updateSelectizeInput(session, "comcode4", selected = input$comcode4[which(!input$comcode4 == "All")])})
  shinyjs::onclick("comcode6", {updateSelectizeInput(session, "comcode6", selected = input$comcode6[which(!input$comcode6 == "All")])})
  shinyjs::onclick("comcode8", {updateSelectizeInput(session, "comcode8", selected = input$comcode8[which(!input$comcode8 == "All")])})

  # DATESLIDER OPTIONS ---------------------------------------------------------

  # Hide Dateslider options if the same date is picked.
  observe({
    input$datestart
    input$dateend

    if (input$datestart == input$dateend) {
      shinyjs::disable("dateSliderAll")
      shinyjs::disable("eudateSliderAll")
    } else {
      shinyjs::enable("dateSliderAll")
      shinyjs::enable("eudateSliderAll")
    }
  })

  observeEvent(input$dateSliderAll, {
    if (!input$dateSliderAll) {
      shinyjs::enable("dateSlider")
    } else {
      shinyjs::disable("dateSlider")
    }
  })

  observeEvent(input$eudateSliderAll, {
    if (!input$eudateSliderAll) {
      shinyjs::enable("eudateSlider")
    } else {
      shinyjs::disable("eudateSlider")
    }
  })

  # CONDITIONAL LABELLING/ENTRIES ON QUERY PARAMETERS --------------------------

  # Remove past dates from dateend selector
  observeEvent(input$datestart, {
    if (!input$datestart == "") {
      updateSelectizeInput(session, "dateend",
                           choices = dates[1:which(dates == input$datestart)])
    }
  })

  # Label Country selector appropriately as Origin/Dispatch
  observeEvent(input$impexpSelect, {
    if (input$impexpSelect == "Imports") {
      updateSelectizeInput(session, "countryselect", "Origin Country:")
    } else {
      updateSelectizeInput(session, "countryselect", "Dispatch Country:")
    }
  })
  # CLEAR DROPDOWNS ------------------------------------------------------------

  observeEvent(input$queryClear, {
    updateSelectizeInput(session, "datestart",
                         choices=dates)
    updateSelectizeInput(session, "dateend",
                         choices=dates)
    updateSelectizeInput(session, "countryselect",
                         selected = "All",
                         choices=list(`All` = c("All", "All EU", "All Non-EU"),
                                      `Non-EU` = noneucountrycode$countryname,
                                      `EU` = eucountrycode$countryname),
                         options = list(maxItems = 20))
    updateSelectizeInput(session, "portselect",
                         selected = "All",
                         choices=list(`All` = "All",
                                      `Region` = c("England", "Scotland", "Wales", "Northern Ireland", "Channel Islands and Isle of Man", "Unknown Region"),
                                      `Specific` = portcode$portname),
                         options = list(maxItems = 20))
    updateSelectizeInput(session, "comcode2",
                         selected = "All",
                         choices=c("All", comcodelong(comcode_2$commoditycode)),
                         options = list(maxItems = 20))
    updateSelectizeInput(session, "comcode4",
                         selected = "All",
                         choices=c("All", comcodelong(comcode_4$commoditycode)),
                         options = list(maxItems = 20))
    updateSelectizeInput(session, "comcode6",
                         selected = "All",
                         choices=c("All", comcodelong(comcode_6$commoditycode)),
                         options = list(maxItems = 20))
    updateSelectizeInput(session, "comcode8",
                         selected = "All",
                         choices=c("All", comcodelong(comcode_8$commoditycode)),
                         options = list(maxItems = 20))
  })

  # DROPDOWN CASCADING =========================================================


  observe({
    comcode_2_selection <- comcodeshort(input$comcode2)
    allDescendants <- descendants(comcode, comcode_2_selection)

    # Update Comcodes
    if (is.null(comcode_2_selection) == FALSE) {
      if ("All" %in% comcode_2_selection){
        updateSelectizeInput(session,"comcode4",
                             selected = "All",
                             choices=c("All", comcodelong(comcode_4$commoditycode)),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode6",
                             selected = "All",
                             choices=c("All", comcodelong(comcode_6$commoditycode)),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(comcode_8$commoditycode)),
                             options = list(maxItems = 20))
      } else {
        updateSelectizeInput(session,"comcode4",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 4]))),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode6",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 6]))),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                             options = list(maxItems = 20))
      }
    }
  })

  observe({
    comcode_4_selection <- comcodeshort(input$comcode4)
    comcode_2_selection <- comcodeshort(input$comcode2)
    allDescendants <- descendants(comcode, comcode_4_selection)
    allDescendants2<-descendants(comcode, comcode_2_selection)


    # Update Comcodes
    if (is.null(comcode_4_selection) == FALSE) {
      if ("All" %in% comcode_4_selection & "All" %in% comcode_2_selection){
        updateSelectizeInput(session,"comcode6",
                             selected = "All",
                             choices=c("All", comcodelong(comcode_6$commoditycode)),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All",  comcodelong(comcode_8$commoditycode)),
                             options = list(maxItems = 20))
      }  else if ("All" %in% comcode_4_selection){
        updateSelectizeInput(session,"comcode6",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants2[nchar(allDescendants2) == 6]))),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All",  comcodelong(sort(allDescendants2[nchar(allDescendants2) == 8]))),
                             options = list(maxItems = 20))

      } else {
        updateSelectizeInput(session,"comcode6",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 6]))),
                             options = list(maxItems = 20))
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                             options = list(maxItems = 20))
      }
    }
  })

  observe({

    comcode_6_selection <- comcodeshort(input$comcode6)
    comcode_4_selection <- comcodeshort(input$comcode4)
    comcode_2_selection <- comcodeshort(input$comcode2)
    allDescendants <- descendants(comcode, comcode_6_selection)
    allDescendants4 <- descendants(comcode, comcode_4_selection)
    allDescendants2 <- descendants(comcode, comcode_2_selection)

    # Update Comcodes
    if (is.null(comcode_6_selection) == FALSE) {
      if ("All" %in% comcode_6_selection & "All" %in% comcode_4_selection & "All" %in% comcode_2_selection){
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All",  comcodelong(comcode_8$commoditycode)),
                             options = list(maxItems = 20))
      }  else if ("All" %in% comcode_6_selection & "All" %in% comcode_4_selection ){
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants2[nchar(allDescendants2) == 8]))),
                             options = list(maxItems = 20))

      } else if ("All" %in% comcode_6_selection) {
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants4[nchar(allDescendants4) == 8]))),
                             options = list(maxItems = 20))
      } else {
        updateSelectizeInput(session,"comcode8",
                             selected = "All",
                             choices=c("All", comcodelong(sort(allDescendants[nchar(allDescendants) == 8]))),
                             options = list(maxItems = 20))
      }
    }
  })




  # QUERY EXECUTION ============================================================

#  observeEvent(input$removenotification,{
#  if (!is.null(noneusuccess)) {removeNotification(noneusuccess)}
#  if (!is.null(eusuccess)) {removeNotification(eusuccess)}
#  if (!is.null(noneunosuccess))
#    removeNotification(noneunosuccess)
#  if (!is.null(eunosuccess))
#    removeNotification(eunosuccess)


  # Defining messages
#  noneusuccess <<- NULL
#  eusuccess <<- NULL
#  noneunosuccess <<- NULL
#  eunosuccess <<- NULL
#  })





  observeEvent(input$queryButton,{
    input$queryButton

    #display "UK Imports" or "UK Exports" header on results panel depending on parameters
    if(input$impexpSelect == "Imports"){
      output$impexpHeader <- renderUI({
        headerPanel("UK Imports Data")
      })
    } else if (input$impexpSelect == "Exports"){
      output$impexpHeader <- renderUI({
        headerPanel("UK Exports Data")
      })
    }


    # Use selectors information to build plot
    isolate({

      #create vector with years 2021-2060
      FutureYears <- as.character(2021:2060)


      #create vector with EU country names and "All EU"
      EU_country_list <- c((eucountrycode %>% pull(countryname)), "All EU")

      #Disappear Number of Consignments radiobutton if datestart or dateend contain a year from 2021 onwards or if any non-EU country is selected
      if((substr(input$datestart,1,4) %in% FutureYears) |
         (substr(input$dateend,1,4) %in% FutureYears) |
         (FALSE %in% (input$countryselect %in% EU_country_list)) |
         (is.null(input$countryselect))
         ){
        updateRadioButtons(session, "unitSelect", choices = c("Price (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)"), inline = TRUE, selected = "Price (GBP)")
        output$ConsignmentsMessage <- renderText({
            "Please note that Number of Consignments data is not available for Non-EU data and is only available for EU data for dates before 2021.  If you wish to view Number of Consignments data, please adjust your query parameters accordingly."
        })
      } else {
        updateRadioButtons(session, "unitSelect", choices = c("Price (GBP)", "Weight (KG)", "Price Per Kilo (GBP/KG)", "Number of Consignments"), inline = TRUE, selected = "Price (GBP)")
      output$ConsignmentsMessage <- NULL
      }

      #reset options after another query
      updateRadioButtons(session, "EUNonEUSelect", choices = c("All", "EU", "Non-EU"), inline = TRUE, selected = "All")
      updateCheckboxInput(session, "dateSliderAll", value = TRUE)
      #updateSliderInput(session, "fontsizeSelect", value = 3)

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())

      progress$set(message = "Processing Selections", value = 0.25)

      # Set nullDataframe flag to FALSE
      nullDataframe$nullDataframe <- FALSE


      # Control handling for comcode selectors
      #Currently it's just putting the code 04069032 for the food/feed filter
      if ("All" %in% input$comcode2 | is.null(input$comcode2)) {comcode2query = "__"}
      else {comcode2query = comcodeshort(input$comcode2)}

      if ("All" %in% input$comcode4 | is.null(input$comcode4)) {comcode4query = "__"}
      else {comcode4query = comcodeshort(input$comcode4)}

      if ("All" %in% input$comcode6 | is.null(input$comcode6)) {comcode6query = "__"}
      else {comcode6query = comcodeshort(input$comcode6)}

      if ("All" %in% input$comcode8 | is.null(input$comcode8)) {comcode8query = "__"}
      else {comcode8query = comcodeshort(input$comcode8)}


      # This is kind of a funny way of doing things, but simply pasting the strings
      # together and taking the last 8 characters works quickly, easily and cleanly.
      comcodequery = paste(comcode2query, comcode4query, comcode6query, comcode8query, sep = "")
      comcodequery = substr(comcodequery, nchar(comcodequery)-7, nchar(comcodequery))

      # Country and Port Queries
      if ("All" %in% input$portselect | is.null(input$portselect)) {
        portquery <- portcode$portcode
      } else {
        portqueryRegion <- portcode %>% filter(region %in% input$portselect) %>% pull(portcode)
        portquerySelect <- portcode %>% filter(portname %in% input$portselect) %>% pull(portcode)
        portquery <- unique(c(portqueryRegion, portquerySelect))

      }

      if ("All" %in% input$countryselect | is.null(input$countryselect) | ("All EU" %in% input$countryselect & "All Non-EU" %in% input$countryselect)){
        countryquery <- countrycode$countrycode
      } else if ("All EU" %in% input$countryselect) {
        countryqueryEU <- eucountrycode$countrycode
        countryquerySelect <- noneucountrycode %>% filter(countryname %in% input$countryselect) %>% pull(countrycode)
        countryquery <- c(countryqueryEU, countryquerySelect)
      } else if ("All Non-EU" %in% input$countryselect) {
        countryquerynonEU <- noneucountrycode$countrycode
        countryquerySelect <- eucountrycode %>% filter(countryname %in% input$countryselect) %>% pull(countrycode)
        countryquery <- c(countryquerynonEU, countryquerySelect)
      } else {
        countryquery <- countrycode %>% filter(countryname %in% input$countryselect) %>% pull(countrycode)
      }


      #  --------------------------------------------------------

      # Obtain date range
      daterangequery <- rev(dates[match(input$dateend,dates):match(input$datestart,dates)])
      # Update dateSlider with daterangequery
      updateSliderTextInput(session,"dateSlider",
                           choices=daterangequery)

      # Convert to Query Format, duplicate dates replacing '20' with 99' to account for BTTA in arrivals data
      daterangequery <- c(paste0(substr(daterangequery,6,7),
                               "/",
                               substr(daterangequery,1,4)),
                          paste0(substr(daterangequery,6,7),
                                 "/99",
                                 substr(daterangequery,3,4)))



      # First line of query dependent on Import or Export - parametrize to select*sumquery vars.
      #Adding flexibility to choose country of origin or dispatch for non-EU imports

      if (input$impexpSelect == "Imports"){
        if (input$codorcooselect == "Country of Origin"){
          selectquery <- "SELECT coo_alpha,comcode,port_alpha,account_date,sum(value),sum(quantity_1),sum(number_of_consignments) FROM imports_and_arrivals "
          wherecountrycondition <- ")') AND (coo_alpha SIMILAR TO '("
          groupbyquery <- "GROUP BY coo_alpha,comcode,port_alpha,account_date"}
        else if (input$codorcooselect == "Country of Dispatch"){
          selectquery <- "SELECT cod_alpha,comcode,port_alpha,account_date,sum(value),sum(quantity_1),sum(number_of_consignments) FROM imports_and_arrivals "
          wherecountrycondition <- ")') AND (cod_alpha SIMILAR TO '("
          groupbyquery <- "GROUP BY cod_alpha,comcode,port_alpha,account_date"}

        joinbyquery <- "imports_and_arrivals.comcode = allfoodfeed.comcodeff "

        if (input$tradeselect == "All data"){tradequery <- " "}
        else if(input$tradeselect == "Special trade"){tradequery <- "AND (trade_indicator = '0' OR trade_indicator = '5') AND NOT suite_indicator = '003' AND NOT suite_indicator  = '009' "}
        else if (input$tradeselect == "General trade"){tradequery <- "AND (trade_indicator = '0' OR trade_indicator = '4') AND NOT suite_indicator = '003' AND NOT suite_indicator  = '008' AND NOT suite_indicator  = '009' "}
      }
      else if (input$impexpSelect == "Exports"){
        selectquery <- "SELECT port_alpha,comcode,cod_alpha,account_date,sum(value),sum(quantity_1), sum(number_of_consignments) FROM exports_and_dispatches "
        wherecountrycondition <- ")') AND (cod_alpha SIMILAR TO '("
        groupbyquery <- "GROUP BY cod_alpha,comcode,port_alpha,account_date"
        joinbyquery <- "exports_and_dispatches.comcode = allfoodfeed.comcodeff "

        #Adding option to look at all data, special trade, or general trade
        if (input$tradeselect == "All data"){tradequery <- " "}
        else if(input$tradeselect == "Special trade"){tradequery <- "AND trade_indicator = '0' AND NOT suite_indicator = '003' AND NOT suite_indicator  = '009' "}
        else if (input$tradeselect == "General trade"){tradequery <- "AND (trade_indicator = '0' OR trade_indicator = '1') AND NOT suite_indicator = '003' AND NOT suite_indicator  = '008' AND NOT suite_indicator  = '009' "}
      }




      #Adding an if statement to do a different query for foodfeed filter
      #if (input$foodfeedfilter == TRUE)
      if(input$byCommodityTab == "By Commodity Type")
      {
        if (input$foodorfeed == "All Food & Feed" ){filterffquery<-""}
        else if (input$foodorfeed == "Food" ){filterffquery<-"AND allfoodfeed.food = 'Yes' "}
        else if (input$foodorfeed == "Feed" ){filterffquery<-"AND allfoodfeed.feed = 'Yes' "}

        if (input$poaoorfnao == "All FNAO & POAO" ){filterfpquery<-""}
        else if (input$poaoorfnao == "FNAO" ){filterfpquery<-"AND allfoodfeed.fnao = 'Yes' "}
        else if (input$poaoorfnao == "POAO" ){filterfpquery<-"AND allfoodfeed.poao = 'Yes' "}

        dataquery = paste0(selectquery,
                           "INNER JOIN allfoodfeed ON ", joinbyquery,
                           "WHERE (port_alpha SIMILAR TO '(",
                           paste(portquery,collapse = "|"),
                           wherecountrycondition, # This depends on cod/coo_alpha!
                           paste(countryquery,collapse = "|"),
                           ")') AND (account_date IN ('",
                           paste(daterangequery, collapse = "', '"),
                           "')) ",
                           filterffquery,
                           filterfpquery,
                           tradequery,
                           groupbyquery)
      }
      else{


      dataquery = paste0(selectquery,
                         "WHERE (comcode SIMILAR TO '(",
                         paste(comcodequery,collapse = "|"),
                         ")') AND (port_alpha SIMILAR TO '(",
                         paste(portquery,collapse = "|"),
                         wherecountrycondition, # This depends on cod/coo_alpha!
                         paste(countryquery,collapse = "|"),
                         ")') AND (account_date IN ('",
                         paste(daterangequery, collapse = "', '"),
                         "')) ",
                         tradequery,
                          groupbyquery) # import = coo_alpha, export = cod_alpha!
      }

      progress$set(message = "Querying Trade Data", value = 0.5)

      # Run Query
      dataraw <- dbGetQuery(tradedata, dataquery)

      # Break out of Non-EU Shaping if query returns no values (ie, df == dim 0,0)
      if (dim(dataraw)[1] == 0) {
        # Set nullDataframe flag to TRUE to stop downstream reactivity
        nullDataframe$nullDataframe <- TRUE
        nullDataframe$comcodequery <- paste(gsub("_","",comcodequery),collapse = ",")

        # Show a red notification warning the user that no data was found.
        isolate({
          showNotification(paste0("No data for selected query parameters."), type = "error", duration = 0, id="noneunosuccess")
        })
      } else {

        # Show a blue notification to notify the user the query was successful
        showNotification(paste0("Query successful!"), type = "message", duration = 0, id="noneusuccess")

        # Transform month back to readable format
        dataraw$account_date <- paste0("20",
                                       substr(dataraw$account_date,6,7),
                                       "-",
                                       substr(dataraw$account_date,1,2))

        # Set up correct colnames and create portsum/countrysum dataframes from dataraw
        if (input$impexpSelect == "Imports") {
            colnames(dataraw) = c("country","comcode","port","month","price", "weight", "consignments")
            dataraw$country[is.na(dataraw$country)] <- "Unknown Country" # blank country = <NA>
            #dataraw$port[dataraw$port == "   "] <- "UNK" # blank port = ""
            #dataraw$country[dataraw$country == "EU UNK"] <- "UNK EU" # change countrycode to match desclookup

            portsumraw <- dataraw %>%
                select(country,comcode,month,price,weight, consignments) %>%
                group_by(country,comcode,month) %>%
                summarise(price = sum(price), weight = sum(weight), consignments = sum(consignments))
            countrysumraw <- dataraw %>%
                select(comcode,port,month,price,weight, consignments) %>%
                group_by(comcode,port,month) %>%
                summarise(price = sum(price), weight = sum(weight), consignments = sum(consignments))
            comcodesumraw <- dataraw %>%
                select(country,port,month,price,weight,consignments) %>%
                group_by(country,port,month) %>%
                summarise(price = sum(price), weight = sum(weight), consignments = sum(consignments))

            #create dataframes filtered for EU countries only
            portsumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(country,comcode,month,price,weight,consignments) %>%
              group_by(country,comcode,month) %>%
              summarise(price = sum(price), weight = sum(weight), consignments = sum(consignments))
            countrysumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(comcode,port,month,price,weight, consignments) %>%
              group_by(comcode,port,month) %>%
              summarise(price = sum(price), weight = sum(weight), consignments=sum(consignments))
            comcodesumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(country,port,month,price,weight,consignments) %>%
              group_by(country,port,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))

            #create dataframes filtered for NonEU countries only
            portsumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(country,comcode,month,price,weight,consignments) %>%
              group_by(country,comcode,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            countrysumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(comcode,port,month,price,weight,consignments) %>%
              group_by(comcode,port,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            comcodesumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(country,port,month,price,weight,consignments) %>%
              group_by(country,port,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))

        } else if (input$impexpSelect == "Exports") {
            colnames(dataraw) = c("port","comcode","country","month","price", "weight", "consignments")
            dataraw$country[is.na(dataraw$country)] <- "Unknown Country" # blank country = <NA>
            #dataraw$port[dataraw$port == "   "] <- "UNK" # blank port = ""

            portsumraw <- dataraw %>%
                select(comcode,country,month,price,weight,consignments) %>%
                group_by(comcode,country,month) %>%
                summarise(price = sum(price), weight = sum(weight), consignments = sum(consignments))
            countrysumraw <- dataraw %>%
                select(port,comcode,month,price,weight,consignments) %>%
                group_by(port,comcode,month) %>%
                summarise(price = sum(price), weight = sum(weight), consignments=sum(consignments))
            comcodesumraw <- dataraw %>%
                select(port,country,month,price,weight,consignments) %>%
                group_by(port,country,month) %>%
                summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))

            portsumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(comcode,country,month,price,weight,consignments) %>%
              group_by(comcode,country,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            countrysumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(port,comcode,month,price,weight,consignments) %>%
              group_by(port,comcode,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            comcodesumraw_EU <- dataraw %>%
              filter(country %in% eucountrycode$countrycode) %>%
              select(port,country,month,price,weight,consignments) %>%
              group_by(port,country,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))

            portsumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(comcode,country,month,price,weight,consignments) %>%
              group_by(comcode,country,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            countrysumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(port,comcode,month,price,weight,consignments) %>%
              group_by(port,comcode,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
            comcodesumraw_NonEU <- dataraw %>%
              filter(country %in% noneucountrycode$countrycode) %>%
              select(port,country,month,price,weight,consignments) %>%
              group_by(port,country,month) %>%
              summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))

        }

        portsumraw <- ungroup(portsumraw)
        portsumraw_EU <- ungroup(portsumraw_EU)
        portsumraw_NonEU <- ungroup(portsumraw_NonEU)

        countrysumraw <- ungroup(countrysumraw)
        countrysumraw_EU <- ungroup(countrysumraw_EU)
        countrysumraw_NonEU <- ungroup(countrysumraw_NonEU)

        comcodesumraw <- ungroup(comcodesumraw)
        comcodesumraw_EU <- ungroup(comcodesumraw_EU)
        comcodesumraw_NonEU <- ungroup(comcodesumraw_NonEU)

      }



 
      # End Isolate
      })

    queryData$dataraw <- dataraw

    if (!nullDataframe$nullDataframe) {

    queryData$dataraw_EU <- dataraw %>% filter(country %in% eucountrycode$countrycode)
    queryData$dataraw_NonEU <- dataraw %>% filter(country %in% noneucountrycode$countrycode)
   # euQueryData$euDataRaw <- euDataRaw

   # if (!nullDataframe$nullDataframe) {
      queryData$portsumraw <- portsumraw
      queryData$portsumraw_EU <- portsumraw_EU
      queryData$portsumraw_NonEU <- portsumraw_NonEU

      queryData$countrysumraw <- countrysumraw
      queryData$countrysumraw_EU <- countrysumraw_EU
      queryData$countrysumraw_NonEU <- countrysumraw_NonEU

      queryData$comcodesumraw <- comcodesumraw
      queryData$comcodesumraw_EU <- comcodesumraw_EU
      queryData$comcodesumraw_NonEU <- comcodesumraw_NonEU
    }

  })


  # DATA SHAPING AND PLOTTING ===========================================

  observe({
    # FILTER EU,NON-EU/DATE/UNIT IN DATA -------------------------------------------------

    # Conditions for observe statement to run
    if (input$queryButton == 0) return()
    req(input$dateSlider)
    if (nullDataframe$nullDataframe == TRUE) {
      # Break out of reactive chain
      req(FALSE)
    }

    # Dependencies - changes to Date Slider and Unit Selector
    input$dateSlider
    input$unitSelect
    input$EUNonEUSelect

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())

    progress$set(message = "Clean and Shape Data", value = 1)

    # Prepare portsum, countrysum and comcodesum into appropriate format for rest of app
    # Based on date and unit, selected from fluidrow beneath comcode legend


    if (input$EUNonEUSelect == "EU"){
      dataraw <- queryData$dataraw_EU
      portsum <- queryData$portsumraw_EU
      countrysum <- queryData$countrysumraw_EU
      comcodesum <- queryData$comcodesumraw_EU
    } else if (input$EUNonEUSelect == "Non-EU"){
      dataraw <- queryData$dataraw_NonEU
      portsum <- queryData$portsumraw_NonEU
      countrysum <- queryData$countrysumraw_NonEU
      comcodesum <- queryData$comcodesumraw_NonEU
    } else {
      dataraw <- queryData$dataraw
      portsum <- queryData$portsumraw
      countrysum <- queryData$countrysumraw
      comcodesum <- queryData$comcodesumraw
    }

    # Select correct month
    if (input$dateSliderAll == TRUE) {
      portsum <- portsum %>% select(-month)
      countrysum <- countrysum %>% select(-month)
      comcodesum <- comcodesum %>% select(-month)
      if (input$impexpSelect == "Imports") {
        portsum <- portsum %>% group_by(country,comcode) %>% summarise(price = sum(price), weight = sum(weight), consignments=sum(consignments))
        countrysum <- countrysum %>% group_by(comcode,port) %>% summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
        comcodesum <- comcodesum %>% group_by(country,port) %>% summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
      } else if (input$impexpSelect == "Exports") {
        portsum <- portsum %>% group_by(comcode,country) %>% summarise(price = sum(price), weight = sum(weight), consignments=sum(consignments))
        countrysum <- countrysum %>% group_by(port,comcode) %>% summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
        comcodesum <- comcodesum %>% group_by(port,country) %>% summarise(price = sum(price), weight = sum(weight),consignments=sum(consignments))
      }
    } else {
      portsum <- portsum %>% filter(month == input$dateSlider) %>% select(-month)
      countrysum <- countrysum %>% filter(month == input$dateSlider) %>% select(-month)
      comcodesum <- comcodesum %>% filter(month == input$dateSlider) %>% select(-month)
    }

    # Select correct unit
    if (input$unitSelect == "Price (GBP)"){
      portsum <- portsum %>% select(-weight)
      countrysum <- countrysum %>% select(-c(weight,consignments))
      comcodesum <- comcodesum %>% select(-c(weight,consignments))



    } else if (input$unitSelect == "Weight (KG)"){
      portsum <- portsum %>% select(-c(price,consignments))
      countrysum <- countrysum %>% select(-c(price,consignments))
      comcodesum <- comcodesum %>% select(-c(price,consignments))


    } else if(input$unitSelect == "Number of Consignments"){
      portsum <- portsum %>% select(-c(price,weight))
      countrysum <- countrysum %>% select(-c(price,weight))
      comcodesum <- comcodesum %>% select(-c(price,weight))


    } else if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
      # For map:
      portcomcodesum <- portsum %>%
                            select(country, price, weight) %>%
                            group_by(country) %>%
                            summarise(price = sum(price), weight = sum(weight)) %>%
                            mutate(value = price/weight) %>%
                            select(country,value)
      # For sankey:
      portsum$value <- portsum$price / portsum$weight
      portsum <- portsum %>% select(-c(price,weight,consignments))
      countrysum$value <- countrysum$price / countrysum$weight
      countrysum <- countrysum %>% select(-c(price,weight,consignments))
      comcodesum$value <- comcodesum$price / comcodesum$weight
      comcodesum <- comcodesum %>% select(-c(price,weight,consignments))


    }



    # At this point there should be two string and one numeric vector in all sum
    # dataframes. Now rename that numeric vector, which is the unit used, to value.

    colnames(portsum)[colnames(portsum) %in% c("price","weight","consignments")] <- "value"
    colnames(countrysum)[colnames(countrysum) %in% c("price","weight","consignments")] <- "value"
    colnames(comcodesum)[colnames(comcodesum) %in% c("price","weight","consignments")] <- "value"

    # Ungroup the data frames.
    portsum <- ungroup(portsum)
    countrysum <- ungroup(countrysum)
    comcodesum <- ungroup(comcodesum)

    # Check again if, after sorting, we're dealing with a blank df.
    if (dim(portsum)[1] == 0) {


      observe({
        if(input$EUNonEUSelect == "All") {
          output$NoDataMessage <- renderText({
            paste0("No ",str_to_lower(input$impexpSelect), " for selected query parameters.")
          })
        } else {
          output$NoDataMessage <- renderText({
            paste0("No ",input$EUNonEUSelect, " ",str_to_lower(input$impexpSelect), " for selected query parameters.")
          })
        }

      })

   #  isolate({

       # if ()
       #  output$EUNonEUMessage <- renderText({
       #    paste0("No ",input$EUNonEUSelect, " ",input$impexpSelect, " for selected query parameters.")
       #  })
        # Show a red notification warning the user that no data was found.
      #   showNotification(paste0("No ",
      #                           input$EUNonEUSelect,
      #                           " ",
      #                           input$impexpSelect,
      #                           " for selected query parameters."
      #                           ), type = "warning", duration = 0)
      # })
      # Break out of reactive chain
     # req(FALSE)
    } else {
      observe({
      output$NoDataMessage <- NULL
      })
    }


    # CLEAN AND SHAPE DATA --------------------------------------------------

    isolate({

      # COMCODE LEGEND SPECIFIC -----------------------------------------------

      # Create df - list of commodity codes displayed. Match description to second col
      comcodelegend <- tibble(commoditycode = unique(portsum$comcode))
      comcodelegend <- left_join(comcodelegend, comcodelookup, by = "commoditycode") %>% arrange(commoditycode)


      # FULL SANKEY SPECIFIC --------------------------------------------------

      # Create Links & Nodes dataframe.

      link_portsum <- portsum
      colnames(link_portsum) <- c("source","target","value")
      link_countrysum <- countrysum
      colnames(link_countrysum) <- c("source","target","value")



      links <- bind_rows(link_portsum,link_countrysum)
      nodes <- data.frame(unique(c(links$source,links$target)),stringsAsFactors = FALSE)
      colnames(nodes) = "name"

      # Replace links source, target columns with IDs specified in nodes.
      # Match to row number in nodes (which is uniquely indexed!)
      # Note - must be zero indexed, hence match - 1
      links$source = vapply(links$source, function(x){
        x = match(x,nodes[,1])-1
      }, double(1))

      links$target = vapply(links$target, function(x){
        x = match(x,nodes[,1])-1
      }, double(1))

      # Replace node codes for country and port with full name
        nodes$name = vapply(nodes$name,function(x){
          replacement = desclookup[match(x,desclookup$keyName),"value"]
          if (is.na(replacement) == FALSE){
            x = replacement
            if(nchar(x) > 30){x = substr(x,1,30)}}
          else {x}
          return(x)
        }, character(1))

      # COUNTRY -> PORT SANKEY SPECIFIC ---------------------------------------

      pclinks <- comcodesum
      colnames(pclinks) <- c("source","target","value")
      pcnodes <- data.frame(unique(c(pclinks$source,pclinks$target)),stringsAsFactors = FALSE)
      colnames(pcnodes) = "name"

      # Replace pclinks source, target columns with IDs specified in pcnodes.
      # Match to row number in pcnodes (which is uniquely indexed!)
      # Note - must be zero indexed, hence match - 1
      pclinks$source = vapply(pclinks$source, function(x){
        x = match(x,pcnodes[,1])-1
      }, double(1))

      pclinks$target = vapply(pclinks$target, function(x){
        x = match(x,pcnodes[,1])-1
      }, double(1))

      # Replace node codes for country and port with full name
      pcnodes$name = vapply(pcnodes$name,function(x){
        replacement = desclookup[match(x,desclookup$keyName),"value"]
        if (is.na(replacement) == FALSE){
          x = replacement
          if(nchar(x) > 30){x = substr(x,1,30)}}
        else {x}
        return(x)
      }, character(1))

      # WORLDMAP SPECIFIC -----------------------------------------------------
      mapWorld <- map_data("world")

      # Convert mapWorld region to iso codes
      mapWorld$region <- iso.alpha(mapWorld$region)

      # Special cases
      ## Nigeria: maps package doesn't distinguish Niger and Nigeria...
      mapWorld$region[map_data("world")$region == "Nigeria"] <- "NG"
      ## replace RS (Serbia mapWorld) with XS (Serbia countrycode)
      mapWorld$region <- mapWorld$region %>% str_replace("RS","XS")

      # If an absolute quantity is selected, use portsum, strip commodities and aggregate.
      # If Price Per Kilo is selected, use portcomcodesum, which is already in our preferred format.
      if (input$unitSelect == "Price Per Kilo (GBP/KG)") {
          portsum_countrytotal <- portcomcodesum
      } else {
          portsum_countrytotal <- portsum[,c("country","value")] %>% group_by(country) %>% summarise(value = sum(value))
      }

      # Join values to mapWorld for plotting
      mapWorld <- left_join(mapWorld, portsum_countrytotal, by = c("region" = "country"))

      mapWorld <- left_join(mapWorld, countrycode, by = c("region" = "countrycode"))
      mapWorld <- mapWorld %>% select(-region)
      mapWorld <- mapWorld %>% rename(region = "countryname")

      # If using GGPlot, mapWorld DF is sufficient. If using Leaflet, need SpatialPolygons object.

      mapWorld_relevant <- mapWorld[!is.na(mapWorld$value),]
      rownames(mapWorld_relevant) <- NULL

      # turn into SpatialPolygons
      sp_mapWorld = lapply(unique(mapWorld_relevant$group), function(x) {
        latlonmatrix = as.matrix(mapWorld_relevant[mapWorld_relevant$group == x, c("long", "lat")])
        countryPolygons = Polygons(list(Polygon(latlonmatrix)), ID = x)
        return(countryPolygons)
        return(latlonmatrix)
      })

      dataPolygons = SpatialPolygonsDataFrame(SpatialPolygons(sp_mapWorld),
                                              distinct(bind_cols(
                                                region = mapWorld_relevant$region,
                                                group = mapWorld_relevant$group,
                                                value = mapWorld_relevant$value)),
                                              match.ID = FALSE)

      # TIME SERIES SPECIFIC --------------------------------------------------

      # > We have to have month information, which means portsum/countrysum aren't sufficient.
      # > We must use the queryData reactive portsumraw/countrysumraw and use dplyr on that.
      # > Unit selections are slightly different too - price per kilo must be summed by
      #   comcode, by port, and by country then divided for each month.


      # Select correct unit
      if (input$unitSelect == "Price (GBP)"){
        byComcode <- dataraw %>% select(month,comcode,price)
        byCountry <- dataraw %>% select(month,country,price)
        byPort <- dataraw %>% select(month,port,price)

      } else if (input$unitSelect == "Weight (KG)"){
        byComcode <- dataraw %>% select(month,comcode,weight)
        byCountry <- dataraw %>% select(month,country,weight)
        byPort <- dataraw %>% select(month,port,weight)

      } else if (input$unitSelect == "Number of Consignments"){
        byComcode <- dataraw %>% select(month,comcode,consignments)
        byCountry <- dataraw %>% select(month,country,consignments)
        byPort <- dataraw %>% select(month,port,consignments)


      }

      # Special case for Price Per Kilo
      if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
        if (input$dateSliderAll != TRUE) {
          byComcode <- dataraw %>% filter(month == input$dateSlider)
          byCountry <- dataraw %>% filter(month == input$dateSlider)
          byPort <- dataraw %>% filter(month == input$dateSlider)
        } else {
          byComcode <- dataraw
          byCountry <- dataraw
          byPort <- dataraw
        }

        byComcode <- byComcode %>%
                       select(month,comcode,price,weight) %>%
                       group_by(month,comcode) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
        byCountry <- byCountry %>%
                       select(month,country,price,weight) %>%
                       group_by(month,country) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
        byPort <- byPort %>%
                       select(month,port,price,weight) %>%
                       group_by(month,port) %>%
                       summarise(price = sum(price), weight = sum(weight)) %>%
                       mutate(value = price/weight) %>%
                       select(-c(price,weight))
      } else {
        # else statement required for non-PricePerKilo options
        colnames(byComcode)[colnames(byComcode) %in% c("price","weight","consignments")] <- "value"
        colnames(byCountry)[colnames(byCountry) %in% c("price","weight","consignments")] <- "value"
        colnames(byPort)[colnames(byPort) %in% c("price","weight","consignments")] <- "value"

        if (input$dateSliderAll != TRUE){
          byComcode <- byComcode %>% filter(month == input$dateSlider)
          byCountry <- byCountry %>% filter(month == input$dateSlider)
          byPort <- byPort %>% filter(month == input$dateSlider)
        }

        # Obtain long format dataframe for time series plot
        byComcode <- byComcode %>% group_by(month,comcode) %>% summarise(value = sum(value))
        byCountry <- byCountry %>% group_by(month,country) %>% summarise(value = sum(value))
        byPort <- byPort %>% group_by(month,port) %>% summarise(value = sum(value))
      }

      # Ungroup the data frames.
      byComcode <- ungroup(byComcode)
      byCountry <- ungroup(byCountry)
      byPort <- ungroup(byPort)

      # Replace country/port codes with full names.
      byCountry <- byCountry %>%
                     left_join(desclookup, by = c("country" = "keyName")) %>%
                     select(-country) %>%
                     rename(value = value.x, country = value.y)

      byPort <- byPort %>%
        left_join(desclookup, by = c("port" = "keyName")) %>%
        select(-port) %>%
        rename(value = value.x, port = value.y)

    # End Isolate
    })

    # TRIGGER PLOT RENDERING ---------------------------------------------------
    # This is done right at the end of the observe() function, before all our
    # lovely datasets disappear into the aether...!
    comcodeLegendData$comcodelegend <- comcodelegend
    sankeyData$links <- links
    sankeyData$nodes <- nodes
    sankeyData$pclinks <- pclinks
    sankeyData$pcnodes <- pcnodes
    mapData$mapWorld <- mapWorld
    mapData$dataPolygons <- dataPolygons
    timeseriesData$byComcode <- byComcode
    timeseriesData$byCountry <- byCountry
    timeseriesData$byPort <- byPort


  })

  # Fill in the comcode legend ================================================

  output$ComcodeLegend = renderDataTable({
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)

    datatable(comcodeLegendData$comcodelegend,
              rownames = FALSE,
              colnames = c("Commodity Code", "Description"),
              options = list(
                dom = "tp", # disable search bar at top
                pageLength = 5, # set number of elements on page
                columnDefs = list(list(width = "150px", targets = 0)))
    )}
  )

  # Fill in the plots =========================================================

  # SANKEY --------------------------------------------------------------------

  output$sankeyTrade <-  renderSankeyNetwork({

  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()
  req(!nullDataframe$nullDataframe)

  # if(dim(sankeyData$nodes)[1] == 0) {return()
  # req(!nullDataframe$nullDataframe)
  # }

   # fsize <- (input$fontsizeSelect)*2.4 + 7

 # set variable font size as a function of number of nodes
  # if(nrow(sankeyData$nodes) < 10) {
  #   fsize = 17
  # } else  if (nrow(sankeyData$nodes) < 32) {
  #   fsize = ((nrow(sankeyData$nodes))*(-0.25) + 20) }
  #  else { fsize = 12 }

  
  
  if(nrow(sankeyData$nodes) < 32) {
    fsize = -0.006*(nrow(sankeyData$nodes)-3)^2 + 17
  } else {fsize = 12}

     sankeyNetwork(sankeyData$links, sankeyData$nodes,
                   "source", "target", "value", "name",
                   fontSize = fsize, nodeWidth = 30)
     

     
  })



  # COUNTRY -> PORT SANKEY -----------------------------------------------------

  output$pcSankeyTrade <- renderSankeyNetwork({

  # Suppress output if nothing has been selected yet
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)

    #set variable font size as a function of number of nodes
    if (nrow(sankeyData$pcnodes) < 32) {
      fsizepc = -0.006*(nrow(sankeyData$pcnodes)-3)^2 + 17 }
    else { fsizepc = 12 }

    sankeyNetwork(sankeyData$pclinks, sankeyData$pcnodes,
                  "source", "target", "value", "name",
                  fontSize = fsizepc, nodeWidth = 30)
  })

  # MAP -----------------------------------------------------------------------

  output$worldMap <- renderLeaflet({
    if (input$queryButton == 0) return()
    req(!nullDataframe$nullDataframe)

    if (dim(mapData$dataPolygons)[1] == 0) return()
      req(!nullDataframe$nullDataframe)

    isolate({
      # Need to scale the map values if we don't want to blow up R...
      if (max(mapData$dataPolygons$value) > 1e12) {
        mapData$dataPolygons$value <- mapData$dataPolygons$value/1e9
        unitSuffix <- "b"
      } else if (max(mapData$dataPolygons$value) > 1e9) {
        mapData$dataPolygons$value <- mapData$dataPolygons$value/1e6
        unitSuffix <- "m"
      } else {
        unitSuffix <- ""
      }

      pal <- colorNumeric(palette = "inferno",
                          domain = c(0,max(mapData$dataPolygons$value)),
                          reverse = TRUE)

      value_popup <- paste0(ifelse(input$impexpSelect == "Imports", "<strong>Country of Origin: </strong>","<strong>Country of Dispatch: </strong>"),
                            mapData$dataPolygons$region,
                            "<br><strong>Value: </strong>",
                            mapData$dataPolygons$value,
                            unitSuffix)
    })

    # Format legend figures
    if (input$unitSelect == "Price (GBP)"){
      legendModifier <- labelFormat(prefix = "Â£", suffix = unitSuffix)
      legendTitle <- "Price"
    } else if (input$unitSelect == "Weight (KG)"){
      legendModifier <- labelFormat(suffix = paste0(unitSuffix," kg"))
      legendTitle <- "Weight"
    } else if (input$unitSelect == "Price Per Kilo (GBP/KG)"){
      legendModifier <- labelFormat(prefix = "Â£", suffix = paste0(unitSuffix, "/kg"))
      legendTitle <- "Price/Weight"
    } else if (input$unitSelect == "Number of Consignments"){
      legendModifier <- labelFormat(suffix = "")
      legendTitle <- "Consignments"
    }

    leaflet(data = mapData$dataPolygons) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      # addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(mapData$dataPolygons$value),
                  smoothFactor = 0.5,
                  weight = 1,
                  color = "#000000",
                  fillOpacity = 0.7,
                  popup = value_popup) %>%
      addLegend(pal = pal,
                values = 0:max(mapData$dataPolygons$value),
                opacity = 0.7,
                title = legendTitle,
                labFormat = legendModifier,
                position = "topright")

  })


  # TIME SERIES ---------------------------------------------------------------


   output$tsByComcode <- renderPlotly({


       if (length(unique(timeseriesData$byComcode$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
       nbars <- length(timeseriesData$byComcode$comcode)
       ggplotly(
           ggplot(data = timeseriesData$byComcode) +
           geom_col(aes(month,value,fill=comcode), show.legend = TRUE, position = colposition) +
           labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
                y = paste(input$unitSelect, " \n"),
                fill = "Commodity Codes") +
             theme_bw() +
           theme(axis.text.x = element_text(angle=45, hjust=1)) +
           scale_y_continuous(labels = comma) +
           scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
       )

       
       # ggplotly(
       #   ggplot(data = timeseriesData$byComcode) +
       #     geom_line(aes(month, value, color=comcode), show.legend = TRUE, group=1) +
       #     geom_point(aes(month, value, color=comcode)) +
       #   #  geom_text(aes(month, value, label = comcode)) +
       #     labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
       #        y = paste(input$unitSelect, " \n"),
       #        color = "Commodity") +
       #              theme_bw() +
       #            theme(axis.text.x = element_text(angle=45, hjust=1)) +
       #            scale_y_continuous(labels = comma)
       # )
       
       
       
       
       
       
       
   })
  
  


  output$tsByCountry <- renderPlotly({
      if (length(unique(timeseriesData$byCountry$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(timeseriesData$byCountry$country)
      ggplotly(
          ggplot(data = timeseriesData$byCountry) +
          geom_col(aes(month,value,fill=country), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Countries") +
            theme_bw() +
          theme(axis.text.x = element_text(angle=45, hjust=1)) +
          scale_y_continuous(labels = comma) +
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })

  output$tsByPort <- renderPlotly({
      if (length(unique(timeseriesData$byPort$month)) < 7) {colposition = "dodge"} else {colposition = "stack"}
      nbars <- length(timeseriesData$byPort$port)
      ggplotly(
          ggplot(data = timeseriesData$byPort) +
          geom_col(aes(month,value,fill=port), show.legend = TRUE, position = colposition) +
          labs(x = paste(substr(input$impexpSelect,1,nchar(input$impexpSelect)-1),"Month"),
               y = paste(input$unitSelect, " \n"),
               fill = "Ports") +
            theme_bw()+
          theme(axis.text.x = element_text(angle=45, hjust=1)) +
          scale_y_continuous(labels = comma) +
          scale_fill_manual(values = rainbow(nbars, s=.6, v=.8)[sample(1:nbars,nbars)])
      )
  })


  # DATA DOWNLOAD ------------------------------------------------------------


  output$dataDownload <- downloadHandler(
    filename = function() {
      # "TradeDataVisNonEUExtract.csv"
      "TradeDataVisExtract.csv"
    },
    content = function(file) {
      #Define metadata information tab
      #noneu_info<-data.frame(c("This data is downloaded from the Overseas Trade website () and is stored in an FSA database. A description of the different columns is given below. For non-EU trade, country of origin is used as default, but there is an option to choose country of dispatch.",NA,"Query details", "Trade type","More"),c(NA,NA,NA,input$tradeselect,"etc"))

      downloadfile <- queryData$dataraw %>%
        left_join(comcodelookup, by=c("comcode" = "commoditycode")) %>%
        left_join(portcode, by=c("port" = "portcode")) %>%
        left_join(countrycode, by = c("country" = "countrycode")) %>%
        mutate(comcode2 = substr(comcode, 1, 2)) %>%
        left_join(comcodelookup, by = c("comcode2" = "commoditycode"))
        downloadfile <- downloadfile %>% select("2-Digit Comcode" = comcode2, "2-Digit Description" = description.y, "Commodity Code" = comcode, "Port Code" = port, "Port Name" = portname, "Port Type" = type, "Country Code" = country, "Country Name" = countryname, "Month" = month, "Value (Â£)" = price, "Weight (kg)" = weight, "Number of Consignments" = consignments, "Commodity Description" = description.x) %>%
          mutate(`Number of Consignments` = ifelse(`Number of Consignments` == 0, "", `Number of Consignments`))
      #write.xlsx2(downloadfile, file, sheetName = "Trade Data", row.names = FALSE)
     # write_xlsx()
      write.csv(downloadfile, file, row.names = FALSE)
      #write.xlsx2(noneu_info,file,sheetName = "Information", append = TRUE,row.names = FALSE,col.names=FALSE)
    }
  )

  output$noneucomcodedownload <- downloadHandler(
    filename = function() {
      "Commoditycodes.csv"
    },
    content = function(file) {
      comcodedownloadfile <- comcodeLegendData$comcodelegend%>%
        select( "Commodity code" = commoditycode, "Description" = description)
      #write.xlsx2(as.data.frame(comcodedownloadfile), file, sheetName = "Commodity Codes", row.names = FALSE)
      write.csv(as.data.frame(comcodedownloadfile), file, row.names = FALSE)
    }
  )



  # Advanced DATA DOWNLOAD ------------------------------------------------------------
  output$noneuadvancedownload <- downloadHandler(
    filename = function() {
      "TradeDataVisExtract.csv"
    },
    content = function(file) {
      noneudownloadfile <- queryData$dataraw %>%
        left_join(portcode, by=c("port" = "portcode")) %>%
        left_join(countrycode, by = c("country" = "countrycode")) %>%
        select(-"lat",-"long",-"eu",-"non_eu")


      noneudownloadfile<-noneudownloadfile%>%
        mutate(comcodex = substr(comcode,1,as.numeric(substr(input$noneucnlevel,1,1))))%>%
        select(-"comcode")%>%
        rename("comcode"="comcodex")%>%
        group_by(country,comcode,port,month,portname,type,countryname)%>%
        summarise("price"=sum(price),"weight"=sum(weight), "consignments"=sum(consignments))

      if(input$noneumonthsum == TRUE) {
        noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(country,comcode,countryname,port,portname,type)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}

      if(input$noneucountrysum == TRUE){if("month" %in% colnames(noneudownloadfile)){
        noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(month,comcode,port,portname,type)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
        else{
          noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(comcode,port,portname,type)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
      }

      if(input$noneuportsum == TRUE){
        if("month" %in% colnames(noneudownloadfile)){
          if("country" %in% colnames(noneudownloadfile)){
            noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(month,comcode,country,countryname)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
          else{noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(month,comcode)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
        }
        else{
          if("country" %in% colnames(noneudownloadfile)){
            noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(comcode,country,countryname)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
          else{noneudownloadfile <-noneudownloadfile%>%ungroup()%>%group_by(comcode)%>%summarise("price"=sum(price),"weight"=sum(weight),"consignments"=sum(consignments))}
        }
      }

      noneudownloadfile<-noneudownloadfile%>%
        left_join(comcodelookup, by=c("comcode" = "commoditycode"))%>%
        select("Commodity code"=one_of("comcode"),"Port Code" = one_of("port"),"Port Name" = one_of("portname"),"Port Type" = one_of("type"),"Country Code" = one_of("country"),"Country Name" = one_of("countryname"),"Month" = one_of("month"),"Value (Â£)" = one_of("price"),"Weight (kg)" = one_of("weight"),"Number of Consignments" = one_of("consignments"), "Commodity description" = one_of("description")) 

      #write.xlsx2(as.data.frame(noneudownloadfile), file, sheetName = "Trade Data", row.names = FALSE)
      write.csv(as.data.frame(noneudownloadfile), file, row.names = FALSE)
    }
  )




  # FF download -------------------------------------------------------------
  output$foodfeeddownload <- downloadHandler(
    filename = function() {
      "Foodfeedlist.csv"
    },
    content = function(file) {
     #  downloadfile <- allfoodfeed %>%
      # left_join(comcodelookup, by=c("comcodeff" = "commoditycode")) #%>%
      #  left_join(portcode, by=c("port" = "portcode")) %>%
      #  left_join(countrycode, by = c("country" = "countrycode")) %>%
      # mutate(comcode2 = substr(comcode, 1, 2)) %>%
      #left_join(comcodelookup, by = c("comcode2" = "commoditycode"))
      downloadfile <- allfoodfeed %>%
        left_join(comcodelookup, by=c("comcodeff" = "commoditycode")) %>%
        select( "Commodity code" = "comcodeff", "Description" = "description", "Food" = "food", "Feed" = "feed", "POAO" = "poao", "FNAO" = "fnao")
      #write.xlsx2(as.data.frame(downloadfile), file, sheetName = "Food and feed list", row.names = FALSE)
      write.csv(as.data.frame(downloadfile), file, row.names = FALSE)
    }
  )





# Close Server Function
}




# SHINY CALL =================================================================

shinyApp(ui = ui, server = server)


# END SCRIPT #################################################################

# JUNKYARD ###################################################################

# Disconnect All Database Cons
#pg = dbDriver("PostgreSQL");lapply(dbListConnections(pg), dbDisconnect)


