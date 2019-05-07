
# VARIABLES ##########################################################################################################
   St <- 2002     # Starting year
   Yr <- 2017     # Current year

# LIBRARIES ##########################################################################################################
   library(shiny)                # For the app iteslf.
   library(shinythemes)          # For the "cerulean" theme.
   library(shinyhelper)          # Used for help modal boxes.
   library(shinyEffects)         # Used for home page effects.
   library(shinyanimate)         # Used for home page effects.
   library(shinycssloaders)      # For loaders.
   library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
   library(data.table)           # Needed for function na.omit().
   library(DT)                   # For the interactive tables.
   library(dygraphs)             # For the interactive graphs.
   library(leaflet)              # For interactive maps.
   library(RColorBrewer)         # Used to create a colour palette for the map.
   library(rgdal)                # Needed to load shapfile for the map.
   library(plyr)                 # Needed for function revalue(), for editing country names.
   library(rmarkdown)            # Used for reports, especially important is function render().
   library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output.
   library(networkD3)            # Used to create the sankey diagram.
   library(treemap)              # Used to create the static treemap.
   library(dplyr)                # Used for data manipulation.
   library(ggplot2)              # Used for plots in country and sector profiles.
   library(leaflet.minicharts)   # Used to superimpose piecharts on a leaflet map.
   library(plotly)               # Used for the streamgraph.
   library(ggsci)                # Used for colour palettes.
   library(ggflags)              # Expands ggplot with new geom for adding flags.
   library(countrycode)          # Enables conversion from Common country names to ISO codes.
   library(knitr)                # Used for sector definitions table
   library(kableExtra)           # Used for styling the sector definitions table

# TABLES ##############################################################################################################
   TOTALS <- read.csv("./www/TOTALS.csv")
   TOTALS <- subset(TOTALS, TOTALS$Year >= St & TOTALS$Year <= Yr)
   SECTORS <- read.csv("./www/SECTORS.csv")
   SECTORS <- subset(SECTORS, SECTORS$Year >= St & SECTORS$Year <= Yr)
   SUBSECTORS <- read.csv("./www/SUBSECTORS.csv")
   SUBSECTORS <- subset(SUBSECTORS, SUBSECTORS$Year >= St & SUBSECTORS$Year <= Yr)
   TABLE_ALL <- read.csv("./www/TABLE_ALL.csv")
   TABLE_INT <- read.csv("./www/TABLE_INT.csv")
   TABLE_RUK <- read.csv("./www/TABLE_RUK.csv")
   SANKEY_INT <- read.csv("./www/SANKEY_INT.csv")
   SANKEY_RUK <- read.csv("./www/SANKEY_RUK.csv")
   SANKEY_ALL <- read.csv("./www/SANKEY_ALL.csv")
   SectorDefinitions <- read.csv("./www/SectorDefinitions.csv")
   COUNTRIES <- read.csv("./www/COUNTRIES.csv")

# DATA MANIPULATION ###################################################################################################
# DATA MANIPULATION 1 - CREATING THE GRAPHS TABLE (argument of dygraph)
   vector_of_sector_names <- c(
         "All Sectors",
         "Food and Drink",
         "Engineering and Advanced Manufacturing",
         "Energy",
         "Financial and Business Services",
         "Technology, Digital and Media",
         "Chemical Sciences",
         "Wholesale and Retail Trade",
         "Life Sciences",
         "Transportation and Storage",
         "Education",
         "Sustainable Tourism",
         "Textiles",
         "Forest and Timber Technologies",
         "Other Services and Accommodation",
         "Construction",
         "Real Estate"
   )
   
   vector_of_subsector_names <- c(
         "Food",
         "Drink",
         "Metal Manufacturing",
         "Advanced Manufacturing",
         "Transport Equipment",
         "Engineering Services",
         "Other Manufacturing",
         "Energy Support",
         "Mining and Utilities",
         "Financial Service Activities",
         "Insurance and Pensions",
         "Financial Support Activities",
         "Legal and Accounting",
         "Management Consultancy",
         "Business Support Activities",
         "Manufacture of Computer, Electronic and Optical Products",
         "Digital Industries",
         "Publishing and Audio Visual",
         "IT and Telecommunications",
         "Chemical Sciences",
         "Trade and Repair of Vehicles",
         "Wholesale Trade",
         "Retail Trade",
         "Pharmaceuticals",
         "Manufacture of Medical Equipment",
         "Research",
         "Transportation",
         "Storage",
         "Education",
         "Sustainable Tourism",
         "Textiles",
         "Forest and Timber Technologies",
         "Other Services and Accommodation",
         "Construction",
         "Real Estate"
   )

   GRAPHS <- data.frame(
      
         # Year
         TOTALS$Year,
      
         # All
         TOTALS$All,
      
         # Int
         TOTALS$International,
      
         # RUK
         TOTALS$RUK,
      
         # Sectors
         subset(SECTORS$All, SECTORS$Sector == "Food and Drink"),
         subset(SECTORS$International, SECTORS$Sector == "Food and Drink"),
         subset(SECTORS$RUK, SECTORS$Sector == "Food and Drink"),
         subset(SECTORS$All, SECTORS$Sector == "Engineering and Advanced Manufacturing"),
         subset(SECTORS$International, SECTORS$Sector == "Engineering and Advanced Manufacturing"),
         subset(SECTORS$RUK, SECTORS$Sector == "Engineering and Advanced Manufacturing"), 
         subset(SECTORS$All, SECTORS$Sector == "Energy"),
         subset(SECTORS$International, SECTORS$Sector == "Energy"),
         subset(SECTORS$RUK, SECTORS$Sector == "Energy"),
         subset(SECTORS$All, SECTORS$Sector == "Financial and Business Services"),
         subset(SECTORS$International, SECTORS$Sector == "Financial and Business Services"),
         subset(SECTORS$RUK, SECTORS$Sector == "Financial and Business Services"),
         subset(SECTORS$All, SECTORS$Sector == "Technology, Digital and Media"),
         subset(SECTORS$International, SECTORS$Sector == "Technology, Digital and Media"),
         subset(SECTORS$RUK, SECTORS$Sector == "Technology, Digital and Media"),
         subset(SECTORS$All, SECTORS$Sector == "Chemical Sciences"),
         subset(SECTORS$International, SECTORS$Sector == "Chemical Sciences"),
         subset(SECTORS$RUK, SECTORS$Sector == "Chemical Sciences"),
         subset(SECTORS$All, SECTORS$Sector == "Wholesale and Retail Trade"),
         subset(SECTORS$International,SECTORS$Sector == "Wholesale and Retail Trade"),
         subset(SECTORS$RUK, SECTORS$Sector == "Wholesale and Retail Trade"),
         subset(SECTORS$All, SECTORS$Sector == "Life Sciences"),
         subset(SECTORS$International, SECTORS$Sector == "Life Sciences"),
         subset(SECTORS$RUK, SECTORS$Sector == "Life Sciences"),
         subset(SECTORS$All, SECTORS$Sector == "Transportation and Storage"),
         subset(SECTORS$International, SECTORS$Sector == "Transportation and Storage"),
         subset(SECTORS$RUK, SECTORS$Sector == "Transportation and Storage"),
         subset(SECTORS$All, SECTORS$Sector == "Education"),
         subset(SECTORS$International, SECTORS$Sector == "Education"),
         subset(SECTORS$RUK, SECTORS$Sector == "Education"),
         subset(SECTORS$All, SECTORS$Sector == "Sustainable Tourism"),
         subset(SECTORS$International, SECTORS$Sector == "Sustainable Tourism"),
         subset(SECTORS$RUK, SECTORS$Sector == "Sustainable Tourism"),
         subset(SECTORS$All, SECTORS$Sector == "Textiles"),
         subset(SECTORS$International, SECTORS$Sector == "Textiles"),
         subset(SECTORS$RUK, SECTORS$Sector == "Textiles"),
         subset(SECTORS$All, SECTORS$Sector == "Forest and Timber Technologies"),
         subset(SECTORS$International, SECTORS$Sector == "Forest and Timber Technologies"),
         subset(SECTORS$RUK, SECTORS$Sector == "Forest and Timber Technologies"),
         subset(SECTORS$All, SECTORS$Sector == "Other Services and Accommodation"),
         subset(SECTORS$International, SECTORS$Sector == "Other Services and Accommodation"),
         subset(SECTORS$RUK, SECTORS$Sector == "Other Services and Accommodation"),
         subset(SECTORS$All, SECTORS$Sector == "Construction"),
         subset(SECTORS$International, SECTORS$Sector == "Construction"),
         subset(SECTORS$RUK, SECTORS$Sector == "Construction"),
         subset(SECTORS$All, SECTORS$Sector == "Real Estate"),
         subset(SECTORS$International, SECTORS$Sector == "Real Estate"),
         subset(SECTORS$RUK, SECTORS$Sector == "Real Estate"),
      
         # Subsectors
         subset(SUBSECTORS$All,SUBSECTORS$Subsector == "Food"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Food"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Food"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Drink"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Drink"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Drink"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Metal Manufacturing"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Metal Manufacturing"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Metal Manufacturing"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Advanced Manufacturing"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Advanced Manufacturing"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Advanced Manufacturing"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Transport Equipment"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Transport Equipment"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Transport Equipment"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Engineering Services"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Engineering Services"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Engineering Services"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Other Manufacturing"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Other Manufacturing"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Other Manufacturing"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Energy Support"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Energy Support"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Energy Support"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Mining and Utilities"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Mining and Utilities"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Mining and Utilities"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Financial Service Activities"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Financial Service Activities"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Financial Service Activities"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Insurance and Pensions"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Insurance and Pensions"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Insurance and Pensions"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Financial Support Activities"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Financial Support Activities"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Financial Support Activities"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Legal and Accounting"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Legal and Accounting"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Legal and Accounting"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Management Consultancy"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Management Consultancy"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Management Consultancy"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Business Support Activities"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Business Support Activities"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Business Support Activities"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Manufacture of Computer, Electronic and Optical Products"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Manufacture of Computer, Electronic and Optical Products"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Manufacture of Computer, Electronic and Optical Products"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Digital Industries"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Digital Industries"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Digital Industries"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Publishing and Audio Visual"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Publishing and Audio Visual"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Publishing and Audio Visual"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "IT and Telecommunications"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "IT and Telecommunications"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "IT and Telecommunications"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Chemical Sciences"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Chemical Sciences"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Chemical Sciences"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Trade and Repair of Vehicles"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Trade and Repair of Vehicles"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Trade and Repair of Vehicles"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Wholesale Trade"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Wholesale Trade"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Wholesale Trade"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Retail Trade"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Retail Trade"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Retail Trade"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Pharmaceuticals"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Pharmaceuticals"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Pharmaceuticals"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Manufacture of Medical Equipment"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Manufacture of Medical Equipment"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Manufacture of Medical Equipment"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Research"),
         subset(SUBSECTORS$International,SUBSECTORS$Subsector == "Research"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Research"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Transportation"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Transportation"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Transportation"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Storage"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Storage"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Storage"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Education"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Education"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Education"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Sustainable Tourism"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Sustainable Tourism"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Sustainable Tourism"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Textiles"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Textiles"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Textiles"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Forest and Timber Technologies"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Forest and Timber Technologies"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Forest and Timber Technologies"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Other Services and Accommodation"),
         subset(SUBSECTORS$International,SUBSECTORS$Subsector == "Other Services and Accommodation"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Other Services and Accommodation"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Construction"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Construction"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Construction"),
         subset(SUBSECTORS$All, SUBSECTORS$Subsector == "Real Estate"),
         subset(SUBSECTORS$International, SUBSECTORS$Subsector == "Real Estate"),
         subset(SUBSECTORS$RUK, SUBSECTORS$Subsector == "Real Estate")
   )
   
   # Renaming the columns of the table GRAPHS created above
   colnames(GRAPHS) <- c(
         "Year",
         "All",
         "Int",
         "RUK",
         # SECTORS
         "Food and Drink (All)",
         "Food and Drink (Int)",
         "Food and Drink (RUK)",
         "Engineering and Advanced Manufacturing (All)",
         "Engineering and Advanced Manufacturing (Int)",
         "Engineering and Advanced Manufacturing (RUK)",
         "Energy (All)",
         "Energy (Int)",
         "Energy (RUK)",
         "Financial and Business Services (All)",
         "Financial and Business Services (Int)",
         "Financial and Business Services (RUK)",
         "Technology, Digital and Media (All)",
         "Technology, Digital and Media (Int)",
         "Technology, Digital and Media (RUK)",
         "Chemical Sciences (All)",
         "Chemical Sciences (Int)",
         "Chemical Sciences (RUK)",
         "Wholesale and Retail Trade (All)",
         "Wholesale and Retail Trade (Int)",
         "Wholesale and Retail Trade (RUK)",
         "Life Sciences (All)",
         "Life Sciences (Int)",
         "Life Sciences (RUK)",
         "Transportation and Storage (All)",
         "Transportation and Storage (Int)",
         "Transportation and Storage (RUK)",
         "Education (All)",
         "Education (Int)",
         "Education (RUK)",
         "Sustainable Tourism (All)",
         "Sustainable Tourism (Int)",
         "Sustainable Tourism (RUK)",
         "Textiles (All)",
         "Textiles (Int)",
         "Textiles (RUK)",
         "Forest and Timber Technologies (All)",
         "Forest and Timber Technologies (Int)",
         "Forest and Timber Technologies (RUK)",
         "Other Services and Accommodation (All)",
         "Other Services and Accommodation (Int)",
         "Other Services and Accommodation (RUK)",
         "Construction (All)",
         "Construction (Int)",
         "Construction (RUK)",
         "Real Estate (All)",
         "Real Estate (Int)",
         "Real Estate (RUK)",
         # SUBSECTORS
         "Food (All)",
         "Food (Int)",
         "Food (RUK)",
         "Drink (All)",
         "Drink (Int)",
         "Drink (RUK)",
         "Metal Manufacturing (All)",
         "Metal Manufacturing (Int)",
         "Metal Manufacturing (RUK)",
         "Advanced Manufacturing (All)",
         "Advanced Manufacturing (Int)",
         "Advanced Manufacturing (RUK)",
         "Transport Equipment (All)",
         "Transport Equipment (Int)",
         "Transport Equipment (RUK)",
         "Engineering Services (All)",
         "Engineering Services (Int)",
         "Engineering Services (RUK)",
         "Other Manufacturing (All)",
         "Other Manufacturing (Int)",
         "Other Manufacturing (RUK)",
         "Energy Support (All)",
         "Energy Support (Int)",
         "Energy Support (RUK)",
         "Mining and Utilities (All)",
         "Mining and Utilities (Int)",
         "Mining and Utilities (RUK)",
         "Financial Service Activities (All)",
         "Financial Service Activities (Int)",
         "Financial Service Activities (RUK)",
         "Insurance and Pensions (All)",
         "Insurance and Pensions (Int)",
         "Insurance and Pensions (RUK)",
         "Financial Support Activities (All)",
         "Financial Support Activities (Int)",
         "Financial Support Activities (RUK)",
         "Legal and Accounting (All)",
         "Legal and Accounting (Int)",
         "Legal and Accounting (RUK)",
         "Management Consultancy (All)",
         "Management Consultancy (Int)",
         "Management Consultancy (RUK)",
         "Business Support Activities (All)",
         "Business Support Activities (Int)",
         "Business Support Activities (RUK)",
         "Manufacture of Computer, Electronic and Optical Products (All)",
         "Manufacture of Computer, Electronic and Optical Products (Int)",
         "Manufacture of Computer, Electronic and Optical Products (RUK)",
         "Digital Industries (All)",
         "Digital Industries (Int)",
         "Digital Industries (RUK)",
         "Publishing and Audio Visual (All)",
         "Publishing and Audio Visual (Int)",
         "Publishing and Audio Visual (RUK)",
         "IT and Telecommunications (All)",
         "IT and Telecommunications (Int)",
         "IT and Telecommunications (RUK)",
         "Chemical Sciences (All)",
         "Chemical Sciences (Int)",
         "Chemical Sciences (RUK)",
         "Trade and Repair of Vehicles (All)",
         "Trade and Repair of Vehicles (Int)",
         "Trade and Repair of Vehicles (RUK)",
         "Wholesale Trade (All)",
         "Wholesale Trade (Int)",
         "Wholesale Trade (RUK)",
         "Retail Trade (All)",
         "Retail Trade (Int)",
         "Retail Trade (RUK)",
         "Pharmaceuticals (All)",
         "Pharmaceuticals (Int)",
         "Pharmaceuticals (RUK)",
         "Manufacture of Medical Equipment (All)",
         "Manufacture of Medical Equipment (Int)",
         "Manufacture of Medical Equipment (RUK)",
         "Research (All)",
         "Research (Int)",
         "Research (RUK)",
         "Transportation (All)",
         "Transportation (Int)",
         "Transportation (RUK)",
         "Storage (All)",
         "Storage (Int)",
         "Storage (RUK)",
         "Education (All)",
         "Education (Int)",
         "Education (RUK)",
         "Sustainable Tourism (All)",
         "Sustainable Tourism (Int)",
         "Sustainable Tourism (RUK)",
         "Textiles (All)",
         "Textiles (Int)",
         "Textiles (RUK)",
         "Forest and Timber Technologies (All)",
         "Forest and Timber Technologies (Int)",
         "Forest and Timber Technologies (RUK)",
         "Other Services and Accommodation (All)",
         "Other Services and Accommodation (Int)",
         "Other Services and Accommodation (RUK)",
         "Construction (All)",
         "Construction (Int)",
         "Construction (RUK)",
         "Real Estate (ALL)",
         "Real Estate (Int)",
         "Real Estate (RUK)"
   )

# DATA MANIPULATION 2 - MAP DATA   
   COUNTRY_MAP <- COUNTRIES[,c("Year","Country","Total")]
   names(COUNTRY_MAP) <- c("Year","Country","Value")
   COUNTRY <- data.frame(NAME = subset(COUNTRY_MAP, COUNTRY_MAP$Year == Yr)$Country, Value = subset(COUNTRY_MAP, COUNTRY_MAP$Year == Yr)$Value)
   mapex <- readOGR(dsn="./www/world_shape_file", layer="TM_WORLD_BORDERS_SIMPL-0.3")
   editing_names <- mapex@data$NAME
   editing_names <- revalue(editing_names, c("United States" = "USA"))
   editing_names <- revalue(editing_names, c("Brunei Darussalam" = "Brunei"))
   editing_names <- revalue(editing_names, c("Democratic Republic of the Congo" = "Democratic Republic of Congo"))
   editing_names <- revalue(editing_names, c("Falkland Islands (Malvinas)" = "Falkland Islands"))
   editing_names <- revalue(editing_names, c("Micronesia, Federated States of" = "Micronesia"))
   editing_names <- revalue(editing_names, c("Iran (Islamic Republic of)" = "Iran"))
   editing_names <- revalue(editing_names, c("Cote d'Ivoire" = "Ivory Coast"))
   editing_names <- revalue(editing_names, c("Korea, Democratic People's Republic of" = "North Korea"))
   editing_names <- revalue(editing_names, c("Korea, Republic of" = "South Korea"))
   editing_names <- revalue(editing_names, c("Lao People's Democratic Republic" = "Laos"))
   editing_names <- revalue(editing_names, c("Libyan Arab Jamahiriya" = "Libya"))
   editing_names <- revalue(editing_names, c("The former Yugoslav Republic of Macedonia" = "North Macedonia"))
   editing_names <- revalue(editing_names, c("Cocos (Keeling) Islands" = "Cocos Islands"))
   editing_names <- revalue(editing_names, c("Republic of Moldova" = "Moldova"))
   editing_names <- revalue(editing_names, c("Syrian Arab Republic" = "Syria"))
   editing_names <- revalue(editing_names, c("United Republic of Tanzania" = "Tanzania"))
   editing_names <- revalue(editing_names, c("United Arab Emirates" = "UAE"))
   editing_names <- revalue(editing_names, c("Holy See (Vatican City)" = "Vatican City"))
   #editing_names <- revalue(editing_names, c("Czech Republic" = "Czechia"))
   mapex@data$NAME <- editing_names
   merged <- merge(mapex@data, COUNTRY, by = intersect(names(mapex@data), names(COUNTRY)), all.x = TRUE, all.y = FALSE, sort=FALSE)
   mapex@data <- merged[match(mapex@data$NAME,merged$NAME),]
   no_NA <- na.omit(merged)

# DATA MANIPULATION 3 - COUNTRIES BAR CHART
   D <- COUNTRIES %>%
      group_by(Year) %>%
      filter() %>%
      top_n(n = 25, wt = Total) %>%
      #mutate(Rank = min_rank(-Total) * 120) %>%
      ungroup()
   D <- D[with(D, order(Year, -Total)),]
   D$Code <- tolower(countrycode(D$Country, "country.name", "iso2c"))
   
# Data manipulation for other outputs is placed on the server and in R Markdown files  