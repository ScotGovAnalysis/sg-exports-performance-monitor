
# VARIABLES ##########################################################################################################
St <- 2002     # Starting year
Yr <- 2018     # Current year

# LIBRARIES ##########################################################################################################
library(shiny)                # For the app iteslf.
library(shinythemes)          # For the "cerulean" theme.
library(shinyhelper)          # Used for help modal boxes.
library(shinyEffects)         # Used for home page effects.
library(shinyanimate)         # Used for home page effects.
library(shinycssloaders)      # For loaders.
library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
library(DT)                   # For the interactive tables.
library(dygraphs)             # For the interactive graphs.
library(leaflet)              # For interactive maps.
library(RColorBrewer)         # Used to create a colour palette for the map.
library(rgdal)                # Needed to load shapfile for the map.
library(plyr)                 # Needed for function revalue(), for editing country names.
library(rmarkdown)            # Used for reports, especially important is function render().
library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output, and used for sector definitions table
library(networkD3)            # Used to create the sankey diagram.
library(treemap)              # Used to create the static treemap.
library(leaflet.minicharts)   # Used to superimpose piecharts on a leaflet map.
library(plotly)               # Used for the streamgraph.
library(ggsci)                # Used for colour palettes.
library(ggflags)              # Expands ggplot with new geom for adding flags.
library(countrycode)          # Enables conversion from Common country names to ISO codes.
library(kableExtra)           # Used for styling the sector definitions table
library(gtools)               # Used for smartbind() (rbinding without the col names having to be in the same order)
library(openxlsx)
library(scales)
library(shinyTree)
library(shinyWidgets)
library(ggrepel)
library(grid)
library(gridExtra)
library(tidyverse)            # Used for data manipulation, lots of packages incl. dplyrs and ggplot2.

# GLOBAL OPTIONS ######################################################################################################
options(stringsAsFactors = FALSE, scipen = 999, shinyTree.refresh = TRUE, kableExtra.latex.load_packages = TRUE)

# TABLES ##############################################################################################################
TOTALS <- read.csv("./Base Datatables/TOTALS.csv") %>%
   subset(Year >= St & Year <= Yr)

SECTORS <- read.csv("./Base Datatables/SECTORS.csv") %>%
   subset(Year >= St & Year <= Yr)
SUBSECTORS <- read.csv("./Base Datatables/SUBSECTORS.csv") %>%
   subset(Year >= St & Year <= Yr)

TABLE_ALL <- read.csv("./Base Datatables/TABLE_ALL.csv")
TABLE_INT <- read.csv("./Base Datatables/TABLE_INT.csv")
TABLE_RUK <- read.csv("./Base Datatables/TABLE_RUK.csv")
TABLE_EU <- read.csv("./Base Datatables/TABLE_EU.csv")
colnames(TABLE_EU) <- gsub("X", "", colnames(TABLE_EU), fixed = TRUE)
TABLE_NONEU <- read.csv("./Base Datatables/TABLE_NONEU.csv")
colnames(TABLE_NONEU) <- gsub("X", "", colnames(TABLE_NONEU), fixed = TRUE)

SANKEY_INT <- read.csv("./Base Datatables/SANKEY_INT.csv")
SANKEY_RUK <- read.csv("./Base Datatables/SANKEY_RUK.csv")
SANKEY_ALL <- read.csv("./Base Datatables/SANKEY_ALL.csv")

SectorDefinitions <- read.csv("./Base Datatables/SectorDefinitions.csv")

COUNTRIES <- read.csv("./Base Datatables/COUNTRIES.csv")

# DATA MANIPULATION ###################################################################################################
# Creating the Vectors for Sector Names and for Subsector Names #
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

list_of_sectors <- structure(list(
   "All Sectors" = "All Sectors",
   "Food and Drink" = "Food and Drink",
   "Engineering and Advanced Manufacturing" = "Engineering and Advanced Manufacturing",
   "Energy" = "Energy",
   "Financial and Business Services" = "Financial and Business Services",
   "Technology, Digital and Media" =  "Technology, Digital and Media",
   "Chemical Sciences" = "Chemical Sciences",
   "Wholesale and Retail Trade" = "Wholesale and Retail Trade",
   "Life Sciences" = "Life Sciences",
   "Transportation and Storage" = "Transportation and Storage",
   "Education" = "Education",
   "Sustainable Tourism" = "Sustainable Tourism",
   "Textiles" = "Textiles",
   "Forest and Timber Technologies" = "Forest and Timber Technologies",
   "Other Services and Accommodation" = "Other Services and Accommodation",
   "Construction" = "Construction",
   "Real Estate" = "Real Estate"
)
)
names(list_of_sectors) <- vector_of_sector_names
attr(list_of_sectors[["All Sectors"]], "stselected") = TRUE

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


list_of_subsectors <- list(
   "All Sectors" = "All Sectors",
   "Food and Drink" = list(
      "Food" = "Food",
      "Drink" = "Drink"
   ),
   "Engineering and Advanced Manufacturing" = list(
      "Metal Manufacturing" =  "Metal Manufacturing" ,
      "Advanced Manufacturing" = "Advanced Manufacturing",
      "Transport Equipment" =  "Transport Equipment" ,
      "Engineering Services"  = "Engineering Services" ,
      "Other Manufacturing" =  "Other Manufacturing"
   ),
   "Energy" = list(
      "Energy Support" = "Energy Support" ,
      "Mining and Utilities" = "Mining and Utilities"
   ),
   "Financial and Business Services" = list(
      "Financial Service Activities" = "Financial Service Activities",
      "Insurance and Pensions" = "Insurance and Pensions" ,
      "Financial Support Activities" = "Financial Support Activities",
      "Legal and Accounting" = "Legal and Accounting" ,
      "Management Consultancy"  = "Management Consultancy" ,
      "Business Support Activities" = "Business Support Activities"
   ),
   "Technology, Digital and Media" = list(
      "Manufacture of Computer, Electronic and Optical Products" = "Manufacture of Computer, Electronic and Optical Products" ,
      "Digital Industries" = "Digital Industries" ,
      "Publishing and Audio Visual" =  "Publishing and Audio Visual",
      "IT and Telecommunications" = "IT and Telecommunications"
   ),
   "Chemical Sciences" = "Chemical Sciences",
   "Wholesale and Retail Trade" = list(
      "Trade and Repair of Vehicles" = "Trade and Repair of Vehicles",
      "Wholesale Trade" =  "Wholesale Trade",
      "Retail Trade" = "Retail Trade"
   ),
   "Life Sciences" = list(
      "Pharmaceuticals" = "Pharmaceuticals" ,
      "Manufacture of Medical Equipment" = "Manufacture of Medical Equipment",
      "Research" = "Research"
   ),
   "Transportation and Storage" = list(
      "Transportation" = "Transportation",
      "Storage" = "Storage"
   ),
   "Education" = "Education",
   "Sustainable Tourism" = "Sustainable Tourism",
   "Textiles" = "Textiles",
   "Forest and Timber Technologies" = "Forest and Timber Technologies",
   "Other Services and Accommodation" = "Other Services and Accommodation",
   "Construction" = "Construction",
   "Real Estate" = "Real Estate"
)

destinationChoices <- list(
   "International" = "International",
   "EU" = "EU",
   "Non-EU" = "Non-EU",
   "Rest of the UK (RUK)" = "Rest of the UK (RUK)",
   "All Destinations" = "All Destinations"
)

# DATA MANIPULATION 1 - CREATING THE GRAPHS TABLE (argument of dygraph)
# Totals Data #
data_totals <- TOTALS %>%
   rename(
      "All Sectors (All)" = All,
      "All Sectors (RUK)" = RUK,
      "All Sectors (Int)" = International,
      "All Sectors (EU)" = EU,
      "All Sectors (Non-EU)" = Non.EU,
   )

# Sectors Data #
data_sectorsA <- SECTORS %>%
   select(Year, Sector, International) %>%
   pivot_wider(names_from = Sector, values_from = International, names_prefix = "(Int)")

data_sectorsB <- SECTORS %>%
   select(Year, Sector, RUK) %>%
   pivot_wider(names_from = Sector, values_from = RUK, names_prefix = "(RUK)")

data_sectorsC <- SECTORS %>%
   select(Year, Sector, All) %>%
   pivot_wider(names_from = Sector, values_from = All, names_prefix = "(All)")

data_sectorsD <- SECTORS %>%
   select(Year, Sector, EU) %>%
   pivot_wider(names_from = Sector, values_from = EU, names_prefix = "(EU)")

data_sectorsE <- SECTORS %>%
   select(Year, Sector, Non.EU) %>%
   pivot_wider(names_from = Sector, values_from = Non.EU, names_prefix = "(Non-EU)")


data_sectors <- merge(data_sectorsA, data_sectorsB, by = intersect(names(data_sectorsA), names(data_sectorsB)))
data_sectors <- merge(data_sectors, data_sectorsC, by = intersect(names(data_sectors), names(data_sectorsC)))
data_sectors <- merge(data_sectors, data_sectorsD, by = intersect(names(data_sectors), names(data_sectorsD)))
data_sectors <- merge(data_sectors, data_sectorsE, by = intersect(names(data_sectors), names(data_sectorsE)))

rm(data_sectorsA,data_sectorsB,data_sectorsC,data_sectorsD,data_sectorsE)
gc()
#rsconnect::showLogs(account = "hmacgregor", appName = "EPM_V3")

# Subsectors Data #
data_subsectorsA <- SUBSECTORS %>%
   select(Year, Subsector, International) %>%
   pivot_wider(names_from = Subsector, values_from = International, names_prefix = "(Int)")

data_subsectorsB <- SUBSECTORS %>%
   select(Year, Subsector, RUK) %>%
   pivot_wider(names_from = Subsector, values_from = RUK, names_prefix = "(RUK)")

data_subsectorsC <- SUBSECTORS %>%
   select(Year, Subsector, All) %>%
   pivot_wider(names_from = Subsector, values_from = All, names_prefix = "(All)")

data_subsectorsD <- SUBSECTORS %>%
   select(Year, Subsector, EU) %>%
   pivot_wider(names_from = Subsector, values_from = EU, names_prefix = "(EU)")

data_subsectorsE <- SUBSECTORS %>%
   select(Year, Subsector, Non.EU) %>%
   pivot_wider(names_from = Subsector, values_from = Non.EU, names_prefix = "(Non-EU)")

data_subsectors <- merge(data_subsectorsA, data_subsectorsB, by = intersect(names(data_subsectorsA), names(data_subsectorsB)))
data_subsectors <- merge(data_subsectors, data_subsectorsC, by = intersect(names(data_subsectors), names(data_subsectorsC)))
data_subsectors <- merge(data_subsectors, data_subsectorsD, by = intersect(names(data_subsectors), names(data_subsectorsD)))
data_subsectors <- merge(data_subsectors, data_subsectorsE, by = intersect(names(data_subsectors), names(data_subsectorsE)))


rm(data_subsectorsA,data_subsectorsB,data_subsectorsC,data_subsectorsD,data_subsectorsE)
gc()

# Overall Graph Table #
graph1 <- merge(data_totals, data_sectors, by = "Year")
graph1 <- merge(graph1, data_subsectors, by = "Year")

# Changing the Colnames to match previous work #
for (i in seq_len(ncol(graph1))) {
   test1 <- colnames(graph1)[i]

   if (!(test1 %in% c("Year","All Sectors (All)", "All Sectors (Int)", "All Sectors (RUK)","All Sectors (EU)","All Sectors (Non-EU)"))) {
      index_end <- gregexpr(")", test1, fixed = TRUE) [[1]] %>% as.numeric()

      test2 <- str_sub(test1, start = 1, end = index_end)
      test1 <- gsub(test2,"", test1, fixed = TRUE)
      test1 <- gsub(".x", "", test1, fixed = TRUE)
      test1 <- gsub(".y", "", test1, fixed = TRUE)
      test1 <- paste(test1, test2, sep = " ") %>% trimws()
      colnames(graph1)[i] <- test1
   }
}
graph1 <- graph1[,grepl(".1", colnames(graph1), fixed = TRUE)==FALSE]

# Putting the columns into the correct order #
graph2A <- graph1 %>%
   select(Year, `All Sectors (Int)`,`All Sectors (EU)`,`All Sectors (Non-EU)`, `All Sectors (RUK)`,`All Sectors (All)`)

graph2B <- graph1 %>%
   select(-Year, -`All Sectors (Int)`, -`All Sectors (EU)`, -`All Sectors (Non-EU)`, -`All Sectors (RUK)`,-`All Sectors (All)`)

graph2B <- graph2B %>%
   select(order(colnames(graph2B)))

graph2 <- cbind(graph2A, graph2B)
rm(graph2A, graph2B)
gc()

# Final Result #
GRAPHS <- graph2

# DATA MANIPULATION 2 - MAP DATA
COUNTRY_MAP <- COUNTRIES %>%
   filter(country_disclosive == 0) %>%
   select(Year, Country, Total) %>%
   rename("Value" = Total) %>%
   distinct()

COUNTRY <- COUNTRY_MAP %>%
   filter(Year == Yr) %>%
   select(Country, Value) %>%
   rename("NAME" = Country) %>%
   as.data.frame()

mapex <- readOGR(dsn = "./Base Datatables/world_shape_file", layer = "TM_WORLD_BORDERS_SIMPL-0.3", encoding = "UTF-8")

#Modifying Russia Data so that it's all in one place

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
mapex@data$NAME <- editing_names
merged <- merge(mapex@data, COUNTRY, by = intersect(names(mapex@data), names(COUNTRY)), all.x = TRUE, all.y = FALSE, sort = FALSE)
mapex@data <- merged[match(mapex@data$NAME,merged$NAME),]
no_NA <- na.omit(merged)

## Getting List of Countries which have values less than £50 million
countriesLessFifty <- COUNTRIES %>%
   filter(Year == Yr) %>%
   filter(country_disclosive == 1) %>%
   pull(Country) %>%
   unique()

# DATA MANIPULATION 3 - COUNTRIES BAR CHART
D <- COUNTRIES %>%
   group_by(Year) %>%
   filter() %>%
   top_n(n = 25, wt = Total) %>%
   #mutate(Rank = min_rank(-Total) * 120) %>%
   ungroup() %>%
   arrange(Year, -Total)  %>%
   add_column("Code" = NA)

D$Code <- tolower(countrycode(D$Country, "country.name", "iso2c"))

# Data manipulation for other outputs is placed on the server and in R Markdown files

# COUNTRY LIST FOR DROP DOWN MENU IN COUNTRY PROFILES #####################################################
countryList <- read.xlsx("./Base Datatables/EPM Country Groupings.xlsx") %>%
   arrange(Order, Country)
countryList2 <- vector("list", length(countryList$Region %>% unique()))
names(countryList2) <- countryList %>% pull(Region) %>% unique()

for (i in seq_len(length(countryList2))) {
   region_select <- names(countryList2) [[i]]

   country_select <- countryList %>%
      filter(Region == region_select) %>%
      pull(Country) %>%
      unique() %>%
      sort()

   ## Putting "Other" options at the end of the list ##
   otherOptions <- country_select[grepl("Other ", country_select, fixed = TRUE)]

   if (length(otherOptions) > 0) {
      country_select <- country_select[!(grepl("Other ", country_select, fixed = TRUE))]
      country_select <- c(country_select,otherOptions)
   }

   countryList2[[i]] <- country_select
   rm(region_select, country_select,otherOptions)
}
countryList2 <- c("",countryList2)

# Custom Colour Palette for Sector Breakdown Line Charts ###############################
jama_palette1 <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#374E55FF", "#80796BFF")
###############################
#### Custom Functions ####
# Base code based on github page for shinyTree get_selected documentation
get_selected_slices_custom <- function(tree, ancestry = NULL, vec = list()) {
   if (is.list(tree)) {
      for (i in seq_len(length(tree))) {
         anc <- c(ancestry, names(tree)[i])
         vec <- get_selected_slices_custom(tree[[i]], anc, vec)
      }
   }

   a <- attr(tree, "stselected", TRUE)
   if (!is.null(a) && a == TRUE) {
      # Get element name
      ancList <- 0

      for (i in length(ancestry):1) {
         nl <- list()
         nl[ancestry[i]] <- list(ancList)
         ancList <- nl
      }

      vec[length(vec) + 1] <- list(ancList)
   }
   return(vec)
}

