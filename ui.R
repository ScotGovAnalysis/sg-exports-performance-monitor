# Loading packacges and data from a separate R file
source("data.R")

# UI
shinyUI(fluidPage(
    theme = shinytheme("cosmo"),
    # HEADING ########################################################################################################################################
    navbarPage(id = "MainNav",
               windowTitle = "Scotland's Exports Performance Monitor",
               title = div(
                   span(a(img(src = "Govscot_logo_white.png", height = 20), href = "https://www.gov.scot/", target = "_blank"), style = "padding-right:40px;"),
                   span("Scotland's Export Performance Monitor")
               ),
               # HOME PAGE ######################################################################################################################################
               tabPanel("Home",
                        tags$head(tags$link(rel="shortcut icon", src="./www/favicon.ico")),
                        fluidRow(
                            h1("Welcome!", style = "font-weight: bold; font-size: 22px; margin: 0px;"),
                            p(paste("This Export Performance Monitor tool uses official data from Export Statistics Scotland",Yr, "to help understand current and past export performance in Scotland.")),
                            img(src="TradingNationBanner2.png", width="100%"),
                            style = "padding: 0px;"
                        ),
                        setZoom(id = "SetEffects"), setShadow(id = "SetEffects"),
                        fluidRow(
                            column(width = 4,
                                   actionLink(
                                       "GoToTimeSeriesTab",
                                       label = div(
                                           tags$b("Time Series", style = "color: black;"),
                                           tags$p("View and download time series data for any combination of sector, subsector and destination (International, EU, Non-EU and RUK).", style = "color: black;"),
                                           tags$img(src = "thumbnails/DygraphThumbnail.png", width = "40%", height = "40%"),
                                           tags$img(src = "thumbnails/TableThumbnail.png", width = "40%", height = "40%"),

                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),
                            column(width = 4,
                                   actionLink(
                                       "GoToDestinationBreakdownsTab",
                                       label = div(
                                           tags$b("Destination Breakdowns", style = "color: black;"),
                                           tags$p("Maps, plots and animations giving a big picture of where Scottish goods and services are exported.", style = "color: black;"),
                                           img(src = "thumbnails/ChoroplethThumbnail.png", width = "50%", height = "50%", style = "opacity: 0.5"),
                                           img(src = "thumbnails/AnimationThumbnail.png", width = "25%", height = "50%", style = "opacity: 0.5"),
                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),
                            column(width = 4,
                                   actionLink(
                                       "GoToSectorBreakdownsTab",
                                       label = div(
                                           tags$b("Sector Breakdowns", style = "color: black;"),
                                           tags$p("A gallery of plots telling a story on the breakdown of Scotland's exports by sector.", style = "color: black;"),
                                           img(src = "thumbnails/SankeyThumbnail.png", width = "50%", height = "50%", style = "opacity: 0.5;"),
                                           img(src = "thumbnails/Animation2.png", width = "25%", height = "50%", style = "opacity: 0.5;"),
                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),

                            style = "padding: 10px; margin-top: 20px;"
                        ),
                        fluidRow(
                            column(width = 4,
                                   actionLink(
                                       "GoToBackgroundTab",
                                       label = div(
                                           tags$b("Background", style = "color: black;"),
                                           tags$p("Background information on the application as well as sector definitions.", style = "color: black;"),
                                           img(src = "thumbnails/InfoThumbnail1.png", width = "40%", height = "50%"),
                                           img(src = "thumbnails/InfoThumbnail2.png", width = "40%", height = "50%"),
                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),
                            column(width = 4,
                                   actionLink(
                                       "GoToCountryProfilesTab",
                                       label = div(
                                           tags$b("Country Profiles", style = "color: black;"),
                                           tags$p("View and download reports on Scotland's exports for individual countries.", style = "color: black;"),
                                           div(
                                               img(src = "./flags/USA.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Netherlands.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/France.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Germany.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Ireland.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Spain.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Norway.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Sweden.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Australia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Canada.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Mexico.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Denmark.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Italy.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/China.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Brazil.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Switzerland.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Belgium.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Japan.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Egypt.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Malaysia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Nigeria.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/South Africa.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Singapore.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Russia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Finland.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Angola.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/India.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/South Korea.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Greece.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Poland.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Pakistan.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Romania.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Turkey.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Ukraine.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Slovakia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Philippines.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Portugal.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Serbia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Lithuania.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Argentina.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Israel.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Jamaica.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Bangladesh.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Czechia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Estonia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Belarus.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Kazakhstan.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Colombia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Armenia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Algeria.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Ethiopia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Ecuador.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Cambodia.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Brunei.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Bulgaria.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               img(src = "./flags/Cuba.png", width = "6%", height = "6%", style = "opacity: 0.5"),
                                               style = "margin: 10px 5px 10px 5px;"
                                           ),
                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),
                            column(width = 4,
                                   actionLink(
                                       "GoToSectorProfilesTab",
                                       label = div(
                                           tags$b("Sector Profiles", style = "color: black;"),
                                           tags$p("View and download reports on Scotland's exports for individual sectors.", style = "color: black;"),
                                           div(
                                               img(src = "./sectors/Food and Drink.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Engineering and Advanced Manufacturing.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Energy.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Financial and Business Services.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Technology, Digital and Media.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Chemical Sciences.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Wholesale and Retail Trade.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Life Sciences.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Transportation and Storage.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Education.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Sustainable Tourism.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Textiles.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Forest and Timber Technologies.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Other Services and Accommodation.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Construction.png", width = "6%", style = "opacity: 0.5; margin: 10px;"),
                                               img(src = "./sectors/Real Estate.png", width = "6%", style = "opacity: 0.5; margin: 10px;")
                                           ),
                                           style = "
                                           border: solid 1px black;
                                           height: 220px;
                                           width: 100%;
                                           text-align: center;
                                           padding: 5px;
                                           border-radius: 0px;
                                           position: static !important;
                                           display: block;
                                           overflow: hidden;
                                           ",
                                           id = "SetEffects"
                                       )
                                   )
                            ),
                            style = "padding: 10px;"
                        ),
                        style = "font-size: 16px;"
               ),
               # BACKGROUND #####################################################################################################################################
               tabPanel(
                   value = "BackgroundTab",
                   title = "Background",
                   tabsetPanel(id = "background_menu",
                       tabPanel("Background Information",
                                h1("Scotland's Export Performance Monitor", img(src="Govscotlogo2.png", style = "float:right;", height = 50), style = "font-weight: bold;"),
                                h2("Introduction", style = "font-weight:bold;"),
                                p("This analysis was initially produced to support the development of our new export growth plan which was published in May 2019 -"),
                              #  p("This analysis has been produced to support the development of our new export growth plan - Scotland: A Trading Nation:"),

                                a("https://www.gov.scot/publications/scotland-a-trading-nation/", href = "https://www.gov.scot/publications/scotland-a-trading-nation/", target = "_blank"),
                                p("It's based on the existing export data from latest version of Export Statistics Scotland (ESS) but presented with the aim to be more accessible and user friendly.  By using new sector definitions and including more detailed sub-sector breakdowns, it can help users to understand current and past export performance in Scotland.  As well as detailed information on international and rest of the UK exports for each sector and sub-sector, this application has been supplemented with data on number of businesses, size of businesses, turnover and employment."),

                                h2("Sectors", style = "font-weight:bold;"),
                                p("The sector and sub-sector definitions used in this analysis, which are based on the Standard Industrial Classification (SIC) of the company exporting, were developed in collaboration with our partner organisations, Scottish Enterprise and Scottish Development International to try and give a more detailed analysis than is currently provided from the published ESS data.  The ESS data excludes exports of oil and gas extracted from the UK Continental Shelf.  However exports of services provided to the oil and gas sector are included and cross over a number of key sectors, including energy (energy support) and engineering.  For some sectors current export data may not capture all export activity and work will continue to improve this data going forward."),
                                actionLink(inputId = "linkToSectorDefinitions", label = "Click here to see the sector definitions."),

                                h2("Destination Country", style = "font-weight:bold;"),
                                p("This analysis provides the top 10 export destination countries for each sector and sub-sector.  However the current ESS methodology means that destination country data should be treated as indicative only, particularly for the smaller sectors and sub-sectors."),

                                h2("Methodology and Sources", style = "font-weight:bold;"),
                                p("All of the export data in this analysis is based on the data produced for ESS 2018. For further information on the methodology and data sources please refer to the ESS publication below."),
                                a("https://www.gov.scot/publications/export-stats-scotland-2018/", href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"),
                                h4(""),
                                p(paste0("Some companies' SIC codes have been changed to reflect the correct Export Growth Plan sector. This means export figures may be different from the sector totals in ESS ", Yr,".")),

                                h2("Business Statistics", style = "font-weight:bold;"),
                                p("The business statistics are sourced from the Inter-Departmental Business Register (IDBR).  The IDBR, maintained by the Office for National Statistics, is a database of all enterprises registered for VAT and/or PAYE, covering 99 per cent of economic activity in the UK.  Those excluded are small businesses with no employees and an annual turnover below the VAT threshold (£83,000 as at March 2017).  Although the IDBR is not usually the preferred source for sectoral employment or turnover, IDBR employment and turnover data has been used here for consistency with the IDBR business counts and to provide a breakdown by business size.  Note that business size here has been defined according to the number of employees that the business employs in Scotland (rather than UK-wide). Further information on business statistics, including methodology, can be found via the link below."),
                                a("https://www2.gov.scot/Topics/Statistics/Browse/Business", href = "https://www2.gov.scot/Topics/Statistics/Browse/Business", target = "_blank"),

                                h2("Feedback", style = "font-weight:bold;"),
                                p("This application has been published as experimental analysis.  We plan to develop this analysis and publish further versions.  To help with this,  we would welcome any feedback you may have and would be particularly interested in knowing how you make use of this data.  Please email any comments to:"),
                                p("exports.statistics@gov.scot")
                       ),
                       tabPanel("Sector Definitions",
                                h1("Sector Definitions", style = "font-weight:bold;"),
                                p("The sector definitions are based on the Standard Industrial Classification (SIC) of the company exporting. Further information on SIC can be found using the link below."),
                                a("https://www.ons.gov.uk/methodology/classificationsandstandards/ukstandardindustrialclassificationofeconomicactivities/uksic2007", href = "https://www.ons.gov.uk/methodology/classificationsandstandards/ukstandardindustrialclassificationofeconomicactivities/uksic2007", target = "_blank"),
                                p("Note, each sector and sub-sector name is followed by G, to denote goods, or S to denote services. This is based on the main activity for that particular sector."),
                                fluidRow(
                                    p(tags$b("Table 1. Sector Definitions", style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                    uiOutput("SectorDefinitionTable"),
                                    align = "center"
                                )
                       )
                   )
               ),
               # TAB 1 - TIME SERIES ############################################################################################################################
               tabPanel(
                   value = "TimeSeriesTab",
                   title = tags$div(icon("chart-line", lib = "font-awesome"), "Time Series"),
                   tabsetPanel(
                       tabPanel("Chart",
                                column(width = 3,
                                       checkboxGroupInput(inputId = "timeSeriesDestination", label = "Please choose a destination below: ",
                                                             choices = destinationChoices,
                                                             selected = c("International","Rest of the UK (RUK)", "All Destinations")
                                                             ),

                                       strong(p("Choose a sector or subsector from the two tabs below. Each checkbox menu can be searched using the search box.")),

                                       tabsetPanel(
                                           tabPanel("Sector",
                                                    shinyTree("treeCheckbox_sector",
                                                              checkbox = TRUE,
                                                              themeIcons = FALSE,
                                                              themeDots = FALSE,
                                                              theme = "proton",
                                                              search = TRUE,
                                                              wholerow = FALSE),

                                           ), # ... sector controls end here
                                           tabPanel("Subsector",
                                                    shinyTree("treeCheckbox_Subsector",
                                                              checkbox = TRUE,
                                                              themeIcons = FALSE,
                                                              themeDots = FALSE,
                                                              theme = "proton",
                                                              search = TRUE,
                                                              wholerow = FALSE),
                                           ) # ... subsector controls end here
                                       ),
                                    h1(" "),
                                       div(align = "center",
                                           actionButton(inputId = "resetTimeSeries", label = "Reset Choices")
                                           )
                                ), # ... graph controls end here

                                # TAB 1 - GRAPH
                                column(width = 9,
                                       fluidRow(
                                           column(12,align = "center",
                                                  p(tags$b(paste("Figure 1. Scotland's Exports by Sector and Destination ", "(", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                  dygraphOutput("sector_graph") %>% withSpinner(type = 5)
                                                  ),
                                           # TAB 1 - GRAPH LEGEND
                                           column(6, align = "left", style = "padding: 0px; margin-bottom: 100px;",
                                                  textOutput("legendDivID"),
                                                  ),
                                           column(6, align = "right", style = "padding: 0px; margin-bottom: 100px;",
                                                  p( HTML(paste("Source: Export Statistics Scotland", Yr,"<br/>",
                                                           "Estimates are rounded to the nearest £5 million.")))
                                                  )
                                       ),
                                       # TAB 1 - GRAPH TABLE
                                       fluidRow(
                                           p(tags$b(paste("Table 1. Scotland's Exports ", "(", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                           DTOutput("table_for_graph"),
                                           align = "center"
                                       )
                                )
                       ), # ... chart ends here
                       tabPanel("Table",
                                fluidRow(
                                    tabsetPanel(
                                        # TAB 1 - TABLE FOR INT
                                        tabPanel("International",
                                                 p(tags$b(paste("Table 2. Scotland's International Exports by Sector", " (", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                 DTOutput("sector_table_int"),
                                                 align = "center"
                                        ),
                                        # TAB 1 - TABLE FOR EU
                                        tabPanel("EU",
                                                 p(tags$b(paste("Table 3. Scotland's EU Exports by Sector", " (", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                 DTOutput("sector_table_EU"),
                                                 align = "center"
                                        ),
                                        # TAB 1 - TABLE FOR Non-EU
                                        tabPanel("Non-EU",
                                                 p(tags$b(paste("Table 4. Scotland's Non-EU Exports by Sector", " (", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                 DTOutput("sector_table_nonEU"),
                                                 align = "center"
                                        ),
                                        # TAB 1 - TABLE FOR RUK
                                        tabPanel("Rest of the UK",
                                                 p(tags$b(paste("Table 5. Scotland's Exports to the Rest of the UK by Sector", " (", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                 DTOutput("sector_table_ruk"),
                                                 align = "center"
                                        ),
                                        # TAB 1 - TABLE FOR ALL
                                        tabPanel("All Destinations",
                                                 p(tags$b(paste("Table 6. Scotland's Total Exports by Sector ", "(", as.character(St), " - ", as.character(Yr), ") ", "(£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 15px; margin-top: 10px;"),
                                                 DTOutput("sector_table_all"),
                                                 align = "center"
                                        )
                                    )
                                )
                       )
                   )
               ),

               # TAB 2 - DESTINATION BREAKDOWNS ##################################################################################################################
               tabPanel(
                   title = tags$div(icon("globe", lib = "font-awesome"), "Destination Breakdowns"),
                   value = "DestinationBreakdownsTab",
                   fluidRow(
                       # TAB 2 - LEAFLET MAP
                       column(12, align = "center",
                              p(tags$b(paste("Map 1. Choropleth showing Scotland's International Exports in ", as.character(Yr), " (£ million)", sep = ""), style = "text-align: center;"), style = "margin-bottom: 20px;")
                       ),
                       column(12, align = "left",
                              div(style = "align: left;",
                                  leafletOutput("BigMap") %>% withSpinner(type = 5)
                              )
                       ),
                       column(12, align = "right", style = "padding: 0px;",
                              p( HTML(paste("Source: Export Statistics Scotland", Yr,"<br/>",
                                            "Estimates are rounded to the nearest £5 million.")))
                       ),
                       column(12,
                              div(style = "border-top: 1px solid; margin: 0px;"),
                       ),
                       column(12, align = "center",
                              # TAB 2 - BAR CHART
                              tags$b(textOutput("Top25Caption")),
                              sliderTextInput(inputId = "CountryBarChartSlide", label = "", choices = seq(St,Yr,by = 1), selected = Yr, width = "50%",grid = TRUE ),
                              plotOutput("CountryBarChart", height = "500px"),
                              div(downloadButton(outputId = "CountryAnimationBtn", label = "Download as GIF Animation"), style = "margin-top: 20px;"),
                              div(style = "border-top: 1px solid; margin: 40px;")
                       )
                   )
               ),

               # TAB 3 - COUNTRY PROFILES #######################################################################################################################
               tabPanel(
                   title = tags$div(icon("atlas", lib = "font-awesome"), "Country Profiles"),
                   value = "CountryProfilesTab",
                   fluidRow(
                       # HELP TEXT
                       fluidRow(
                           p("Please select country below to find out the value of Scottish exports to that destination in ", as.character(Yr), style = "margin-left:24px;"),
                       ),
                       # TAB 3 - MAP
                       column(width = 5,
                              p("You can select a country from the drop down list below. This list can be typed in to search."),
                              selectizeInput("countryProfileDropDown", label = NULL, choices = countryList2, selected = NULL, multiple = FALSE,
                                             width = "100%", options = list(placeholder = "...")),
                              p("Or, you can click on a country in the map below to select a country."),
                              leafletOutput(outputId = "country_profile_map", height = 550) %>% withSpinner(type = 5),
                              column(12, align = "right",
                                     p( HTML(paste("Source: Export Statistics Scotland", Yr,"<br/>",
                                                   "Estimates are rounded to the nearest £5 million.")))
                              )
                       ),
                       # TAB 3 - COUNTRY REPORT
                       column(width = 7,
                              fluidPage(
                                  uiOutput("repex"),
                                  style = "font-size:18px"
                              ),
                              # TAB 3 - DOWNLOAD BUTTON
                              fluidRow(
                                  radioButtons("FileFormatCountry", label = "", choices = c("PDF", "HTML", "Word"), inline = TRUE),
                                  downloadButton(outputId = "CountryBtn", label = "Generate Report"),
                                  style = "border-top: 1px solid; margin-top: 50px;"
                              )
                       )
                   )
               ),

               # TAB 4 - SECTOR BREAKDOWNS #######################################################################################################################
               tabPanel(
                   title = tags$div(icon("chart-pie", lib = "font-awesome"), "Sector Breakdowns"),
                   value = "SectorBreakdownsTab",
                   fluidRow(
                       column(12, align = "center",
                              # TAB 4 - SANKEY
                              radioButtons(inputId = "PickedSankeyData", label = NULL, choiceNames = c("All Destinations", "International", "Rest of the UK"), choiceValues = c("SANKEY_ALL", "SANKEY_INT", "SANKEY_RUK"), inline = TRUE),

                       ),
                       div(style = "border-top: none;"),
                       column(12, align = "center",
                              tags$b(textOutput("SankeyCaption")),
                              div(align = "center",
                                  column(12,  align = "center",
                                         sankeyNetworkOutput("SectorSankey", height = "700px", width = "100%") %>% withSpinner(type = 5)
                                  )
                              ),
                              column(12, align = "right",
                                     p( HTML(paste("Source: Export Statistics Scotland", Yr,"<br/>",
                                                   "Estimates are rounded to the nearest £5 million.")))
                                     )
                       ),
                       column(12,
                              div(style = "border-top: 1px solid; margin: 40px;"),
                              # TAB 2 - BAR CHART
                              radioButtons(inputId = "PickedSectorBarChartData", label = NULL, choiceNames = c("All Destinations", "International", "Rest of the UK"), choiceValues = c("All", "International", "RUK"), inline = TRUE),
                              tags$b(textOutput("Top25CaptionSector")),
                              sliderTextInput(inputId = "SectorBarChartSlide", label = "", choices = seq(St,Yr,by = 1), selected = Yr, width = "50%",grid = TRUE ),
                              plotOutput("SectorBarChart", height = "500px"),
                              div(downloadButton(outputId = "SectorAnimationBtn", label = "Download as GIF Animation"), style = "margin-top: 20px;"),
                              div(style = "border-top: 1px solid; margin: 40px;"),

                              align = "center"
                       )
                   )
               ),

               # TAB 5 - SECTOR PROFILES #########################################################################################################################
               tabPanel(
                   title = tags$div(icon("industry", lib = "font-awesome"), "Sector Profiles"),
                   value = "SectorProfilesTab",
                   fluidRow(
                       # TAB 5 - CHOOSE SECTOR
                       column(width  =  3,
                              tags$button(id = "SectBtnChemical", img(src = "sectors/Chemical Sciences.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Chemical Sciences", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnConstruction", img(src = "sectors/Construction.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Construction", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnEducation", img(src = "sectors/Education.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Education", style  =  "padding: 5px"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnEngineering", img(src = "sectors/Engineering and Advanced Manufacturing.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Engineering and Advanced Manufacturing", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnEnergy", img(src = "sectors/Energy.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Energy", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnFinancial", img(src = "sectors/Financial and Business Services.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Financial and Business Services", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnFood", img(src = "sectors/Food and Drink.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Food and Drink", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnForest", img(src = "sectors/Forest and Timber Technologies.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Forest and Timber Technologies", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnLife", img(src = "sectors/Life Sciences.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Life Sciences", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnOther", img(src = "sectors/Other Services and Accommodation.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Other Services and Accommodation ", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnReal", img(src = "sectors/Real Estate.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Real Estate", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnSustainable", img(src = "sectors/Sustainable Tourism.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Sustainable Tourism", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnTechnology", img(src = "sectors/Technology, Digital and Media.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Technology, Digital and Media", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnTextiles", img(src = "sectors/Textiles.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Textiles", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnTransportation", img(src = "sectors/Transportation and Storage.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Transportation and Storage", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button"),
                              tags$button(id = "SectBtnWholesale", img(src = "sectors/Wholesale and Retail Trade.png", style  =  "float: left; height:32px; width: 32px; margin-right: 20px;"), p("Wholesale and Retail Trade", style  =  "padding: 5px; font-size:13px;"), style  =  "width: 100%; text-align: left; vertical-align: middle;", class  =  "btn action-button")
                       ),

                       # TAB 5 - SECTOR REPORT
                       column(width = 9,
                              fluidRow(
                                  textOutput("skil"),
                                  uiOutput("RenderSectorReport"),
                                  style = "font-size:18px"
                              ),
                              # TAB 5 - DOWNLOAD REPORT BUTTON
                              fluidRow(
                                  radioButtons("FileFormatSector", label = "", choices = c("PDF", "HTML", "Word"), inline = TRUE),
                                  downloadButton(outputId = "SectorBtn", label = "Generate Report"),
                                  style = "border-top: 1px solid; margin-top: 50px;"
                              )
                       )
                   )
               ) # ... TAB 5 end here
    ), # ... tabset ends here.

    # FOOTER ##########################################################################################################################################
    fluidRow(
        br(),
        wellPanel(
            fluidRow(
                # FOOTER - ABOUT
                column(width = 3,
                       icon("info", lib = "font-awesome"),
                       strong("ABOUT"),
                       p("This analysis was initially produced to support the development of our new export growth plan which was published in May 2019 -"),
                       #p("This analysis has been produced to support the development of our new export growth plan - "),
                       a("A Trading Nation: a Plan for Growing Scotland's Exports", href = "https://www.gov.scot/publications/scotland-a-trading-nation/", target = "_blank", ".")
                ),
                # FOOTER - COPYRIGHT NOTICE
                column(width = 3,
                       icon("copyright", lib = "font-awesome"),
                       strong("COPYRIGHT NOTICE"),
                       p("You may use or re-use this information (not including logos) free of charge in any format or medium, under the terms of the ",
                         a("Open Government Licence", href = "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank"), ".")
                ),
                # FOOTER - CONTACT DETAILS
                column(width = 3,
                       icon("at", lib = "font-awesome"),
                       strong("CONTACT DETAILS"),
                       p("We welcome your feedback:"),
                       p("exports.statistics@gov.scot", style = "line-height: 0%;")
                ),
                # FOOTER - EXTERNAL LINKS
                column(width = 3,
                       icon("external-link", lib = "font-awesome"),
                       strong("EXTERNAL LINKS"),
                       p(a("Export Statistics Scotland", href = "https://www.gov.scot/publications/about-export-statistics-Scotland", target = "_blank")),
                       p(a("STATISTICS.GOV.SCOT", href = "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fexports", target = "_blank"), style = "line-height: 0%;"),
                       p(a("Exports Performance Monitor", href = "https://www.gov.scot/publications/scotlands-export-performance-monitor/", target = "_blank"))
                )
            ),
            fluidRow(
                p("Reload the page should you experience any issues."),
                style = "text-align: center; outline: 0px;"
            )
        )
    ), # Navbar page ends here
    #### Setting style HTML tags - Options Groupings for lists ####
    tags$head(
        tags$style(
            HTML("
                    /* Changing the Header for each Option Grouping */
                     .selectize-dropdown .optgroup .optgroup-header{
                        font-size:14px;
                        color: #000000;
                        font-weight: 600;
                        background:#FFFFFF;
                     }

                     /* Image properties */
                     img {
                     max-width: 100%;
                     max-height: 100%;
                     }
                     "
            )
        )
    ), # End of Style Tag
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}")
)
) # UI ends here

