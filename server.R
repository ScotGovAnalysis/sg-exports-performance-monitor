# SERVER
shinyServer(
  function(input, output, session) {

    # ENTIRE PAGE ####################################################################################################################################
    observe_helpers()

    # Stop the app when the page has been closed (development work only)
    session$onSessionEnded(function() {
      stopApp()
    })

    # HOME PAGE ######################################################################################################################################
    # Making Home Page boxes take you to the corresponding tabs
    observeEvent(input$GoToBackgroundTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "BackgroundTab"
      )
    })
    observeEvent(input$GoToTimeSeriesTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "TimeSeriesTab"
      )
    })
    observeEvent(input$GoToDestinationBreakdownsTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "DestinationBreakdownsTab"
      )
    })
    observeEvent(input$GoToCountryProfilesTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "CountryProfilesTab"
      )
    })
    observeEvent(input$GoToSectorBreakdownsTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "SectorBreakdownsTab"
      )
    })
    observeEvent(input$GoToSectorProfilesTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "SectorProfilesTab"
      )
    })

    # BACKGROUND TAB #################################################################################################################################
    # TAB 1 - TABLE 2
    output$SectorDefinitionTable <- renderUI({
      HTML(
        knitr::kable(SectorDefinitions, col.names = c("Sector", "Subsector", "Goods/Services", "SIC Code")) %>%
          collapse_rows(columns = c(1,2)) %>%
          kable_styling(bootstrap_options = c("striped", "bordered"), fixed_thead = TRUE)

      )
    })

    observeEvent(input$linkToSectorDefinitions, {
      updateTabsetPanel(session,
                        inputId = "background_menu",
                        selected = "Sector Definitions"
                          )
    })
    # TAB 1 - TIME SERIES ############################################################################################################################
    # TAB 1 - HIERARCHICAL CHECKBOXES
    sectorSelection <- reactiveVal(list())

    output$treeCheckbox_sector <- renderTree({
      list <- list_of_sectors
      return(list)
    })

    sectorFilter <- reactive({
      if (length(sectorSelection()) > 0) {
        ## Getting the Sectors Wanted ##
        var1 <- sectorSelection()

        ## Getting the Destinations wanted ##
        destinations <- input$timeSeriesDestination
        destinations <- gsub("International", "(Int)", destinations, fixed = TRUE)
        destinations <- gsub("Non-EU", "(Non-EU)", destinations, fixed = TRUE)
        if ("EU" %in% destinations) {
          destinations <- c(destinations[destinations != "EU"], "(EU)")
        }
        destinations <- gsub("Rest of the UK (RUK)", "(RUK)", destinations, fixed = TRUE)
        destinations <- gsub("All Destinations", "(All)", destinations, fixed = TRUE)

        ## Pasting Sectors and Destinations together and returning result ##
        vector_list <- as.vector(outer(var1, destinations, paste, sep = " "))

        return(vector_list)
      } else {
        return(NULL)
      }
    })

    observeEvent(input$treeCheckbox_sector, {
      sectorSelection(get_selected(input$treeCheckbox_sector, format = "classid") %>% unlist())
    })

    output$treeCheckbox_Subsector <- renderTree({
      list <- list_of_subsectors
      return(list)
    })


    outputOptions(output, "treeCheckbox_Subsector", suspendWhenHidden = FALSE)
    subsectorTreeSelection <- reactiveVal(list())

    observeEvent(input$treeCheckbox_Subsector, {
      subsectorTreeSelection(get_selected(input$treeCheckbox_Subsector, format = "classid") %>% unlist())
    })

    subsectorFilter <- reactive({
      if (length(subsectorTreeSelection()) > 0) {
        ## Getting the Sectors Wanted ##
        var1 <- subsectorTreeSelection()

        # Removing entries which are the sector names for choices with subsectors #
        var1 <- var1[!(var1 %in% c("Engineering and Advanced Manufacturing","Energy","Food and Drink","Financial and Business Services",
                                   "Technology, Digital and Media","Wholesale and Retail Trade","Life Sciences","Transportation and Storage"))]

        ## Getting the Destinations wanted ##
        destinations <- input$timeSeriesDestination

        destinations <- gsub("International", "(Int)", destinations, fixed = TRUE)
        destinations <- gsub("Non-EU", "(Non-EU)", destinations, fixed = TRUE)
        if ("EU" %in% destinations) {
          destinations <- c(destinations[destinations != "EU"], "(EU)")
        }
        destinations <- gsub("Rest of the UK (RUK)", "(RUK)", destinations, fixed = TRUE)
        destinations <- gsub("All Destinations", "(All)", destinations, fixed = TRUE)

        ## Pasting Sectors and Destinations together and returning result ##
        vector_list <- as.vector(outer(var1, destinations, paste, sep = " "))

        return(vector_list)
      } else {
        return(NULL)
      }

    })

    ## Reacting to Reset Button ##
    observeEvent(input$resetTimeSeries,{
      ## Updating Desintations Checkbox to default values ##
      updateCheckboxGroupInput( session = session,
        inputId = "timeSeriesDestination",
        choices = destinationChoices,
        selected = c("International","Rest of the UK (RUK)", "All Destinations")
      )

      ## Updating Sector Checkboxes to default values ##
      updateTree(session, "treeCheckbox_sector", list_of_sectors)
      subsectorTreeSelection(list("All Sectors" = "All Sectors"))

      ## Updating Subsector Checkboxes to default values ##
      updateTree(session, "treeCheckbox_Subsector", list_of_subsectors)
      subsectorTreeSelection(list())

    })

    # TAB 1 - FIGURE 1
    output$sector_graph <- renderDygraph({
      ## Getting Character vectors of the Sectors and sub sectors selected ##
      filter <- NA
      sectors_selected <- c(sectorFilter()) %>% unique() %>% sort()
      if (!is.null(sectors_selected)) {filter <- c(filter, sectors_selected)}

     subsectors_selected <- c(subsectorFilter()) %>% unique() %>% sort()
      if (!is.null(subsectors_selected)) {filter <- c(filter, subsectors_selected)}

    #  sectors_selected <- c(input$select_sector_all,input$select_sector_ruk, input$select_sector_int,input$select_sector_EU,input$select_sector_NonEU) %>% unique() %>% sort()
    #  if (length(sectors_selected) > 0) {filter <- c(filter, sectors_selected)}

    #  subsectors_selected <- c(input$select_subsector_ruk, input$select_subsector_all, input$select_subsector_int,input$select_subsector_EU,input$select_subsector_NonEU) %>% unique() %>% sort()
    #  if (length(subsectors_selected) > 0) {filter <- c(filter, subsectors_selected)}

      filter <- filter %>%
        na.omit() %>%
        as.character() %>%
        as.data.frame()
      colnames(filter) <- "Temp1"

      filter <- filter %>%
        mutate(Order = case_when(
          grepl("(INT)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 1,
          grepl("(EU)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 2,
          grepl("(NON-EU)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 3,
          grepl("(RUK)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 4,
          grepl("(ALL)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 5
        )) %>%
        arrange(Order, Temp1) %>%
        pull(Temp1) %>%
        trimws()

      if (length(filter) > 0) {
        ## Creating dygraph ##
        dy_graph_data <- GRAPHS %>%
          select(Year, one_of(filter)) %>%
          mutate(Year = as.numeric(Year))

        dyGraph1 <- dygraph(dy_graph_data) %>%
          dyGroup(dy_graph_data %>%
                    colnames(),
                  strokeWidth = 4) %>%
          dyRangeSelector() %>%
          dyAxis("x", label = "Year",drawGrid = TRUE, rangePad = 5,
                 valueRange = c(min(GRAPHS$Year), max(GRAPHS$Year)),
                 ticker = JS("function() {
                             return [
                             {v: 2002, label : '2002'},
                             {v: 2003, label : '2003'},
                             {v: 2004, label : '2004'},
                             {v: 2005, label : '2005'},
                             {v: 2006, label : '2006'},
                             {v: 2007, label : '2007'},
                             {v: 2008, label : '2008'},
                             {v: 2009, label : '2009'},
                             {v: 2010, label : '2010'},
                             {v: 2011, label : '2011'},
                             {v: 2012, label : '2012'},
                             {v: 2013, label : '2013'},
                             {v: 2014, label : '2014'},
                             {v: 2015, label : '2015'},
                             {v: 2016, label : '2016'},
                             {v: 2017, label : '2017'},
                             {v: 2018, label : '2018'}
                             ]
                             }"

                 )

                 ) %>%
          dyAxis(
            "y",
            label = "Value (£ million)",
            valueFormatter = "function(d) {return '£' + d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') + ' million'; }",
            axisLabelFormatter = "function(d) {return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') ; }",
            axisLabelWidth = 70
          ) %>%
          dyHighlight(
            highlightCircleSize = 5,
            highlightSeriesBackgroundAlpha = 0.2,
            hideOnMouseOut = FALSE,
            highlightSeriesOpts = list(strokeWidth = 6)
          ) %>%
          dyOptions(
            gridLineColor = "lightgrey",
            digitsAfterDecimal = 0
          ) %>%
          dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = TRUE) %>%
          dyUnzoom() %>%
          dyCrosshair(direction = "vertical")

        return(dyGraph1)
      }
    })

    # TAB 1 - TABLE 2
    output$table_for_graph <- DT::renderDT({
      ## Gathering Data for the data table ##
      ## Getting Character vectors of the Sectors and sub sectors selected ##
      ## Getting Character vectors of the Sectors and sub sectors selected ##
      filter <- NA
      sectors_selected <- c(sectorFilter()) %>% unique() %>% sort()
      if (!is.null(sectors_selected)) {filter <- c(filter, sectors_selected)}

      subsectors_selected <- c(subsectorFilter()) %>% unique() %>% sort()
      if (!is.null(subsectors_selected)) {filter <- c(filter, subsectors_selected)}

#      sectors_selected <- c(input$select_sector_all,input$select_sector_ruk, input$select_sector_int, input$select_sector_EU,input$select_sector_NonEU) %>% unique() %>% sort()
#      if (length(sectors_selected) > 0) {filter <- c(filter, sectors_selected)}

 #     subsectors_selected <- c(input$select_subsector_ruk, input$select_subsector_all, input$select_subsector_int, input$select_subsector_EU,input$select_subsector_NonEU) %>% unique() %>% sort()
#      if (length(subsectors_selected) > 0) {filter <- c(filter, subsectors_selected)}

      filter <- filter %>%
        na.omit() %>%
        as.character() %>%
        as.data.frame()
      colnames(filter) <- "Temp1"

      filter <- filter %>%
        mutate(Order = case_when(
          grepl("(INT)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 1,
          grepl("(EU)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 2,
          grepl("(NON-EU)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 3,
          grepl("(RUK)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 4,
          grepl("(ALL)", toupper(enc2utf8(Temp1)), fixed = TRUE) == TRUE ~ 5
        )) %>%
        arrange(Order, Temp1) %>%
        pull(Temp1)


      if (length(filter) > 0) {
        ## Creating dygraph ##
        dy_graph_data <- GRAPHS %>%
          select(Year, one_of(filter))

        ## Creating Data Table ##
        dataTable1 <- DT::datatable(
          dy_graph_data,
          extensions = "Buttons",
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: left; color: black;",
            HTML(paste(
              "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
            ))
          ),
          options = list(
            dom = 'frtBip',
            buttons = list(
              list(extend = 'excel', filename = "Scottish Exports"),
              list(extend = 'csv', filename = "Scottish Exports"),
              list(extend = 'pdf', filename = "Scottish Exports"),
              list(extend = 'copy', filename = "Scottish Exports"),
              list(extend = 'print', filename = "Scottish Exports")
            ),
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE
          ), rownames = FALSE
        )  %>%
          formatCurrency(columns = seq(2,ncol(dy_graph_data)), currency = "", interval = 3, digits = 0, mark = ",")
        return(dataTable1)
      }
    })

    # TAB 1 - TABLE 3
    output$sector_table_int <- DT::renderDT({

      DT::datatable(
        TABLE_INT,
        colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; color: black;",
          HTML(paste(
            "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
          ))
        ),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        options = list(
          buttons = list(
            list(extend = 'excel', filename = "Scottish Exports"),
            list(extend = 'csv', filename = "Scottish Exports"),
            list(extend = 'pdf', filename = "Scottish Exports"),
            list(extend = 'copy', filename = "Scottish Exports"),
            list(extend = 'print', filename = "Scottish Exports")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 2)
      )) %>%
        formatCurrency(columns = seq(1,ncol(TABLE_INT)), currency = "", interval = 3, digits = 0, mark = ",") %>%
        formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
    })

    # TAB 1 - TABLE 4
    output$sector_table_EU <- DT::renderDT({
      DT::datatable(
        TABLE_EU,
        colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; color: black;",
          HTML(paste(
            "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
          ))
        ),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        options = list(
          buttons = list(
            list(extend = 'excel', filename = "Scottish Exports"),
            list(extend = 'csv', filename = "Scottish Exports"),
            list(extend = 'pdf', filename = "Scottish Exports"),
            list(extend = 'copy', filename = "Scottish Exports"),
            list(extend = 'print', filename = "Scottish Exports")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 2)
        )
      ) %>%
        formatCurrency(columns = seq(1,ncol(TABLE_EU)), currency = "", interval = 3, digits = 0, mark = ",") %>%
        formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
    })

    # TAB 1 - TABLE 5
    output$sector_table_nonEU <- DT::renderDT({
      DT::datatable(
        TABLE_NONEU,
        colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; color: black;",
          HTML(paste(
            "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
          ))
        ),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        options = list(
          buttons = list(
            list(extend = 'excel', filename = "Scottish Exports"),
            list(extend = 'csv', filename = "Scottish Exports"),
            list(extend = 'pdf', filename = "Scottish Exports"),
            list(extend = 'copy', filename = "Scottish Exports"),
            list(extend = 'print', filename = "Scottish Exports")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 2)
        )
      ) %>%
        formatCurrency(columns = seq(1,ncol(TABLE_NONEU)), currency = "", interval = 3, digits = 0, mark = ",") %>%
        formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
    })

    # TAB 1 - TABLE 6
    output$sector_table_ruk <- DT::renderDT({
      DT::datatable(
        TABLE_RUK,
        colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; color: black;",
          HTML(paste(
            "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
          ))
        ),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        options = list(
          buttons = list(
            list(extend = 'excel', filename = "Scottish Exports"),
            list(extend = 'csv', filename = "Scottish Exports"),
            list(extend = 'pdf', filename = "Scottish Exports"),
            list(extend = 'copy', filename = "Scottish Exports"),
            list(extend = 'print', filename = "Scottish Exports")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 2)
        )
      ) %>%
        formatCurrency(columns = seq(1,ncol(TABLE_RUK)), currency = "", interval = 3, digits = 0, mark = ",") %>%
        formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
    })

    # TAB 1 - TABLE 7
    output$sector_table_all <- DT::renderDT({
      DT::datatable(
        TABLE_ALL,
        colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; color: black;",
          HTML(paste(
            "Source:", a(paste("Exports Statistics Scotland", Yr), href = "https://www.gov.scot/publications/export-stats-scotland-2018/", target = "_blank"), "</br>
              Notes:</br>
              1. Figures rounded to the nearest £5 million</br>
              2. Categories may not sum to total due to rounding
              "
          ))
        ),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend = 'excel', filename = "Scottish Exports"),
            list(extend = 'csv', filename = "Scottish Exports"),
            list(extend = 'pdf', filename = "Scottish Exports"),
            list(extend = 'copy', filename = "Scottish Exports"),
            list(extend = 'print', filename = "Scottish Exports")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 2)
        )
      )  %>%
        formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits = 0) %>%
        formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
        )
    })


    # TAB 2 - DESTINATION BREAKDOWNS ##################################################################################################################
    # TAB 2 - RENDER LEAFLET MAP
    output$BigMap <- renderLeaflet({
      data1 <- mapex@data %>%
        mutate(valueLabel = number(Value, accuracy = 1, scale = 1, big.mark = ",")) %>%
        mutate(popOverLabel = case_when(
          toupper(enc2utf8(NAME)) %in% toupper(enc2utf8(countriesLessFifty)) ~ paste0(NAME,": Available data for this country is not robust enough to be published (less than £50 million)."),
          ((!(toupper(enc2utf8(NAME)) %in% toupper(enc2utf8(countriesLessFifty)))) & (is.na(Value))) ~ paste0(NAME,": Scotland does not export to this country or we do not hold data on this country."),
          TRUE ~ paste0(NAME,": ", dollar(Value, accuracy = 1, scale = 1, prefix = "£", suffix = " million"))
        ))

      mapex@data <- data1


      leaflet(mapex) %>%
        setView(zoom = 1.5, lat = 20, lng = 20) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke = FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth(mapex@data$Value), fillOpacity = 1,
                    popup = ~paste(mapex@data$popOverLabel %>% as.character()),
                    highlightOptions = highlightOptions(color = "black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright",
                  pal = choropleth,
                  values = data1$Value,
                  title = paste("Exports in", as.character(Yr), "(£ million)"),
                  opacity = 1, labFormat = labelFormat(prefix = "£"),
                  na.label = "<£50 million or No Data Available"
                  )
    })
    # TAB 2 - BAR CHART
    output$CountryBarChart <- renderPlot({
      ## Creating Data for Chart ##
      data1 <- D %>%
        filter(Year == input$CountryBarChartSlide) %>%
        mutate(Rank = Rank * 120) %>%
        mutate(textLabel = paste(dollar(Total, prefix = "£", accuracy = 1, scale = 1), "million"))

      ## Creating Chart ##
      ggplot(data1) +
        geom_col(aes(x = Rank, y = Total), width = 100, fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = TRUE) + # Flip
        labs(x = "", y = "Value (£ million)",
             caption = paste("Source: Export Statistics Scotland", Yr,"\n",
                             "Estimates are rounded to the nearest £5 million.")
             ) + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank, y = -200, label = Country), hjust = 1) + # Names
        geom_text(aes(x = Rank, y = Total + 50, label = as.character(textLabel)), hjust = 0, color = "black") + # Values
        geom_flag(aes(x = Rank, y = -100,  country = Code), size = 10) + # Flags
        scale_y_continuous(labels = scales::comma, expand = expand_scale(add = c(0.6, 1000))) + # Format y-axis values
        scale_x_reverse(expand = expand_scale(add = c(-1,-1))) + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,3,"cm"),
          axis.text.y  = element_blank()
        )
    })
    # TAB 2 - BAR CHART CAPTION
    output$Top25Caption <- renderText({
      paste("Figure 2. Scotland's Top 25 Export Destinations in ", as.character(input$CountryBarChartSlide), " (£ million)", sep = "")
    })
    # TAB 2 - DOWNLOAD ANIMATION
    output$CountryAnimationBtn <- downloadHandler(
      filename <- function() {
        paste("CountryAnimation", "gif", sep = ".")
      },
      content <- function(file) {
        file.copy("./www/CountryAnimation.gif", file)
      }
    )

    # TAB 3 - COUNTRY PROFILES #######################################################################################################################
    # TAB 3 - MAP
    ## Scenario: User clicks on map to get location ##
    # Creating an object that will store click data
    data_of_click <- reactiveValues(clickedShape = NULL)
    # Colour palette
    binOptions <- c(50, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000)
    choropleth <- colorBin(palette = brewer.pal(n = 9, name = "YlOrRd"), no_NA$Value, bins = binOptions)
    # TAB 3 - RENDER LEAFLET MAP
    output$country_profile_map <- renderLeaflet({
      leaflet(mapex) %>%
        setView(zoom = 1.5, lat = 20, lng = 20) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke = FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth(mapex@data$Value), fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")) %>%
        addLegend("bottomright", pal = choropleth, values = mapex@data$Value, title = paste("Exports in ", as.character(Yr), " (£ million)", sep = ""),
                  opacity = 1, labFormat = labelFormat(prefix = "£"),
                  na.label = "<£50 million or No Data Available"
                  )
    })
    # Saving the click data
    observeEvent(input$country_profile_map_shape_click,{
      data_of_click$clickedShape <- input$country_profile_map_shape_click
    })

    ## Scenario: User uses drop down menu to get location ##
    countryDropDownResult <- eventReactive(input$countryProfileDropDown,{
      result <- input$countryProfileDropDown
      return(result)
    })

    ## Getting Final Answer from either Scenarios ##
    location <- reactiveValues(selected = NULL)

    observe({
      if (!is.null(data_of_click$clickedShape)) {
        location$selected <- data_of_click$clickedShape$id
          data_of_click$clickedShape <- NULL
      }
      if ((!is.null(input$countryProfileDropDown)) & (input$countryProfileDropDown != "")) {
        location$selected <- countryDropDownResult()

        updateSelectizeInput(inputId = "countryProfileDropDown", label = NULL,
                             session = session,
                             choices = countryList2, selected = NULL)
      }
    })

    # TAB 3 - COUNTRY PROFILE - RENDER ON PAGE
    observeEvent(location$selected,{
      if (location$selected %in% no_NA$NAME) {
        rmarkdown::render(
          input = "./CountryProfile.Rmd",
          output_format = "md_document",
          envir = new.env(parent = globalenv()),
          knit_root_dir = getwd(),
          encoding = "UTF-8",
          params = list(
            ClickedCountry = location$selected,
            StartingYear = St,
            CurrentYear = Yr

            )
        )
        output$repex <- renderUI({
          includeMarkdown("./CountryProfile.md")
        })
      } else {
        rmarkdown::render(
        input = "./NoDataMSG.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedCountry = location$selected,
                      StartingYear = St,
                      CurrentYear = Yr,
                      disclosive = case_when(
                        location$selected %in% countriesLessFifty ~ "YES",
                        !(location$selected %in% countriesLessFifty) ~ "NO"
                      )
                      )
      )
        output$repex <- renderUI({
          includeMarkdown("./NoDataMSG.md")
        })
      }
    })

    # TAB 3 - COUNTRY PROFILE - DOWNLOAD
    output$CountryBtn <- downloadHandler(
      filename = function() {
        paste(as.character(location$selected), sep = ".",
              switch(input$FileFormatCountry, PDF = "pdf", HTML = "html", Word = "docx"))
      },
      content = function(file) {
        if (location$selected %in% no_NA$NAME) {
          # Knit the document.
          ReportRend <- rmarkdown::render(
            input = "./CountryProfile.Rmd",
            encoding = "UTF-8",
            output_format = switch(input$FileFormatCountry, PDF = "pdf_document", HTML = "html_document", Word = "word_document"),
            params = list(
              ClickedCountry = location$selected,
              StartingYear = St,
              CurrentYear = Yr,
              disclosive = case_when(
              location$selected %in% countriesLessFifty ~ "YES",
              !(location$selected %in% countriesLessFifty) ~ "NO"
            )),
            envir = new.env(parent = globalenv())
          )
          file.rename(ReportRend, file)
        } else {
          # Knit the document.
          ReportRend <- rmarkdown::render(
            input = "./NoDataMSG.Rmd",
            encoding = "UTF-8",
            output_format = switch(input$FileFormatCountry, PDF = "pdf_document", HTML = "html_document", Word = "word_document"),
            params = list(
              ClickedCountry = location$selected,
              StartingYear = St,
              CurrentYear = Yr,
              disclosive = case_when(
                location$selected %in% countriesLessFifty ~ "YES",
                !(location$selected %in% countriesLessFifty) ~ "NO"
              )),
            envir = new.env(parent = globalenv())
          )
          file.rename(ReportRend, file)
        }





      }
    )

    # TAB 4 - SECTOR BREAKDOWNS #######################################################################################################################
    # TAB 4 - SANKEY CAPTION
    output$SankeyCaption <- renderText({
      paste("Figure 1. Sankey Diagram Representing the Breakdown of Scotland's Exports by Sector and Subsector in ", as.character(Yr), " (£ million), ",
            if (input$PickedSankeyData == "SANKEY_ALL") {
              "All Destinations"
            } else if (input$PickedSankeyData == "SANKEY_INT") {
              "Internationally"
            } else {
              "Rest of the UK"},
            sep = "")
    })

    # TAB 4 - SANKEY DIAGRAM
    output$SectorSankey <- renderSankeyNetwork({
      # CHOOSING DATA SET
      SANKEY <- read.csv(paste("./Base Datatables/", as.character(input$PickedSankeyData), ".csv", sep = ""))

      # DATA MANIPULATUION
      links =  SANKEY %>%
        select(Export, Total, Sector, Subsector, Value) %>%
        rename(
          "source" = Export,
          "value" = Total,
          "source2" = Sector,
          "value2" = Value,
          "target" = Subsector
        ) %>%
        as.data.frame()

      nodes = data.frame(name = c(as.character(links$source), as.character(links$source2), as.character(links$target)) %>% unique())

      links$IDsource = match(links$source, nodes$name) - 1
      links$IDsource2 = match(links$source2, nodes$name) - 1
      links$IDtarget = match(links$target, nodes$name) - 1
      ?expand_scale
      links1 <- links %>%
        select(IDsource, IDsource2, value) %>%
        rename("source" = IDsource, "target" = IDsource2) %>%
        distinct() %>%
        as.data.frame()

      links2 <- links %>%
        select(IDsource2, IDtarget, value2) %>%
        rename("source" = IDsource2, "target" = IDtarget, "value" = value2) %>%
        distinct() %>%
        as.data.frame()

      links <- rbind(links1,links2) %>% as.data.frame()

      # CREATING THE SANKEY NETWORK
      sankey1 <- networkD3::sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "source"  ,
        Target = "target",
        Value = "value",
        NodeID = "name",
        sinksRight = TRUE,
        fontSize = 12,
        margin = 0,
        nodePadding = 10
      )

      # CREATING DIFFERENT TOOLTIP FOR SANKEY DIAGRAM
      # Nodes
      sankey2 <- htmlwidgets::onRender(
        sankey1,
          'function(el, x) {
          d3.selectAll(".node").select("title foreignObject body pre")
          .text(function(d) {
          var Value1 = d.value.toLocaleString("en");

          return d.name + " £" + Value1 + " million";
          });
        }
        '
      )

      # Links
      sankey2 <-  htmlwidgets::onRender(
        sankey2,
        'function(el, x) {
          d3.selectAll(".link").select("title foreignObject body pre")
          .text(function(d) {
          var Value1 = d.value.toLocaleString("en");

          return  d.source.name + " \U2192 " + d.target.name + " £" + Value1 + " million";
          });
        }
        '
      )
      return(sankey2)
    })

    # Responding to Change in top radio button
    observeEvent(input$PickedSankeyData, {
      selected <- case_when(
        input$PickedSankeyData == "SANKEY_ALL" ~ "All",
        input$PickedSankeyData == "SANKEY_INT" ~ "International",
        input$PickedSankeyData == "SANKEY_RUK" ~ "RUK"

      )


      updateRadioButtons(session = session,
                         inputId = "PickedSectorBarChartData",
                         choiceNames = c("All Destinations", "International", "Rest of the UK"),
                         choiceValues = c("All", "International", "RUK"),
                         selected = selected,
                         inline = TRUE
      )
    })
?updateRadioButtons
    # TAB 4 - BAR CHART CAPTION
    output$Top25CaptionSector <- renderText({
      year1 <- input$SectorBarChartSlide

      result <- paste0("Figure 2. Scotland's Top Exporting Sectors in ", as.character(year1), " (£ million), ",
                      case_when(
                        input$PickedSectorBarChartData == "All" ~ "All Destinations",
                        input$PickedSectorBarChartData == "International" ~ "Internationally",
                        TRUE ~ "Rest of the UK"
                      ),
            sep = "")
      return(result)
    })

    # TAB 4 - BAR CHART
    output$SectorBarChart <- renderPlot({
      # CHOOSING DATA SET
      SECTORS <- read.csv("./Base Datatables/SECTORS.csv")

      area_want <- case_when(
        input$PickedSectorBarChartData == "All" ~ "All",
        input$PickedSectorBarChartData == "International" ~ "International",
        input$PickedSectorBarChartData == "RUK" ~ "RUK"
      ) %>% as.character()

      rank_want <- case_when(
        input$PickedSectorBarChartData == "All" ~ "RankAll",
        input$PickedSectorBarChartData == "International" ~ "Rank_int",
        input$PickedSectorBarChartData == "RUK" ~ "Rank_ruk"
      ) %>% as.character()

      DD <- SECTORS %>%
        mutate(Value = SECTORS[,area_want]) %>%
        mutate(Rank = SECTORS[,rank_want]) %>%
        group_by(Year) %>%
        top_n(n = 25, wt = Value) %>%
        mutate(Rank = Rank * 120) %>%
        ungroup() %>%
        mutate(textHover = paste(dollar(Value, accuracy = 1, scale = 1, prefix = "£"), "million"))

      # RENDERING BAR CHART
      ggplot(subset(DD, DD$Year == input$SectorBarChartSlide)) +
        geom_col(aes(x = Rank, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = TRUE) + # Flip
        labs(x = "", y = "Value (£ million)",
             caption = paste("Source: Export Statistics Scotland", Yr,"\n",
                             "Estimates are rounded to the nearest £5 million.")
             ) + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank, y = -100, label = Sector), hjust = 1) + # Names
        geom_text(aes(x = Rank, y = Value + 50, label = as.character(textHover)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma,expand = expand_scale(add = c(10, 2000))) + # Format y-axis values
        scale_x_reverse(expand = expand_scale(add = -1)) + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })

    # TAB 4 - DOWNLOAD ANIMATION
    output$SectorAnimationBtn <- downloadHandler(
      filename <- function() {
        paste("SectorAnimation",as.character(input$PickedSectorBarChartData),".gif", sep = "")
      },
      content <- function(file) {
        file.copy(paste("./www/","SectorAnimation",as.character(input$PickedSectorBarChartData),".gif", sep = ""), file)
      }
    )

    # TAB 5 - SECTOR PROFILES #########################################################################################################################

    sector1 <- reactiveValues(selected = NULL)

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnChemical,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Chemical Sciences", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Chemical Sciences"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnConstruction,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Construction", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Construction"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnEducation,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Education", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Education"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnEngineering,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Engineering and Advanced Manufacturing", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Engineering and Advanced Manufacturing"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnEnergy,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Energy", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Energy"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnFinancial,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Financial and Business Services", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Financial and Business Services"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnFood,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Food and Drink", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Food and Drink"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnForest,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Forest and Timber Technologies", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Forest and Timber Technologies"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnLife,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Life Sciences", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Life Sciences"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnOther,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Other Services and Accommodation", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Other Services and Accommodation"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnConstruction,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Construction", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Construction"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnReal,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Real Estate", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Real Estate"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnSustainable,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Sustainable Tourism", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Sustainable Tourism"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnTechnology,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Technology, Digital and Media", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Technology, Digital and Media"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnTextiles,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Textiles", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Textiles"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnTransportation,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Transportation and Storage", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Transportation and Storage"
    })

    # TAB 5 - SECTOR REPORT - RENDER ON PAGE
    observeEvent(input$SectBtnWholesale,{
      rmarkdown::render(
        input = "./SectorProfile.Rmd",
        output_format = "md_document",
        encoding = "UTF-8",
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd(),
        params = list(ClickedSector = "Wholesale and Retail Trade", StartingYear = St, CurrentYear = Yr)
      )
      output$RenderSectorReport <- renderUI({
        includeMarkdown("./SectorProfile.md")
      })
      sector1$selected <- "Wholesale and Retail Trade"
    })

    # TAB 5 - SECTOR REPORT - DOWNLOAD
    output$SectorBtn <- downloadHandler(
      filename = function() {
        paste(as.character(sector1$selected), sep=".", switch(input$FileFormatSector, PDF = "pdf", HTML = "html", Word = "docx"))
      },
      content = function(file) {
        # Knit the document.
        RReportRend <- rmarkdown::render(
          input = "./SectorProfile.Rmd",
          encoding = "UTF-8",
          knit_root_dir = getwd(),
          output_format = switch(input$FileFormatSector, PDF = "pdf_document", HTML = "html_document", Word = "word_document"),
          params = list(ClickedSector = sector1$selected, StartingYear = St, CurrentYear = Yr),
          envir = new.env(parent = globalenv())
        )
        file.rename(RReportRend, file)
      }
    )


  } # ... server ends here.
)
