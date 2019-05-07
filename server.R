# Loading packacges and data from a separate R file
source("data.R")

# SERVER
shinyServer(
    function(input, output, session) {
      
# ENTIRE PAGE ####################################################################################################################################
    observe_helpers()
      
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
          kable_styling(bootstrap_options = c("striped", "bordered"), fixed_thead = T)
          
      )
    })
    
    
# TAB 1 - TIME SERIES ############################################################################################################################
        # TAB 1 - FIGURE 1
        output$sector_graph <- renderDygraph({
            dygraph(
                GRAPHS[,c(1, as.numeric(input$select_sector_all), as.numeric(input$select_sector_int), as.numeric(input$select_sector_ruk), as.numeric(input$select_subsector_all), as.numeric(input$select_subsector_int), as.numeric(input$select_subsector_ruk))]
            ) %>%
                dyGroup(names(GRAPHS)[c(1, as.numeric(input$select_sector_all), as.numeric(input$select_sector_int), as.numeric(input$select_sector_ruk), as.numeric(input$select_subsector_all), as.numeric(input$select_subsector_int), as.numeric(input$select_subsector_ruk))], strokeWidth = 4) %>%
                dyRangeSelector() %>%
                dyAxis("x", label = "Year", rangePad = 5) %>%
                dyAxis("y", label = "Value (£ million)") %>%
                dyHighlight(
                    highlightCircleSize = 5,
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = FALSE,
                    highlightSeriesOpts = list(strokeWidth = 6)
                ) %>%
                dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 0) %>%
                dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = TRUE) %>%
                dyUnzoom() %>%
                dyCrosshair(direction = "vertical")
        })

        # TAB 1 - TABLE 2
        output$table_for_graph <- DT::renderDT({
            DT::datatable(
                GRAPHS[,c(1, as.numeric(input$select_sector_all), as.numeric(input$select_sector_int), as.numeric(input$select_sector_ruk), as.numeric(input$select_subsector_all), as.numeric(input$select_subsector_int), as.numeric(input$select_subsector_ruk))],
                extensions = "Buttons",
                options = list(
                    dom = 'frtBip',
                    buttons = list(
                        list(extend='excel', filename="Scottish Exports"),
                        list(extend='csv', filename="Scottish Exports"),
                        list(extend='pdf', filename="Scottish Exports"),
                        list(extend='copy', filename="Scottish Exports"),
                        list(extend='print', filename="Scottish Exports")
                    ),
                    scrollX = TRUE,
                    paging = FALSE,
                    searching = FALSE
                    
                ), rownames= FALSE
            ) %>%
            formatCurrency(columns = seq(2, ncol(GRAPHS)), currency = "", interval = 3, digits=0, mark = ",") 
        })

        # TAB 1 - TABLE 3
        output$sector_table_all <- DT::renderDT({
            DT::datatable(
                TABLE_ALL,
                colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
                extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
                rownames = FALSE,
                options = list(
                    buttons = list(
                        list(extend='excel', filename="Scottish Exports"),
                        list(extend='csv', filename="Scottish Exports"),
                        list(extend='pdf', filename="Scottish Exports"),
                        list(extend='copy', filename="Scottish Exports"),
                        list(extend='print', filename="Scottish Exports")
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
              formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
              formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
          )
        })

        # TAB 1 - TABLE 4
        output$sector_table_int <- DT::renderDT({
            DT::datatable(
                TABLE_INT,
                colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
                rownames = FALSE,
                extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
                options = list(
                    buttons = list(
                        list(extend='excel', filename="Scottish Exports"),
                        list(extend='csv', filename="Scottish Exports"),
                        list(extend='pdf', filename="Scottish Exports"),
                        list(extend='copy', filename="Scottish Exports"),
                        list(extend='print', filename="Scottish Exports")
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
            formatCurrency(columns = seq(1,ncol(TABLE_INT)), currency = "", interval = 3, digits=0, mark = ",") %>% 
            formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
        })

        # TAB 1 - TABLE 5
        output$sector_table_ruk <- DT::renderDT({
            DT::datatable(
                TABLE_RUK,
                colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
                rownames = FALSE,
                extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
                options = list(
                    buttons = list(
                        list(extend='excel', filename="Scottish Exports"),
                        list(extend='csv', filename="Scottish Exports"),
                        list(extend='pdf', filename="Scottish Exports"),
                        list(extend='copy', filename="Scottish Exports"),
                        list(extend='print', filename="Scottish Exports")
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
            formatCurrency(columns = seq(1,ncol(TABLE_RUK)), currency = "", interval = 3, digits=0, mark = ",") %>% 
            formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold")))
        })
  

# TAB 2 - DESTINATION BREAKDOWNS ##################################################################################################################        
        # TAB 2 - RENDER LEAFLET MAP
        output$BigMap <- renderLeaflet({
          leaflet(mapex) %>%
            setView(zoom = 1.5, lat = 20, lng= 20) %>%
            addProviderTiles("Esri.WorldGrayCanvas") %>%
            addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth(mapex@data$Value), fillOpacity=1, popup = ~paste(as.character(mapex@data$NAME), " £", as.character(mapex@data$Value), " million", sep = ""),
              highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
            ) %>%
            addLegend("bottomright", pal = choropleth, values = no_NA$Value, title = paste("Exports in ", as.character(Yr), " (£ million)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
        })
        # TAB 2 - BAR CHART
        output$CountryBarChart <- renderPlot({
          ggplot(subset(D, D$Year == input$CountryBarChartSlide)) + #, aes(x = Rank, group = Country, country = as.factor(Code)
            geom_col(aes(x = Rank*120, y = Total), width = 100, fill = "azure3", color = "black") + # Columns
            coord_flip(clip = "off", expand = FALSE) + # Flip
            labs(x = "", y = "Value (£ million)") + # Labels
            theme_minimal() + # Theme
            geom_text(aes(x = Rank*120, y = -200, label = Country), hjust = 1) + # Names
            geom_text(aes(x = Rank*120, y = Total + 50, label = as.character(Total)), hjust = 0, color = "black") + # Values  
            geom_flag(aes(x = Rank*120, y = -100,  country = Code), size = 10) + # Flags
            scale_y_continuous(labels = scales::comma) + # Format y-axis values
            scale_x_reverse() + # Highest values on top
            theme(
              plot.margin = margin(0,2,0,3,"cm"),
              axis.text.y  = element_blank()
            )
        })
        # TAB 2 - BAR CHART CAPTION
        output$Top25Caption <- renderText({
          paste("Figure 2. Scotland's Top 25 Export Destinations in ", as.character(input$CountryBarChartSlide), " (£ million)", sep="")
        })
        # TAB 2 - DOWNLOAD ANIMATION
        output$CountryAnimationBtn <- downloadHandler(
          filename <- function() {
            paste("CountryAnimation", "gif", sep=".")
          },
          content <- function(file) {
            file.copy("./www/CountryAnimation.gif", file)
          }
        )
        
# TAB 3 - COUNTRY PROFILES #######################################################################################################################
        # TAB 3 - MAP  
        # Creating an object that will store click data
        data_of_click <- reactiveValues(clickedShape=NULL)
        # Colour palette
        choropleth <- colorBin(palette=brewer.pal(n=9, name="YlOrRd"), no_NA$Value, bins = 9)        
        # TAB 3 - RENDER LEAFLET MAP
        output$country_profile_map <- renderLeaflet({
            leaflet(mapex) %>%
                setView(zoom = 1.5, lat = 20, lng= 20) %>%
                addProviderTiles("Esri.WorldGrayCanvas") %>%
                addPolygons(stroke=FALSE, layerId = ~mapex@data$NAME, fillColor = ~choropleth(mapex@data$Value), fillOpacity=1,
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")) %>%
                addLegend("bottomright", pal = choropleth, values = no_NA$Value, title = paste("Exports in ", as.character(Yr), " (£ million)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
        })
        # Saving the click data
        observeEvent(input$country_profile_map_shape_click,{
            data_of_click$clickedShape <- input$country_profile_map_shape_click
        })

        # TAB 3 - COUNTRY PROFILE - RENDER ON PAGE
        observeEvent(input$country_profile_map_shape_click,{
          if(data_of_click$clickedShape$id %in% no_NA$NAME){
          rmarkdown::render(
            input = "./www/CountryProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedCountry = data_of_click$clickedShape$id, StartingYear = St, CurrentYear = Yr)
          )
          output$repex <- renderUI({
              includeMarkdown("./www/CountryProfile.md")
            })}else{rmarkdown::render(
              input = "./www/NoDataMSG.Rmd",
              output_format = "md_document",
              envir = new.env(parent = globalenv()),
              params = list(ClickedCountry = data_of_click$clickedShape$id, StartingYear = St, CurrentYear = Yr)
            )
            output$repex <- renderUI({
              includeMarkdown("./www/NoDataMSG.md")
            })
          }
        })
        
        # TAB 3 - COUNTRY PROFILE - DOWNLOAD  
        output$CountryBtn <- downloadHandler(
          filename = function() {
            paste(as.character(data_of_click$clickedShape$id), sep=".", switch(input$FileFormatCountry, PDF="pdf", HTML="html", Word="docx"))
            },
          content = function(file) {
            # Knit the document.
            ReportRend <- rmarkdown::render(
              input = "./www/CountryProfile.Rmd",
              output_format = switch(input$FileFormatCountry, PDF = "pdf_document", HTML = "html_document", Word = "word_document"),
              params = list(ClickedCountry = data_of_click$clickedShape$id, StartingYear = St, CurrentYear = Yr),
              envir = new.env(parent = globalenv())
            )
            file.rename(ReportRend, file)
          }
        )

# TAB 4 - SECTOR BREAKDOWNS #######################################################################################################################
        # TAB 4 - SANKEY CAPTION
        output$SankeyCaption <- renderText({
            paste("Figure 1. Sankey Diagram Representing the Breakdown of Scotland's Exports by Sector and Subsector in ", as.character(Yr), " (£ million), ", 
              if(input$PickedSankeyData == "SANKEY_ALL"){"All Destinations"}else if(input$PickedSankeyData == "SANKEY_INT"){"Internationally"}else{"Rest of the UK"}, sep="")
        })
        
        # TAB 4 - SANKEY DIAGRAM
        output$SectorSankey <- renderSankeyNetwork({
          # CHOOSING DATA SET
          SANKEY <- read.csv(paste("./www/", as.character(input$PickedSankeyData), ".csv", sep=""))
          # DATA MANIPULATUION  
          links = data.frame(source = SANKEY$Export, value = SANKEY$Total, source2 = SANKEY$Sector, target = SANKEY$Subsector, value2 = SANKEY$Value)
          nodes = data.frame(name = c(as.character(links$source), as.character(links$source2), as.character(links$target)) %>% unique())
          links$IDsource = match(links$source, nodes$name) - 1
          links$IDsource2 = match(links$source2, nodes$name) - 1
          links$IDtarget = match(links$target, nodes$name) - 1
          links1 <- unique(data.frame(source = links$IDsource, target = links$IDsource2, value = links$value))
          links2 <- data.frame(source = links$IDsource2, target = links$IDtarget, value = links$value2)
          links <- rbind(links1,links2)
          # CREATING THE SANKEY NETWORK
          networkD3::sankeyNetwork(
            Links = links,
            Nodes = nodes,
            Source = "source"  ,
            Target = "target",
            Value = "value",
            NodeID = "name",
            sinksRight = FALSE,
            fontSize = 12,
            margin = 0,
            nodePadding = 10
          )
        })
        
        # TAB 4 - BAR CHART CAPTION
        output$Top25CaptionSector <- renderText({
          paste("Figure 2. Scotland's Top 25 Exporting Sectors in ", as.character(Yr), " (£ million), ", 
            if(input$PickedSectorBarChartData == "All"){"All Destinations"}else if(input$PickedSectorBarChartData == "International"){"Internationally"}else{"RUK"}, sep="")
        })
        
        # TAB 4 - BAR CHART
        output$SectorBarChart <- renderPlot({
          # CHOOSING DATA SET
          SECTORS <- read.csv("./www/SECTORS.csv")
          DD <- SECTORS %>%
            mutate(Value = SECTORS[,c(as.character(input$PickedSectorBarChartData))]) %>%
            mutate(Rank = SECTORS[,c(paste("Rank",as.character(input$PickedSectorBarChartData),sep=""))]) %>%
            group_by(Year) %>%
            filter() %>%
            top_n(n = 25, wt = Value) %>%
            mutate(Rank = Rank * 120) %>%
            ungroup()
          # RENDERING BAR CHART
          ggplot(subset(DD, DD$Year == input$SectorBarChartSlide)) + #, aes(x = Rank, group = Country, country = as.factor(Code)
            geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
            coord_flip(clip = "off", expand = FALSE) + # Flip
            labs(x = "", y = "Value (£ million)") + # Labels
            theme_minimal() + # Theme
            geom_text(aes(x = Rank*120, y = -100, label = Sector), hjust = 1) + # Names
            geom_text(aes(x = Rank*120, y = Value + 50, label = as.character(Value)), hjust = 0, color = "black") + # Values
            scale_y_continuous(labels = scales::comma) + # Format y-axis values
            scale_x_reverse() + # Highest values on top
            theme(
              plot.margin = margin(0,2,0,7,"cm"),
              axis.text.y  = element_blank()
            )
        })
        
        # TAB 4 - DOWNLOAD ANIMATION
        output$SectorAnimationBtn <- downloadHandler(
          filename <- function() {
            paste("SectorAnimation",as.character(input$PickedSectorBarChartData),".gif", sep="")
          },
          content <- function(file) {
            file.copy(paste("./www/","SectorAnimation",as.character(input$PickedSectorBarChartData),".gif", sep=""), file)
          }
        )
        
# TAB 5 - SECTOR PROFILES #########################################################################################################################
        
        Klikens <- reactiveValues(Dekens=NULL)
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnChemical,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Chemical Sciences", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Chemical Sciences"
        })

        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnConstruction,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Construction", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Construction"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnEducation,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Education", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Education"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnEngineering,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Engineering and Advanced Manufacturing", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Engineering and Advanced Manufacturing"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnEnergy,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Energy", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Energy"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnFinancial,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Financial and Business Services", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Financial and Business Services"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnFood,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Food and Drink", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Food and Drink"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnForest,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Forest and Timber Technologies", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Forest and Timber Technologies"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnLife,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Life Sciences", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Life Sciences"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnOther,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Other Services and Accommodation", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Other Services and Accommodation"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnConstruction,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Construction", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Construction"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnReal,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Real Estate", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Real Estate"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnSustainable,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Sustainable Tourism", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Sustainable Tourism"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnTechnology,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Technology, Digital and Media", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Technology, Digital and Media"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnTextiles,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Textiles", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Textiles"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnTransportation,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Transportation and Storage", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Transportation and Storage"
        })
        
        # TAB 5 - SECTOR REPORT - RENDER ON PAGE
        observeEvent(input$SectBtnWholesale,{
          rmarkdown::render(
            input = "./www/SectorProfile.Rmd",
            output_format = "md_document",
            envir = new.env(parent = globalenv()),
            params = list(ClickedSector = "Wholesale and Retail Trade", StartingYear = St, CurrentYear = Yr)
          )
          output$RenderSectorReport <- renderUI({
            includeMarkdown("./www/SectorProfile.md")
          })
          Klikens$Dekens <- "Wholesale and Retail Trade"
        })
        
        # TAB 5 - SECTOR REPORT - DOWNLOAD  
        output$SectorBtn <- downloadHandler(
          filename = function() {
            paste(as.character(Klikens$Dekens), sep=".", switch(input$FileFormatSector, PDF="pdf", HTML="html", Word="docx"))
          },
          content = function(file) {
            # Knit the document.
            RReportRend <- rmarkdown::render(
              input = "./www/SectorProfile.Rmd",
              output_format = switch(input$FileFormatSector, PDF = "pdf_document", HTML = "html_document", Word = "word_document"),
              params = list(ClickedSector = Klikens$Dekens, StartingYear = St, CurrentYear = Yr),
              envir = new.env(parent = globalenv())
            )
            file.rename(RReportRend, file)
          }
        )        
        
                
    } # ... server ends here.
)