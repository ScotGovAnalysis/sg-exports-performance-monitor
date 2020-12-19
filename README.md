# Exports Performance Monitor
Code behind the Scottish Government's application for display of exports data titled "Exports Performance Monitor". The highlight of the code is the original solution to generate downloadable reports, where R markdown document is used to render the report on the app as well as for rendering the document in the form of PDF and Word document. Additionally, the repository includes code used for generating gif animations to produce a rank bar chart.

![Image of the home page](https://github.com/DataScienceScotland/sg-exports-performance-monitor/blob/master/exports.png?raw=true)

## 📦 Packages
- library(shiny)                # For the app iteslf.
- library(shinythemes)          # For the "cerulean" theme.
- library(shinyhelper)          # Used for help modal boxes.
- library(shinyEffects)         # Used for home page effects.
- library(shinyanimate)         # Used for home page effects.
- library(shinycssloaders)      # For loaders.
- library(magrittr)             # Dependency of other packages. Lets use the pipe operator (%>%).
- library(data.table)           # Needed for function na.omit().
- library(DT)                   # For the interactive tables.
- library(dygraphs)             # For the interactive graphs.
- library(leaflet)              # For interactive maps.
- library(RColorBrewer)         # Used to create a colour palette for the map.
- library(rgdal)                # Needed to load shapfile for the map.
- library(plyr)                 # Needed for function revalue(), for editing country names.
- library(rmarkdown)            # Used for reports, especially important is function render().
- library(knitr)                # Takes Rmd. file and turns it into .md document which includes the R code and it’s output.
- library(networkD3)            # Used to create the sankey diagram.
- library(treemap)              # Used to create the static treemap.
- library(dplyr)                # Used for data manipulation.
- library(ggplot2)              # Used for plots in country and sector profiles.
- library(plotly)               # Used for the streamgraph.
- library(ggsci)                # Used for colour palettes.
- library(ggflags)              # Expands ggplot with new geom for adding flags.
- library(countrycode)          # Enables conversion from Common country names to ISO codes.
- library(knitr)                # Used for sector definitions table
- library(kableExtra)           # Used for styling the sector definitions table

## 🔗 Links
* [Exports Performance Monitor (This shiny app)](https://scotland.shinyapps.io/sg-exports-performance-monitor/)
* [Scotland's export performance monitor (Excel spreadsheet)](https://www.gov.scot/publications/scotlands-export-performance-monitor/)

## 📧 Contact
[![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&style=social&url=https%3A%2F%2Ftwitter.com%2FSzymkowskiDev)](https://twitter.com/SzymkowskiDev) [![](https://img.shields.io/twitter/url?label=/kamil-szymkowski/&logo=linkedin&logoColor=%230077B5&style=social&url=https%3A%2F%2Fwww.linkedin.com%2Fin%2Fkamil-szymkowski%2F)](https://www.linkedin.com/in/kamil-szymkowski/) [![](https://img.shields.io/twitter/url?label=@szymkowskidev&logo=medium&logoColor=%23292929&style=social&url=https%3A%2F%2Fmedium.com%2F%40szymkowskidev)](https://medium.com/@szymkowskidev) [![](https://img.shields.io/twitter/url?label=/SzymkowskiDev&logo=github&logoColor=%23292929&style=social&url=https%3A%2F%2Fgithub.com%2FSzymkowskiDev)](https://github.com/SzymkowskiDev)






