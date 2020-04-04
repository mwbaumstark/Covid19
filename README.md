#####  Covid19 Dashboard by Manfred W. Baumstark 

This dashboard aims to display diagrams that I was missing in other dashboards. 

As data sources repositories from Johns Hopkins, ECDC, and RKI can be used. 

To run the dashboard some R-packages are required, that can be installed with the commands below:

    require2(shiny)
    require2(shinydashboard)
    require2(shinyjs)
    require2(readr)
    require2(lubridate)
    require2(reshape2)
    require2(ggplot2)

Then simply run from R or RStudio with:

    library(shiny)
    runGitHub( "Covid19", "mwbaumstark")

or checkout and run from a local dir.


References:

Johns Hopkins data: https://github.com/CSSEGISandData/COVID-19

ECDC data: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

RKI data: https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv

R: https://cran.r-project.org/

RStudio: https://rstudio.com/products/rstudio/
