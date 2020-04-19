#####  Covid19 Dashboard by Manfred W. Baumstark 

This dashboard aims to display diagrams that I was missing in other dashboards. 

As data sources repositories from Johns Hopkins, and RKI are used. 

To run the dashboard some R-packages are required, that can be installed with the commands below:

    install.packages(shiny)
    install.packages(shinydashboard)
    install.packages(shinyjs)
    install.packages(readr)
    install.packages(lubridate)
    install.packages(reshape2)
    install.packages(ggplot2)
    install.packages(dplyr)
    install.packages(R0)

Then simply run from R or RStudio with:

    library(shiny)
    runGitHub( "Covid19", "mwbaumstark")

or checkout and run from a local dir.



References:

Johns Hopkins data incl. population: https://datahub.io/core/covid-19
Upstream repository of Johns Hopkins data: https://github.com/CSSEGISandData/COVID-19

RKI data: https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv

German population data: https://www.destatis.de/ Dataset 12411-0013.csv

Reproduction number: 
https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2020/Ausgaben/17_20_SARS-CoV2_vorab.pdf?__blob=publicationFile
https://staff.math.su.se/hoehle/blog/2020/04/15/effectiveR0.html


R: https://cran.r-project.org/

RStudio: https://rstudio.com/products/rstudio/
