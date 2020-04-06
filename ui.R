library(shiny)
library(shinyjs)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Covid19 Data"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem("Show Cases", 
               icon = icon("atlas"),
               tabName = "wwd1"
      ),
      radioButtons("cases", NULL, choices = ctype, selected = ctype[1]),
      menuItem("Other Data", 
               icon = icon("bar-chart"),
               startExpanded = FALSE,
               menuSubItem("Deaths / Confirmed Case", tabName = "wwd5"),
               menuSubItem("RKI, Altersverteilung", tabName = "rki2")
      )
      
    ),
    radioButtons("repo", "Repository:",
                 choices = c("Johns Hopkins" 
                             , "ECDC"
                             , "RKI (Germany)"
                             # , "J. Hopkins Web" 
                             , "RKI (Bundesländer)"
                 ),
                 selected = "Johns Hopkins"),
    
    selectizeInput("show_c", "Select Countries to show:", choices = countries, 
                   selected = c("Germany", "Switzerland"),
                   multiple = TRUE),
    
    checkboxInput("normalize", label = "Cases per Million Inhabitants", value = TRUE),
    
    
    dateInput("startd", "From Date:", value = "2020-03-01", min = min_date, max = max_date,
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    
    dateInput("stopd", "To Date:", value = max_date, min = min_date, max = max_date,
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    
    collapsed = FALSE  
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wwd1",
              fluidRow(
                box(width = 4, plotOutput('Plot1', click = "plot_click")),
                box(width = 4, plotOutput('Plot2')),
                box(width = 4, plotOutput('Plot3'))
              ),
              fluidRow(
                column(width = 4,
                       radioButtons("yaxt", "y-axis:",
                                    choices = c("linear", "logarithmic"), selected = "logarithmic", 
                                    inline = TRUE),
                       
                       radioButtons("yafit", "Fit:",
                                    choices = c("exponential","no fit"), selected = "exponential", 
                                    inline = TRUE) 
                ),
                column(width = 4,
                       radioButtons("rfit", "Fit:",
                                    choices = c("constant", "loess", "no fit"), selected = "constant", 
                                    inline = TRUE)
                ),
                column(width = 4,
                       tableOutput('table1')                )
              ) ,
              fluidRow(
                column(width = 3,
                       verbatimTextOutput("info1")),
                column(width = 1,
                       actionButton("reset", "Reset"))
              )
      ),
      tabItem(tabName = "wwd5",
              fluidRow(
                box(width = 5, plotOutput('Plot99'))
              ),
      ),
      tabItem(tabName = "rki2",
              fluidRow(
                box(width = 4, plotOutput('AvFaelle')),  
                box(width = 4, plotOutput('AvTodesFaelle')),
                box(width = 4, plotOutput('CFR')) 
              ),
              fluidRow(
                column(width = 4,
                       verbatimTextOutput("selinfo")
                )
              )
      )
    )
  )
)
