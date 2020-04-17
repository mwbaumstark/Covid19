library(shiny)
library(shinyjs)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Covid-19 Data"),
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
               tabName = "other",
               startExpanded = FALSE,
               
               menuSubItem("Eff. reproduction number", tabName = "wwd4"),
               menuSubItem("Deaths / Confirmed Case", tabName = "wwd5"),
               menuSubItem("RKI, Altersverteilung", tabName = "rki2"),
               menuSubItem("Links", tabName = "links")
      )
      
    ),

    selectizeInput("show_c", 
                   "Select Countries/Region to show (including Bundesländer and Germany (RKI):", 
                   choices = countries, 
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
                                    choices = c("linear", "logarithmic"), 
                                    selected = "logarithmic", 
                                    inline = TRUE),
                       
                       radioButtons("yafit", "Fit:",
                                    choices = c("exponential", "loess", "no fit"), 
                                    selected = "loess", 
                                    inline = TRUE) 
                ),
                column(width = 4,
                       radioButtons("show_2", "Show:",
                                    choices = c("Doubling period", "Daily rate"), 
                                    selected = "Doubling period", 
                                    inline = TRUE),
                       radioButtons("rfit", "Fit:",
                                    choices = c("constant", "loess", "no fit"), 
                                    selected = "loess", 
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
              fluidRow(
                column(width = 4,
                       sliderInput("delay", "Deaths per Cases N ago", min = 0, max = 30, value = 15 )
                )
              )
      ),
      tabItem(tabName = "rki2",
              fluidRow(
                box(width = 4, plotOutput('AvFaelle')),  
                box(width = 4, plotOutput('AvTodesFaelle')),
                box(width = 4, plotOutput('CFR')) 
              ),
              fluidRow(
                column(width = 4,
                       radioButtons("rki_cases", NULL, choices = ctype, selected = ctype[1]),
                       selectizeInput("rki_show_c", 
                                      "Select 'Germany (RKI)' or one 'Bundesland'", choices = rki_countries, 
                                      selected = c("Germany (RKI)"),
                                      multiple = FALSE),
                )
              )
      ), 
      tabItem(tabName = "links",
              fluidRow(
                column(width = 4,
                       uiOutput("link1"),
                       uiOutput("link2"),
                       uiOutput("link3"),
                       uiOutput("link4"),
                       uiOutput("link5"),
                       uiOutput("link7")
                )
              )
      )
      
    )
  )
)
