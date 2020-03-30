library(shiny)
library(shinyjs)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Covid19 Data"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem("Worldwide Data", icon = icon("atlas"), startExpanded = TRUE,
               menuSubItem(ctype[1], tabName = "wwd1"),
               menuSubItem(ctype[2], tabName = "wwd2"),
               menuSubItem(ctype[3], tabName = "wwd3"),
               menuSubItem(ctype[4], tabName = "wwd4"),
               menuSubItem(ctype[5], tabName = "wwd5")
      ),
      # menuItem("RKI, Bundesländer", tabName = "rki1", icon = icon("dashboard")
      # ),
      menuItem("RKI, Altersverteilung", tabName = "rki2", icon = icon("bar-chart")
      )
      
    ),
    
    radioButtons("repo", "Repository:",
                 choices = c("Johns Hopkins" 
                             , "ECDC"
                             , "RKI (Germany)"
                             # , "J. Hopkins Web" 
                             # , "RKI (Bundesländer)"
                 ),
                 selected = "Johns Hopkins"),
    
    selectizeInput("show_c", "Select Countries to show:", choices = countries, 
                   selected = c("Germany", "Switzerland"),
                   multiple = TRUE),
    
    checkboxInput("normalize", label = "Cases per Million Inhabitants", value = TRUE),
    
    radioButtons("yaxt", "Y-axis of Cases:",
                 choices = c("linear", "logarithmic"), selected = "logarithmic", 
                 inline = TRUE),
    
    radioButtons("rfit", "Fit of Rate:",
                 choices = c("linear", "loess"), selected = "linear", 
                 inline = TRUE),
    
    # sliderInput("delay", "Delay [days]",
    #             min = 0, max = 40, value = 5),
    
    collapsed = FALSE  
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wwd1",
              fluidRow(
                box(width = 4, plotOutput('Plot1')),
                box(width = 4, plotOutput('Plot2')),
                box(width = 4, plotOutput('Plot3'))
              )
      ),
      tabItem(tabName = "wwd2",
              fluidRow(
                box(width = 4, plotOutput('Plot4')),
                box(width = 4, plotOutput('Plot5')),
                box(width = 4, plotOutput('Plot6'))
              )
              
      ),
      tabItem(tabName = "wwd3",
              fluidRow(
                box(width = 4, plotOutput('Plot7')),
                box(width = 4, plotOutput('Plot8')),
                box(width = 4, plotOutput('Plot9'))
              )      
      ),
      tabItem(tabName = "wwd4",
              fluidRow(
                box(width = 4, plotOutput('Plot10')),
                box(width = 4, plotOutput('Plot11')),
                box(width = 4, plotOutput('Plot12'))
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
              )
      )
    )
  )
)
