library(shiny)
library(shinyjs)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Covid-19 Data"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      # icons see:
      menuItem("Show Cases",
               icon = icon("chart-line"),
               tabName = "wwd1"
      ),
      radioButtons("cases", NULL, choices = ctype, selected = ctype[1]),
      menuItem("Other Data",
               icon = icon("bar-chart"),
               tabName = "other",
               startExpanded = FALSE,
               
               menuSubItem("Eff. reproduction number", tabName = "wwd4"),
               menuSubItem("CRF, % Detected, % Infected", tabName = "wwd5"),
               menuSubItem("RKI, Altersverteilung", tabName = "rki2")
      ),
      menuItem("Links, Documentation",
               icon = icon("info"),
               tabName = "links"
      )
      
    ),
    
    selectizeInput("show_c",
                   "Select Countries/Region to show (see 'Other Data' 'Links...' for details):",
                   choices = countries,
                   selected = c("Germany", "Switzerland"),
                   multiple = TRUE),
    
    checkboxInput("normalize", label = "Cases per Million Inhabitants", value = TRUE),
    
    
    dateInput("startd", "From Date:", value = "2020-03-01", min = min_date, max = max_date,
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    
    dateInput("stopd", "To Date:", value = max_date, min = min_date, max = max_date,
              #    dateInput("stopd", "To Date:", value = "2020-03-31", min = min_date, max = max_date,
              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
              language = "en", width = NULL),
    tags$hr(style="border-color: white;"),
    #    for (i in 2:length(warn_msg)) { # does not work for me
    
    if (lw >=2) print(warn_msg[2]),
    if (lw >=2) br(),
    if (lw >=3) print(warn_msg[3]),
    if (lw >=3) br(),
    if (lw >=4) print(warn_msg[4]),
    if (lw >=4) br(),
    if (lw >=5) print(warn_msg[5]),
    if (lw >=5) br(),
    if (lw >=6) print(warn_msg[6]),
    
    #    },
    
    collapsed = FALSE
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wwd1",
              fluidRow(
                column(width = 4,
                       box(width = NULL, plotOutput('Plot1', click = "plot_click")),
                       radioButtons("yaxt", "y-axis:",
                                    choices = c("linear", "logarithmic"),
                                    selected = "logarithmic",
                                    inline = TRUE),
                       
                       radioButtons("yafit", "Fit:",
                                    choices = c("exponential", "loess", "no fit"),
                                    selected = "loess",
                                    inline = TRUE),
                       verbatimTextOutput("info1"),
                       actionButton("reset", "Reset"),
                       
                       tags$hr(style="border-color: black;"),
                       print("'split fit' macht Sinn am Ende der exponentiellen Phase,
                             um zwei unterschiedlich steile Fits zu erhalten."),
                       br(),
                       print("Dazu als Fit links 'exponential' und in der Mitte 'constant'
                             auswählen.")
                ),
                column(width = 4,
                       box(width = NULL, plotOutput('Plot2')),
                       radioButtons("show_2", "Show:",
                                    choices = c("Doubling period", "Daily rate of increase"),
                                    selected = "Daily rate of increase",
                                    inline = TRUE),
                       radioButtons("rfit", "Fit:",
                                    choices = c("constant", "loess", "no fit"),
                                    selected = "loess",
                                    inline = TRUE)
                ),
                column(width = 4,
                       box(width = NULL, plotOutput('Plot3')),
                       tableOutput('table1')
                ),
              ),
      ),
      tabItem(tabName = "wwd5",
              fluidRow(
                column(width = 5,
                       box(width = NULL, plotOutput('Plot99')),
                       # sliderInput("delay", "Deaths per Cases(N days ago)",
                       #             min = 0, max = 30, value = 14 ),
                       sliderInput("ifr",
                                   "Assumed true infection fatality rate",
                                   min = 0.2, max = 2.0, value = 1.3, step = 0.01)
                ),
                column(width = 6,
                       tableOutput('table2'),
                       
                       tags$hr(style="border-color: black;"),
                       print("Nach [Bommer & Vollmer (2020)] kann man die Detektionrate und den
                             Anteil der schon infizierten Bevölkerung schätzen, wenn man die 'wahre'
                             'infection fatality rate' kennt. Das Paper gibt Werte für verschiedene Länder
                             an. z.B.: Deutschland: 1.3, Schweiz: 1.13.  [Salje et al. (2020)] geben in 'Table S2' Werte für die
                             französische Bevölkerung an. Daraus ergibt sich für Deutschland nach gleicher Methodik 
                             'population shares' 
                             ein Mittelwert von 1.05. Berücksichtigt man die Altersverteilung bei den Infizierten ist der Wert
                             1.5.
                             Für Johns Hopkins Daten kann Geschlecht und Alter berücksichtigt werden, da nicht bekannt.
                             Ziel ist eine Abschätzung der Größenordnung, durch
                             Verändern der 'Assumed true infection fatality rate'."),
                       br(),
                       print("'Deaths per Cases(N days ago)' ist in [Bommer & Vollmer (2020)] erklärt.
                             Das Delay ist 14 für Johns Hopkins Daten. Mit 'To Date: 31.03.2020' kann man die 'Table 1' des
                             genannten Papers reproduzieren. Für RKI Daten benötigt man kein Delay (N=0), 
                             da alle Fälle jeweils auf das Erkrankungsdatum bezogen sind.")
                )
                
              )
              
      ),
      tabItem(tabName = "wwd4",
              fluidRow(
                box(width = 4, plotOutput('Plot98'))
              ),
              fluidRow(
                column(width = 4,
                       selectizeInput("r_show_c",
                                      "Select one Country/Region to show (including Bundesländer and Germany (RKI):",
                                      choices = countries,
                                      selected = c("Germany (RKI)"),
                                      multiple = FALSE),
                       
                       tags$hr(style="border-color: black;"),
                       print("Die Berechnung der effektiven Reproduktionzahl benutzt die unveränderten,
                                    täglich gemeldeten, neuen Fälle. Das RKI benutzt via Nowcast korrrigierte
                                    Daten [Epidemiologisches Bulletin 17/2020], was zu wesentlich glatteren Verläufen führt.
                                    Die Glättung (dicke rote Linie) ergibt jedoch ähnliche Werte.
                                    Zum Vergleich wird die effektive Reproduktionszahl auch mit der
                                    Methode von Wallinga and Teunis (2004) berechnet. Siehe:
                                    [Effective reproduction number estimation with R0]"
                       ),
                       br(),
                       print("Gegen Ende des Zeitraums 'heute' sind die Werte ungenauer. Abwarten...")
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
                                      "Select 'Germany (RKI)' or one 'Bundesland'", choices = rki_countries_noLk,
                                      selected = c("Germany (RKI)"),
                                      multiple = FALSE)
                ),
                column(width = 4, tableOutput('table3')),
                column(width = 4, tableOutput('table4'))
                
              )
      ),
      tabItem(tabName = "links",
              fluidRow(
                column(width = 4,
                       uiOutput("link01"),
                       uiOutput("link02"),
                       uiOutput("link03"),
                       uiOutput("link1"),
                       uiOutput("link2"),
                       uiOutput("link3"),
                       uiOutput("link4"),
                       uiOutput("link5"),
                       uiOutput("link6"),
                       uiOutput("link7"),
                       uiOutput("link8"),
                       uiOutput("link9"),
                       uiOutput("link10"),
                       uiOutput("link11"),
                       uiOutput("link12"),
                       uiOutput("link13"),
                       
                       tags$hr(style="border-color: black;"),
                       print("Verfügbare Länder/Regionen sind alle Länder die im Johns Hopkins
                             Datensatz enthalten sind, alle Bundesländer aus dem RKI Datensatz und
                             Deutschland aus dem RKI Datensatz 'Germany (RKI)'. Als kleinere Einheiten:
                             'Tessin', 'LK Breisgau-Hochschwarzwald' und 'SK Freiburg i.Breisgau'.
                             Die 'Recovered' Zahlen werden für Johns Hopkins Daten wie bei [D. Kriesel] 
                             beschrieben korrigiert."),
                       br(),
                       print("Das Programm benutzt für RKI Daten das Referenzdatum. Dabei bezieht 
                             das RKI alle Fälle (Bestätigte Fälle, Todesfälle und Genesene) auf das
                             Erkrankungsdatum, ersatzweise das Meldedatum. Um die aktiven Fällen darstellen zu können, benötigt 
                             man aber das Datum der Erholung. Dieses wird geschätzt in dem die Genesenen um 18 Tage 
                             verschoben werden.")
                )
              )
      )
      
    )
  )
)

