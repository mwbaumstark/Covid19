library(shiny)
library(shinyjs)
library(shinydashboard)
library(lubridate)
library(ggplot2)


#function to plot cumulative cases, optionally with exp. fit
mk_plot1 <- function(tss, col, titel, normalize, ylog, inh) {
  y <- tss[[col]]
  if (normalize) y <- y / inh[tss$Country_Region]
  p1 <- ggplot(tss, aes(x = Date, y = y, color = Country_Region)) +
    geom_point() +
    ylab("") +
    ggtitle(paste(titel, max(tss$Date), ")", sep = "")) +
    theme(legend.position = "none")
  if (ylog) {
    p1 <- p1 + scale_y_log10() +
      geom_smooth(aes(group = paste(vc, Country_Region)), method="lm")
  }
  return(p1)
}

#function to plot rate of increase, optionally with fit
mk_plot2 <- function(tss, col, titel, fit) {
  y <- tss[[col]]
  p1 <- ggplot(tss, aes(x = Date, y = y, color = Country_Region)) +
    geom_point() +
    ylab("%") +
    ggtitle(paste(titel , max(tss$Date), ")", sep = ""))+
    theme(legend.position = "none")
  if (fit == "linear") {
    p1 <-p1 + geom_smooth(aes(group = paste(vc, Country_Region)), method = "lm", formula = (y ~ 1))
  } else if (fit == "loess")  {
    p1 <-p1 + geom_smooth(method = "loess")
  }
  
  return(p1)
}

#function to plot rate of increase, optionally with fit
mk_plot3 <- function(tss, col, titel, normalize, inh) {
  y <- tss[[col]]
  if (normalize) y <- y / inh[tss$Country_Region]
  p1 <- ggplot(tss, aes(x = Date, y = y, fill = Country_Region, color = Country_Region)) +
    geom_bar(position="dodge", stat = "identity") +
    ylab("") +
    ggtitle(paste(titel, max(tss$Date), ")", sep = ""))
  return(p1)
}
##############################################################################
shinyServer(function(input, output, session) {
  
  defl <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    
    
    inh <- g_inh
    
    if (input$repo == "Johns Hopkins") {
      td <- tm
      # updateRadioButtons(session, "type", choices = ctype, selected = input$type)
    } else if (input$repo == "ECDC") {
      td <- ecdc
      # updateRadioButtons(session, "type", choices = ctype[c(1,2)], selected = input$type) 
    } else if (input$repo == "RKI (Germany)") {
      td <- rkig
      # updateRadioButtons(session, "type", choices = ctype[c(1,2)], selected = input$type)  
    } else if (input$repo == "RKI (Bundesländer)") {
      td <- rkia
      inh <- rki_inh
      # updateRadioButtons(session, "type", choices = ctype[c(1,2)], selected = input$type)
      if (input$show_c[1] %in% rki_countries) select <- input$show_c
      else select <- "Baden-Württemberg"
      # updateSelectizeInput(session,"show_c", "Bundesländer:", choices = rki_countries, 
      # selected = select)
    } else {
      td <- ts
      # updateRadioButtons(session, "type", choices = ctype, selected = input$type)
    }

    td$vc <- 1
    if (is.numeric(defl$x)) {
      if (as_date(defl$x) < max(td$Date)) {
        td$vc[td$Date >= as_date(defl$x)] <- 2
      }
    }
    
    tsss <- subset(td, (td$Country_Region %in% input$show_c) )
    #    tsss <- subset(td, (td$Country_Region == "Berlin") ) # debug
    
    mdate <- as.character(max(tsss$Date))
    
    tss <- subset(tsss, Date >= "2020-03-01")
    tss2 <- subset(tsss, Date >= "2020-03-15")
    
    if (input$tabs == "wwd1") {
      show("normalize")
      show("yaxt")
      show("repo")
      show("show_c")
      show("rfit")
      
      p1 <- mk_plot1(tss, "Confirmed", 
                     "Confirmed Cases (", 
                     input$normalize == TRUE,
                     input$yaxt == "logarithmic",
                     inh
      )
      
      p2 <- mk_plot2(tss2, "Rate_Confirmed",
                     "Daily rate of increase [%], Confirmed Cases (", 
                     input$rfit
      )
      
      p3 <- mk_plot3(tss, "Delta_Confirmed", 
                     "Daily delta, Confirmed Cases (", 
                     input$normalize == TRUE,
                     inh
      )
      output$Plot1 <- renderPlot({p1})
      output$Plot2 <- renderPlot({p2})
      output$Plot3 <- renderPlot({p3})
      
    } else if (input$tabs == "wwd2") {
      show("normalize")
      show("yaxt")
      show("repo")
      show("show_c")
      show("rfit")
      
      p4 <- mk_plot1(tss2, "Deaths", "Deaths (", 
                     input$normalize == TRUE,
                     input$yaxt == "logarithmic",
                     inh
      )
      
      p5 <- mk_plot2(tss2, "Rate_Deaths", 
                     "Daily rate of increase [%], Deaths (", 
                     input$rfit
      )
      
      p6 <- mk_plot3(tss2, "Delta_Deaths", 
                     "Daily delta, Deaths (", 
                     input$normalize == TRUE,
                     inh
      )
      output$Plot4 <- renderPlot({p4})
      output$Plot5 <- renderPlot({p5})
      output$Plot6 <- renderPlot({p6})
      
    } else if (input$tabs == "wwd3") { 
      show("normalize")
      show("yaxt")
      show("repo")
      show("show_c")
      show("rfit")
      
      p7 <- mk_plot1(tss2, "Recovered", "Recovered Cases (", 
                     input$normalize == TRUE,
                     input$yaxt == "logarithmic",
                     inh
      )
      
      p8 <- mk_plot2(tss2, "Rate_Recovered", 
                     "Daily rate of increase [%], Recovered Cases (", 
                     input$rfit
      )
      
      p9 <- mk_plot3(tss2, "Delta_Recovered", 
                     "Daily delta, Recovered Cases (", 
                     input$normalize == TRUE,
                     inh
      )
      output$Plot7 <- renderPlot({p7})
      output$Plot8 <- renderPlot({p8})
      output$Plot9 <- renderPlot({p9})
      
    } else if (input$tabs == "wwd4") {
      show("normalize")
      show("yaxt")
      show("repo")
      show("show_c")
      show("rfit")
      
      p10 <- mk_plot1(tss, "Active", "Active Cases (", 
                      input$normalize == TRUE,
                      input$yaxt == "logarithmic",
                      inh
      )
      
      p11 <- mk_plot2(tss2, "Rate_Active", 
                      "Daily rate of increase [%], Active Cases (", 
                      input$rfit
      )
      
      p12 <- mk_plot3(tss, "Delta_Active", 
                      "Daily delta, Active Cases (", 
                      input$normalize == TRUE,
                      inh
      )
      output$Plot10 <- renderPlot({p10})
      output$Plot11 <- renderPlot({p11})
      output$Plot12 <- renderPlot({p12})
      
    } else if (input$tabs == "wwd5") {
      
      hide("normalize")
      hide("yaxt")
      hide("rfit")
      show("repo")
      show("show_c")
      
      delay <- 0
      tss <- tss2
      
      tss$DeathsRatio <- NA
      tss$DeathsRatio[(delay + 1):length(tss$Date)] <-
        (tss$Deaths[(delay + 1):length(tss$Date)] /
           tss$Confirmed[1:(length(tss$Date) - delay)]) * 100
      
      tss$cmatch <- NA
      tss$cmatch[(delay + 1):length(tss$Date)] <- tss$Country_Region[(delay + 1):length(tss$Date)] ==
        tss$Country_Region[1:(length(tss$Date) - delay)]
      tss$DeathsRatio[! tss$cmatch] <- NA
      
      p99 <- ggplot(tss, aes(x = Date, y = DeathsRatio, color = Country_Region)) +
        geom_point() +
        geom_smooth(method = "loess") +
        ggtitle(paste("Deaths / Confirmed Cases [%] (", mdate, ")", sep = "")) +
        ylab("")
    } else if (input$tabs == "rki2") {
      hide("normalize")
      hide("yaxt")
      hide("repo")
      hide("show_c")
      hide("rfit")
    }
    
    output$Plot1 <- renderPlot({p1})
    output$Plot2 <- renderPlot({p2})
    output$Plot3 <- renderPlot({p3})
    output$Plot99 <- renderPlot({p99})
    
    
    output$AvTodesFaelle <- renderPlot({prki1})
    output$AvFaelle <- renderPlot({prki2})
    output$CFR <- renderPlot({prki3})
    
  }) # End observe
  observeEvent(input$plot_click, {
    clickx <- input$plot_click$x
    if (!is.null(clickx)) {
      defl$x <- clickx
    } else {
      defl$x <- NULL
    }
  })
  
  output$info1 <- renderText({
    if (is.numeric(defl$x)) {
      paste0("Date = ", as_date(defl$x), "To remove select max. date")
    } else {
      "Click to select deflection point"
    }
  })
  output$info2 <- renderText({
    if (is.numeric(defl$x)) {
      paste0("Date = ", as_date(defl$x))
    } else {
      "Click to select deflection point"
    }
  })
  output$info3 <- renderText({
    if (is.numeric(defl$x)) {
      paste0("Date = ", as_date(defl$x))
    } else {
      "Click to select deflection point"
    }
  })
  output$info4 <- renderText({
    if (is.numeric(defl$x)) {
      paste0("Date = ", as_date(defl$x))
    } else {
      "Click to select date to split fit"
    }
  })
  
}
)
