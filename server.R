library(shiny)
library(shinyjs)
library(shinydashboard)
library(lubridate)
library(ggplot2)


#function to plot cumulative cases, optionally with exp. fit
mk_plot1 <- function(tss, col, titel, normalize, ylog, yafit, inh) {
  tss$y <- tss[[col]]
  if (normalize) tss$y <- tss$y / inh[tss$Country_Region]
  
  #   lm fit is not correct. Work in progress :-)  
  #  
  #   tss$yf[tss$y >= 1] <- tss$y[tss$y >= 1] 
  #   # 
  #   # print(factor(tss$Country_Region))
  #   # print(nlevels(factor(tss$Country_Region)))
  #   # print(unique(tss$vc))
  #   # print(nlevels(factor(tss$vc)))
  #   
  #   if ((nlevels(factor(tss$Country_Region)) > 1) & (nlevels(factor(tss$vc))) > 1) {
  #     fm <- lm(log(yf) ~ Date:Country_Region:vc, tss)
  #   } else if ((nlevels(factor(tss$Country_Region)) == 1) & (nlevels(factor(tss$vc))) > 1) {
  #     fm <- lm(log(yf) ~ Date:vc, tss)
  #   } else if ((nlevels(factor(tss$Country_Region)) > 1) & (nlevels(factor(tss$vc))) == 1) {
  #     fm <- lm(log(yf) ~ Date:Country_Region, tss)
  #   } else {
  #     fm <- lm(log(yf) ~ Date, tss) 
  #   }
  #   
  #    print(fm)
  #   
  #   predslm = as.data.frame(predict(fm, interval = "confidence"))
  # #  print(predslm)
  #   tss$fit[!is.na(tss$yf)] <- exp(predslm$fit)
  #   tss$lwr[!is.na(tss$yf)] <- exp(predslm$lwr)
  #   tss$upr[!is.na(tss$yf)] <- exp(predslm$upr)
  
  p1 <- ggplot(tss, aes(x = Date, y = y, color = Country_Region)) +
    geom_point() +
    ylab("") +
    ggtitle(paste(titel, max(tss$Date), ")", sep = "")) +
    theme(legend.position = "none")
  if (yafit == "no fit") {
    p1 <- p1 + geom_line()
  }
  if (ylog) {
    p1 <- p1 + scale_y_log10()
  }
  if (ylog & (yafit == "exponential")) {
    p1 <- p1 + geom_smooth(aes(group = paste(vc, Country_Region), fill = Country_Region), 
                           method = "lm",
                           alpha = .15
    )
  }
  return(p1)
}

#function to plot rate of increase, optionally with fit
mk_plot2 <- function(tss, col, titel, fit) {
  y <- tss[[col]]
  p1 <- ggplot(tss, aes(x = Date, y = 100/y, color = Country_Region)) +
    geom_point() +
    #    ylab("%") +
    ylab("Days") +
    ggtitle(paste(titel , max(tss$Date), ")", sep = ""))+
    theme(legend.position = "none")
  if (fit == "linear") {
    p1 <- p1 + geom_smooth(aes(group = paste(vc, Country_Region), fill = Country_Region), 
                           alpha = .15,
                           method = "lm", 
                           formula = (y ~ 1))
  } else if (fit == "loess")  {
    p1 <- p1 + geom_smooth(method = "loess", 
                           alpha = .15,
                           aes(fill = Country_Region))
  } else if (fit == "no fit")  {
    p1 <- p1 + geom_line()
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
  # print("Start Server") #DEBUG
  defl <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    RKI <- FALSE
    inh <- g_inh
    if (input$repo == "Johns Hopkins") {
      td <- tm
      if (input$show_c[1] %in% countries) select <- input$show_c
      else select <- c("Germany", "Switzerland")
      updateSelectizeInput(session,"show_c", "Select Countries to show:", choices = countries,
                           selected = select)
      
    } else if (input$repo == "ECDC") {
      td <- ecdc
      if (input$show_c[1] %in% countries) select <- input$show_c
      else select <- c("Germany", "Switzerland")
      updateSelectizeInput(session,"show_c", "Select Countries to show:", choices = countries,
                           selected = select)
      
    } else if (input$repo == "RKI (Germany)") {
      RKI <- TRUE
      td <- rkig
      rkigg <- aggregate(cbind(Delta_Deaths, Delta_Confirmed) ~ Geschlecht + Altersgruppe, rki, sum)
      updateSelectizeInput(session,"show_c", "Select Countries to show:", choices = "Germany",
                           selected = "Germany")
      selectinfo <- "Germany"
      
    } else if (input$repo == "RKI (Bundesländer)") {
      RKI <- TRUE
      td <- rkia
      inh <- rki_inh
      if (input$show_c[1] %in% rki_countries) {
        select <- input$show_c
      } else {
        select <- "Baden-Württemberg"
      }
      rkigg <- aggregate(cbind(Delta_Deaths, Delta_Confirmed) ~ Geschlecht + Altersgruppe
                         , subset(rki, (Country_Region %in% select)), sum)
      
      updateSelectizeInput(session,"show_c", "Bundesländer:", choices = rki_countries,
                           selected = select)
      selectinfo <- select
      # } else {
      #   td <- ts
    }
    
    td$vc <- "A"
    if (is.numeric(defl$x)) {
      #      if (as_date(defl$x) < max(td$Date)) {
      td$vc[td$Date >= as_date(defl$x)] <- "B"
      #      }
    }
    
    # print(paste("td:", dim(td))) # DEBUG
    # print(paste("input$show_c:", input$show_c)) # DEBUG
    
    tsss <- subset(td, (td$Country_Region %in% input$show_c) )
    
    #    tsss <- subset(td, (td$Country_Region == "Berlin") ) # debug
    # print(paste("tsss:", dim(tsss))) # DEBUG
    
    mdate <- as.character(max(tsss$Date))
    
    tss <- subset(tsss, (Date >= input$startd) & (Date <= input$stopd))
    
#    print(paste("tss:", dim(tss))) # DEBUG
    
    if (input$tabs == "wwd1") {
      
      show("normalize")
      show("cases")
      show("startd")
      show("stopd")
      
      if (input$cases == ctype[1]) {
        
        p1 <- mk_plot1(tss, "Confirmed", 
                       "Confirmed Cases (", 
                       input$normalize == TRUE,
                       input$yaxt == "logarithmic",
                       input$yafit,
                       inh
        )
        
        p2 <- mk_plot2(tss, "Rate_Confirmed",
                       # "Daily rate of increase [%], Confirmed Cases (", 
                       "Doubling period [days], Confirmed Cases (",
                       input$rfit
        )
        
        p3 <- mk_plot3(tss, "Delta_Confirmed", 
                       "Daily delta, Confirmed Cases (", 
                       input$normalize == TRUE,
                       inh
        )
        
      } else if (input$cases == ctype[2]) {
        
        p1 <- mk_plot1(tss, "Deaths", "Deaths (", 
                       input$normalize == TRUE,
                       input$yaxt == "logarithmic",
                       input$yafit,
                       inh
        )
        
        p2 <- mk_plot2(tss, "Rate_Deaths", 
                       "Doubling period [days], Deaths (", 
                       input$rfit
        )
        
        p3 <- mk_plot3(tss, "Delta_Deaths", 
                       "Daily delta, Deaths (", 
                       input$normalize == TRUE,
                       inh
        )
        
      } else if (input$cases == ctype[3]) { 
        
        p1 <- mk_plot1(tss, "Recovered", "Recovered Cases (", 
                       input$normalize == TRUE,
                       input$yaxt == "logarithmic",
                       input$yafit,
                       inh
        )
        
        p2 <- mk_plot2(tss, "Rate_Recovered", 
                       "Daily rate of increase [%], Recovered Cases (", 
                       input$rfit
        )
        
        p3 <- mk_plot3(tss, "Delta_Recovered", 
                       "Daily delta, Recovered Cases (", 
                       input$normalize == TRUE,
                       inh
        )
        
      } else if (input$cases == ctype[4]) {
        
        p1 <- mk_plot1(tss, "Active", "Active Cases (", 
                       input$normalize == TRUE,
                       input$yaxt == "logarithmic",
                       input$yafit,
                       inh
        )
        
        p2 <- mk_plot2(tss, "Rate_Active", 
                       "Daily rate of increase [%], Active Cases (", 
                       input$rfit
        )
        
        p3 <- mk_plot3(tss, "Delta_Active", 
                       "Daily delta, Active Cases (", 
                       input$normalize == TRUE,
                       inh
        )
      }
      
    } else if (input$tabs == "wwd5") {
      
      hide("normalize")
      show("repo")
      show("show_c")
      hide("cases")
      show("startd")
      show("stopd")
      
      delay <- 0
      if (dim(tss)[1] > 0) {    # Hack to prevent crash
        tss$DeathsRatio <- NA # error
        tss$DeathsRatio[(delay + 1):length(tss$Date)] <-
          (tss$Deaths[(delay + 1):length(tss$Date)] /
             tss$Confirmed[1:(length(tss$Date) - delay)]) * 100
        
        tss$cmatch <- NA
        tss$cmatch[(delay + 1):length(tss$Date)] <- tss$Country_Region[(delay + 1):length(tss$Date)] ==
          tss$Country_Region[1:(length(tss$Date) - delay)]
        tss$DeathsRatio[! tss$cmatch] <- NA
        
        p99 <- ggplot(tss, aes(x = Date, y = DeathsRatio, color = Country_Region)) +
          geom_point() +
          geom_smooth(method = "loess",
                      alpha = .15,
                      aes(fill = Country_Region)) +
          ggtitle(paste("Deaths / Confirmed Cases [%] (", mdate, ")", sep = "")) +
          ylab("")
      } else {
        p99 <- ggplot + theme_void()
      }
      
    } else if (input$tabs == "rki2") {
      
      hide("normalize")
      hide("cases")
      hide("startd")
      hide("stopd")
      
      if (RKI) {
        rkigg$Geschlecht[rkigg$Geschlecht == "W"] <- "F"
        rkigg$Geschlecht[rkigg$Geschlecht == "unbekannt"] <- "U"
        names(rkigg)[names(rkigg) == "Geschlecht"] <- "Sex" 
        rkigg$Altersgruppe <- gsub("A", "", rkigg$Altersgruppe)
        
        rkigg$CFR <- (rkigg$Delta_Deaths / rkigg$Delta_Confirmed) * 100
        rkigg$CFR[rkigg$Sex == "U"] <- NA
        
        prki1 <- ggplot(rkigg, aes(x = Altersgruppe, y = Delta_Deaths, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste("Altersverteilung, Todesfälle (N=", sum(rkigg$Delta_Deaths), ", ",
                        max(rki$Date), ")", sep = ""))
        
        prki2 <- ggplot(rkigg, aes(x = Altersgruppe, y = Delta_Confirmed, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste("Altersverteilung, Positiv Getestete (N=", sum(rkigg$Delta_Confirmed), ", ", 
                        max(rki$Date), ")", sep = ""))
        
        prki3 <- ggplot(rkigg, aes(x = Altersgruppe, y = CFR, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste("Todesfälle / Positiv Getestete [%] (", max(rki$Date), ")", sep = ""))
        output$selinfo <- renderText({
          selectinfo 
        })
        
      } else {
        prki1 <- ggplot() + theme_void()
        prki2 <- prki1
        prki3 <- prki1
        output$selinfo <- renderText({
          "Not available" 
        })
        
      }
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
    }
  })
  
  observeEvent(input$reset, {
    defl$x <- NULL    
  })
  
  output$info1 <- renderText({
    if (is.numeric(defl$x)) {
      paste0("Date = ", as_date(defl$x))
    } else {
      "Click to select deflection point"
    }
  })
}
)
