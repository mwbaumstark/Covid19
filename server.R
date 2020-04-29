library(shiny)
library(shinyjs)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(R0)
library(dplyr)

GT_data <- c(3.31,2.9) # https://hal-pasteur.archives-ouvertes.fr/pasteur-02548181/document
# GT_obj <- R0::generation.time("gamma", c(4.7,2.9))    # @nishiura_serial_2020 
GT_obj <- R0::generation.time("gamma", GT_data)    

## Wallinga and Teunis (2004)
est_rt_wt <- function(ts, GT_obj) {
  end <- length(ts) - 3 
  R0::est.R0.TD(ts, GT=GT_obj, begin=1, end=end, nsim=1000)
}

## RKI Method
est_rt_rki <- function(ts, GT=4L) {
  # Sanity check
  if (!is.integer(GT) | !(GT>0)) stop("GT has to be postive integer.")
  # Estimate
  res <- sapply( (1+GT):(length(ts)-GT), function(t) {
    sum(ts[t+(0:(GT-1))]) / sum(ts[t-(1:GT)]) 
  })
  names(res) <- names(ts)[(1+GT):(length(ts)-GT)]
  return(res)
}

#function to plot cumulative cases, optionally with exp. fit
mk_plot1 <- function(tss, col, titel, normalize, ylog, yafit, inh) {
  if (dim(tss)[1] > 0) {    # Hack to prevent crash
    
    tss$y <- tss[[col]]
    
    if (normalize) tss$y <- tss$y / inh[tss$Country_Region]
    
    p1 <- ggplot(tss, aes(x = Date, y = y, color = Country_Region)) +
      geom_point() +
      ylab("") +
      ggtitle(paste(titel, max(tss$Date), ")", sep = "")) +
      theme(legend.position = "none")
    if (yafit == "no fit") {
      p1 <- p1 + geom_line()
    } else if (yafit == "loess") {
      p1 <- p1 + geom_smooth(aes(group = paste(vc, Country_Region), fill = Country_Region),
                             method = "loess",
                             alpha = .15)
    }
    if (ylog) {
      p1 <- p1 + scale_y_log10()
    }
    if (ylog & yafit == "exponential") {
      p1 <- p1 + geom_smooth(aes(group = paste(vc, Country_Region), fill = Country_Region),
                             method = "lm",
                             alpha = .15
      ) 
    }
  } else {
    p1 <- ggplot + theme_void()
  }
  return(p1)
}

#function to plot rate of increase, optionally with fit
mk_plot2 <- function(tss, col, fit, show) {
  if (dim(tss)[1] > 0) {    # Hack to prevent crash
    tss$y <- tss[[col]]
    
    if (show == "Doubling period") {
      tss$y <- 100 / tss$y
      ylimits <- ylim(-30, 30)
      titel <- "Doubling period [days]"
    } else if (show == "Daily rate of increase") {
      ylimits <- ylim(-10, 10)
      titel <- "Daily rate of increase [%]"
    } 
    
    p1 <- ggplot(tss, aes(x = Date, y = y, color = Country_Region)) +
      geom_point() +
      ylimits +
      ylab("") +
      ggtitle(paste(titel , " (", max(tss$Date), ")", sep = ""))+
      theme(legend.position = "none")
    if (fit == "constant") {
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
  } else {
    p1 <- ggplot + theme_void()
  }
  return(p1)
}

#function to plot rate of increase, optionally with fit
mk_plot3 <- function(tss, col, titel, normalize, inh) {
  if (dim(tss)[1] > 0) {    # Hack to prevent crash
    y <- tss[[col]]
    if (normalize) y <- y / inh[tss$Country_Region]
    p1 <- ggplot(tss, aes(x = Date, y = y, fill = Country_Region, color = Country_Region)) +
      geom_bar(position="dodge", stat = "identity") +
      ylab("") +
      ggtitle(paste(titel, max(tss$Date), ")", sep = ""))
  } else {
    p1 <- ggplot + theme_void()
  }
  return(p1)
}

##############################################################################
shinyServer(function(input, output, session) {
  # print("Start Server") #DEBUG
  defl <- reactiveValues(x = NULL, y = NULL)
  # sel_c <- reactiveValues(maxI = 10) # sel_c$maxI
  
  observe({
    td <- all
    selectinfo <- input$show_c   # select
    
    td$vc <- "A"
    
    if (is.numeric(defl$x)) {
      tmp <- as_date(defl$x)
      td$vc[td$Date >= tmp] <- "B"
    }
    
    ts <- subset(td, (td$Country_Region %in% input$show_c) )
    tss <- subset(ts, (Date >= input$startd) & (Date <= input$stopd))
    
    #    print(paste("tss:", dim(tss))) # DEBUG
    
    if (input$tabs == "wwd1") {
      
      show("normalize")
      show("cases")
      show("startd")
      show("stopd")
      show("show_c")
      # if (compareNA(sel_c$maxI, 1)) {
      #   updateSelectizeInput(session, "show_c", 
      #                        options = list(maxItems = 10),
      #                        selected = selectinfo)
      # }
      if (! is.null(input$show_c)) {    
        
        tabd <- aggregate(cbind(Confirmed, Deaths, Recovered, Active, format(Date)) ~
                    Country_Region, tss,  FUN=tail,1)
        names(tabd)[6] <- "Date"

          output$table1 <- renderTable({tabd},
          striped = FALSE,
          bordered = TRUE,
          digits = 0,
          caption = "<b> <span style='color:#000000'> Absolut number of cases </b>",
          caption.placement = getOption("xtable.caption.placement", "top"),
          caption.width = getOption("xtable.caption.width", NULL)
        )
      } else {
        output$table1 <- renderTable({})
      }
      
      if (input$cases == ctype[1]) {
        
        p1 <- mk_plot1(tss, "Confirmed", 
                       "Confirmed Cases (", 
                       input$normalize == TRUE,
                       input$yaxt == "logarithmic",
                       input$yafit,
                       inh
        )
        
        p2 <- mk_plot2(tss, "Rate_Confirmed",
                       input$rfit,
                       input$show_2
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
                       input$rfit,
                       input$show_2
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
                       input$rfit,
                       input$show_2
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
                       input$rfit,
                       input$show_2
        )
        
        p3 <- mk_plot3(tss, "Delta_Active", 
                       "Daily delta, Active Cases (", 
                       input$normalize == TRUE,
                       inh
        )
      }
      
    } else if (input$tabs == "wwd5") {
      
      hide("normalize")
      show("show_c")
      hide("cases")
      show("startd")
      show("stopd")
      # if (compareNA(sel_c$maxI, 1)) {
      #   updateSelectizeInput(session, "show_c", 
      #                        options = list(maxItems = 10),
      #                        selected = selectinfo)
      # }
      
      delay <- input$delay      
      if (dim(tss)[1] > 0) {
        
        dateMax <- min(aggregate(Date ~ Country_Region, data = tss, max)$Date)
        tss <- subset(tss, Date <= dateMax) # same max Date for all Countries
        
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
          geom_smooth(method = "loess",
                      alpha = .15,
                      aes(fill = Country_Region)) +
          ggtitle(paste("Deaths / Confirmed Cases [%] (", 
                        as.character(max(tss$Date)), ")", sep = "")) +
          ylab("")
        
        dateDelay <- dateMax - delay
        
        dst <- subset(tss, 
                      Date %in% c(dateDelay, dateMax), 
                      select = c(Date, Country_Region, Deaths, Confirmed, DeathsRatio))
        
        dst$DeathsRatio[dst$Date == dateDelay] <- NA
        dst$detRate <- input$ifr / dst$DeathsRatio

        dst$Country_Region <- as.character(dst$Country_Region)
        
        for (i in unique(dst$Country_Region)) {
          dst$detRate[dst$Country_Region == i] <- dst$detRate[(dst$Country_Region == i) &
                                                                (! is.na(dst$detRate))]
        }
        dst <- subset(dst, select = -DeathsRatio)
        dst$Estimated <- dst$Confirmed / dst$detRate
        dst$SharePopulInfected <- dst$Estimated / (inh[dst$Country_Region] * 1000000) * 100 
        dst$detRate <- dst$detRate * 100 # %

        dst$Date <- format(dst$Date, "%d.%m.%Y")
        dst$Estimated <- format(dst$Estimated, digits = 0, scientific = FALSE)
        dst$Confirmed <- format(dst$Confirmed, digits = 0, scientific = FALSE)
        dst$Deaths <- format(dst$Deaths, digits = 0, scientific = FALSE)
        
        names(dst)[names(dst) == "detRate"] <- "Detection Rate"
        names(dst)[names(dst) == "SharePopulInfected"] <- "Share of Population Infected"
        
        output$table2 <- renderTable({dst},
                                     striped = TRUE,
                                     bordered = TRUE,
                                     digits = 2,
                                     align = "r",
                                     caption = "<b> <span style='color:#000000'> 
                                     Estimation of 'detection rate' and 'share of population infected' </b>",
                                     caption.placement = getOption("xtable.caption.placement", "top"),
                                     caption.width = getOption("xtable.caption.width", NULL)
        )
      } else {
        p99 <- ggplot + theme_void()
        output$table2 <- renderTable({})
      }
      
    } else if (input$tabs == "rki2") {
      
      hide("cases")
      hide("show_c")
      hide("startd")
      hide("stopd")
      hide("normalize")
      
      if (input$rki_show_c != "") {    
        
        if (input$rki_show_c == "Germany (RKI)") {
          rkigg <- aggregate(cbind(Delta_Deaths, Delta_Confirmed, Delta_Recovered, Delta_Active) ~ 
                               Sex + Altersgruppe
                             , rki_full, sum)
          rkigg$Country_Region <- "Germany (RKI)"
        } else {
          rkigg <- aggregate(cbind(Delta_Deaths, Delta_Confirmed, Delta_Recovered, Delta_Active) ~ 
                               Sex + Altersgruppe
                             , subset(rki_full, (Country_Region == input$rki_show_c)), sum)
          rkigg$Country_Region <- input$rki_show_c
        } 
        
        
        rkigg <- merge(rkigg, xxa_ge, 
                       by = c("Country_Region", "Sex", "Altersgruppe"), 
                       all.x = TRUE)    
        
        rkigg$Altersgruppe <- gsub("A", "", rkigg$Altersgruppe)
        
        if (input$rki_cases == ctype[4]) {
          rkigg$y <- rkigg$Delta_Active
          cname <- "Aktive Fälle"
          dsum <- sum(rkigg$Delta_Active)
        } else if (input$rki_cases == ctype[3]) {
          rkigg$y <- rkigg$Delta_Recovered
          cname <- "Genesene"
          dsum <- sum(rkigg$Delta_Recovered)
        } else if (input$rki_cases == ctype[2]) {
          rkigg$y <- rkigg$Delta_Death
          cname <- "Todesfälle"
          dsum <- sum(rkigg$Delta_Death)
        } else { 
          rkigg$y <- rkigg$Delta_Confirmed
          cname <- "Positiv Getestete"
          dsum <- sum(rkigg$Delta_Confirmed)
        }
        
        rkigg$CFR <- (rkigg$y / rkigg$Delta_Confirmed) * 100
        rkigg$CFR[rkigg$Sex == "U"] <- NA
        
        csum <- sum(rkigg$Delta_Confirmed)
        
        #      if (input$normalize == TRUE) {
        rkigg$yn <- rkigg$y / rkigg$Population
        #        rkigg$Delta_Confirmed <- rkigg$Delta_Confirmed  / rkigg$Population
        #      }
        
        prki1 <- ggplot(rkigg, aes(x = Altersgruppe, y = y, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste("Altersverteilung ", cname, " (N=", csum, ", ", 
                        rki_Datenstand, ")", sep = ""))
        
        prki2 <- ggplot(rkigg, aes(x = Altersgruppe, y = yn, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste("Altersverteilung / Mio. Einwohner ", sep = ""))
        
        prki3 <- ggplot(rkigg, aes(x = Altersgruppe, y = CFR, fill = Sex, color = Sex)) +
          geom_bar(position="dodge", stat = "identity" ) +
          ylab("") +
          ggtitle(paste(cname, " / Positiv Getestete [%]", sep = ""))
      } else {
        prki1 <- ggplot + theme_void()
        prki2 <- ggplot + theme_void()
        prki3 <- ggplot + theme_void()
      } 
    } else if (input$tabs == "wwd4") {
      
      hide("cases")
      hide("show_c")
      show("startd")
      show("stopd")
      hide("normalize")
      
      sel_r0 <- input$r_show_c[1]
      
      print(sel_r0)
      
      if(sel_r0 != "") {    
        ts <- subset(all, (all$Country_Region %in% sel_r0 ))
        tss <- subset(ts, (Date >= input$startd) & (Date <= input$stopd))
        
        tss$y <- tss$Delta_Confirmed
        
        tsss <- subset(tss, (y > 2)) # & (Date < max(Date - 3)))
        
        if (dim(tsss)[1] > 10) {       
          # Wallinga Teunis approach with "correct" GT distribution
          rt_wt <- est_rt_wt( tsss$y, GT=GT_obj)
          
          # RKI method
          rt_rki <- est_rt_rki( tsss$y %>% setNames(tsss$Date) , GT=4L)
          
          rt_wt_df <- cbind(Date=tsss$Date[as.numeric(names(rt_wt$R))], 
                            R_hat=rt_wt$R, 
                            rt_wt$conf.int, 
                            Method=paste0("W & T, GT=(", GT_data[1], "±", GT_data[2], ")"))
          
          rt_rki_df <- data.frame(Date=as.Date(names(rt_rki)),
                                  R_hat=rt_rki,
                                  Method="simplified RKI, GT=4")
          
          p98 <- ggplot(rt_wt_df, aes(x=Date, y=R_hat, color=Method, fill=Method)) +  
            geom_ribbon(aes(x=Date,  ymin=lower, ymax=upper, color=NULL), alpha=0.15) +
            geom_line() +
            geom_line(data=rt_rki_df) +
            geom_smooth(data=rt_rki_df, method = "loess", alpha=0.15, se = FALSE) +
            coord_cartesian(ylim=c(0, 2)) +
            ylab(expression(R[e](t))) +
            theme(legend.position="bottom")
        } else {
          p98 <- ggplot + theme_void()
        }  
      } else {
        p98 <- ggplot + theme_void()
      }     
      output$Plot98 <- renderPlot({p98})
      
    }
    else if (input$tabs == "links") {
      hide("normalize")
      hide("show_c")
      hide("cases")
      hide("startd")
      hide("stopd")
      
      output$link01 <- renderUI({
        tagList("Datenquelle weltweit:", 
                a("Johns Hopkins", 
                  href="https://datahub.io/core/covid-19"))
      })
      output$link02 <- renderUI({
        tagList("Datenquelle Deutschland:", 
                a("RKI, bereitgestellt durch die Fa. ESRI", 
                  href="https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0"))
      })
      output$link03 <- renderUI({
        tagList("Datenquelle Tessin:", 
                a("SARS-CoV-2 Cases communicated by Swiss Cantons and Principality of Liechtenstein (FL)", 
                  href="https://github.com/openZH/covid_19"))
      })
      output$link1 <- renderUI({
        tagList("RKI:", 
                a("Neuartiges Coronavirus in Deutschland", 
                  href="https://www.rki.de/DE/Home/homepage_node.html"))
      })
      output$link2 <- renderUI({
        tagList("RKI:", 
                a("COVID-19-Dashboard", 
                  href="https://experience.arcgis.com/experience/478220a4c454480e823b17327b2bf1d4"))
      })
      output$link3 <- renderUI({
        tagList("BW:", 
                a("Übersicht Infektionen und Todesfälle in Baden-Württemberg", 
                  href="https://www.baden-wuerttemberg.de/de/service/presse/pressemitteilung/pid/uebersicht-infektionen-und-todesfaelle-in-baden-wuerttemberg/"))
      })
      output$link4 <- renderUI({})
      output$link5 <- renderUI({
        tagList("BGA:", 
                a("Neues Coronavirus: Situation Schweiz und International", 
                  href="https://www.bag.admin.ch/bag/de/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html"))
      })
      output$link6 <- renderUI({
        tagList("Situation weltweit, mit Angaben zu Test-Raten:", 
                a("worldometer", 
                  href="https://www.worldometers.info/coronavirus/"))
        })
      output$link7 <- renderUI({
        tagList("Ticino:", 
                a("Situation im Tessin", 
                  href="https://www4.ti.ch/dss/dsp/covid19/home/"))
      })
      output$link8 <- renderUI({
        tagList("Artikel:", 
                a("Bommer & Vollmer (2020)", 
                  href="http://www.uni-goettingen.de/en/606540.html"))
      })
      output$link9 <- renderUI({
        tagList("Artikel:", 
                a("Henrik Salje, Cécile Tran Kiem, Noémie Lefrancq, Noémie Courtejoie, Paolo Bosetti, et al.. Estimating
the burden of SARS-CoV-2 in France. 2020. ffpasteur-02548181", 
                  href="https://hal-pasteur.archives-ouvertes.fr/pasteur-02548181/document"))
      })
      output$link10 <- renderUI({
        tagList("Artikel:", 
                a("Effective reproduction number estimation with R0", 
                  href="https://staff.math.su.se/hoehle/blog/2020/04/15/effectiveR0.html"))
      })
      output$link11 <- renderUI({
        tagList("RKI, Epidemiologisches Bulletin 17/2020:", 
                a("Schätzung der aktuellen Entwicklung der SARS-CoV-2-Epidemie in Deutschland - Nowcasting", 
                  href="https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2020/Ausgaben/17_20_SARS-CoV2_vorab.pdf?__blob=publicationFile"))})
      output$link12 <- renderUI({tagList("D. Kriesel:", 
                                         a("Corona-Plots und Interpretationshilfen, garantiert unaufgeregt", 
                                           href="http://www.dkriesel.com/corona"))})
      output$link13 <- renderUI({})
      
    }
    
    output$Plot1 <- renderPlot({p1})
    output$Plot2 <- renderPlot({p2})
    output$Plot3 <- renderPlot({p3})
    output$Plot99 <- renderPlot({p99})
    
    output$AvFaelle <- renderPlot({prki1})
    output$AvTodesFaelle <- renderPlot({prki2})
    output$CFR <- renderPlot({prki3})
    
    output$info2 <- renderText({paste("Country/Region =", sel_r0)})
    
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
      "Click in plot to split fit"
    }
  })
}
)
