library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)

# compare with NAs
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

compareLE <- function(v1,v2) {
  same <- (v1 <= v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

compareGE <- function(v1,v2) {
  same <- (v1 >= v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

delta2cum <- function(df, delta_col) {
  cum_col <- sub("Delta_", "", delta_col)
  df[[cum_col]] <- 0
  df[[cum_col]][1] <- df[[delta_col]][1]
  
  for (i in 2:length(df[[cum_col]])) {
    df[[cum_col]][i] = df[[cum_col]][i-1] + df[[delta_col]][i]
    if (df$Country_Region[i] != df$Country_Region[i-1]){
      df[[cum_col]][i] = df[[delta_col]][i]
    }
  }
  return(df[[cum_col]])
}

cum2delta <- function(df, cum_col) {
  delta_col <- paste("Delta_", cum_col, sep = "")
  df[[delta_col]] <- 0
  df[[delta_col]][2:length(df[[delta_col]])] <- diff(df[[cum_col]], 1)
  df[[delta_col]][! df$cmatch] <- df[[cum_col]][! df$cmatch]
  return(df[[delta_col]])
}

##### type of cases ####
ctype <- c("Confirmed Cases" , 
           "Deaths", 
           "Recovered Cases",
           "Active Cases" 
)

# Johns Hopkins Master Repository #############################
# from datahub.io (more simple repository)

tm_data <- "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"
tm_raw <- read_csv(url(tm_data))

tm <- aggregate(cbind(Confirmed, Deaths, Recovered) ~ Date + `Country/Region`, tm_raw, sum)

names(tm)[2] <- "Country_Region"

tm$Active <- tm$Confirmed - tm$Deaths -tm$Recovered

tm <- tm[order(tm$Country_Region, tm$Date),]
tm$cmatch <- NA
tm$cmatch[1] <- FALSE
tm$cmatch[(2):length(tm$Date)] <- tm$Country_Region[(2):length(tm$Date)] ==
  tm$Country_Region[1:(length(tm$Date) - 1)]

tm$Delta_Confirmed <- cum2delta(tm, "Confirmed")
tm$Delta_Recovered <- cum2delta(tm, "Recovered")
tm$Delta_Active <- cum2delta(tm, "Active")
tm$Delta_Deaths <- cum2delta(tm, "Deaths")

tm$Rate_Confirmed <- (tm$Delta_Confirmed / tm$Confirmed) * 100
tm$Rate_Recovered <- (tm$Delta_Recovered / tm$Recovered) * 100
tm$Rate_Active <- (tm$Delta_Active / tm$Active) * 100
tm$Rate_Deaths <- (tm$Delta_Deaths / tm$Deaths) * 100


#### RKI Data #### URL may change !!!
rki_data <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
rki_full <- read_delim(rki_data, ",")

rki_full$Datenstand <- dmy(substr(rki_full$Datenstand, 1, 10))
rki_full$Meldedatum <- ymd(rki_full$Meldedatum) # convert to date
rki_full$Refdatum <- ymd(rki_full$Refdatum) # convert to date

names(rki_full)[names(rki_full) == "AnzahlFall"] <- "Delta_Confirmed" 
names(rki_full)[names(rki_full) == "AnzahlTodesfall"] <- "Delta_Deaths" 
# names(rki_full)[names(rki_full) == "Refdatum"] <- "Date"  # should this be used??
# names(rki_full)[names(rki_full) == "Meldedatum"] <- "Date"  
names(rki_full)[names(rki_full) == "Bundesland"] <- "Country_Region"
names(rki_full)[names(rki_full) == "AnzahlGenesen"] <- "Delta_Recovered"
names(rki_full)[names(rki_full) == "Geschlecht"] <- "Sex" 
rki_full$Sex[rki_full$Sex == "W"] <- "F"
rki_full$Sex[rki_full$Sex == "unbekannt"] <- "U"

rki_full$ddate <- rki_full$Meldedatum - rki_full$Refdatum
rki_full$Date <- rki_full$Meldedatum

rki_full$Delta_Active <- rki_full$Delta_Confirmed - rki_full$Delta_Deaths - rki_full$Delta_Recovered # correct?

rki_Datenstand <- max(rki_full$Datenstand)

# rki <- subset(rki_full, select = -c(IdBundesland, Landkreis, ObjectId, IdLandkreis, NeuerFall,
#                                     NeuerTodesfall, Datenstand, NeuGenesen))

#### RKI Bundesländer
rkia <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered) ~ Date + Country_Region, 
                  rki_full, sum)

rkia$Confirmed <- delta2cum(rkia, "Delta_Confirmed")
rkia$Deaths <- delta2cum(rkia, "Delta_Deaths")
rkia$Recovered <- delta2cum(rkia, "Delta_Recovered")

rkia$Active <- rkia$Confirmed - rkia$Deaths - rkia$Recovered
rkia$Delta_Active <- rkia$Delta_Confirmed - rkia$Delta_Deaths - rkia$Delta_Recovered  # correct?

rkia$Rate_Recovered <- (rkia$Delta_Recovered / rkia$Recovered) * 100
rkia$Rate_Active <- (rkia$Delta_Active / rkia$Active) * 100
rkia$Rate_Confirmed <- (rkia$Delta_Confirmed / rkia$Confirmed) * 100
rkia$Rate_Deaths <- (rkia$Delta_Deaths / rkia$Deaths) * 100

#### Germany (RKI)
rkig <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered, Delta_Active) ~ Date, 
                  rkia, sum)

rkig$Country_Region <- "Germany (RKI)"

rkig$Confirmed <- delta2cum(rkig, "Delta_Confirmed")
rkig$Deaths <- delta2cum(rkig, "Delta_Deaths")
rkig$Recovered <- delta2cum(rkig, "Delta_Recovered")
rkig$Active <- delta2cum(rkig, "Delta_Active")

rkig$Rate_Confirmed <- (rkig$Delta_Confirmed / rkig$Confirmed) * 100
rkig$Rate_Deaths <- (rkig$Delta_Deaths / rkig$Deaths) * 100
rkig$Rate_Recovered <- (rkig$Delta_Recovered / rkig$Recovered) * 100
rkig$Rate_Active <- (rkig$Delta_Active / rkig$Active) * 100

max_date <- max(max(tm$Date), max(rkig$Date))
min_date <- min(min(tm$Date), min(rkig$Date))

#### selected Landkreise
sel_lk <- c("SK Freiburg i.Breisgau", "LK Breisgau-Hochschwarzwald")

rki_lk <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered) ~ 
                      Date + Landkreis, 
                    subset(rki_full, Landkreis %in% sel_lk), 
                    sum)

names(rki_lk)[2] <- "Country_Region"

rki_lk$Confirmed <- delta2cum(rki_lk, "Delta_Confirmed")
rki_lk$Deaths <- delta2cum(rki_lk, "Delta_Deaths")
rki_lk$Recovered <- delta2cum(rki_lk, "Delta_Recovered")

rki_lk$Active <- rki_lk$Confirmed - rki_lk$Deaths - rki_lk$Recovered
rki_lk$Delta_Active <- rki_lk$Delta_Confirmed - rki_lk$Delta_Deaths - rki_lk$Delta_Recovered  # correct?

rki_lk$Rate_Recovered <- (rki_lk$Delta_Recovered / rki_lk$Recovered) * 100
rki_lk$Rate_Active <- (rki_lk$Delta_Active / rki_lk$Active) * 100
rki_lk$Rate_Confirmed <- (rki_lk$Delta_Confirmed / rki_lk$Confirmed) * 100
rki_lk$Rate_Deaths <- (rki_lk$Delta_Deaths / rki_lk$Deaths) * 100

#### Tessin
ch <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv", 
               col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                time = col_character()))


Ti <- subset(ch, 
             (abbreviation_canton_and_fl == "TI") &
               (! is.na(ncumul_conf)), 
             select = -c(time, source, abbreviation_canton_and_fl))

Ti <- Ti %>% rename(Confirmed = ncumul_conf)
Ti <- Ti %>% rename(Deaths = ncumul_deceased)
Ti <- Ti %>% rename(Recovered = ncumul_released)
Ti <- Ti %>% rename(Date = date)
Ti$Active <- Ti$Confirmed - Ti$Recovered - Ti$Deaths

Tis <- subset(Ti, select = -c(ncumul_tested, new_hosp, current_hosp, current_icu, current_vent))
# names(Ti)

Tis$Country_Region <- "Tessin"

Tis <- Tis[order(Tis$Country_Region, Tis$Date),]
Tis$cmatch <- NA
Tis$cmatch[1] <- FALSE
Tis$cmatch[(2):length(Tis$Date)] <- Tis$Country_Region[(2):length(Tis$Date)] ==
  Tis$Country_Region[1:(length(Tis$Date) - 1)]

Tis$Delta_Confirmed <- cum2delta(Tis, "Confirmed")
Tis$Delta_Recovered <- cum2delta(Tis, "Recovered")
Tis$Delta_Active <- cum2delta(Tis, "Active")
Tis$Delta_Deaths <- cum2delta(Tis, "Deaths")

Tis$Rate_Confirmed <- (Tis$Delta_Confirmed / Tis$Confirmed) * 100
Tis$Rate_Recovered <- (Tis$Delta_Recovered / Tis$Recovered) * 100
Tis$Rate_Active <- (Tis$Delta_Active / Tis$Active) * 100
Tis$Rate_Deaths <- (Tis$Delta_Deaths / Tis$Deaths) * 100


#### Total data set 
all <- bind_rows(tm, rkia, rkig, rki_lk, Tis) 

all <- all[order(all$Country_Region, all$Date),]

# Recovered Korrektur nach D. Kriesel
all$Rec_corr <- lag(all$Confirmed, 18) - all$Death - all$Recovered
all$Rec_corr[all$Country_Region != lag(all$Country_Region, 18, default = 0)] <- 0
all$Rec_corr[all$Rec_corr < 0] <- 0
all$Recovered <- all$Recovered + all$Rec_corr

# summary(all$Rec_corr)

all <- subset(all, select = -Rec_corr) 

#### countries with N of confirmed cases > c_limit
c_limit <- 0
tmc <- as.data.frame(unique(subset(bind_rows(tm, Tis), 
                                   Confirmed >= c_limit)$Country_Region) )
names(tmc)[1] <- "Country_Region"

### load population data
load("pop_data.Rdata")

cp <- merge(tmc, population, by = "Country_Region", all.x = TRUE)

cp <- subset(cp, ! is.na(cp$Population))

countries <- as.character(cp$Country_Region)
g_inh <- array(0, dim = length(countries), dimnames = list(countries))
g_inh[countries] <- cp$Population

countries <- c(countries, rki_countries)
inh <- c(g_inh, rki_inh)

closeAllConnections()

#### Infos
# https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

#### github
# https://happygitwithr.com/existing-github-first.html

#### tanh fit
#https://www.reddit.com/r/dataisbeautiful/comments/fs9tj2/oc_fit_tanh_to_covid19_data_provided_by_john/fm0731j/
#(a0 * (tanh((x + a1) * a2) + 1))

# RKI via json
# extremely slow 
#rki_json_file <- 'https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson'
#rki_json_data <- fromJSON(paste(readLines(rki_json_file), collapse=""))

#R0
# https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147

# save(rkig, file = "../Test/rki.Rdata")
