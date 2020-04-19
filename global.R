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

cumFromDelta <- function(df, delta_col) {
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

tm$Delta_Confirmed <- 0
tm$Delta_Confirmed[2:length(tm$Delta_Confirmed)] <- diff(tm$Confirmed, 1)
tm$Delta_Confirmed[! tm$cmatch] <- tm$Confirmed[! tm$cmatch]

tm$Delta_Recovered <- 0
tm$Delta_Recovered[2:length(tm$Delta_Recovered)] <- diff(tm$Recovered, 1)
tm$Delta_Recovered[! tm$cmatch] <- tm$Recovered[! tm$cmatch]

tm$Delta_Active <- 0
tm$Delta_Active[2:length(tm$Delta_Active)] <- diff(tm$Active, 1)
tm$Delta_Active[! tm$cmatch] <- tm$Active[! tm$cmatch]

tm$Delta_Deaths <- 0
tm$Delta_Deaths[2:length(tm$Delta_Deaths)] <- diff(tm$Deaths, 1)
tm$Delta_Deaths[! tm$cmatch] <- tm$Deaths[! tm$cmatch]

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
names(rki_full)[names(rki_full) == "Refdatum"] <- "Date"  # should this be used??
# names(rki_full)[names(rki_full) == "Meldedatum"] <- "Date"  
names(rki_full)[names(rki_full) == "Bundesland"] <- "Country_Region"
names(rki_full)[names(rki_full) == "AnzahlGenesen"] <- "Delta_Recovered"
names(rki_full)[names(rki_full) == "Geschlecht"] <- "Sex" 
rki_full$Sex[rki_full$Sex == "W"] <- "F"
rki_full$Sex[rki_full$Sex == "unbekannt"] <- "U"

rki_full$Delta_Active <- rki_full$Delta_Confirmed - rki_full$Delta_Deaths - rki_full$Delta_Recovered # correct?

rki_Datenstand <- max(rki_full$Datenstand)

# rki <- subset(rki_full, select = -c(IdBundesland, Landkreis, ObjectId, IdLandkreis, NeuerFall,
#                                     NeuerTodesfall, Datenstand, NeuGenesen))

#### RKI Bundesländer
rkia <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered) ~ Date + Country_Region, 
                  rki_full, sum)

rkia$Confirmed <- cumFromDelta(rkia, "Delta_Confirmed")
rkia$Deaths <- cumFromDelta(rkia, "Delta_Deaths")
rkia$Recovered <- cumFromDelta(rkia, "Delta_Recovered")

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

rkig$Confirmed <- cumFromDelta(rkig, "Delta_Confirmed")
rkig$Deaths <- cumFromDelta(rkig, "Delta_Deaths")
rkig$Recovered <- cumFromDelta(rkig, "Delta_Recovered")
rkig$Active <- cumFromDelta(rkig, "Delta_Active")

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

rki_lk$Confirmed <- cumFromDelta(rki_lk, "Delta_Confirmed")
rki_lk$Deaths <- cumFromDelta(rki_lk, "Delta_Deaths")
rki_lk$Recovered <- cumFromDelta(rki_lk, "Delta_Recovered")

rki_lk$Active <- rki_lk$Confirmed - rki_lk$Deaths - rki_lk$Recovered
rki_lk$Delta_Active <- rki_lk$Delta_Confirmed - rki_lk$Delta_Deaths - rki_lk$Delta_Recovered  # correct?

rki_lk$Rate_Recovered <- (rki_lk$Delta_Recovered / rki_lk$Recovered) * 100
rki_lk$Rate_Active <- (rki_lk$Delta_Active / rki_lk$Active) * 100
rki_lk$Rate_Confirmed <- (rki_lk$Delta_Confirmed / rki_lk$Confirmed) * 100
rki_lk$Rate_Deaths <- (rki_lk$Delta_Deaths / rki_lk$Deaths) * 100

#### Total data set 
all <- bind_rows(tm, rkia, rkig, rki_lk) 

#### countries with N of confirmed cases > c_limit
c_limit <- 0
tmc <- as.data.frame(unique(subset(tm, Confirmed >= c_limit)$Country_Region) )
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
