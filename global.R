library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)
library(curl)

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

warn_msg <- array(data = "")

# Johns Hopkins Master Repository #############################

# Original repo #############################
confirmed_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
confirmed <- read_csv(confirmed_data)

cm <- melt(confirmed, id.vars = c("Province/State", "Country/Region", "Lat", "Long" ),
           value.name = "Confirmed", variable.name = "Date")
names(cm)[2] <- "Country_Region"
cm <- subset(cm, select = c("Country_Region", "Date", "Confirmed") )
cm <- aggregate(Confirmed ~ Country_Region + Date, cm, sum)
cm$Date <-  mdy(cm$Date)


deaths_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths <- read_csv(deaths_data)

dm <- melt(deaths, id.vars = c("Province/State", "Country/Region", "Lat", "Long" ),
           value.name = "Deaths", variable.name = "Date")
names(dm)[2] <- "Country_Region"
dm <- subset(dm, select = c("Country_Region", "Date", "Deaths") )
dm <- aggregate(Deaths ~ Country_Region + Date, dm, sum)
dm$Date <-  mdy(dm$Date)


recovered_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recovered <- read_csv(recovered_data)

rm <- melt(recovered, id.vars = c("Province/State", "Country/Region", "Lat", "Long" ),
           value.name = "Recovered", variable.name = "Date")
names(rm)[2] <- "Country_Region"
rm <- subset(rm, select = c("Country_Region", "Date", "Recovered") )
rm <- aggregate(Recovered ~ Country_Region + Date, rm, sum)
rm$Date <-  mdy(rm$Date)

tmp <- merge(cm, dm, by= c("Country_Region", "Date"), all = TRUE)
tm <- merge(tmp, rm, by= c("Country_Region", "Date"), all = TRUE)



# # from datahub.io (more simple repository) #### 2020-04-30: not up to date
# 
# tm_data <- "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"
# 
# if (exists("tm_raw")) rm("tm_raw")
# 
# try(tm_raw <- read_csv(url(tm_data)), silent = TRUE)
# 
# if (exists("tm_raw")) {
#   save(tm_raw, file = "tm_raw.Rdata")
#   warn_msg[length(warn_msg) + 1] <- "* J. Hopkins data ok *"
# } else {
#   warn_msg[length(warn_msg) + 1] <- "* J. Hopkins down. Use cached data *"
#   load("tm_raw.Rdata")
# }
# 
# tm <- aggregate(cbind(Confirmed, Deaths, Recovered) ~ Date + `Country/Region`, tm_raw, sum)
# 
# names(tm)[2] <- "Country_Region"

######### 

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

warn_msg[length(warn_msg) + 1] <- paste("- J.Hopkins data from:", max(tm$Date) + 1) 

#### RKI Data #### URL may change !!!
rki_data <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
# rki_data <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
# rki_data <- "RKI_COVID19.csv"
# rki_data <- ""
# https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74

if (exists("rki_full")) rm("rki_full")

try(rki_full <- read_delim(rki_data, ","), silent = TRUE)
load("dim_rki_full.Rdata")
if (exists("rki_full")) {
  dim_rki <- dim(rki_full)
  load("dim_rki_full.Rdata")
  if ((dim_rki[1] >= dim_rki_save[1]) & (dim_rki[2] == dim_rki_save[2])) {
    dim_rki_save <- dim(rki_full)
    rki_full$Datenstand <- dmy(substr(rki_full$Datenstand, 1, 10))
    save(dim_rki_save, file = "dim_rki_full.Rdata")
    save(rki_full, file = "rki_full.Rdata")
    warn_msg[length(warn_msg) + 1] <- paste("- RKI data from:", head(rki_full$Datenstand,1)) 
  } else {
    if (dim_rki[1] <= dim_rki_save[1]) warn_msg[length(warn_msg) + 1] <- "***RKI Data truncated***"
    if (dim_rki[2] != dim_rki_save[2]) warn_msg[length(warn_msg) + 1] <- "***Layout of RKI Data changed***"
    load("rki_full.Rdata")
    warn_msg[length(warn_msg) + 1] <- paste("* Cached data used: ", head(rki_full$Datenstand,1)) 
  } 
} else {
  load("rki_full.Rdata")
  warn_msg[length(warn_msg) + 1] <- paste("* RKI down. Cached: ", head(rki_full$Datenstand,1)) 
}

rki_full$Datenstand <- dmy(substr(rki_full$Datenstand, 1, 10))
rki_full$Meldedatum <- ymd(substr(rki_full$Meldedatum, 1, 10)) # convert to date
rki_full$Refdatum <- ymd(substr(rki_full$Refdatum, 1, 10)) # convert to date

names(rki_full)[names(rki_full) == "AnzahlFall"] <- "Delta_Confirmed" 
names(rki_full)[names(rki_full) == "AnzahlTodesfall"] <- "Delta_Deaths" 
names(rki_full)[names(rki_full) == "Bundesland"] <- "Country_Region"
names(rki_full)[names(rki_full) == "AnzahlGenesen"] <- "Delta_Recovered"
names(rki_full)[names(rki_full) == "Geschlecht"] <- "Sex" 
rki_full$Sex[rki_full$Sex == "W"] <- "F"
rki_full$Sex[rki_full$Sex == "unbekannt"] <- "U"

# rki_full$ddate <- rki_full$Meldedatum - rki_full$Refdatum
# rki_full$Date <- rki_full$Meldedatum
rki_full$Date <- rki_full$Refdatum

rki_full$Delta_Active <- rki_full$Delta_Confirmed - rki_full$Delta_Deaths - rki_full$Delta_Recovered # correct?

# if (unique(rki_full$Altersgruppe2)[1] != "nicht übermittelt") { # geht nicht ohne weiteres dynamisch
#   rki_full$Altersgruppe <- rki_full$Altersgruppe2
# }

# rki_Datenstand <- max(rki_full$Datenstand)

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
if (exists("ch")) rm("ch")

try(
  ch <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv", 
                 col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                  time = col_character()))
  , silent = TRUE)

if (exists("ch")) {
  save(ch, file = "ch.Rdata")
  warn_msg[length(warn_msg) + 1] <- paste("- openZH data from:", max(ch$date)) 
} else {
  load("ch.Rdata")
  warn_msg[length(warn_msg) + 1] <- paste("* openZH down. Cached: ", max(ch$date)) 
}

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

#### population data

tmc <- as.data.frame(unique(bind_rows(tm, Tis)))
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

lw <- length(warn_msg)

#### Data adjustments

all <- all[order(all$Country_Region, all$Date),]

# Recovered Korrektur nach D. Kriesel für Johns Hopkins und CH Daten
all$Rec_corr <- lag(all$Confirmed, 18) - all$Death - all$Recovered
all$Rec_corr[all$Country_Region != lag(all$Country_Region, 18, default = 0)] <- 0
all$Rec_corr[all$Rec_corr < 0] <- 0
all$Recovered[! (all$Country_Region %in% rki_countries)] <-
  all$Recovered[! (all$Country_Region %in% rki_countries)] +
  all$Rec_corr[! (all$Country_Region %in% rki_countries)]
# summary(all$Rec_corr)
all <- subset(all, select = -Rec_corr)

# # Johns Hopkins like Recovered für RKI:
rki_delay <- 18
# all$RecCalc <- lag(all$Confirmed, rki_delay) - all$Death
all$RecCalc <- lag(all$Recovered, rki_delay)
all$RecCalc[all$Country_Region != lag(all$Country_Region, rki_delay, default = 0)] <- 0
all$RecCalc[all$RecCalc < 0] <- 0
all$Recovered[all$Country_Region %in% rki_countries] <-
  all$RecCalc[all$Country_Region %in% rki_countries]

all$Active <- all$Confirmed - all$Deaths - all$Recovered

all$cmatch <- NA  # move to function!
all$cmatch[1] <- FALSE
all$cmatch[(2):length(all$Date)] <- all$Country_Region[(2):length(all$Date)] ==
  all$Country_Region[1:(length(all$Date) - 1)]

all$Delta_Recovered <- cum2delta(all, "Recovered")
all$Delta_Active <- cum2delta(all, "Active")
all$Rate_Recovered <- (all$Delta_Active / all$Recovered) * 100
all$Rate_Active <- (all$Delta_Active / all$Active) * 100

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

# # check Recovered calculations
# dfplt1 <- subset(all, Country_Region == "Germany")
# dfplt2 <- subset(all, Country_Region == "Germany (RKI)")
# 
# ggplot(data = dfplt1, aes(x=Date, y=Confirmed, color = "Confirmed")) +  
#   geom_line() +
#   geom_line(aes(y=Recovered, color = "Recovered")) +
#   geom_point(data = dfplt2) +
#   geom_point(data = dfplt2, aes(y=Recovered, color = "Recovered")) +
#   geom_line(aes(y=Active, color = "Active")) +
#   geom_point(data = dfplt2, aes(y=Active, color = "Active")) +
#   geom_line(aes(y=Deaths, color = "Deaths")) +
#   geom_point(data = dfplt2, aes(y=Deaths, color = "Deaths")) +
#   xlim(date("2020-03-01"), date("2020-05-01"))



