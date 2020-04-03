library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(reshape2)
library(readxl)
library(httr)
library(xml2)
library(htmltab)
library(ggplot2)


#compare with NAs
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

# Johns Hopkins Master Repository #############################
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
tm$Active <- tm$Confirmed - tm$Deaths -tm$Recovered

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
tm$Rate_Confirmed[compareLE(tm$Rate_Confirmed, 0)] <- NA
tm$Rate_Recovered <- (tm$Delta_Recovered / tm$Recovered) * 100
tm$Rate_Recovered[compareLE(tm$Rate_Recovered, 0)] <- NA
tm$Rate_Active <- (tm$Delta_Active / tm$Active) * 100
tm$Rate_Active[compareLE(tm$Rate_Active, 0)] <- NA
tm$Rate_Deaths <- (tm$Delta_Deaths / tm$Deaths) * 100
tm$Rate_Deaths[compareLE(tm$Rate_Deaths, 0)] <- NA


# # Johns Hopkins Web repository ######################################
# ts_data <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/web-data/data/cases_time.csv"
# ts <- read_csv(ts_data)
# names(ts)[2] <- "Date"
# ts$Date <-  mdy(ts$Date)
# 
# ts$cmatch <- NA
# ts$cmatch[1] <- FALSE
# ts$cmatch[(2):length(ts$Date)] <- ts$Country_Region[(2):length(ts$Date)] ==
#   ts$Country_Region[1:(length(ts$Date) - 1)]
# 
# ts$Delta_Active <- 0
# ts$Delta_Active[2:length(ts$Delta_Active)] <- diff(ts$Active, 1)
# ts$Delta_Active[! ts$cmatch] <- ts$Active[! ts$cmatch]
# 
# ts$Delta_Deaths <- 0
# ts$Delta_Deaths[2:length(ts$Delta_Deaths)] <- diff(ts$Deaths, 1)
# ts$Delta_Deaths[! ts$cmatch] <- ts$Deaths[! ts$cmatch]
# 
# ts$Rate_Confirmed <- (ts$Delta_Confirmed / ts$Confirmed) * 100
# ts$Rate_Confirmed[compareLE(ts$Rate_Confirmed, 0)] <- NA
# ts$Rate_Recovered <- (ts$Delta_Recovered / ts$Recovered) * 100
# ts$Rate_Recovered[compareLE(ts$Rate_Recovered, 0)] <- NA
# ts$Rate_Active <- (ts$Delta_Active / ts$Active) * 100
# ts$Rate_Active[compareLE(ts$Rate_Active, 0)] <- NA
# ts$Rate_Deaths <- (ts$Delta_Deaths / ts$Deaths) * 100
# ts$Rate_Deaths[compareLE(ts$Rate_Deaths, 0)] <- NA


# ECDC repository #########################
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")

#current day not always present
try(
  status <-  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
)
if (status$status_code != 200) {
  #get day before today 
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date()-1, "%Y-%m-%d"), ".xlsx", sep = "")
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
}

#read the Dataset sheet 
ecdc_raw <- read_excel(tf)

#rename countries to match Johns Hopkins
ecdc_raw$countriesAndTerritories <- gsub("_", " ", ecdc_raw$countriesAndTerritories, fixed = TRUE)
ecdc_raw$countriesAndTerritories[ecdc_raw$countriesAndTerritories == "South Korea"] <- "Korea, South"
ecdc_raw$countriesAndTerritories[ecdc_raw$countriesAndTerritories == "United States of America"] <- "US"
ecdc_raw$countriesAndTerritories[ecdc_raw$countriesAndTerritories == "Czech Republic"] <- "Czechia"

ecdc <- subset(ecdc_raw, select = c("dateRep", "cases", "deaths", "countriesAndTerritories"))
names(ecdc) <- c("Date", "Delta_Confirmed", "Delta_Deaths", "Country_Region")

ecdc <- ecdc[order(ecdc$Country_Region, ecdc$Date),]

ecdc$Confirmed <- 0
ecdc$Confirmed[1] <- ecdc$Delta_Confirmed[1]

for (i in 2:length(ecdc$Confirmed)) {
  ecdc$Confirmed[i] = ecdc$Confirmed[i-1] + ecdc$Delta_Confirmed[i]
  if (ecdc$Country_Region[i] != ecdc$Country_Region[i-1]){
    ecdc$Confirmed[i] = ecdc$Delta_Confirmed[i]
  }
}

ecdc$Deaths <- 0
ecdc$Deaths[1] <- ecdc$Delta_Deaths[1]

for (i in 2:length(ecdc$Deaths)) {
  ecdc$Deaths[i] = ecdc$Deaths[i-1] + ecdc$Delta_Deaths[i]
  if (ecdc$Country_Region[i] != ecdc$Country_Region[i-1]){
    ecdc$Deaths[i] = ecdc$Delta_Deaths[i]
  }
}

ecdc$Recovered <- 0
ecdc$Active <- 0
ecdc$Delta_Recovered <- 0
ecdc$Delta_Active <- 0
ecdc$Rate_Recovered <- 0
ecdc$Rate_Active <- 0

ecdc$Rate_Confirmed <- (ecdc$Delta_Confirmed / ecdc$Confirmed) * 100
ecdc$Rate_Confirmed[compareLE(ecdc$Rate_Confirmed, 0)] <- NA
ecdc$Rate_Deaths <- (ecdc$Delta_Deaths / ecdc$Deaths) * 100
ecdc$Rate_Deaths[compareLE(ecdc$Rate_Deaths, 0)] <- NA

# RKI Data
# warning("vor RKI Daten") #DEBUG
rki_data <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
rki_full <- read_delim(rki_data, ",")

rki_full$Datenstand <- dmy(substr(rki_full$Datenstand, 1, 10))

rki_Datenstand <- max(rki_full$Datenstand)

rki <- subset(rki_full, select = -c(IdBundesland, Landkreis, ObjectId, IdLandkreis, NeuerFall,
                                    NeuerTodesfall, Datenstand))

names(rki)[names(rki) == "AnzahlFall"] <- "Delta_Confirmed" 
names(rki)[names(rki) == "AnzahlTodesfall"] <- "Delta_Deaths" 
names(rki)[names(rki) == "Meldedatum"] <- "Date"
names(rki)[names(rki) == "Bundesland"] <- "Country_Region"

rkia <- aggregate(cbind(Delta_Confirmed, Delta_Deaths) ~ Date + Country_Region, rki, sum)

rkia$Confirmed <- 0
rkia$Confirmed[1] <- rkia$Delta_Confirmed[1]
for (i in 2:length(rkia$Confirmed)) {
  rkia$Confirmed[i] = rkia$Confirmed[i-1] + rkia$Delta_Confirmed[i]
  if (rkia$Country_Region[i] != rkia$Country_Region[i-1]){
    rkia$Confirmed[i] = rkia$Delta_Confirmed[i]
  }
}

rkia$Deaths <- 0
rkia$Deaths[1] <- rkia$Delta_Deaths[1]

for (i in 2:length(rkia$Deaths)) {
  rkia$Deaths[i] = rkia$Deaths[i-1] + rkia$Delta_Deaths[i]
  if (rkia$Country_Region[i] != rkia$Country_Region[i-1]){
    rkia$Deaths[i] = rkia$Delta_Deaths[i]
  }
}

rkia$Recovered <- 0
rkia$Active <- 0
rkia$Delta_Recovered <- 0
rkia$Delta_Active <- 0
rkia$Rate_Recovered <- 0
rkia$Rate_Active <- 0

rkia$Rate_Confirmed <- (rkia$Delta_Confirmed / rkia$Confirmed) * 100
rkia$Rate_Confirmed[compareLE(rkia$Rate_Confirmed, 0)] <- NA
rkia$Rate_Deaths <- (rkia$Delta_Deaths / rkia$Deaths) * 100
rkia$Rate_Deaths[compareLE(rkia$Rate_Deaths, 0)] <- NA

rkig <- aggregate(cbind(Delta_Confirmed, Delta_Deaths) ~ Date, rki, sum)
rkig$Country_Region <- "Germany"

rkig$Confirmed <- 0
rkig$Confirmed[1] <- rkig$Delta_Confirmed[1]

for (i in 2:length(rkig$Confirmed)) {
  rkig$Confirmed[i] = rkig$Confirmed[i-1] + rkig$Delta_Confirmed[i]
  if (rkig$Country_Region[i] != rkig$Country_Region[i-1]){
    rkig$Confirmed[i] = rkig$Delta_Confirmed[i]
  }
}

rkig$Deaths <- 0
rkig$Deaths[1] <- rkig$Delta_Deaths[1]

for (i in 2:length(rkig$Deaths)) {
  rkig$Deaths[i] = rkig$Deaths[i-1] + rkig$Delta_Deaths[i]
  if (rkig$Country_Region[i] != rkig$Country_Region[i-1]){
    rkig$Deaths[i] = rkig$Delta_Deaths[i]
  }
}

rkig$Recovered <- 0
rkig$Active <- 0
rkig$Delta_Recovered <- 0
rkig$Delta_Active <- 0
rkig$Rate_Recovered <- 0
rkig$Rate_Active <- 0

rkig$Rate_Confirmed <- (rkig$Delta_Confirmed / rkig$Confirmed) * 100
rkig$Rate_Confirmed[compareLE(rkig$Rate_Confirmed, 0)] <- NA
rkig$Rate_Deaths <- (rkig$Delta_Deaths / rkig$Deaths) * 100
rkig$Rate_Deaths[compareLE(rkig$Rate_Deaths, 0)] <- NA

max_date <- max(max(ecdc$Date), max(rkig$Date))
max_date <- max(max_date, max(tm$Date)) 
min_date <- min(min(tm$Date), min(ecdc$Date), min(rkig$Date))

######################################################################
ctype <- c("Confirmed Cases" , 
           "Deaths", 
           "Recovered Cases",
           "Active Cases" # ,
           # "Deaths / Confirmed Case"
)

# Wichtige Länder (cases > 1000) und deren Einwohnerzahlen
# tsc <- as.data.frame(unique(subset(ts, Confirmed > 1000)$Country_Region) )
# names(tsc)[1] <- "Country_Region"

tmc <- as.data.frame(unique(subset(tm, Confirmed > 1000)$Country_Region) )
names(tmc)[1] <- "Country_Region"

# ctr1 <- merge(tmc, tsc, by = "Country_Region", all = TRUE)

ecdcc <- as.data.frame(unique(subset(ecdc, Confirmed > 1000)$Country_Region) )
names(ecdcc)[1] <- "Country_Region"

population <- aggregate(popData2018 ~ countriesAndTerritories, 
                        subset(ecdc_raw, !is.na(popData2018)), mean )
# check_sd <- aggregate(popData2018 ~ countriesAndTerritories, #DEBUG
#                       subset(ecdc_raw, !is.na(popData2018)), sd) #DEBUG

names(population) <- c("Country_Region", "Population")

population$Population <- population$Population / 1000000

pc <- as.data.frame(unique(population$Country_Region) )
names(pc)[1] <- "Country_Region"

ctr2 <- merge(ecdcc, population, by = "Country_Region", all.x = TRUE)

cp <- merge(tmc, ctr2, by = "Country_Region", all = TRUE)

# print(cp$Country_Region[is.na(cp$Population)]) # DEBUG

cp <- subset(cp, ! is.na(cp$Population))

countries <- as.character(cp$Country_Region)
g_inh <- array(0, dim = length(countries), dimnames = list(countries))
g_inh[countries] <- cp$Population

##########
rki_countries <- as.character(unique(rkia$Country_Region))
rki_inh <- array(0, dim = length(rki_countries), dimnames = list(rki_countries))
rki_inh["Baden-Württemberg"] <- 11.07 
rki_inh["Bayern"] <- 13.08
rki_inh["Berlin"] <- 3.6
rki_inh["Brandenburg"] <- 2.52
rki_inh["Bremen"] <- 0.68
rki_inh["Hamburg"] <- 1.84
rki_inh["Hessen"] <- 6.27
rki_inh["Mecklenburg-Vorpommern"] <- 1.61
rki_inh["Niedersachsen"] <- 7.98
rki_inh["Nordrhein-Westfalen"] <- 17.93
rki_inh["Rheinland-Pfalz"] <- 4.09
rki_inh["Saarland"] <- 0.99
rki_inh["Sachsen"] <- 4.08
rki_inh["Sachsen-Anhalt"] <- 2.21
rki_inh["Schleswig-Holstein"] <- 2.14
rki_inh["Thüringen"] <- 2.14
rki_inh["Germany"] <- 82.79

closeAllConnections()


###################### ToDo
# https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

###################### github:
# https://happygitwithr.com/existing-github-first.html

#https://www.reddit.com/r/dataisbeautiful/comments/fs9tj2/oc_fit_tanh_to_covid19_data_provided_by_john/fm0731j/
#(a0 * (tanh((x + a1) * a2) + 1))
