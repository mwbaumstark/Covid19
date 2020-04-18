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

##### type of cases ####
ctype <- c("Confirmed Cases" , 
           "Deaths", 
           "Recovered Cases",
           "Active Cases" 
)

# Johns Hopkins Master Repository #############################
# from datahub.io (more simple repository)

# json_file <- 'https://datahub.io/core/covid-19/datapackage.json'
# json_data <- fromJSON(paste(readLines(json_file), collapse=""))
# tm_data = json_data$resources$path[4]

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

rki_Datenstand <- max(rki_full$Datenstand)

rki <- subset(rki_full, select = -c(IdBundesland, Landkreis, ObjectId, IdLandkreis, NeuerFall,
                                    NeuerTodesfall, Datenstand, NeuGenesen))

names(rki)[names(rki) == "AnzahlFall"] <- "Delta_Confirmed" 
names(rki)[names(rki) == "AnzahlTodesfall"] <- "Delta_Deaths" 
names(rki)[names(rki) == "Refdatum"] <- "Date"  # should this be used??
# names(rki)[names(rki) == "Meldedatum"] <- "Date"  
names(rki)[names(rki) == "Bundesland"] <- "Country_Region"
names(rki)[names(rki) == "AnzahlGenesen"] <- "Delta_Recovered"
names(rki)[names(rki) == "Geschlecht"] <- "Sex" 
rki$Sex[rki$Sex == "W"] <- "F"
rki$Sex[rki$Sex == "unbekannt"] <- "U"

rki$Delta_Active <- rki$Delta_Confirmed - rki$Delta_Deaths - rki$Delta_Recovered # correct?

rkia <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered) ~ Date + Country_Region, rki, sum)

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
rkia$Recovered[1] <- rkia$Delta_Recovered[1]

for (i in 2:length(rkia$Recovered)) {
  rkia$Recovered[i] = rkia$Recovered[i-1] + rkia$Delta_Recovered[i]
  if (rkia$Country_Region[i] != rkia$Country_Region[i-1]){
    rkia$Recovered[i] = rkia$Delta_Recovered[i]
  }
}


rkia$Active <- rkia$Confirmed - rkia$Deaths - rkia$Recovered

rkia$Delta_Active <- rkia$Delta_Confirmed - rkia$Delta_Deaths - rkia$Delta_Recovered  # correct?


rkia$Rate_Recovered <- (rkia$Delta_Recovered / rkia$Recovered) * 100
rkia$Rate_Active <- (rkia$Delta_Active / rkia$Active) * 100

rkia$Rate_Confirmed <- (rkia$Delta_Confirmed / rkia$Confirmed) * 100
rkia$Rate_Deaths <- (rkia$Delta_Deaths / rkia$Deaths) * 100

rkig <- aggregate(cbind(Delta_Confirmed, Delta_Deaths, Delta_Recovered, Delta_Active) ~ Date, rkia, sum)
rkig$Country_Region <- "Germany (RKI)"

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
rkig$Recovered[1] <- rkig$Delta_Recovered[1]

for (i in 2:length(rkig$Recovered)) {
  rkig$Recovered[i] = rkig$Recovered[i-1] + rkig$Delta_Recovered[i]
  if (rkig$Country_Region[i] != rkig$Country_Region[i-1]){
    rkig$Recovered[i] = rkig$Delta_Recovered[i]
  }
}

rkig$Active <- 0
rkig$Active[1] <- rkig$Delta_Active[1]

for (i in 2:length(rkig$Active)) {
  rkig$Active[i] = rkig$Active[i-1] + rkig$Delta_Active[i]
  if (rkig$Country_Region[i] != rkig$Country_Region[i-1]){
    rkig$Active[i] = rkig$Delta_Active[i]
  }
}

rkig$Rate_Confirmed <- (rkig$Delta_Confirmed / rkig$Confirmed) * 100
rkig$Rate_Deaths <- (rkig$Delta_Deaths / rkig$Deaths) * 100
rkig$Rate_Recovered <- (rkig$Delta_Recovered / rkig$Recovered) * 100
rkig$Rate_Active <- (rkig$Delta_Active / rkig$Active) * 100

max_date <- max(max(tm$Date), max(rkig$Date))
min_date <- min(min(tm$Date), min(rkig$Date))

#### Total data set 
all <- bind_rows(tm, rkia, rkig) 

#### countries with N of confirmed cases > c_limit
c_limit <- 0
tmc <- as.data.frame(unique(subset(tm, Confirmed >= c_limit)$Country_Region) )
names(tmc)[1] <- "Country_Region"

### load population data
load("pop_data.Rdata")

cp <- merge(tmc, population, by = "Country_Region", all.x = TRUE)

# print(cp$Country_Region[is.na(cp$Population)]) # DEBUG

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

#### Bugs
# Crash if no country selected

# save(rkig, file = "../Test/rki.Rdata")
