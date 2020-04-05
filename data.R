library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(RCurl)
library(reshape)

library(httr)
#data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-01-2020.csv")

#data[is.na(data)] <- "None"

#data$Country_Region <- if_else(data$Country_Region == 'United Kingdom' & data$Province_State != 'None', 'United Kingdom - Other', data$Country_Region)


#us_data <- data %>%
 # dplyr::filter(Country_Region == "US")

#data_country <- data %>%
  #dplyr::group_by(Country_Region) %>%
  #dplyr::summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Active = sum(Active))

#data_country <- data_country %>%
 # dplyr::filter(Confirmed >= 0) %>%
  #dplyr::mutate(completed = Deaths + Recovered) %>%
  #dplyr::mutate(recoveryPercentage = paste(round(Recovered / completed * 100, 2), "%")) %>%
  #dplyr::mutate(deathPercentage = paste(round(Deaths / completed * 100, 2), "%"))

#

gather_function <- function(url)
{
  data <- read_csv(url)
  
  data[is.na(data)] <- "None"
  
  data <- data.frame(data)
  
  data$Country.Region <- if_else(data$Country.Region == 'United Kingdom' & data$Province.State != 'None', 'United Kingdom - Other', data$Country.Region)
  
  data <- data %>%
    gather(variable, value, -Province.State, -Country.Region, -Lat, -Long) %>% 
    mutate(variable = gsub("X", "0", variable)) %>%
    mutate(variable = as.Date(variable, "%m.%d.%y"))
}

Confirmed <- gather_function("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
Deaths <- gather_function("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
Recoveries <- gather_function("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

figures <- left_join(Confirmed, Deaths, by = c("Province.State", "Country.Region", "Lat", "Long", "variable"))
figures <- left_join(figures, Recoveries, by = c("Province.State", "Country.Region", "Lat", "Long", "variable"))

names(figures)[1] <- "Province_State"
names(figures)[2] <- "Country_Region"
names(figures)[5] <- "Date"
names(figures)[6] <- "Confirmed"
names(figures)[7] <- "Deaths"
names(figures)[8] <- "Recovered"

figures <- figures %>%
  dplyr::mutate(Active = Confirmed - (Deaths + Recovered))


running_total <- figures %>%
  dplyr::filter(Date == max(Date)) %>%
  dplyr::summarise(TotalConfirmed = sum(Confirmed), TotalDeaths = sum(Deaths)) %>%
  dplyr::mutate(DeathPercentage = paste(round(TotalDeaths / TotalConfirmed * 100, 2), "%"))

overall_date <- figures %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(TotalConfirmed = sum(Confirmed), TotalDeaths = sum(Deaths)) %>%
  dplyr::mutate(DeathPercentage = paste(round(TotalDeaths / TotalConfirmed * 100, 2), "%")) %>%
  dplyr::mutate(increaseConfirmed = TotalConfirmed - lag(TotalConfirmed)) %>%
  dplyr::mutate(increasePConfirmed = paste(if_else(TotalConfirmed == 0, 0, round(increaseConfirmed / TotalConfirmed * 100, 2)), "%")) %>%
  dplyr::mutate(increaseDeaths = TotalDeaths - lag(TotalDeaths)) %>%
  dplyr::mutate(increasePDeaths = paste(if_else(TotalDeaths == 0, 0, round(increaseDeaths / TotalDeaths * 100, 2)), "%"))

country_date <- figures %>%
  dplyr::group_by(Country_Region, Date) %>%
  dplyr::summarise(TotalConfirmed = sum(Confirmed), TotalDeaths = sum(Deaths)) %>%
  dplyr::mutate(DeathPercentage = paste(if_else(TotalConfirmed == 0, 0, round(TotalDeaths / TotalConfirmed * 100, 2)), "%")) %>%
  dplyr::mutate(increaseConfirmed = TotalConfirmed - lag(TotalConfirmed)) %>%
  dplyr::mutate(increasePConfirmed = paste(if_else(TotalConfirmed == 0, 0, round(increaseConfirmed / TotalConfirmed * 100, 2)), "%")) %>%
  dplyr::mutate(increaseDeaths = TotalDeaths - lag(TotalDeaths)) %>%
  dplyr::mutate(increasePDeaths = paste(if_else(TotalDeaths == 0, 0, round(increaseDeaths / TotalDeaths * 100, 2)), "%"))

total_by_country <- figures %>%
  dplyr::filter(Date == max(Date)) %>%
  dplyr::group_by(Country_Region) %>%
  dplyr::summarise(TotalConfirmed = sum(Confirmed), TotalDeaths = sum(Deaths)) %>%
  dplyr::mutate(DeathPercentage = paste(if_else(TotalConfirmed == 0, 0, round(TotalDeaths / TotalConfirmed * 100, 2)), "%"))

figures_today <- figures %>%
  dplyr::filter(Date == max(Date))

figures_today <- figures_today[order(-figures_today$Deaths, figures_today$Country_Region),]

figures_today <- figures_today %>%
  dplyr::mutate(completed = Deaths + Recovered) %>%
  dplyr::mutate(recoveryPercentage = paste(round(Recovered / completed * 100, 2), "%")) %>%
  dplyr::mutate(deathPercentage = paste(round(Deaths / completed * 100, 2), "%"))

figures_today <- top_n(figures_today, 10, Confirmed)

figures_today <- figures_today[order(-figures_today$Confirmed, figures_today$Country_Region),]

figures_united_kingdom <- figures %>%
  dplyr::filter(Country_Region == 'United Kingdom') %>%
  dplyr::group_by(Country_Region, Date) %>%
  dplyr::summarise(TotalConfirmed = sum(Confirmed), TotalDeaths = sum(Deaths)) %>%
  dplyr::mutate(DeathPercentage = paste(if_else(TotalConfirmed == 0, 0, round(TotalDeaths / TotalConfirmed * 100, 2)), "%"))

figures_italy <- figures %>%
  dplyr::filter(Country_Region == 'Italy') %>%
  dplyr::mutate(DeathPercentage = paste(round(Deaths / Confirmed * 100, 2), "%"))
