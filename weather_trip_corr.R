#Weather and Trip Correlation Plot

#Loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)

#reading in all relevant data files 

station_df <- read_csv("station.csv")

################################################################
#Loaded in trip file after outlier-removal 
################################################################
trip_clean2 <- readRDS("trip_clean2.rds")
trip_clean2 <- trip_clean2 %>% 
  mutate(start_date = as.POSIXct(start_date, format="%d/%m/%y")) %>%
  mutate(end_date = as.POSIXct(end_date, format="%d/%m/%y"))
################################################################
#Loaded in modified weather file after outlier-removal 
################################################################

weather_df_2 <- readRDS("weather_no_outliers.rds")

################################################################
#Correlation Plot Analysis
################################################################
#Joining station to trip dataset to obtain details on city, will be used afterwards as a UID
#Merged df assigned to station_trip object
station_trip <- left_join(trip_clean2,station_df, by=c("start_station_name" = "name"))
#removing irrelevant variables from dataframe 
station_trip <- station_trip %>% select(-c(id.x, bike_id, start_station_id, 
                                       end_station_id, subscription_type, zip_code,id.y,installation_date,
                                       start_station_name,end_station_name, dock_count, long, lat))

#creating dataframe grouped by number of trips per day per city, as weather-variables were provided per day
trip_data_count <- station_trip %>% group_by(start_date, city) %>% count() 
#joining the trip and weather data 
trip_weather_corr <- left_join(trip_data_count,weather_df_2, by=c("start_date" = "date", "city"))

#Correlation for San Francisco
san_fran <- as.data.frame(trip_weather_corr %>% filter(city == "San Francisco"))
san_fran <- san_fran %>%
  select(-c(duration, start_date, city, zip_code, events, end_date)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
corrplot(cor(san_fran))
str(san_fran)

#mountain view city 
mountain_view <- as.data.frame(trip_weather_corr %>% filter(city == "Mountain View"))
mountain_view <- mountain_view %>%
  select(-c(start_date,city, zip_code)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
#testing correlation values 
corrplot(cor(mountain_view))

#palo alto city 
palo_alto_view <- as.data.frame(trip_weather_corr %>% filter(city == "Palo Alto"))
palo_alto_view <- palo_alto_view %>%
  select(-c(start_date,city, zip_code)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
#testing correlation values 
corrplot(cor(palo_alto_view))

#Redwood City
redwood_city <- as.data.frame(trip_weather_corr %>% filter(city == "Redwood City"))
redwood_city <- redwood_city %>%
  select(-c(start_date,city, zip_code)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
#testing correlation values 
corrplot(cor(redwood_city))

#San Jose City
sanjose_city <- as.data.frame(trip_weather_corr %>% filter(city == "San Jose"))
sanjose_city <- sanjose_city %>%
  select(-c(start_date,city, zip_code)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
#testing correlation values 
corrplot(cor(sanjose_city))

