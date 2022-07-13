#Weather and Trip Correlation Plot

#Loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)

#reading in all relevant data files 
trip_data <- read_csv("/Users/papihajoharapurkar/Downloads/babs\ 2/trip.csv")
station_df <- read_csv("station.csv")

################################################################
#Data transformations for Trip Data - copied from EDA_Trip File 
################################################################
# Find the number of cancelled trips (<2min) and remove from data set 
trip_clean <- trip_data %>%
  filter(duration >= 120)
# removed 2,499 trips

# Identify outliers in the data set. Record and remove from the data set.
iqr_trip <- IQR(trip_clean$duration)

Q1 <- quantile(trip_clean$duration, .25)
Q3 <- quantile(trip_clean$duration, .75)

up <- 1.5*iqr_trip + Q3 # Upper Range  

low <- 1.5*iqr_trip - Q1 # Lower Range

# trip_clean = 323840 obs
trip_clean2 <- trip_clean

trip_clean2$duration[trip_clean2$duration > 1352.5] <- 1352.5
trip_clean2$duration[trip_clean2$duration < 259.5] <- 259.5

# trip_clean2 = 258645 obs, removed 65,195 obs 

# So date will be recognized as a date
trip_clean2 <- trip_clean2 %>% 
  mutate(start_date = as.POSIXct(start_date, format="%d/%m/%y")) %>%
  mutate(end_date = as.POSIXct(end_date, format="%d/%m/%y"))

trip_data$start_station_name[trip_data$start_station_name == "Post at Kearny"] <- "Post at Kearney"
trip_data$start_station_name[trip_data$start_station_name == "Washington at Kearny"] <- "Washington at Kearney"

trip_data$end_station_name[trip_data$end_station_name == "Post at Kearny"] <- "Post at Kearney"
trip_data$end_station_name[trip_data$end_station_name == "Washington at Kearny"] <- "Washington at Kearney"

start_stat_tab <- data_frame(unique(trip_data$start_station_name))
start_id_tab <- data_frame(unique(trip_data$start_station_id))

# Found "Broadway at Main" and "Stanford in Redwood City" are both coded under station ID 80
# Combining them into one station name 
trip_data$start_station_name[trip_data$start_station_name == "Broadway at Main"] <- "Stanford in Redwood City"
trip_data$end_station_name[trip_data$end_station_name == "Broadway at Main"] <- "Stanford in Redwood City"


# Found "San Jose Government Center" and "Santa Clara County Civic Center" are both coded under station ID 25
# Combining them into one station name 
trip_data$start_station_name[trip_data$start_station_name == "San Jose Government Center"] <- "Santa Clara County Civic Center"
trip_data$end_station_name[trip_data$end_station_name == "San Jose Government Center"] <- "Santa Clara County Civic Center"

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
trip_data_count <- station_trip %>% group_by(start_date, city) %>% count() %>% rename(trip_freq = n)
#joining the trip and weather data 
trip_weather_corr <- left_join(trip_data_count,weather_df_2, by=c("start_date" = "date", "city"))

#removing character-names after joining 
trip_weather_corr <- trip_weather_corr %>% select(-c(start_date,city,events))

#Correlation for San Francisco
san_fran <- as.data.frame(trip_weather_corr %>% filter(city == "San Francisco"))
san_fran <- san_fran %>%
  select(-c(start_date,city, zip_code)) %>%
  mutate(cloud_cover = as.numeric(cloud_cover)) %>%
  filter(!is.na(max_visibility_miles)) %>%
  filter(!is.na(max_gust_speed_mph))
corrplot(cor(san_fran))

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

