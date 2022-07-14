#Station Outlier Removal and the Identification of Stations not in Trip Data 

#Importing libraries
library(tidyverse)
library(dplyr)

#reading in station file 
station_df <- read.csv("station.csv")

#structure of file 
str(station_df)

#summary of file 
summary(station_df)

#determining number of unique station-names from station-file 
nrow(unique(station_df))

#reading in trip file
trip_file <- read.csv("trip.csv")
#determining number of unique station-names from trip-file
length(unique(trip_file$end_station_name))

#determining which stations have not been coded in station_file with anti_join function
#the trip_file was used as the left-file because it has greater values 
unique_stations_end <- anti_join(trip_file,station_df, by=c("end_station_name" = "name"))
#the stations not encoded in station_file were retrieved with unique function
unique(unique_stations_end$end_station_name)

