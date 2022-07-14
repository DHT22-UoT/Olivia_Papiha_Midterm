# Olivia_Papiha_Midterm
Midterm assignment Special Topics

# EDA script for weather and trip

Run EDA for trip (Olivia) and weather (Papiha) 
In separate report - figures and comments 
- File for EDA for Trip: EDA_trip.R  
- File for EDA for Weather: EDA_Weather.R  

# Script 2 - Just trip - Olivia
- File for Trip: Just_trip.R  
 
Find the number of cancelled trips (trips less than 2 min). Record and remove from the data set. Just trip. 

Identify outliers in the data set.Record and remove from the data set. All data sets.

#1.5 *IQR  - Q1 OR 1.5*IQR + Q3

Establish the highest volume hours on weekdays. Used to build 'rush hours' into their model (lubridate package).Have to find the hours of weekdays where the trip volume is highest (eg. can try histograms). Just trip.

Determine the 10 most frequent starting stations and ending stations during the ‘rush hours’.

Determine the 10 most frequent starting stations and ending stations during weekends.

Calculate the average utilization of bikes for each month.
Total time used/total time in month.

# Script 3 - Just weather - Papiha
- File for Weather: weather_file.R  

Identify outliers in the data set.Record and remove from the data set. All data sets.
#1.5 *IQR  - Q1 OR 1.5*IQR + Q3


# Script 4 - Just station - Papiha
- File for Station: station_outliers.R  

Identify outliers in the data set.Record and remove from the data set. All data sets.
#1.5 *IQR  - Q1 OR 1.5*IQR + Q3


# Script 5 - Weather and trip - possibly station - Papiha
- File for weather and trip: weather_trip_corr.R  

Help decide whether temperature, weather events, visibility or other weather measurements has an impact on bike rental patterns. Trip and weather- possibly station. 

Create a correlation matrix for the new data set using cor() function from the corrplot package.Flag the highest correlations.

