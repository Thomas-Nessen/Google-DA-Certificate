# Case Study Google Data Analytics Certificate
# Cyclist - Bike sharing company
# The companies rents out bike from multiple locations throughout Chicago.
# Customers are either a member or a causal user and can use a bike for as long as they want and return the bike in one of the stations.

# Goal: Determine usage differences between causal riders and annual members.
# Period: 05/2020 - 05/2021
# Important Case Alteration: As I'm analyzing this case on my laptop with a free R studio download I ran in the processing power issues. 
# Alteration: I removed odd months to limit the amount of data.

# Library & packages ----
install.packages('tidyverse') 
install.packages('lubridate')
install.packages('ggplot2')
install.packages('rvest')
install.packages("stargazer")
install.packages("dplyr")
install.packages("ggstatsplot") 

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rvest)
library(stargazer)
library(dplyr)
library(ggstatsplot)

# Set Working Directory ----
getwd()
# backslashes are escape characters so two \\ is one \
wd_path <- "C:\\Users\\Gebruiker\\Documents\\Overig\\Google DA certificate\\Case_studies\\Case1_Divvy_Bike-Share"
wd_path_2 <- paste0(wd_path, "\\Raw_zips_bike_trips")
setwd(wd_path)


# Loading files ----
# In the next chunk I am reading in all the files as data frames (DFs). As there are a multitude of files I'm running through all files in the filepath.
# All the DF names are put in a list so I can loop through them for further checks.

#list if file names
file_list <- list.files(path = wd_path_2)
#empty list for adding data frames
list_dfs = list()
# create df for each csv file 
for (i in 1:length(file_list)){
  # Get Windows file path
  file_path <- paste0(wd_path_2,"\\",file_list[i],"\\",file_list[i],".csv")
  temp_data <- read.csv(file_path)
  #for organized dubble digit ordering (01, 02, ..., 10, 11)
  if ( i < 10) {
    assign(paste0("df_m0", i), temp_data)  
    list_dfs <- append(list_dfs, list(temp_data))
  } else {
    assign(paste0("df_m", i), temp_data)  
    list_dfs <- append(list_dfs, list(temp_data))
  }
  remove(temp_data)
}


# WRANGLE & COMBINE DATAFRAMES ----
# To combine all the DF's into one all the columns need to be the same in name and data type.
# With many files and columns it hard to manually check if they're all the same so using a loop is very useful.
# In this case study the number of files isn't too great but I added it anyway in order to better understand how to use lists & vectors, loops and breaks  in R when comparing values.

cols_are_same = TRUE
for (i in 1:(length(list_dfs) - 1)){
# This creates a vector of Boolean values comparing if the columns of two data frames are the same.
  col_names_same <- colnames(list_dfs[[i]]) == colnames(list_dfs[[i + 1]])
  #Go trough the vector to check if they are all TRUE
  for (j in 1:length(col_names_same)){
    if (col_names_same[j] == FALSE){ 
      print(paste('Columns are not the same in data frame:', i, " or", i + 1, " column:", j))
      cols_are_same = FALSE
      stop("Let's break out of this loop and fix the column!")
    }        
  }
}
if (cols_are_same == TRUE){print('All columns in all data frames are the same')}

# Tried to aggerate all the different DF's in one.
# not all data types are the same so we have to see which column in which df is different
# Thrown Error: Error: Can't combine `..1$start_station_id` <integer> and `..5$start_station_id` <character>.
bind_rows(list_dfs)

# The error above suggested that the data types of the station ID's are not the same.  
for(i in 1:length(list_dfs)){
  # Change start_station_id from integer to character type
  if (is.character(list_dfs[[i]]$start_station_id) == FALSE){
    list_dfs[[i]]$start_station_id <- as.character(list_dfs[[i]]$start_station_id)
  }
  # Change end_station_id from integer to character type
  if (is.character(list_dfs[[i]]$end_station_id) == FALSE){
    list_dfs[[i]]$end_station_id   <- as.character(list_dfs[[i]]$end_station_id)
  }
}

#Now that all the DF's are in the same format we can combine everything and remove monthly dataframes
df_all_trips <- bind_rows(list_dfs)
remove(df_m01, df_m02, df_m03, df_m04, df_m05, df_m06, df_m07, list_dfs)



# CLEANING DATA ----
# In this section I'm inspecting and cleaning the data to make it ready for analysis. I'm making sure missing data and outliers are dealt with.
# In addition, I am creating a "length of ride" by processing the time stamp variable. 


# Structure
# NOTE: Shows station ID numbers as character types this is due to a few station ID's beginning with letters.
str(df_all_trips)

# Summary 
# NOTE: shows NA's in end latitude and longitude column.
summary(df_all_trips) 

#dimensions of the dataframe
dim(df_all_trips)

#Check if there aren't any other types of Bikes or customers 
unique(df_all_trips[c("rideable_type")]) #Docked, electric & classic
unique(df_all_trips[c("member_casual")]) #Casual, member

# Create an Master dataframe before altering the dataset.
df_all_trips_master <- df_all_trips

# Remove coordinate columns Longitude and Latitude data. We don't need those. 
df_all_trips <- df_all_trips %>% select(-c(start_lat, end_lat, start_lng, end_lng))

#Test rides should be removed. Here I search through the start and end station names for the word "TEST".
find_string <- "TEST"
df_all_trips %>% filter(grepl(find_string, df_all_trips$start_station_name, ignore.case = TRUE)| 
                        grepl(find_string, df_all_trips$end_station_name  , ignore.case = TRUE))

# Station ID 671 is for Hubbard st Bike Checking 
# Station ID 676 is for Watson Testing - DIVVY
# Removes # removed (2401391 - 2329408) = 4897 71983 rows
df_all_trips <- df_all_trips %>% filter(!((start_station_id == "671") | (end_station_id == "671") |
                                          (start_station_id == "676") | (end_station_id == "676")))


# LENGTH OF RIDE VARIABLE
#Set start- and endtime as date-time objects
df_all_trips$started_at  <- as_datetime(df_all_trips$started_at)   
df_all_trips$ended_at    <- as_datetime(df_all_trips$ended_at)
# 
df_all_trips$date          <- as.Date(df_all_trips$started_at) #The default format is yyyy-mm-dd
df_all_trips$month         <- format(as.Date(df_all_trips$date), "%m")
df_all_trips$day           <- format(as.Date(df_all_trips$date), "%d")
df_all_trips$year          <- format(as.Date(df_all_trips$date), "%Y")
df_all_trips$day_of_week   <- format(as.Date(df_all_trips$date), "%A")
df_all_trips$month_of_year <- format(as.Date(df_all_trips$date), "%B")

# Ordering the days of the week for nicer presentation in the table.
# Note: my laptop is set in Dutch so days and months had to be set to Dutch also.
df_all_trips$day_of_week   <- ordered(df_all_trips$day_of_week, levels=c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag"))
df_all_trips$month_of_year <- ordered(df_all_trips$month_of_year, levels=c("januari", "februari","maart", "april", "mei", "juni", "juli",  "augustus", "september", "oktober", "november", "december"))

#Calculate the length of each ride in seconds
df_all_trips$ride_length_sec <- difftime(df_all_trips$ended_at,df_all_trips$started_at)
#Change data type to numeric so we can calculate 
df_all_trips$ride_length_sec <- as.numeric(as.character(df_all_trips$ride_length_sec))

# remove rides with a negative ride length
# removed (2329408 - 2324511) = 4897
df_all_trips <- df_all_trips[!(df_all_trips$ride_length<0),]


# DETERMINING AND REMOVING OUTLIERS
###
# Not all users use the bike the same. Most users use the bikes for short trips from A to B or a small trip.
# However some users use these bikes for far longer, half a day to a month (potentially corrupted datapoint).
# Down below I'm determining which users are outliers and which are regular users.
Q <- quantile(df_all_trips$ride_length_sec, probs=c(.25, .75), na.rm = FALSE) # 25% (Q1) & 75%(Q3) quantile
iqr <- IQR(df_all_trips$ride_length_sec) # Inner quantile range (Q3 - Q1)
up <-  Q[2]+1.5*iqr # Upper bound = 3294 sec 
low<- Q[1]-1.5*iqr # Lower bound = -1218 sec

# Creating subset by removing data point above or below the two bounds.
df_all_trips_v2 <- subset(df_all_trips, df_all_trips$ride_length_sec > low & df_all_trips$ride_length_sec < up)

ggplot(df_all_trips_v2, aes(x = ride_length_sec, fill = member_casual)) +
  geom_histogram(bins = 60)

# Subset with only outliers
df_all_outliers <- subset(df_all_trips, df_all_trips$ride_length_sec > up) 
  

# ANALYZE ----

# Basic summary statistics
mean(df_all_trips_v2$ride_length) 
median(df_all_trips_v2$ride_length) 
max(df_all_trips_v2$ride_length) 
min(df_all_trips_v2$ride_length) 

# Compare members and casual users
# Mean - per customer type
aggregate(df_all_trips_v2$ride_length ~ df_all_trips_v2$member_casual, FUN = mean)
# Median - per customer type
aggregate(df_all_trips_v2$ride_length ~ df_all_trips_v2$member_casual, FUN = median)
# Maximum - per customer type
aggregate(df_all_trips_v2$ride_length ~ df_all_trips_v2$member_casual, FUN = max)
# Minimum - per customer type
aggregate(df_all_trips_v2$ride_length ~ df_all_trips_v2$member_casual, FUN = min)

# This function does all of the above
stat_types <- tapply(df_all_trips_v2$ride_length_sec, df_all_trips_v2$member_casual,
       function(x) format(summary(x)))


# Mean ride length for customer types
# Day of the week
aggregate(df_all_trips_v2$ride_length ~ df_all_trips_v2$member_casual + df_all_trips_v2$day_of_week, FUN = mean)
# Month of the year
aggregate(df_all_trips_v2$ride_length_sec ~ df_all_trips_v2$member_casual + df_all_trips_v2$month, FUN = mean)

# Busiest stops per members
busy_start_station <- df_all_trips_v2 %>% 
  count(start_station_name, start_station_id, member_casual, sort = TRUE)
busy_end_station   <- df_all_trips_v2 %>% 
  count(end_station_name  , end_station_id  , member_casual, sort = TRUE)


stat_types <- df_all_trips_v2 %>%
                group_by(member_casual) %>%
                summarise(minimal = min(ride_length_sec),
                          Q1      = quantile(ride_length_sec, probs=c(.25)),
                          median  = quantile(ride_length_sec, probs=c(.50)),
                          average = mean(ride_length_sec),
                          Q3      = quantile(ride_length_sec, probs=c(.75)), 
                          maximum = max(ride_length_sec)
                )

# analyze ridership data by usertype and weekday
stat_types_dow <- df_all_trips_v2 %>%
                  group_by(member_casual, day_of_week)%>% #groups by usertype and weekday
                  summarise(number_of_rides = n(),      # set of summary statistics
                            average_length  = mean(ride_length_sec),
                            median_length   = median(ride_length_sec),
                            sd_length       = sd(ride_length_sec))

# analyze ridership data by usertype and month
stat_types_moy <- df_all_trips_v2 %>%
                  group_by(member_casual, month_of_year)%>%
                  summarise(number_of_rides = n(),
                            average         = mean(ride_length_sec),
                            median          = median(ride_length_sec),
                            sd_length       = sd(ride_length_sec))
# Visualize in R
# Number of rides per day of the week for both types of riders
df_all_trips_v2 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE,week_start = 1)) %>% 
  group_by(member_casual, day_of_week) %>% 
      summarise(number_of_rides = n()) %>% 
         arrange(member_casual, day_of_week) %>% 
            ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
            geom_col(position = "dodge")

# Let's create a visualization for average duration
df_all_trips_v2 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE,week_start = 1)) %>% 
  group_by(member_casual, day_of_week) %>% 
      summarise(average_duration = mean(ride_length_sec)) %>% 
         arrange(member_casual, day_of_week)  %>% 
            ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
            geom_col(position = "dodge")


#EXPORTING ----
write.csv(df_all_trips_v2, file = paste0(wd_path, "\\stats_tableau\\DF_all_trips_v2.csv"))
write.csv(df_all_outliers, file = paste0(wd_path, "\\stats_tableau\\DF_all_outliers.csv"))

write.csv(busy_start_station, file = paste0(wd_path, "\\stats_tableau\\busy_start_stations.csv"))
write.csv(busy_end_station, file = paste0(wd_path, "\\stats_tableau\\busy_end_stations.csv"))

write.csv(stat_types    , file = paste0(wd_path, "\\stats_tableau\\stat_types.csv"))
write.csv(stat_types_dow, file = paste0(wd_path, "\\stats_tableau\\stat_types_dow.csv"))
write.csv(stat_types_moy, file = paste0(wd_path, "\\stats_tableau\\stat_types_moy.csv"))


#End of script
