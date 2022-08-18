library(tidyverse)
library(lubridate)
library(janitor)
library(readr)
library(readxl)
library(hms)
library(scales) # To convert numbers to percentages
library(ggpubr)
library(chron)
library(ggtext) 
library(rvest)
library(reshape2)
library(tm) # text analytics - text mining
library(wordcloud) # create wordcloud
library(syuzhet) # to analyze sentiment
library(RColorBrewer)

daily_activity <- read_excel("~/Documents/Coursera/Capstone Project/Fitabase Data 4.12.16-5.12.16/daily_activity.xlsx", 
                             col_types = c("text", "date", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric"))
hourly_steps <- read_excel("~/Documents/Coursera/Capstone Project/Fitabase Data 4.12.16-5.12.16/hourly_steps.xlsx", 
                           col_types = c("text", "date", "numeric"))
sleep_day <- read_excel("~/Documents/Coursera/Capstone Project/Fitabase Data 4.12.16-5.12.16/sleep_day.xlsx", 
                        col_types = c("text", "date", "numeric", 
                                      "numeric", "numeric"))
weight_log_info <- read_excel("~/Documents/Coursera/Capstone Project/Fitabase Data 4.12.16-5.12.16/weight_log_info.xlsx", 
                              col_types = c("text", "date", "skip", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "logical", "numeric"))

# Summarize
glimpse(daily_activity)
summary(daily_activity)
colnames(daily_activity)
glimpse(sleep_day)
summary(sleep_day)
colnames(sleep_day)
glimpse(weight_log_info)
summary(weight_log_info)
colnames(weight_log_info)
glimpse(hourly_steps)
summary(hourly_steps)
colnames(hourly_steps)

# Cleaning the data
# Clean daily_activity. Converted "id" to chr from dbl on import and "date" to dttm format.
get_dupes(daily_activity)

daily_activity <- daily_activity %>%
  clean_names() %>% 
  rename(date = activity_date) 

# daily_activity summary of important stats
daily_activity %>% 
  select(total_steps, total_distance, calories, sedentary_minutes, logged_activities_distance) %>% 
  summary() 
# Minimum total steps = 0 and max sedentary minutes = 1440 may indicate user not wearing the device.
# Average logged activities distance is very low (0.1082), indicating users did not use the device to actively track workouts.

# Remove rows where user is expected to have not worn their device.
daily_activity_filtered <- 
  daily_activity[daily_activity$total_steps != 0, ]

# Clean sleepDay_merged 
get_dupes(sleep_day)

sleep_day <- sleep_day %>% 
  clean_names() %>% 
  rename(date = sleep_day) %>% 
  distinct()


# Clean weightLogInfo_merged. Using as_datetime
get_dupes(weight_log_info)

weight_log_info <- weight_log_info %>% 
  clean_names()

# Clean hourly_steps. Imported with readxl. Converted "id" to chr format
get_dupes(hourly_steps)

hourly_steps <- hourly_steps %>% 
  clean_names()

# Number of days of observations.
difftime("2016-04-12","2016-05-12", units = "days")-1 # -1 to include start date and end date = 31 Days

# How many unique participants in each group?
n_distinct(daily_activity$id)
n_distinct(hourly_steps$id)
n_distinct(sleep_day$id)
n_distinct(weight_log_info$id)

table_names <- c("daily_activity", "hourly_steps", "sleep_day", "weight_log_info")
unique_participants <- c(33, 33, 24, 8)

distinct_participants <- data.frame(table_names, unique_participants)
head(distinct_participants) # There are only 8 unique id's who participated in the weight log feature


