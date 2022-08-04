bellabeat_markdown
================
Chase Carlson
2022-08-04

## Table of Contents

1.  Introduction
    -   About the company
    -   Business task
    -   Questions to answer
2.  Prepare the data
    -   About the data
    -   Load libraries
    -   Load data
    -   File structure & content
    -   Cleaning the data
3.  Analyze the data
    -   Activity by weekday
    -   Hourly activity
    -   Use Frequency
    -   Relationships
    -   Sleep data
4.  Recommendations
5.  Acknowledgements

## 1. Introduction

#### Background

Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that
manufactures health-focused smart products. Sršen used her background as
an artist to develop beautifully designed technology that informs and
inspires women around the world. Collecting data on activity, sleep,
stress, and reproductive health has allowed Bellabeat to empower women
with knowledge about their own health and habits. Since it was founded
in 2013, Bellabeat has grown rapidly and quickly positioned itself as a
tech-driven wellness company for women.

By 2016, Bellabeat had opened offices around the world and launched
multiple products. Bellabeat products became available through a growing
number of online retailers in addition to their own e-commerce channel
on their website. The company has invested in traditional advertising
media, such as radio, out-of-home billboards, print, and television, but
focuses on digital marketing extensively. Bellabeat invests year-round
in Google Search, maintaining active Facebook and Instagram pages, and
consistently engages consumers on Twitter. Additionally, Bellabeat runs
video ads on Youtube and display ads on the Google Display Network to
support campaigns around key marketing dates.

Sršen knows that an analysis of Bellabeat’s available consumer data
would reveal more opportunities for growth. She has asked the marketing
analytics team to focus on a Bellabeat product and analyze smart device
usage data in order to gain insight into how people are already using
their smart devices. Then, using this information, she would like
high-level recommendations for how these trends can inform Bellabeat
marketing strategy.

#### Business Task

The goal of this analysis is to determine how consumers currently use
non-Bellabeat smart devices, and then select one Bellabeat product to
apply these insights to in my presentation. Then, using this
information, I will provide high-level recommendations for how these
trends can inform Bellabeat’s marketing strategy.

#### Questions to answer

1.  What are some trends in smart device usage?
2.  How could these trends apply to Bellabeat customers?
3.  How could these trends help influence Bellabeat marketing strategy?

## 2. Prepare the data

#### About the data

For this analysis I will use the data set that Sršen recommended:
[Fitbit Fitness Tracker
Data](https://www.kaggle.com/datasets/arashnic/fitbit), made available
by [Möbius](https://www.kaggle.com/arashnic). In this data set thirty
three participants consented to the submission of personal tracker data,
including minute-level output for physical activity, heart rate, and
sleep monitoring. It contains 18 CSV files, This data is limited, in
that it does not include demographic information to determine male
vs. female, and it has a small sample size of just 33 users. At the time
of this analysis, the data set is over 6 years old. Since public fitness
tracker data is not readily available, I will be using this data set to
analyze tracker data.

#### Load libraries

I’ll start by loading the libraries I will need to use for this
analysis.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(readr)
library(readxl)
library(hms)
```

    ## 
    ## Attaching package: 'hms'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     hms

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(ggpubr)
```

#### Load data

Next I opened each file in Excel to get a quick visual glance at all of
the files and rename the files for consistency prior to importing. I
noticed that the daily_activity table was a summary of several of the
other tables, so for this analysis I will focus primarily on the
daily_activity, hourly_steps, sleep_day, and weight_log_info data sets.

``` r
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
```

#### File structure & content

Let’s have an overview of the data sets using the summary and glimpse
tools.

First the daily_activity data:

``` r
glimpse(daily_activity)
```

    ## Rows: 940
    ## Columns: 15
    ## $ Id                       <chr> "1503960366", "1503960366", "1503960366", "15…
    ## $ ActivityDate             <dttm> 2016-04-12, 2016-04-13, 2016-04-14, 2016-04-…
    ## $ TotalSteps               <dbl> 13162, 10735, 10460, 9762, 12669, 9705, 13019…
    ## $ TotalDistance            <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ TrackerDistance          <dbl> 8.50, 6.97, 6.74, 6.28, 8.16, 6.48, 8.59, 9.8…
    ## $ LoggedActivitiesDistance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveDistance       <dbl> 1.88, 1.57, 2.44, 2.14, 2.71, 3.19, 3.25, 3.5…
    ## $ ModeratelyActiveDistance <dbl> 0.55, 0.69, 0.40, 1.26, 0.41, 0.78, 0.64, 1.3…
    ## $ LightActiveDistance      <dbl> 6.06, 4.71, 3.91, 2.83, 5.04, 2.51, 4.71, 5.0…
    ## $ SedentaryActiveDistance  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ VeryActiveMinutes        <dbl> 25, 21, 30, 29, 36, 38, 42, 50, 28, 19, 66, 4…
    ## $ FairlyActiveMinutes      <dbl> 13, 19, 11, 34, 10, 20, 16, 31, 12, 8, 27, 21…
    ## $ LightlyActiveMinutes     <dbl> 328, 217, 181, 209, 221, 164, 233, 264, 205, …
    ## $ SedentaryMinutes         <dbl> 728, 776, 1218, 726, 773, 539, 1149, 775, 818…
    ## $ Calories                 <dbl> 1985, 1797, 1776, 1745, 1863, 1728, 1921, 203…

``` r
summary(daily_activity)
```

    ##       Id             ActivityDate                      TotalSteps   
    ##  Length:940         Min.   :2016-04-12 00:00:00.00   Min.   :    0  
    ##  Class :character   1st Qu.:2016-04-19 00:00:00.00   1st Qu.: 3790  
    ##  Mode  :character   Median :2016-04-26 00:00:00.00   Median : 7406  
    ##                     Mean   :2016-04-26 06:53:37.01   Mean   : 7638  
    ##                     3rd Qu.:2016-05-04 00:00:00.00   3rd Qu.:10727  
    ##                     Max.   :2016-05-12 00:00:00.00   Max.   :36019  
    ##  TotalDistance    TrackerDistance  LoggedActivitiesDistance VeryActiveDistance
    ##  Min.   : 0.000   Min.   : 0.000   Min.   :0.0000           Min.   : 0.000    
    ##  1st Qu.: 2.620   1st Qu.: 2.620   1st Qu.:0.0000           1st Qu.: 0.000    
    ##  Median : 5.245   Median : 5.245   Median :0.0000           Median : 0.210    
    ##  Mean   : 5.490   Mean   : 5.475   Mean   :0.1082           Mean   : 1.503    
    ##  3rd Qu.: 7.713   3rd Qu.: 7.710   3rd Qu.:0.0000           3rd Qu.: 2.053    
    ##  Max.   :28.030   Max.   :28.030   Max.   :4.9421           Max.   :21.920    
    ##  ModeratelyActiveDistance LightActiveDistance SedentaryActiveDistance
    ##  Min.   :0.0000           Min.   : 0.000      Min.   :0.000000       
    ##  1st Qu.:0.0000           1st Qu.: 1.945      1st Qu.:0.000000       
    ##  Median :0.2400           Median : 3.365      Median :0.000000       
    ##  Mean   :0.5675           Mean   : 3.341      Mean   :0.001606       
    ##  3rd Qu.:0.8000           3rd Qu.: 4.782      3rd Qu.:0.000000       
    ##  Max.   :6.4800           Max.   :10.710      Max.   :0.110000       
    ##  VeryActiveMinutes FairlyActiveMinutes LightlyActiveMinutes SedentaryMinutes
    ##  Min.   :  0.00    Min.   :  0.00      Min.   :  0.0        Min.   :   0.0  
    ##  1st Qu.:  0.00    1st Qu.:  0.00      1st Qu.:127.0        1st Qu.: 729.8  
    ##  Median :  4.00    Median :  6.00      Median :199.0        Median :1057.5  
    ##  Mean   : 21.16    Mean   : 13.56      Mean   :192.8        Mean   : 991.2  
    ##  3rd Qu.: 32.00    3rd Qu.: 19.00      3rd Qu.:264.0        3rd Qu.:1229.5  
    ##  Max.   :210.00    Max.   :143.00      Max.   :518.0        Max.   :1440.0  
    ##     Calories   
    ##  Min.   :   0  
    ##  1st Qu.:1828  
    ##  Median :2134  
    ##  Mean   :2304  
    ##  3rd Qu.:2793  
    ##  Max.   :4900

``` r
colnames(daily_activity)
```

    ##  [1] "Id"                       "ActivityDate"            
    ##  [3] "TotalSteps"               "TotalDistance"           
    ##  [5] "TrackerDistance"          "LoggedActivitiesDistance"
    ##  [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    ##  [9] "LightActiveDistance"      "SedentaryActiveDistance" 
    ## [11] "VeryActiveMinutes"        "FairlyActiveMinutes"     
    ## [13] "LightlyActiveMinutes"     "SedentaryMinutes"        
    ## [15] "Calories"

Then the sleep_day data:

``` r
glimpse(sleep_day)
```

    ## Rows: 413
    ## Columns: 5
    ## $ Id                 <chr> "1503960366", "1503960366", "1503960366", "15039603…
    ## $ SleepDay           <dttm> 2016-04-12, 2016-04-13, 2016-04-15, 2016-04-16, 20…
    ## $ TotalSleepRecords  <dbl> 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ TotalMinutesAsleep <dbl> 327, 384, 412, 340, 700, 304, 360, 325, 361, 430, 2…
    ## $ TotalTimeInBed     <dbl> 346, 407, 442, 367, 712, 320, 377, 364, 384, 449, 3…

``` r
summary(sleep_day)
```

    ##       Id               SleepDay                      TotalSleepRecords
    ##  Length:413         Min.   :2016-04-12 00:00:00.00   Min.   :1.000    
    ##  Class :character   1st Qu.:2016-04-19 00:00:00.00   1st Qu.:1.000    
    ##  Mode  :character   Median :2016-04-27 00:00:00.00   Median :1.000    
    ##                     Mean   :2016-04-26 12:40:05.80   Mean   :1.119    
    ##                     3rd Qu.:2016-05-04 00:00:00.00   3rd Qu.:1.000    
    ##                     Max.   :2016-05-12 00:00:00.00   Max.   :3.000    
    ##  TotalMinutesAsleep TotalTimeInBed 
    ##  Min.   : 58.0      Min.   : 61.0  
    ##  1st Qu.:361.0      1st Qu.:403.0  
    ##  Median :433.0      Median :463.0  
    ##  Mean   :419.5      Mean   :458.6  
    ##  3rd Qu.:490.0      3rd Qu.:526.0  
    ##  Max.   :796.0      Max.   :961.0

``` r
colnames(sleep_day)
```

    ## [1] "Id"                 "SleepDay"           "TotalSleepRecords" 
    ## [4] "TotalMinutesAsleep" "TotalTimeInBed"

The weight_log data:

``` r
glimpse(weight_log_info)
```

    ## Rows: 67
    ## Columns: 8
    ## $ Id             <chr> "1503960366", "1503960366", "1927972279", "2873212765",…
    ## $ Date           <dttm> 2016-05-02, 2016-05-03, 2016-04-13, 2016-04-21, 2016-0…
    ## $ WeightKg       <dbl> 52.6, 52.6, 133.5, 56.7, 57.3, 72.4, 72.3, 69.7, 70.3, …
    ## $ WeightPounds   <dbl> 115.96, 115.96, 294.32, 125.00, 126.32, 159.61, 159.39,…
    ## $ Fat            <dbl> 22, NA, NA, NA, NA, 25, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ BMI            <dbl> 22.65, 22.65, 47.54, 21.45, 21.69, 27.45, 27.38, 27.25,…
    ## $ IsManualReport <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, …
    ## $ LogId          <dbl> 1.46223e+12, 1.46232e+12, 1.46051e+12, 1.46128e+12, 1.4…

``` r
summary(weight_log_info)
```

    ##       Id                 Date                           WeightKg     
    ##  Length:67          Min.   :2016-04-12 00:00:00.00   Min.   : 52.60  
    ##  Class :character   1st Qu.:2016-04-19 00:00:00.00   1st Qu.: 61.40  
    ##  Mode  :character   Median :2016-04-27 00:00:00.00   Median : 62.50  
    ##                     Mean   :2016-04-26 22:12:32.24   Mean   : 72.04  
    ##                     3rd Qu.:2016-05-04 00:00:00.00   3rd Qu.: 85.05  
    ##                     Max.   :2016-05-12 00:00:00.00   Max.   :133.50  
    ##                                                                      
    ##   WeightPounds        Fat             BMI        IsManualReport 
    ##  Min.   :116.0   Min.   :22.00   Min.   :21.45   Mode :logical  
    ##  1st Qu.:135.4   1st Qu.:22.75   1st Qu.:23.96   FALSE:26       
    ##  Median :137.8   Median :23.50   Median :24.39   TRUE :41       
    ##  Mean   :158.8   Mean   :23.50   Mean   :25.19                  
    ##  3rd Qu.:187.5   3rd Qu.:24.25   3rd Qu.:25.56                  
    ##  Max.   :294.3   Max.   :25.00   Max.   :47.54                  
    ##                  NA's   :65                                     
    ##      LogId          
    ##  Min.   :1.460e+12  
    ##  1st Qu.:1.461e+12  
    ##  Median :1.462e+12  
    ##  Mean   :1.462e+12  
    ##  3rd Qu.:1.462e+12  
    ##  Max.   :1.463e+12  
    ## 

``` r
colnames(weight_log_info)
```

    ## [1] "Id"             "Date"           "WeightKg"       "WeightPounds"  
    ## [5] "Fat"            "BMI"            "IsManualReport" "LogId"

And the hourly_steps data:

``` r
glimpse(hourly_steps)
```

    ## Rows: 22,099
    ## Columns: 3
    ## $ Id           <chr> "1503960366", "1503960366", "1503960366", "1503960366", "…
    ## $ ActivityHour <dttm> 2016-04-12 00:00:00, 2016-04-12 01:00:00, 2016-04-12 02:…
    ## $ StepTotal    <dbl> 373, 160, 151, 0, 0, 0, 0, 0, 250, 1864, 676, 360, 253, 2…

``` r
summary(hourly_steps)
```

    ##       Id             ActivityHour                      StepTotal      
    ##  Length:22099       Min.   :2016-04-12 00:00:00.00   Min.   :    0.0  
    ##  Class :character   1st Qu.:2016-04-19 01:00:00.00   1st Qu.:    0.0  
    ##  Mode  :character   Median :2016-04-26 06:00:00.00   Median :   40.0  
    ##                     Mean   :2016-04-26 11:46:42.58   Mean   :  320.2  
    ##                     3rd Qu.:2016-05-03 19:00:00.00   3rd Qu.:  357.0  
    ##                     Max.   :2016-05-12 15:00:00.00   Max.   :10554.0

``` r
colnames(hourly_steps)
```

    ## [1] "Id"           "ActivityHour" "StepTotal"

We find:

-   During import I notice that some varialbles wanted to import in the
    incorrect format, so I manually converted inaccurate classifications
    during the read_excel process.
-   Date columns will be renamed for consistency across the data.
-   In the daily_activity data we see that the minimum total steps is 0,
    and the maximum sedentary time is 1440, which likely indicates the
    participant did not wear their device or log any activity for the
    day.
-   There are less records in the sleep_day data set than in the
    daily_activity data set, indicating that not all users recorded
    sleep information each night.
-   The weight_log table has significantly fewer records than the other
    tables, indicating that not all users participated in this feature.
    I’ll be curious to see if this group was more active than the
    others.
-   The maximum hourly steps is 10554, which shows this user probably
    used their device to log exercise.
-   The time frame of this study ran from 2016-04-12 to 2016-05-12, or
    31 days.

#### Cleaning the data

Now I will run some basic cleaning functions on each data set.

##### daily_activity

``` r
get_dupes(daily_activity)
```

    ## No variable names specified - using all columns.

    ## No duplicate combinations found of: Id, ActivityDate, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDistance, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, ... and 6 other variables

    ## # A tibble: 0 × 16
    ## # … with 16 variables: Id <chr>, ActivityDate <dttm>, TotalSteps <dbl>,
    ## #   TotalDistance <dbl>, TrackerDistance <dbl>, LoggedActivitiesDistance <dbl>,
    ## #   VeryActiveDistance <dbl>, ModeratelyActiveDistance <dbl>,
    ## #   LightActiveDistance <dbl>, SedentaryActiveDistance <dbl>,
    ## #   VeryActiveMinutes <dbl>, FairlyActiveMinutes <dbl>,
    ## #   LightlyActiveMinutes <dbl>, SedentaryMinutes <dbl>, Calories <dbl>,
    ## #   dupe_count <int>

There are no duplicated rows in the daily_activity table.

``` r
daily_activity <- daily_activity %>%
  clean_names() %>% 
  rename(date = activity_date)
```

I also want to create a separate data frame for some analyses to filter
out entire days where participants did not wear their device. This will
allow us to see statistics based only on when the device was in use,
rather than aggregating 0 steps or 1440 sedentary minutes into our
calculations.

``` r
daily_activity_filtered <- 
  daily_activity[daily_activity$total_steps != 0, ]
```

##### sleep_day

``` r
get_dupes(sleep_day)
```

    ## No variable names specified - using all columns.

    ## # A tibble: 6 × 6
    ##   Id        SleepDay            TotalSleepRecor… TotalMinutesAsl… TotalTimeInBed
    ##   <chr>     <dttm>                         <dbl>            <dbl>          <dbl>
    ## 1 43881618… 2016-05-05 00:00:00                1              471            495
    ## 2 43881618… 2016-05-05 00:00:00                1              471            495
    ## 3 47029216… 2016-05-07 00:00:00                1              520            543
    ## 4 47029216… 2016-05-07 00:00:00                1              520            543
    ## 5 83785632… 2016-04-25 00:00:00                1              388            402
    ## 6 83785632… 2016-04-25 00:00:00                1              388            402
    ## # … with 1 more variable: dupe_count <int>

There are 3 duplicate rows in the sleep_day table. We will remove those.

``` r
sleep_day <- sleep_day %>% 
  clean_names() %>% 
  rename(date = sleep_day) %>% 
  distinct()
```

##### weight_log_info

``` r
get_dupes(weight_log_info)
```

    ## No variable names specified - using all columns.

    ## No duplicate combinations found of: Id, Date, WeightKg, WeightPounds, Fat, BMI, IsManualReport, LogId

    ## # A tibble: 0 × 9
    ## # … with 9 variables: Id <chr>, Date <dttm>, WeightKg <dbl>,
    ## #   WeightPounds <dbl>, Fat <dbl>, BMI <dbl>, IsManualReport <lgl>,
    ## #   LogId <dbl>, dupe_count <int>

There are no duplicate rows in the weight_log_info table.

``` r
weight_log_info <- weight_log_info %>% 
  clean_names()
```

##### hourly_steps

``` r
get_dupes(hourly_steps)
```

    ## No variable names specified - using all columns.

    ## No duplicate combinations found of: Id, ActivityHour, StepTotal

    ## # A tibble: 0 × 4
    ## # … with 4 variables: Id <chr>, ActivityHour <dttm>, StepTotal <dbl>,
    ## #   dupe_count <int>

There are no duplicate rows in the hourly_steps table.

``` r
hourly_steps <- hourly_steps %>% 
  clean_names()
```

Finally, I want to see how many users are included in each data frame.
To do this, I will count the number of distince users in each data
frame, and then create a table to see what it looks like.

``` r
n_distinct(daily_activity$id)
```

    ## [1] 33

``` r
n_distinct(hourly_steps$id)
```

    ## [1] 33

``` r
n_distinct(sleep_day$id)
```

    ## [1] 24

``` r
n_distinct(weight_log_info$id)
```

    ## [1] 8

``` r
table_names <- c("daily_activity", "hourly_steps", "sleep_day", "weight_log_info")
unique_participants <- c(33, 33, 24, 8)

distinct_participants <- data.frame(table_names, unique_participants)
head(distinct_participants) # There are only 8 unique id's who participated in the weight log feature
```

    ##       table_names unique_participants
    ## 1  daily_activity                  33
    ## 2    hourly_steps                  33
    ## 3       sleep_day                  24
    ## 4 weight_log_info                   8

## 3. Analyze the data

Now that the data is clean and formatted, I will dive in to identify how
participants used their devices during this trial.

#### Activity by weekday

The first look into the usage patterns will be to determine which days
of the week participants are the most active. In order to see this, I
pulled out the day of the week for each date in the
daily_activity_filtered data into a new column. Then, I ordered and
summarized the data based on day of the week, focusing on steps,
calories, very active minutes, and distance:

``` r
weekday_daily_activity <- daily_activity_filtered %>%
  mutate(weekday = weekdays(date))

weekday_daily_activity$weekday <-ordered(weekday_daily_activity$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                            "Friday", "Saturday", "Sunday"))

weekday_daily_activity <- weekday_daily_activity %>%
  group_by(weekday) %>%
  summarize (avg_steps = mean(total_steps), avg_calories = mean(calories), avg_very_active_minutes = mean(very_active_minutes), avg_distance = mean(total_distance))

head(weekday_daily_activity)
```

    ## # A tibble: 6 × 5
    ##   weekday   avg_steps avg_calories avg_very_active_minutes avg_distance
    ##   <ord>         <dbl>        <dbl>                   <dbl>        <dbl>
    ## 1 Monday        8488.        2386.                    25.2         6.06
    ## 2 Tuesday       8949.        2441.                    25.3         6.42
    ## 3 Wednesday     8158.        2339.                    22.4         5.92
    ## 4 Thursday      8185.        2274.                    21.5         5.87
    ## 5 Friday        7821.        2352.                    21.1         5.58
    ## 6 Saturday      8947.        2429.                    23.8         6.42

Next, calculated the total population average for each variable, and
created a visualization to see what it looks like:

``` r
avg_steps <- daily_activity_filtered %>% 
  summarize(mean(total_steps))
head(avg_steps) # 8319 steps
```

    ## # A tibble: 1 × 1
    ##   `mean(total_steps)`
    ##                 <dbl>
    ## 1               8319.

``` r
avg_cal <- daily_activity_filtered %>% 
  summarize(mean(calories))
head(avg_cal) # 2361 calories
```

    ## # A tibble: 1 × 1
    ##   `mean(calories)`
    ##              <dbl>
    ## 1            2361.

``` r
avg_active_min <- daily_activity_filtered %>% 
  summarize(mean(very_active_minutes))
head(avg_active_min) # 23 very active minutes
```

    ## # A tibble: 1 × 1
    ##   `mean(very_active_minutes)`
    ##                         <dbl>
    ## 1                        23.0

``` r
avg_sedentary_time <- daily_activity_filtered %>% 
  summarize(mean(sedentary_minutes))
head(avg_sedentary_time) # 956 minutes
```

    ## # A tibble: 1 × 1
    ##   `mean(sedentary_minutes)`
    ##                       <dbl>
    ## 1                      956.

``` r
avg_dist <- daily_activity_filtered %>% 
  summarize(mean(total_distance))
head(avg_dist) # 5.98 miles
```

    ## # A tibble: 1 × 1
    ##   `mean(total_distance)`
    ##                    <dbl>
    ## 1                   5.98

``` r
ggarrange(
ggplot(weekday_daily_activity) + 
  geom_col(aes(weekday, avg_steps), fill = "#205493") +
  labs(x = NULL, y = "Average Steps", title = "Average Steps") +
  geom_hline(yintercept = 8319) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0,0)), 
ggplot(weekday_daily_activity) +
  geom_col(aes(weekday, avg_calories), fill = "#26C6DA") +
  labs(x = NULL, y = "Average Calories", title = "Average Calories") +
  geom_hline(yintercept = 2361) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0,0)),
ggplot(weekday_daily_activity) +
  geom_col(aes(weekday, avg_very_active_minutes), fill = "#78909C") +
  labs(x = NULL, y = "Average Very \nActive Minutes", title = "Average Very Active Minutes") + 
  geom_hline(yintercept = 23) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0,0)),
ggplot(weekday_daily_activity) +
  geom_col(aes(weekday, avg_distance), fill = "#FF9776") +
  labs(x = NULL, y = "Average Distance", title = "Average Distance") + 
  geom_hline(yintercept = 5.98) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(face = "bold")) +
  scale_y_continuous(expand = c(0,0))
)
```

![](bellabeat_markdown_files/figure-gfm/weekday%20activity%20plots-1.png)<!-- -->

Given this information, we can see that Monday, Tuesday, and Saturday
are the most active days, with Friday and Sunday being the least active.

#### Hourly Activity

The next step to analyze user activity is to determine how they use
their devices throughout the day. To do this, I had to separate out the
time from activity_hour into a separate column, and then group by time,
and summarize the average steps for each hour. Then I added a
visualization to see what it looks like:

``` r
steps_by_hour <- hourly_steps %>% 
  mutate(hour = format(as.POSIXct(activity_hour),
                       format = "%H:%M:%S")) %>% 
  group_by(hour) %>% 
  summarize(average_steps = mean(step_total)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = hour, y =average_steps, fill = average_steps)) +
  scale_fill_gradient(name="Average Steps") +
  labs(x = "Activity Hour", y = "Average Steps", title = "Avg Steps By Hour") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        axis.ticks = element_blank())
steps_by_hour
```

![](bellabeat_markdown_files/figure-gfm/steps%20by%20hour-1.png)<!-- -->

Based on this column chart, we can see that the most active times of the
day are between 12:00-14:00, and from 17:00-19:00, suggesting people
tend to experience the highest levels of activity around meal times.

Another way to visualize this information is at the “id” level using a
heatmap:

``` r
heatmap <- hourly_steps %>% 
  mutate(hour = format(as.POSIXct(activity_hour),
                       format = "%H:%M:%S")) %>% 
  group_by(id, hour) %>% 
  summarize(avg_steps = mean(step_total), .groups = "drop") %>% 
  ggplot(aes(x=hour, y=id)) + # y axis has to be in "chr" format to work.
  geom_tile(aes(fill = avg_steps)) +
  scale_fill_gradient(name = "Average Steps", 
                      low = "#FFFFFF",
                      high = "#FF0000") +
  labs(x = NULL, y = NULL, title = "Activity Heatmap") +
  theme_classic() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", margin = margin(b=10)),
        axis.text.x = element_text(angle = 90),
        legend.title.align = 0.5,
        legend.margin = margin(l = 10),
        axis.ticks = element_blank())
  
heatmap
```

![](bellabeat_markdown_files/figure-gfm/steps%20heatmap-1.png)<!-- -->

This visualizations allows us to see that there were only a few highly
active people (over 3000 steps/hr) in this group. We also get a sense
that most users get up and start being active around 07:00, but others
seem to sleep a little longer and stay active later. Also, some users
were highly inactive throughout the entire day.

#### Use Frequency

Because I noticed a difference of 77 observations when I filtered out
users with 0 activity, I want to see how often people actually used
their device during the 1 month period.To do this, I will classify users
by Low use (1-10 days), Moderate use (11-20 days), and High use (21-31
days) categories.

``` r
device_usage <- daily_activity %>% 
  filter(total_steps > 0) %>% 
  count(id, name = "days_used", sort = TRUE) %>% 
  mutate(use_category = case_when(days_used <= 10 ~ "Low Use - 1-10 days", 
                                  days_used > 10 & days_used <= 20 ~ "Moderate Use - 11-20 days", 
                                  TRUE ~ "High Use - 21-31 days" ))
head(device_usage)
```

    ## # A tibble: 6 × 3
    ##   id         days_used use_category         
    ##   <chr>          <int> <chr>                
    ## 1 1624580081        31 High Use - 21-31 days
    ## 2 2022484408        31 High Use - 21-31 days
    ## 3 2026352035        31 High Use - 21-31 days
    ## 4 2320127002        31 High Use - 21-31 days
    ## 5 2873212765        31 High Use - 21-31 days
    ## 6 4319703577        31 High Use - 21-31 days

``` r
tail(device_usage)
```

    ## # A tibble: 6 × 3
    ##   id         days_used use_category             
    ##   <chr>          <int> <chr>                    
    ## 1 2347167796        18 Moderate Use - 11-20 days
    ## 2 8253242879        18 Moderate Use - 11-20 days
    ## 3 1927972279        17 Moderate Use - 11-20 days
    ## 4 4020332650        17 Moderate Use - 11-20 days
    ## 5 6775888955        17 Moderate Use - 11-20 days
    ## 6 4057192912         3 Low Use - 1-10 days

Next, I will compute percentages and set up the data frame for
visualization:

``` r
device_usage_percentage <- device_usage %>% 
  group_by(use_category) %>% 
  summarize(number_of_users = n()) %>% 
  mutate(percent_of_users = number_of_users/sum(number_of_users), 
         ymax = cumsum(percent_of_users), 
         ymin = c(0, head(ymax, n=-1)), 
         label_position = (ymax + ymin)/2, 
         label = paste0(scales::percent(percent_of_users))) %>%
  arrange(desc(percent_of_users))

head(device_usage_percentage)
```

    ## # A tibble: 3 × 7
    ##   use_category number_of_users percent_of_users  ymax  ymin label_position label
    ##   <chr>                  <int>            <dbl> <dbl> <dbl>          <dbl> <chr>
    ## 1 High Use - …              25           0.758  0.758 0              0.379 76%  
    ## 2 Moderate Us…               7           0.212  1     0.788          0.894 21%  
    ## 3 Low Use - 1…               1           0.0303 0.788 0.758          0.773 3%

Here is the result:

``` r
device_usage <- device_usage_percentage %>% 
  mutate(use_category = factor(use_category),
         use_category = fct_reorder(use_category, percent_of_users, .desc = TRUE)) %>%
  ggplot(aes(ymin = ymin, ymax = ymax,fill = use_category,
             xmin= 0, xmax = 2)) + 
  geom_rect() +
  scale_fill_manual(values = c("#006C7A", "#FFBEA9", "#78909C"), name = "Use Category") +
  theme_classic() +
  geom_text(aes(x = 1, y = label_position, label = label, fontface = "bold")) +
  theme(text = element_text(size = 15),
        axis.ticks = element_blank(),
        plot.title = element_text(face = "bold", margin= margin(b=10))) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL, title = "Device Usage")

device_usage
```

![](bellabeat_markdown_files/figure-gfm/visualize%20use%20frequency%20by%20population-1.png)<!-- -->

These results show that only 76% of users logged activity more than 21
days during the study period, 21% logged activity between 11-20 days,
and 3% (one participant), used the device less than 10 days.

Possible reasons for not wearing the device may include: \* Aesthetic
doesn’t go with outfit \* Didn’t want to track fitness activity that day
\* Forgot to put it on \* Battery life

##### Physical Activity Intensities

Next, I want to see usage statistics relating to exercise, or “active
minutes”. According to Fitbit, active minutes are “earned through 10
minutes or more of continuous moderate-to-intense activity.”
<sup>[1](https://help.fitbit.com/articles/en_US/Help_article/1379.htm)</sup>
This will show how many participants incorporated regular exercise into
their daily activities.

``` r
exercise <- daily_activity_filtered %>% 
  group_by(id) %>%
  summarize(avg_active_min = mean(very_active_minutes)) %>% 
  arrange(desc(avg_active_min))

head(exercise)
```

    ## # A tibble: 6 × 2
    ##   id         avg_active_min
    ##   <chr>               <dbl>
    ## 1 5577150313           93.6
    ## 2 8053475328           85.2
    ## 3 8877689391           66.1
    ## 4 8378563200           58.7
    ## 5 7086361926           44  
    ## 6 1503960366           40

``` r
tail(exercise)
```

    ## # A tibble: 6 × 2
    ##   id         avg_active_min
    ##   <chr>               <dbl>
    ## 1 6117666160         1.91  
    ## 2 8792009665         1.47  
    ## 3 2320127002         1.35  
    ## 4 4057192912         1     
    ## 5 1844505072         0.190 
    ## 6 2026352035         0.0968

Since the recommended amount of exercise is 30 minutes per
day<sup>[2](https://www.cdc.gov/physicalactivity/basics/adults/index.htm "cdc.gov")</sup>,
I will use this as my measurement to see how many met this standard:

``` r
recommended_exercise <- exercise %>% 
  tally((avg_active_min >= 30)/33, name = "avg_active_min") %>% 
  mutate(percent_over_30 = paste0(scales::percent(avg_active_min)))
head(recommended_exercise)
```

    ## # A tibble: 1 × 2
    ##   avg_active_min percent_over_30
    ##            <dbl> <chr>          
    ## 1          0.242 24%

24% of participants achieved 30+ very active minutes per day.

#### Correlations

Another aspect we want to look at when analyzing user statistics is the
relationships between activities. I also wanted to compare the activity
of participants who made the effort to manually track their weight
vs. those who did not.

Because sample size is so small, I will join the daily_activity table
with the weight_log table to determine which users do not appear in
both. Then I will identify and graph the average calories burned,
comparing the two groups.

##### **Logged Weight**

``` r
logged_weight_calories = daily_activity_filtered %>% 
  inner_join(weight_log_info, by="id") %>% 
  summarize(avg_calories = mean(calories))
head(logged_weight_calories)
```

    ## # A tibble: 1 × 1
    ##   avg_calories
    ##          <dbl>
    ## 1        2525.

List of users who logged weight stats:

``` r
logged <- daily_activity_filtered %>% 
  inner_join(weight_log_info, by="id", "date")
head(logged)
```

    ## # A tibble: 6 × 22
    ##   id         date.x              total_steps total_distance tracker_distance
    ##   <chr>      <dttm>                    <dbl>          <dbl>            <dbl>
    ## 1 1503960366 2016-04-12 00:00:00       13162           8.5              8.5 
    ## 2 1503960366 2016-04-12 00:00:00       13162           8.5              8.5 
    ## 3 1503960366 2016-04-13 00:00:00       10735           6.97             6.97
    ## 4 1503960366 2016-04-13 00:00:00       10735           6.97             6.97
    ## 5 1503960366 2016-04-14 00:00:00       10460           6.74             6.74
    ## 6 1503960366 2016-04-14 00:00:00       10460           6.74             6.74
    ## # … with 17 more variables: logged_activities_distance <dbl>,
    ## #   very_active_distance <dbl>, moderately_active_distance <dbl>,
    ## #   light_active_distance <dbl>, sedentary_active_distance <dbl>,
    ## #   very_active_minutes <dbl>, fairly_active_minutes <dbl>,
    ## #   lightly_active_minutes <dbl>, sedentary_minutes <dbl>, calories <dbl>,
    ## #   date.y <dttm>, weight_kg <dbl>, weight_pounds <dbl>, fat <dbl>, bmi <dbl>,
    ## #   is_manual_report <lgl>, log_id <dbl>

Confirming accurate join:

``` r
n_distinct(logged$id)
```

    ## [1] 8

A correlation coefficient of 0.80 indicates a strong positive
correlation exists between steps and calories for users who logged their
weight:

``` r
cor.test(logged$total_steps, logged$calories)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  logged$total_steps and logged$calories
    ## t = 60.858, df = 2056, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.7859197 0.8168045
    ## sample estimates:
    ##       cor 
    ## 0.8018972

##### **Did not log weight**

``` r
no_weight_calories = daily_activity_filtered %>%
  anti_join(weight_log_info, by="id") %>%
  summarize(avg_calories = mean(calories))
head(no_weight_calories)
```

    ## # A tibble: 1 × 1
    ##   avg_calories
    ##          <dbl>
    ## 1        2357.

List of users who did not log weight:

``` r
did_not_log <- daily_activity_filtered %>% 
  anti_join(weight_log_info, by="id")

head(did_not_log)
```

    ## # A tibble: 6 × 15
    ##   id         date                total_steps total_distance tracker_distance
    ##   <chr>      <dttm>                    <dbl>          <dbl>            <dbl>
    ## 1 1624580081 2016-04-12 00:00:00        8163          5.31             5.31 
    ## 2 1624580081 2016-04-13 00:00:00        7007          4.55             4.55 
    ## 3 1624580081 2016-04-14 00:00:00        9107          5.92             5.92 
    ## 4 1624580081 2016-04-15 00:00:00        1510          0.980            0.980
    ## 5 1624580081 2016-04-16 00:00:00        5370          3.49             3.49 
    ## 6 1624580081 2016-04-17 00:00:00        6175          4.06             4.06 
    ## # … with 10 more variables: logged_activities_distance <dbl>,
    ## #   very_active_distance <dbl>, moderately_active_distance <dbl>,
    ## #   light_active_distance <dbl>, sedentary_active_distance <dbl>,
    ## #   very_active_minutes <dbl>, fairly_active_minutes <dbl>,
    ## #   lightly_active_minutes <dbl>, sedentary_minutes <dbl>, calories <dbl>

Confirming accurate join:

``` r
n_distinct(did_not_log$id)
```

    ## [1] 25

A correlation coefficient of 0.57 indicates a strong positive
correlation exists between steps and calories for users who logged their
weight. However, there is a stronger relationship for the group who did
log their weight stats:

``` r
cor.test(did_not_log$total_steps, did_not_log$calories)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  did_not_log$total_steps and did_not_log$calories
    ## t = 17.712, df = 631, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5217614 0.6260726
    ## sample estimates:
    ##       cor 
    ## 0.5762592

Calculate the percentage difference in calorie burn:

``` r
percent_difference <- (logged_weight_calories/no_weight_calories)*100
percent_difference
```

    ##   avg_calories
    ## 1     107.1359

Visualize relationships:

``` r
steps_vs_calories <- 
  ggplot(data = did_not_log, mapping = aes(x = total_steps, y = calories)) +
  geom_point(alpha = 0.5, color = "#006C7A") + 
  geom_point(data = logged, alpha = 0.5, mapping = aes(x = total_steps, y = calories), color = "#FF7043", ) +
  geom_smooth(data = did_not_log, formula = y~x, method = lm, se = FALSE, aes(color = "#006C7A")) +
  geom_smooth(data = logged, formula = y~x, method = lm, se = FALSE, aes(color = "#FF7043")) + 
  theme_classic2() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank()) +
  labs(x = "Total Steps", y = "Calories", 
       title = "Steps Vs. Calories", 
       subtitle = "People who manually logged their weight burned 7% more calories \nthan people who did not log their weight") +
  scale_color_manual(labels = c("Did Not Log Weight", "Logged Weight"),
                     values = c("#006C7A", "#FF7043"))

steps_vs_calories
```

![](bellabeat_markdown_files/figure-gfm/visualize%20calorie%20burn%20relationships-1.png)<!-- -->

We can see from this visualization the relationship between steps and
calorie burn. As steps increase, calorie burn also increases. There was
a stronger relationship between steps and calories with the group that
tracked their own weight than in the group that did not. These results
might indicate the importance of having strong performance feedback
integrated into the device in a way that motivates customers to be
physically active.

#### Sleep Data

The last metric to review is the sleep data. Since I am unable to test
the accuracy of the sleep data, it will be best to keep this analysis at
a very high level at the daily level.

``` r
sleep_day %>%
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>%
  summary() 
```

    ##  total_sleep_records total_minutes_asleep total_time_in_bed
    ##  Min.   :1.00        Min.   : 58.0        Min.   : 61.0    
    ##  1st Qu.:1.00        1st Qu.:361.0        1st Qu.:403.8    
    ##  Median :1.00        Median :432.5        Median :463.0    
    ##  Mean   :1.12        Mean   :419.2        Mean   :458.5    
    ##  3rd Qu.:1.00        3rd Qu.:490.0        3rd Qu.:526.0    
    ##  Max.   :3.00        Max.   :796.0        Max.   :961.0

From this basic summary we can see that the average time in bed was
approximately 7.6hrs (458.5/60), and average time asleep was
approximately 7hrs (419.2/60). People spent about 36 minutes per day in
bed while not sleeping.

``` r
sleep <- ggplot(data=sleep_day, 
       aes(x=total_minutes_asleep, y=total_time_in_bed)) + 
  geom_point()+
  theme_classic2() +
  labs(x = "Minutes Asleep", y = "Time In Bed", title = "Minutes Asleep Vs. Time In Bed")

sleep
```

![](bellabeat_markdown_files/figure-gfm/sleep%20scatterplot-1.png)<!-- -->

## 4. Results

Although this data set has its limitations, we were able to identify
some key takeaways that can be valuable in guiding Bellabeat’s marketing
strategy.

-   We found that people did not wear their devices all the time. Only
    76% of users logged activity more than 21 days during the study
    period, 21% logged activity between 11-20 days, and 3% (one
    participant), used the device less than 10 days. This data suggests
    that they may have removed it for a number of reasons which include,
    but not limited to: battery life, aesthetics, comfort, or forgot to
    put it on.

-   Even though only 8 participants logged their weight statistics, we
    can still take away some important information. People who chose to
    log their weight burned 7% more calories than those who did not.
    Features that are manual, complex, or difficult to use or receive
    feedback are less likely to be used by the customer. The more
    automated the device can be, by itself or with companion products
    (like a weigh scale in this case), the higher the potential to reach
    and motivate the customer.

-   The data showed that people are generally most active around meal
    times, and the highest activity tends to be at the beginning of the
    week and on Saturdays.

-   About 24% of the population achieved 30+ very active minutes per
    day.

-   The participants in this study slept an average of 7 hours per
    night, with 7.6 hours total time in bed.

## 5. Recommendations

In order to get more accurate data including demographic information, I
would strongly encourage collecting additional data, ideally on
Bellabeat customers, as well as analyzing customer feedback on existing
devices to determine the best path forward.

As part of this analysis, I was asked to select one Bellabeat product to
apply my insights to in my presentation. I believe the Bellabeat
membership is the best place to focus our attention, as it
differentiates Bellabeat from its competitors. The following
recommendations are made with the Bellabeat membership in mind:

-   One of the biggest issues with the data in this analysis was lack of
    participation. The Bellabeat membership’s unique features gives
    customers strong motivation to use their devices. By offering
    workouts, nutrition, mindfulness techniques, and menstrual cycle
    support, Bellabeat is able to offer a compelling combination of
    services that customers can’t get from other devices. Also, since
    the membership is a source of predictable continuous income, it is
    most imperative to drive continuous improvements to the membership
    service.

-   With the Bellabeat membership and mobile app, push notifications can
    be a powerful tool to alert the customer at certain times of the day
    to perform a certain activity, drink water, or suggest a good meal
    based on the user’s preferences. I recommend using the
    individualized data for each user to enable highly personalized push
    notifications around the times when customers need to be thinking
    about certain activities.

-   Customers love when things are easy. Since weight tracking seemed
    like the biggest challenge with missing data, but also showed a
    strong correlation to increased calorie burn, I see an opportunity
    to make this easier for the customer. Options include in-app
    reminders to log weight at specific intervals, or, thinking bigger,
    potentially expand the ecosystem with a new smart fitness scale that
    automatically syncs with the Bellabeat app and membership service.

-   Bellabeat’s elegant designs and long battery life are a compelling
    reason for customers to keep their devices on all day long. This
    analysis identified that not all users logged data using their smart
    device. Sleep data is an important metric for Bellabeat to calculate
    accurate readiness scores and wellness scores. Bellabeat should
    include these two factors as part of its marketing strategy to
    encourage customers to choose Bellabeat devices over the
    competition.

-   Celebrity endorsements are of course an excellent way to reach new
    audiences and attract customers. Bellabeat should focus efforts on
    building more strategic partnerships with popular public figures and
    athletes who endorse their products. Not only does sponsorship help
    with brand exposure, but it also can help build customer trust
    knowing that influential people are wearing them.

## 6. Acknowledgements

1.  "Fitbit Help." *Fitbit MyHelp*,
    <https://help.fitbit.com/articles/en_US/Help_article/1379.htm.> 
2.  "How Much Physical Activity Do Adults Need?" *Centers for Disease
    Control and Prevention*, Centers for Disease Control and Prevention,
    2 June 2022,
    <https://www.cdc.gov/physicalactivity/basics/adults/index.htm.> 
