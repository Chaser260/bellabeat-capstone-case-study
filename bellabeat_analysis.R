install.packages("tidyverse")
install.packages("here")
install.packages("janitor")
install.packages("readr")
install.packages("webr")
install.packages("ggpubr") # to arrange multiple ggplots
install.packages("ggtext")
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

# Converted most csv files to xlsx to fix date issue on import.
# Clean daily_activity. Converted "id" to chr from dbl on import and "date" to dttm format.
daily_activity <- daily_activity %>%
  clean_names() %>% 
  rename(date = activity_date) 


# Remove rows where user is expected to have not worn their device.
daily_activity_filtered <- 
  daily_activity[daily_activity$total_steps != 0, ]

# daily_activity summary of important stats
daily_activity %>% 
  select(total_steps, total_distance, calories, sedentary_minutes, logged_activities_distance) %>% 
  summary() 
# Minimum total steps = 0 and max sedentary minutes = 1440 may indicate user not wearing the device.
# Average logged activities distance is very low (0.1082), indicating users did not use the device to actively track workouts.



# Clean sleepDay_merged 
sleep_day <- sleep_day %>% 
  clean_names() %>% 
  rename(date = sleep_day) %>% 
  distinct()


# Clean weightLogInfo_merged. Using as_datetime
weight_log_info <- weight_log_info %>% 
  clean_names()

# Clean hourly_steps. Imported with readxl. Converted "id" to chr format
hourly_steps <- hourly_steps %>% 
  clean_names()



# Number of days of observations.
difftime("2016-04-12","2016-05-12", units = "days")-1 # -1 to include start date and end date = 31 Days

#___________________________________________________________________________________________________________________

# What days are people most active?
# Calculate weekday from date and summarize by average steps and average calories per weekday.
weekday_daily_activity <- daily_activity_filtered %>%
  mutate(weekday = weekdays(date))

weekday_daily_activity$weekday <-ordered(weekday_daily_activity$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                  "Friday", "Saturday", "Sunday"))

weekday_daily_activity <- weekday_daily_activity %>%
  group_by(weekday) %>%
  summarize (avg_steps = mean(total_steps), avg_calories = mean(calories), avg_very_active_minutes = mean(very_active_minutes), avg_distance = mean(total_distance))

head(weekday_daily_activity)

# Calculate total population average for steps, calories, and very active minutes.
avg_steps <- daily_activity_filtered %>% 
  summarize(mean(total_steps))
head(avg_steps) # 8319 steps

avg_cal <- daily_activity_filtered %>% 
  summarize(mean(calories))
head(avg_cal) # 2361 calories

avg_active_min <- daily_activity_filtered %>% 
  summarize(mean(very_active_minutes))
head(avg_active_min) # 23 very active minutes

avg_sedentary_time <- daily_activity_filtered %>% 
  summarize(mean(sedentary_minutes))
head(avg_sedentary_time) # 956 minutes

avg_dist <- daily_activity_filtered %>% 
  summarize(mean(total_distance))
head(avg_dist) # 5.98 miles

# Visualize average daily activity by weekday. We can see that Monday, Tuesday, and Saturday are the most active days, with Sunday being the least active.
ggarrange(
  ggplot(weekday_daily_activity) +
    geom_col(aes(weekday, avg_steps), fill = "#205493") +
    labs(y = "average steps", title = "Average Steps") +
    geom_hline(yintercept = 8319) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)), # Shows average steps per weekday for days where participants logged activity.
  ggplot(weekday_daily_activity) +
    geom_col(aes(weekday, avg_calories), fill = "#26C6DA") +
    labs(y = "average calories", title = "Average Calories") +
    geom_hline(yintercept = 2361) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)),
  ggplot(weekday_daily_activity) +
    geom_col(aes(weekday, avg_very_active_minutes), fill = "#78909C") +
    labs(y = "average very active minutes", title = "Average Very Active Minutes") + 
    geom_hline(yintercept = 23) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)),
  ggplot(weekday_daily_activity) +
    geom_col(aes(weekday, avg_distance), fill = "#FF9776") +
    labs(y = "average distance", title = "Average Distance") + 
    geom_hline(yintercept = 5.98) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
)

#_______________________________________________________________________________________________________-

# What time are people most active?

# Column Chart - Users are most active between 12:00-14:00 and 17:00-19:00, suggesting highest activity around mealtimes.
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

# Heatmap of details by "id" __________________________________________________________________________________
# This heatmap gives a good overview of each user's activity throughout the day. 
# You can see that most users get up and start being active around 07:00, and activity tapers off between 21:00-23:00 at bed time. 
heatmap <- hourly_steps %>% 
  mutate(hour = format(as.POSIXct(activity_hour),
                       format = "%H:%M:%S")) %>% 
  group_by(id, hour) %>% 
  summarize(avg_steps = mean(step_total)) %>% 
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
#______________________________________________________________________________________________________________

# Because I noticed a difference of 77 observations when I filtered out users with 0 activity, I want to see how often people actually used their device during the 1 month period.
# To do this, I will classify users by Low use (1-10 days), Moderate use (11-20 days), and High use (21-31 days) categories
device_usage <- daily_activity %>% 
  filter(total_steps > 0) %>% 
  count(id, name = "days_used", sort = TRUE) %>% 
  mutate(use_category = case_when(days_used <= 10 ~ "Low Use - 1-10 days", 
                                  days_used > 10 & days_used <= 20 ~ "Moderate Use - 11-20 days", 
                                  TRUE ~ "High Use - 21-31 days" ))

# Compute percentages and add columns for ymax, ymin, and labels
device_usage_percentage <- device_usage %>% 
  group_by(use_category) %>% 
  summarize(number_of_users = n()) %>% 
  mutate(percent_of_users = number_of_users/sum(number_of_users), 
         ymax = cumsum(percent_of_users), 
         ymin = c(0, head(ymax, n=-1)), 
         label_position = (ymax + ymin)/2, 
         label = paste0(scales::percent(percent_of_users))) %>%
  arrange(desc(number_of_users))

# Visualize frequency of use by classification.
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





# Are users who did not participate in the weight log burn less calories?

# Count distinct users for each data set
n_distinct(daily_activity$id)
n_distinct(hourly_steps$id)
n_distinct(sleep_day$id)
n_distinct(weight_log_info$id)

table_names <- c("daily_activity", "hourly_steps", "sleep_day", "weight_log_info")
unique_participants <- c(33, 33, 24, 8)

distinct_participants <- data.frame(table_names, unique_participants)
head(distinct_participants) # There are only 8 unique id's who participated in the weight log feature

# Because sample size is so small, I will join the daily_activity table with the weight_log table to determine which users do not appear in both. 
# Then I will graph the average sedentary time comparing both groups

# Average sedentary time for people who logged their weight - ONLY 8 OUT OF 33 LOGGED THEIR WEIGHT
logged_weight_calories = daily_activity_filtered %>% 
  inner_join(weight_log_info, by="id") %>% 
  summarize(avg_calories = mean(calories))
head(logged_weight_calories)   #2525 avg calories


# List of users who did log their weight
logged <- daily_activity_filtered %>% 
  inner_join(weight_log_info, by="id", "date")

n_distinct(logged$id) # Confirming accurate join

# Correlation between steps and sedentary time
cor.test(logged$total_steps, logged$calories) # cor=0.8018972 indicates strong positive correlation between total steps and calories.

# Average calories for people who did NOT log their weight
no_weight_calories = daily_activity_filtered %>% 
  anti_join(weight_log_info, by="id") %>% 
  summarize(avg_calories = mean(calories))
head(no_weight_calories) # 2357 avg calories

# List of users who did NOT log their weight
did_not_log <- daily_activity_filtered %>% 
  anti_join(weight_log_info, by="id")

n_distinct(did_not_log$id) # Confirming accurate join

# Correlation between steps and sedentary time for users who did not log their weight
cor.test(did_not_log$total_steps, did_not_log$calories) # cor=-0.5762592 indicates strong correlation between total steps and calories burned, but less than users who logged their weight.

# Graph the comparison between users who utilized the weight log feature vs. those who did not.

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

# Calculate how much more calories burned for users who logged their weight.
percent_difference <- (logged_weight_calories/no_weight_calories)*100
percent_difference# 7.13% more calories burned for the users who logged their weight.






# Sleep statistics
sleep_day %>%
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>%
  summary() 
# We can see that the average sleep time was approximately 7hrs (419.5/60).
# Average time in bed was approximately 7.6hrs (458.6/60). 
# People spent about 36 minutes per day in bed while not sleeping. 
sleep <- ggplot(data=sleep_day, 
                aes(x=total_minutes_asleep, y=total_time_in_bed)) + 
  geom_point()+
  theme_classic2() +
  labs(x = "Minutes Asleep", y = "Time In Bed", title = "Minutes Asleep Vs. Time In Bed")

sleep

# Return results that show a list of users who averaged >= 30 very active minutes per day to show what % of users got the recommended 30 minutes of exercise per day
exercise <- daily_activity_filtered %>% 
  group_by(id) %>%
  summarize(avg_active_min = mean(very_active_minutes)) %>% 
  arrange(desc(avg_active_min))

head(exercise)

# Summarize what percent of users achieved 30+ very_active_minutes per day

recommended_exercise <- exercise %>% 
  tally((avg_active_min >= 30)/33, name = "avg_active_min") %>% 
  mutate(percent_over_30 = paste0(scales::percent(avg_active_min)))
head(recommended_exercise)