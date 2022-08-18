source('code/import_clean_summarize.R')


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
