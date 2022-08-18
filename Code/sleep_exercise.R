source('code/import_clean_summarize.R')

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
  geom_smooth(se = F) +
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


