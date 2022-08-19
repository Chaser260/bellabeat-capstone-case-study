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

ggsave('images/sleep_vs_tib.png')

# Return results that show a list of users who averaged >= 30 very active minutes per day to show what % of users got the recommended 30 minutes of exercise per day
exercise <- daily_activity_filtered %>% 
  group_by(id) %>%
  summarize(avg_active_min = mean(very_active_minutes)) %>% 
  arrange(desc(avg_active_min))

head(exercise)

# Summarize what percent of users achieved 30+ very_active_minutes per day
colors <- c("percent_over_30" = "dodgerblue", "percent_under_30" = "gray")

recommended_exercise <- exercise %>% 
  tally((avg_active_min >= 30)/33, name = "over_30") %>% 
  mutate(percent_over_30 = percent(over_30),
         percent_under_30 = percent(1 - over_30)) %>% 
  select(percent_over_30, percent_under_30) %>% 
  pivot_longer(cols = starts_with("percent"), names_to = "group") %>% 
  ggplot(aes(x = "", y = value)) +
  geom_bar(stat = "identity", aes(fill = group)) +
  geom_text(aes(label = paste0(value)),
                position = position_stack(vjust = 0.5)) +
  coord_polar(start = 0, theta = "y") +
  scale_discrete_manual(labels = c('Over 30 Minutes', 'Under 30 Minutes'),
                        aesthetics = "fill",
                        values = colors) +
  labs(fill = "",
       title = "Exercise Time",
       subtitle = "24% of users achieved 30+ very active minutes per day") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


ggsave("images/exercise_time.png")


