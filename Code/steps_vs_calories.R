source('code/import_clean_summarize.R')

# Are users who did not participate in the weight log burn less calories?

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

ggsave("images/steps_vs_calories.png")

# Calculate how much more calories burned for users who logged their weight.
percent_difference <- (logged_weight_calories/no_weight_calories)*100
percent_difference# 7.13% more calories burned for the users who logged their weight.

