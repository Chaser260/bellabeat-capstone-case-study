source('code/import_clean_summarize.R')

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

ggsave('images/steps_by_hour.png')

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

ggsave('images/activity_heatmap.png')
