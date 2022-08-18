source('code/import_clean_summarize.R')

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
