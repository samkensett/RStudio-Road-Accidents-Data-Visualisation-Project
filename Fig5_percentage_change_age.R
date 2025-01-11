# Plot 5: percentage change road accidents by age

# Calculate percentage change between 2012 and 2023
accident_count_by_age <- df_recoded %>% 
  group_by(age_of_driver, accident_year.x) %>% 
  summarise(accident_count = n(), .groups="drop") %>% 
  filter(age_of_driver > 16, accident_year.x %in% c(2012, 2023))

accident_count_by_age$accident_year.x <- as.factor(accident_count_by_age$accident_year.x)

percent_change_data <- accident_count_by_age %>%
  pivot_wider(names_from = accident_year.x, values_from = accident_count) %>%
  mutate(percentage_change = ((`2023` - `2012`) / `2012`) * 100) %>%
  filter(!is.na(percentage_change), age_of_driver < 90)  # Remove any rows with missing data

# Plot the percentage change as a single line
percentage_change_age <- ggplot(data = percent_change_data, 
                                 aes(x = age_of_driver, y = percentage_change)) +
  geom_line(linewidth = 1)+
  geom_point(aes(color = percentage_change), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  labs(title = "Percentage Change in UK Road Accidents by Age (2012 to 2023)",
       x = "Age of Driver",
       y = "Percentage Change",
       caption = "GOV.UK Road Safety Data") +
  geom_hline(yintercept=0, colour="black", size=0.5, linetype="dashed")+
  theme_clean() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.background = element_blank()) +
  scale_x_continuous(breaks = seq(20, 90, 10), limits = c(16, 90))+
  scale_y_reverse(labels = scales::percent_format(scale = 1),
                     breaks = seq(-60, 20, 20),
                     limits =c(20,-60))+
  scale_color_gradient2(low = "#56B4E9", mid = "#D55E00", high = "#D55E00", midpoint = -5)
  
# Display the plot
percentage_change_age

ggsave("percentage_change_age.png", plot = percentage_change_age, width=8, height=4.8, dpi = 300)

