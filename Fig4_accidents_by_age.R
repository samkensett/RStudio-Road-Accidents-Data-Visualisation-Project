# Shaded line of Accident Count by Age 2012 vs 2023

# shaded line chart, using Okabe-Ito  colours to ensure colourblind accessibility

accident_count_by_age <- df_recoded %>% 
  group_by(age_of_driver, accident_year.x) %>% 
  summarise(accident_count = n(), .groups="drop") %>% 
  filter(age_of_driver > 16, accident_year.x %in% c(2012, 2023))

accident_count_by_age$accident_year.x <- as.factor(accident_count_by_age$accident_year.x)


shaded_data <- accident_count_by_age %>%
  pivot_wider(names_from = accident_year.x, values_from = accident_count) %>%
  mutate(min_count = pmin(`2012`, `2023`),  
         max_count = pmax(`2012`, `2023`))

age_line <- ggplot() +
  geom_ribbon(data = shaded_data,
              aes(x = age_of_driver, ymin = min_count, ymax = max_count),
              fill = "grey", alpha = 0.3) +
  geom_line(data = accident_count_by_age,
            aes(x = age_of_driver, y = accident_count, group = accident_year.x, colour = accident_year.x),
            linewidth = 0.8) +
  labs(title = "UK Road Accidents by Age of Driver (2012 vs 2023)",
       x = "Age of Driver",
       y = "Accident Count",
       caption = "GOV.UK Road Safety Data",
       colour = "Year") +
  scale_color_manual(values = c("2012" = "#CC79A7", "2023" =  "#0072B2")) +
  theme_clean() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.background = element_blank()
  ) +
  scale_x_continuous(breaks = seq(20, 100, 10),
                     limits = c(16, 100))

age_line

ggsave("age_line.png", plot = age_line, width=8, height=4.8, dpi = 300)
