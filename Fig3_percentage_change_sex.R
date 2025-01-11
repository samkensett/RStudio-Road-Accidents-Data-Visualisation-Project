# Calculating percentage change relative to 2012
accidents_by_year_sex <- df_recoded %>%
  group_by(accident_year.x, sex_of_driver) %>%
  summarize(accident_count = n(), .groups = "drop") %>%
  pivot_wider(names_from = sex_of_driver, values_from = accident_count, values_fill = 0) %>%
  mutate(
    Total = Female + Male
  ) %>%
  # Calculating percentage change relative to 2012
  mutate(
    Female_pct = (Female - Female[accident_year.x == 2012]) / Female[accident_year.x == 2012] * 100,
    Male_pct = (Male - Male[accident_year.x == 2012]) / Male[accident_year.x == 2012] * 100,
    Total_pct = (Total - Total[accident_year.x == 2012]) / Total[accident_year.x == 2012] * 100
  )

# Plotting the percentage change
sex_total_pct_change <- ggplot(accidents_by_year_sex, aes(x = accident_year.x)) +
  geom_line(aes(y = Female_pct, color = "Female"), linewidth = 1) +
  geom_line(aes(y = Male_pct, color = "Male"), linewidth = 1) +
  geom_line(aes(y = Total_pct, color = "Total"), linewidth = 1) +
  labs(
    title = "Percentage Change in UK Road Accidents by Year and Sex (Relative to 2012)",
    x = "Year",
    y = "Percentage Change Relative to 2012",
    color = "Sex of Driver",
    caption = "GOV.UK Road Safety Data"
  ) +
  geom_hline(yintercept=0, colour="black", size=0.5, linetype="dashed")+
  scale_x_continuous(breaks = seq(2012, 2023, 1)) +
  scale_y_continuous(breaks = seq(-50, 50, 10),
                     labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = c("Total" = "black", "Male" = "#56B4E9", "Female" = "#009E73"))+
  theme_clean() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.3),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1 ),
    plot.background = element_blank()
  )

sex_total_pct_change

ggsave("sex_total_pct_change.png", plot = sex_total_pct_change, width=8, height=4.8, dpi = 300)


