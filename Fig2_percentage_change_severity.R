
# accident severity bar grouping serious and fatal:
df_recoded <- df_recoded %>%
  mutate(severity_combined = ifelse(accident_severity %in% c("Serious", "Fatal"), "Fatal or Serious", "Slight"))

accidents_by_severity_comb_year <- df_recoded %>% 
  group_by(severity_combined, accident_year.x) %>% 
  summarise(accident_count = n(), .groups="drop")%>%
  filter(accident_year.x %in% c(2012, 2023))

percentage_change_severity2 <- accidents_by_severity_comb_year %>%
  pivot_wider(names_from = accident_year.x, values_from = accident_count, names_prefix="year_") %>%
  mutate(
    percentage_change_severity2 = ((year_2023 - year_2012) / year_2012) * 100
  ) %>%
  select(severity_combined, percentage_change_severity2)

severity_perc_change2 <- ggplot(percentage_change_severity2, aes(x = severity_combined, y = percentage_change_severity2, fill = severity_combined)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Percentage Change in UK Road Accidents by Severity (2012 to 2023)",
    x = "Accident Severity",
    y = "Percentage Change",
    caption="GOV.UK Road Safety Data"
  ) +
  geom_hline(yintercept=0, colour="black", size=0.5)+
  scale_fill_manual(values=c("#D55E00", "#F0E442",
                             "#D55E00", "#F0E442"))+
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits=c(-50, 50),
                     breaks=seq(-50, 50, 10)) +
  theme_clean()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust=0.5),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.background = element_blank()
        )

severity_perc_change2

ggsave("severity_perc_change.png", plot = severity_perc_change2, width=8, height=4.8, dpi = 300)

 