# using rnaturalearth to retrieve county-level data for the uk
uk_counties <- ne_states(country = "United Kingdom", returnclass = "sf")

# coordinates must be numeric, let's check:
str(df_sample)

# longitude and latitude are not numeric so I will convert them:
df_sample$longitude <- as.numeric(df_sample$longitude)
df_sample$latitude <- as.numeric(df_sample$latitude)


# checking for NA values:
sum(is.na(df_sample$longitude))
sum(is.na(df_sample$latitude))

# filtering out the NA values:
df_sample <- df_sample %>%
  filter(!is.na(longitude) & !is.na(latitude))

# adding column of coordinate points in CRS form
df_sample <- st_as_sf(df_sample, coords = c("longitude", "latitude"), crs = 4326)

# this removed the longitude and latitude columns, let's add them back in:
df_sample <- df_sample %>%
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  )



# dataframes with geometry only to improve processing speed
geometry_only_2012 <- subset(df_sample[df_sample$accident_year.x %in% 2012, ], select = c(accident_index, geometry, longitude, latitude))
geometry_only_2018 <- subset(df_sample[df_sample$accident_year.x %in% 2018, ], select = c(accident_index, geometry, longitude, latitude))
geometry_only_2023 <- subset(df_sample[df_sample$accident_year.x %in% 2023, ], select = c(accident_index, geometry, longitude, latitude))





# plotting uk_counties with geometry overlayed representing accident locations in 2012:
map2012 <- ggplot(data = uk_counties) +
  geom_sf(fill = "grey95", color = "black") + 
  geom_sf(data = geometry_only_2012, color = "firebrick", alpha = 0.7, size=0.1) +  # Use geom_sf for consistency
  ggtitle("2012") +
  #labs(caption="gov.uk\nRoad Safety Data")+
  #coord_sf(xlim = c(-1.2, -1.7), ylim = c(53, 53.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 20, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_blank(),  
    axis.text = element_blank()    
  )



# map for 2023
map2023 <- ggplot(data = uk_counties) +
  geom_sf(fill = "grey95", color = "black") + 
  geom_sf(data = geometry_only_2023, color = "firebrick", alpha = 0.7, size=0.1) +  # Use geom_sf for consistency
  ggtitle("2023") +
  #labs(caption="gov.uk\nRoad Safety Data")+
  #coord_sf(xlim = c(-1.2, -1.7), ylim = c(53, 53.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 20, hjust = 0.5),
    #plot.caption = element_text(size=8, hjust=0.5),
    panel.background = element_rect(fill = "white", color = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_blank(),  
    axis.text = element_blank()    
  )


# combining the plots with patchwork
combined_map <- (map2012 | map2023) + 
  plot_annotation(caption = "GOV.UK Road Safety Data", title = "UK Road Accidents by Year") &
  theme(plot.caption = element_text(size = 18, hjust = 0.5),
        plot.title = element_text(size = 26, face="bold", hjust=0.5))

combined_map


# saving the plot as an image (these dimensions also make the plot clearer)
ggsave("mapviz_final.png", plot = combined_map, width=15, height=9, dpi = 300)

