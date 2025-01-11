#all required packages:
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
install.packages("devtools")
devtools::install_github("ropensci/rnaturalearthhires")
install.packages("patchwork")
install.packages("scales")
install.packages("ggthemes")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(gridExtra)
library(patchwork)
library(scales)
library(ggthemes)

# STATS19 accident report form for reference:
# https://assets.publishing.service.gov.uk/media/60d0cc548fa8f57ce4615110/stats19.pdf

# importing the original datasets from https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data
road_data_casualty <- read.csv("dft-road-casualty-statistics-casualty-1979-latest-published-year.csv")
road_data_collision <- read.csv("dft-road-casualty-statistics-collision-1979-latest-published-year.csv")
road_data_vehicle <- read.csv("dft-road-casualty-statistics-vehicle-1979-latest-published-year.csv")

#joining the three sets into a single dataframe
df_complete <- road_data_collision %>% 
  dplyr::left_join(road_data_vehicle, by = "accident_index") %>%
  dplyr::left_join(road_data_casualty, by = "accident_index")
  
# creating a subset with only the relevant columns
df_subset <- subset(df_complete, select = c(accident_index, accident_year.x, longitude, latitude, 
                                            accident_severity, sex_of_driver, age_of_driver 
                                            ))

#the stats19 form was updated in 2011 and 2024, so take subset with 2012-2023 for consistency 
df_sub_2012 <- df_subset[df_subset$accident_year.x %in% 2012:2023, ]



# recoding the values of relevant variables to correspond with their labels on the form
df_recoded <- df_sub_2012 %>% 
  mutate(accident_severity = recode(accident_severity,
                                    '1' = "Fatal",
                                    '2' = "Serious",
                                    '3' = "Slight"),
         sex_of_driver = recode(sex_of_driver, 
                                '1' = "Male",
                                '2' = "Female",
                                '3' = "Not known"))
View(df_recoded)

# taking a random sample of 100,000 cases to make processing speeds more manageable
df_sample <- df_recoded %>% sample_n(100000)







# saving necessary subsets
write.csv(df_sample, "df_sample.csv", row.names=FALSE)
write.csv(df_recoded, "df_recoded.csv", row.names=FALSE)

### to save time importing each session:
df_sample <- read.csv("df_sample.csv") #sample 100,000 2012-2023
df_recoded <- read.csv("df_recoded.csv") # recoded dataframe




