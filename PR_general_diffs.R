### PUERTO RICO GENERAL ELECTION 2020 ###

#Packages
library(tidyverse)
library(urbnmapr)

#Data Upload

territories_counties <- get_urbn_map(map = "territories_counties", sf = TRUE) %>%
  filter(state_name == "Puerto Rico")

territories_counties %>%
  filter(state_name == "Puerto Rico") %>%
  ggplot() + 
  geom_sf(mapping = aes(),
          fill = "grey", color = "#ffffff") + 
  theme_void()


