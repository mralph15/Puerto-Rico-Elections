### PUERTO RICO GENERAL ELECTION 2020 ###

#Packages
library(tidyverse)
library(urbnmapr)

#Data Upload
results <- read_csv("general_2020.csv")
map_base <- get_urbn_map(map = "territories_counties", sf = TRUE) %>%
  filter(state_name == "Puerto Rico")

##Data Wrangling
#Party Identification
results <- results %>%
  mutate(party = str_extract(name, "PPD|PNP|PD|PIP|MVC|Ind"))

#Votes to numeric
results <- results %>%
  mutate(votes = str_sub(votes, 3, -1),
         votes = str_replace_all(votes2, ",", ""),
         votes = as.numeric(votes2))

#Fix municipality names
results <- results %>%
  mutate(municipality = case_when(str_detect(municipality, "asco$") ~ "Anasco",
                                  str_detect(municipality, "^Bayam") ~ "Bayamon",
                                  str_detect(municipality, "^Can") ~ "Canovanas",
                                  str_detect(municipality, "^Cata") ~ "Catano",
                                  str_detect(municipality, "^Comer") ~ "Comerio",
                                  str_detect(municipality, "nica$") ~ "Guanica",
                                  str_detect(municipality, "^Juana") ~ "Juana Diaz",
                                  str_detect(municipality, "^Las Mar") ~ "Las Marias",
                                  str_detect(municipality, "^Lo") ~ "Loiza",
                                  str_detect(municipality, "^Manat") ~ "Manati",
                                  str_detect(municipality, "^Mayag") ~ "Mayaguez",
                                  str_detect(municipality, "^Pe") ~ "Penuelas",
                                  str_detect(municipality, "^Rinc") ~ "Rincon",
                                  str_detect(municipality, "o Grande$") ~ "Rio Grande",
                                  str_detect(municipality, "^San Germ") ~ "San German",
                                  str_detect(municipality, "^San Sebasti") ~ "San Sebastian",
                                  TRUE ~ municipality),
         county_name = paste(municipality, "Municipio"))

#Pivoting
results_pivot <- results %>%
  select(county_name, party, votes, election)


territories_counties %>%
  filter(state_name == "Puerto Rico") %>%
  ggplot() + 
  geom_sf(mapping = aes(),
          fill = "grey", color = "#ffffff") + 
  theme_void()