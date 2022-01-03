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
         votes = str_replace_all(votes, ",", ""),
         votes = as.numeric(votes))

#Percentages to numeric
results <- results %>%
  mutate(perc = as.numeric(str_extract(perc, "\\d+.\\d+")))

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

#PNP
pnp_results <- results %>%
  select(county_name, party, perc, election) %>%
  filter(party == "PNP")

gov <- pnp_results %>% filter(election == "G") %>%
  rename("gov_perc" = "perc")
res <- pnp_results %>% filter(election == "R") %>%
  rename("res_perc" = "perc")

pnp_results <- left_join(gov, res, by = c("county_name"))

pnp_diff <- pnp_results %>%
  mutate(diff = res_perc - gov_perc)

#Mapping
prmap <- left_join(map_base, pnp_diff, by = "county_name")

#tiff("pnp_diff.tiff", units="in", width=7, height=5, res=1000)

prmap %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = diff/100),
          color = "#ffffff") + 
  labs(title = "Puerto Rico 2020 General Election",
       subtitle = "New Progressive Party (PNP)",
       fill = "Res. Commissioner %\n - Governor %") + 
  scale_fill_distiller(breaks = c(.06,.07,.08,.09,.10,.11,.12),
                       palette = "Blues", direction = 1, labels = scales::percent_format(accuracy = 1)) + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, margin = margin(0,0,2,0)),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(0,0,10,0)),
        legend.position = "bottom",
        legend.key.width = unit(2, 'cm'))

#dev.off()