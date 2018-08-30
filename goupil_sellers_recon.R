library(dplyr)
library(tidyr)

goupil_sellers <- read.csv("/Users/svanginhoven/Desktop/goupil_sellers.csv")

sellers_star <- goupil_sellers %>%
  group_by(Sell1) %>%
  summarize(star_all = paste0(star, collapse = ";"))

sellers_ulan <- goupil_sellers %>%
  select(-label, -agentType, -Query) %>%
  group_by(Sell1) %>%
  summarize_all(funs(first))

sellers_ulan_new <- sellers_ulan %>%
  gather(key, value, -Sell1, -star) %>%
  extract(key, c("info", "alternative_nr"), "([A-Za-z_]+)_(\\d+)") %>%
  spread(info, value)

sellers_ulan_new <- sellers_ulan_new %>%
  left_join(sellers_star, by = "Sell1") %>%
  mutate(selected = "",
         alternative_nr = as.numeric(alternative_nr)) %>%
  select(Sell1, alternative_nr, selected, prefLabel, nationalities, birthDate, deathDate, endDate, labels, role, Score, startDate, type, URL, Vocab_ID, star_all)

write.csv(sellers_ulan_new, "/Users/svanginhoven/Desktop/goupil_sellers_new.csv")
