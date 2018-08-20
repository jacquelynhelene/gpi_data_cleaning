library(tidyverse)

#Load Goupil Sellers CSV
goupil_sellers <- read.csv("goupil_sellers.csv")

goupil_sellers_filtered <- goupil_sellers %>%
  select(STAR, Sell1, prefLabel_1) %>%
  group_by(Sell1) %>%
  summarize(STAR_no = paste(STAR, collapse=", "))

goupil_sellers_pref <- goupil_sellers %>%
  select(STAR, Sell1, prefLabel_1, prefLabel_2, prefLabel_3, prefLabel_4) %>%
  left_join(goupil_sellers_filtered, by = c("STAR_no"))
            