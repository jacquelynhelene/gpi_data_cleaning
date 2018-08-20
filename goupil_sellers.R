library(tidyverse)
library(dplyr)

#Load Goupil Sellers CSV
goupil_sellers <- read.csv("goupil_sellers.csv")

goupil_sellers_filtered <- goupil_sellers %>%
  select(STAR, Sell1, prefLabel_1)
  group_by(goupil_sellers_filtered, Sell1, add = TRUE)
  mutate(goupil_sellers_filtered, STAR)
  