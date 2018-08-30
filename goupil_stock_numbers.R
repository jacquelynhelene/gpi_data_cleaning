# create bindings

library(remake)
library(tidyverse)
library(forestry)
library(grid)
library(gridExtra)
library(scales)
library(remake)
library(tidyverse)
library(forestry)
library(grid)
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(lubridate)
library(stringr)
library(readr)
library(knitr)
library(reshape2)
library(ggridges)

# Clear any bindings before running make()
delete_bindings()
# Refresh .remake object cache

# IMPORTANT ---- If you have reinstalled GPIdata2, you must run the make("tidy")
# command to refresh the basic GPI data in the remake directory. After that, run
# make() as usual.
make("tidy")
make("goupil")
make("goupil_stock_book_nos")
make("goupil_artists")

make()
# Create bindings for use in Rmd document
create_bindings()

roll_up <- function(v, sep = "; ", collapse = TRUE) {
  if (collapse)
    v <- sort(unique(v))
  res <- paste0(na.omit(v), collapse = sep)
  if (res == "")
    return(NA_character_)
  res
}

goupil_artists_names <- goupil_artists %>%
  group_by(star_record_no) %>%
  arrange(artist_authority) %>%
  summarize(artists = roll_up(artist_authority))

goupil_rev <- goupil %>%
  select(star_record_no, pi_record_no, stock_book, goupil_number, page_number, row_number, title, entry_date_year, sale_date_year)
goupil_sbn <- goupil_stock_book_nos

goupil_rev <- goupil_rev %>%
  left_join(goupil_artists_names, by = "star_record_no")

# 1- CONCORDANCE BY MARCHING GOUPIL# AND ARTIST = 32,590 UNIQUE OBJECTS
goupil_conc <- goupil_rev %>%
  filter(artists != "[ANONYMOUS]", !is.na(artists))
goupil_conc_rest <- goupil_rev %>%
  anti_join(goupil_conc)
goupil_conc_rest <- goupil_conc_rest %>%
  select(concordance_star = star_record_no, concordance_pinos = pi_record_no, goupil_number, artists) %>%
  mutate(repeated_rows = 1, gn_repeated = FALSE)

goupil_conc <- goupil_conc %>%
  group_by(goupil_number, artists) %>%
  mutate(concordance_star = roll_up(star_record_no),
         concordance_pinos = roll_up(pi_record_no)) %>%
  select(concordance_star, concordance_pinos, goupil_number, artists) %>%
  group_by(concordance_star, concordance_pinos, goupil_number, artists) %>%
  mutate(repeated_rows = n())
goupil_conc_summary <- unique(goupil_conc)
goupil_conc_summary <- goupil_conc_summary %>%
  group_by(goupil_number) %>%
  mutate(gn_repeated = ifelse(n()>1, TRUE, FALSE)) %>%
  ungroup()

#adding to have all 43969 groupil records
goupil_conc_summary <- goupil_conc_summary %>%
  bind_rows(goupil_conc_rest)
# this brings total objects to 32,771 entries due to entries duplicated for co-authored works

write_csv(goupil_conc_summary, "/Users/svanginhoven/Desktop/goupil_conc_summary.csv")

goupil_concordance_stock_nos <- goupil_conc_summary %>%
  select(concordance_star) %>%
  separate(concordance_star, c("goupil_sn1", "goupil_sn2", "goupil_sn3", "goupil_sn4", "goupil_sn5", "goupil_sn6", "goupil_sn7", "goupil_sn8"), sep = ";", remove = TRUE)
write_csv(goupil_concordance_stock_nos, "/Users/svanginhoven/Desktop/goupil_concordance_stock_nos.csv")

### THROW AWAY ----------------------------------------------------------------------------
# one example is star_record_no == "13171"
# corresponds to goupil_stock_no == "10018"
# records say it appears in books 7 and 8
# these are the only ones possible to connect
# as references to books 1 and 2 do not have page or row numbers
# and I cannot find paintings by Corot in those books
# there are no easily  matching titles with "bois" or "vache" in title...

# another example: goupil_stock_no == "12994"
# by JACQUE, CHARLES EMILE entitled "Cochons"
# in book 9 and 10 complete references
# but star_record_no == "20289" should have book 10 (not 9) as "other book"

# 2-CONCORDANCE BY OTHER SB AND NUMBER FIELDS = DOES NOT WORK
goupil_rev_other <- goupil_rev %>%
  select(add_other_star = star_record_no, add_other_pi_record_no = pi_record_no, add_other_stock_book = stock_book, add_other_goupil_number = goupil_number, add_other_page_number = page_number, add_other_row_number = row_number) %>%
  mutate(other_sb_gn = paste0(add_other_stock_book, "-", add_other_goupil_number))
# join "other" information
goupil_sbn_new <- goupil_sbn %>%
  mutate(other_sb_gn = paste0(other_stock_book_no, "-", other_stock_book_goupil_no)) %>%
  left_join(goupil_rev_other, by = "other_sb_gn")
# now we have the star_record_no and pi_record_no for the "other" instances
# next is to append this "other" information to the original entries, so to make a concordance for star_record_nos and pi_record_nos
goupil_concordance2 <- goupil_rev %>%
  select(original_star_record_no = star_record_no, original_pi_record_no = pi_record_no, original_stock_book = stock_book, original_goupil_number = goupil_number, original_page_number = page_number, original_row_number = row_number) %>%
  left_join(goupil_sbn_new, by = c("original_star_record_no" = "star_record_no"))

# when stock numbers are different but tracked:

# example 1: star_record_no == "24589" is stock_no == "10905" and the other_stock_no == "10558" but these stock items are unrelated!

# example 2, must be a typo:
# star_record_no =="25318"
# first goupil_no === "16270" --> DIAZ DE LA PEÑA, NARCISSE VIRGILE	Femme blonde 2 amours
# and other_goupil_no == "16170" --> DAUBIGNY, CHARLES FRANÇOIS	Paysage, rivière

# example 3: old# == "317", sn == "1115"
# early #317 page 25 is PAPETY, DOMINIQUE LOUIS FERRÉOL (French) Consolatrix afflictorum
# book 1 page 125:  #1115 is BROCHART, CONSTANT JOSEPH (French) Concorde
# turns into #317 in book 2

# example 4: 370 and 1168
# sn 370 book 1 page 41 is SCHEFFER, ARY (French)	Marguerite au jardin - sold
# sn 1168 book 1 page 131 is MEISSONIER, JEAN LOUIS ERNEST (French) Le mousquetaire gris, purchased - unsold
# takes on sn 370 book 2 page 41 sold (but this record is G-7339 is duplicated???)

# NOTE: other number re-uses
# goupil_rev %>% select(goupil_number, artist_authority, title) %>% group_by(goupil_number, artist_authority) %>% summarize(n = n()) %>% View()
