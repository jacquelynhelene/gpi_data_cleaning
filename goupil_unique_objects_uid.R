# Goupil Stock Numbers and Unique Object IDs

library(tidyverse)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)
library(broom)
library(lubridate)
library(stringr)
library(readr)
library(knitr)
library(tidytext)
library(stringr)
library(ggraph)
library(ggrepel)
library(igraph)
library(uuid)


# Goupil concordance sheet:
raw_goupil_stocknumber_concordance <- read_csv("/Users/svanginhoven/Documents/GPI Projects/gpi_data_cleaning/data/goupil_stock_no_concordance.csv")

goupil <- read_csv("/Users/svanginhoven/Documents/GPI Projects/gpi_data_cleaning/data/goupil.csv")
# correct colum names -- look for code for that
goupil_colnames <- colnames(goupil)
goupil_colnames <- goupil_colnames %>%
  str_trim() %>%
  str_replace_all("[[:punct:] ]+$", "") %>%
  str_replace_all("[[:punct:] ]+", "_") %>%
  tolower()
colnames(goupil) <- goupil_colnames


## IDENTIFY RELATED OBJECTS AND CONNECT THEM VIA COMPONENTS:
# Editors have compiled links between stocknumbers that actually represent the
# same object. We compose a graph of these relationships, identify connected
# components that represnent all the stock numbers related to a single object,
# and then produce a lookup table pairing each stock number with a UID for its
# group
produce_goupil_stocknumber_concordance <- function(raw_goupil_stocknumber_concordance) {
  # Produce a 'long' table from the 'wide' version entered by editors
  goupil_stocknumber_concordance <- raw_goupil_stocknumber_concordance %>%
    select(contains("sn")) %>%
    mutate(prime_stock_number = sn1) %>%
    gather(number_index, stock_number, contains("sn"), na.rm = TRUE) %>%
    select(source = prime_stock_number, target = stock_number) %>%
    #filter(!(source %in% goupil_untrustworthy_numbers$goupil_number) & !(target %in% goupil_untrustworthy_numbers$goupil_number)) %>%
    na.omit()
  
  # Create a graph from this edgelist
  sn_graph <- igraph::graph_from_data_frame(goupil_stocknumber_concordance, directed = FALSE) %>%
    igraph::simplify()
  
  # Identify components and produce a lookup table
  sn_components <- igraph::components(sn_graph)
  igraph::V(sn_graph)$component <- sn_components$membership
  goupil_stocknumber_concordance <- igraph::as_data_frame(sn_graph, what = "vertices") %>%
    rename(persistent_uid = name)
  
  goupil_stocknumber_concordance
}

goupil_concordance <- produce_goupil_stocknumber_concordance(raw_goupil_stocknumber_concordance)

## PRODUCE UNIQUE IDS
# from discussions online, seems uuid package cannot be used to batch-generate uuid, so keeping knoedler's method:
# Produce unique ids for goupil objects based on their stock numbers
identify_goupil_objects <- function(df, goupil_concordance) {
  df %>%
    left_join(goupil_concordance, by = "persistent_uid") %>%
    # Because some of the goupil stock numbers changed or were re-used, we
    # will consult against a persistent id concordance that we can use to create
    # a "functional" id number - an identifier that connects objects even
    # when their nominal stock numbers are different. Those entries without any
    # stock nubmers at all are assumed to be standalone objects, and given a
    # unique id.
    mutate(
      prepped_sn = case_when(
        # When there is no number, generate a unique ID
        is.na(persistent_uid) ~ paste("gennum", as.character(seq_along(star_record_no)), sep = "-"),
        # When there is a number that has a prime # replacement from the
        # concordance, use that prime #
        !is.na(component) ~ paste("componentnum", component, sep = "-"),
        # When the original number has no recorded changes, group based on that
        # original number
        TRUE ~ paste("orignnum", persistent_uid, sep = "-"))) %>%
    mutate(object_id = paste("g", "object", group_indices(., prepped_sn), sep = "-")) %>%
    select(-component, -prepped_sn)
}

goupil_objects <- identify_goupil_objects(goupil, goupil_concordance)


## ORDERING EVENTS
# For a given object_id, attempt to discern an event order, which can be useful
# for discerning timespand boundaries as well as figuring out when an object
# first entered, and then finally left, Goupil's collection.
#
# This is called from within identify_goupil_transactions because it is a
# prerequisite to discerning which entries represent purchases by goupil vs.
# inventory events by goupil
order_goupil_object_events <- function(df) {
  df %>%
    # Use the entry date as the primary index of event date, falling back to the sale date if the entry date is not available.
    mutate(
      event_year = case_when(
        is.na(entry_date_year) ~ sale_date_year,
        TRUE ~ entry_date_year),
      event_month = case_when(
        is.na(entry_date_month) ~ sale_date_month,
        TRUE ~ entry_date_month),
      event_day = case_when(
        is.na(entry_date_day) ~ sale_date_day,
        TRUE ~ entry_date_day)) %>%
    # Produce an index per object_id based on this event year/month/day, falling
    # back to position in stock book if there is no year.
    group_by(object_id) %>%
    arrange(stock_book_no_1, stock_book_pg_1, stock_book_row_1, event_year, event_month, event_day, .by_group = TRUE) %>%
    mutate(event_order = seq_along(persistent_uid)) %>%
    ungroup() %>%
    # Remove intermediate columns
    select(-event_year, -event_month, -event_day)
}

goupil_objects_events <- order_goupil_object_events(goupil_objects)

# csv to upload
goupil_objects_events_upload <- goupil_objects_events %>%
  select(persistent_uid, object_id, event_order, pi_record_no)

# checks prior to upload
write_csv(goupil_objects_events_upload, "/Users/svanginhoven/Documents/GPI Projects/gpi_data_cleaning/data/goupil_objects_events_upload.csv")

goupil_check <- read_csv("/Users/svanginhoven/Documents/GPI Projects/gpi_data_cleaning/data/goupil_objects_events_upload.csv")
roll_up <- function(v, sep = "; ", collapse = TRUE) {
  if (collapse)
    v <- sort(unique(v))
  res <- paste0(na.omit(v), collapse = sep)
  if (res == "")
    return(NA_character_)
  res
}
goupil_check %>%
  group_by(object_id) %>%
  summarize(n = n(),
            pir = roll_up(pi_record_no)) %>%
  sample_n(20, replace = FALSE) %>%
  View()

#---