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
library(googlesheets)


# Goupil concordance sheet:
goupil_stocknumber_concordance <- gs_url("https://docs.google.com/spreadsheets/d/1BzYIBz4UR0oQYmclcLZkP3Gsu0QYgrFSG2g0sWWZD9o/edit#gid=601877111")
raw_goupil_stocknumber_concordance <- gs_read(goupil_stocknumber_concordance)

# correct colum names
raw_goupil_colnames <- colnames(raw_goupil_stocknumber_concordance)
raw_goupil_colnames <- raw_goupil_colnames %>%
  str_replace_all("goupil_", "")
colnames(raw_goupil_stocknumber_concordance) <- raw_goupil_colnames

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
# ordering events by book, page and row since, unlike knoedler, in goupil entry dates are 
# intial entry dates
order_goupil_object_events <- function(df) {
  df %>%
    # Use the entry date as the primary index of event date, falling back to the sale date if the entry date is not available. TOOK OUT  event_year, event_month, event_day
    group_by(object_id) %>%
    arrange(stock_book_no_1, stock_book_pg_1, stock_book_row_1, .by_group = TRUE) %>%
    mutate(event_order = seq_along(persistent_uid)) %>%
    ungroup()
}

goupil_objects_events <- order_goupil_object_events(goupil_objects)

# csv to upload
goupil_objects_events_upload_final <- goupil_objects_events %>%
  select(persistent_uid, object_id, event_order)

# checks prior to upload
write_csv(goupil_objects_events_upload_final, "/Users/svanginhoven/Documents/GPI Projects/gpi_data_cleaning/data/goupil_objects_events_upload_final.csv")
