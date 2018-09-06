library(tidyverse)


# Load data from CSV

goupil_object_events <- read.csv("C:\\Users\\jclements\\Desktop\\GPI_PIR\\GPI_Data_Cleaning\\gpi_data_cleaning\\data\\goupil_objects_events_upload.csv")

# Rollup function

roll_up <- function(v, sep = "; ", collapse = TRUE) {
  if (collapse)
    v <- sort(unique(v))
  res <- paste0(na.omit(v), collapse = sep)
  if (res == "")
    return(NA_character_)
  res
}


# Group Goupil objects by pi_record_no

goupil_objects <- goupil_object_events %>%
  group_by(object_id) %>%
  summarise(count = n(), goupil_numbers = roll_up(pi_record_no, sep="; ")) %>%
  filter(count >1)
      
