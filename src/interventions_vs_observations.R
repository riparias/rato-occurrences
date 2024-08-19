# Distinguish between observations and interventions based on the Actie field

# load libraries ----------------------------------------------------------
library(dplyr)

# load data ---------------------------------------------------------------

# run fetch_data.Rmd

glimpse(raw_occurrences)


# Plants ------------------------------------------------------------------
plant_records <- filter(raw_occurrences, Domein == "Plant")

## Any action means an intervention ---------------------------------------

# Let's list all the different actions for plants
plant_records$Actie %>%
  stringr::str_split(stringr::fixed(";")) %>%
  unlist() %>%
  # drop quantities
  stringr::str_remove(" = [0-9]+$") %>%
  stringr::str_trim() %>%
  # count via a tibble
  dplyr::tibble(Action = .) %>%
  count(Action, sort = TRUE)

### Set empty strings as NA -----------------------------------------------

plant_records <-
  plant_records %>%
  mutate(Actie = ifelse(Actie == "", NA, Actie)) %>%µ
  # Any action is an intervention
  mutate(intervention = ifelse(!is.na(Actie), TRUE, FALSE))

# QUESTION: does this mean that the remaining records are observations?


