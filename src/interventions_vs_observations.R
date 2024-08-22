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


# Animals -----------------------------------------------------------------

animal_records <- filter(raw_occurrences, Domein == "Dier")



## The first record of a case ----------------------------------------------

  # is always an observation, it can be an absence, and it can be an
  # intervention as well!

  # The first record of a case is the one where Laatst_Bewerkt_Datum is the lowest
  # for a given Dossier_ID


# animal_records %>%
#   group_by(Dossier_ID) %>%
#   summarise(
#     dossier_create_date = min(Laatst_Bewerkt_Datum)
#   ) %>%
#   ungroup() %>%
#   filter(first_intervention) %>%
#   select(Dossier_ID, first_action, first_intervention)

## Change within a case ----------------------------------------------------


# Did the action change relative to the previous action of the same case

animal_records %>%
  group_by(Dossier_ID) %>%
  summarise(first_action = first(Actie),
            .groups = "keep") %>%
  mutate(
    previous_action = lag(Actie),
    action_changed = ifelse(Actie != previous_action & !first_action, TRUE, FALSE)
  ) %>%
  ungroup() %>%
  filter(action_changed) %>%
  select(Dossier_ID, Actie, previous_action, action_changed)

## is the action in the list? ----------------------------------------------

animal_intervention_actions <-
    c(
      "vang",
      "schud",
      "bestreden",
      "behandeld"
    )

animal_records %>%
  filter(stringr::str_detect(Actie,
                             paste(animal_intervention_actions,
                                   collapse = "|")
                             )
         ) %>%


# QUESTION: what to do with nevenvangsten?
