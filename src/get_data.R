library(ratatouille)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(sf)
library(readr)
library(here)

# GET LIVE DATA
raw_data <- ratatouille::ratatouille(source = "rato")

# SELECT DATA FOR ANIMALS & PLANTS
interim_data <-
  raw_data |>
  dplyr::filter(Domein %in% c("Dier", "Plant"))

# SELECT RELEVANT COLUMNS
relevant_cols <- c(
  "Dossier_ID",
  "Soort",
  "Gemeente",
  "X",
  "Y",
  "OBJECTID",
  "GlobalID",
  "Laatst_Bewerkt_Datum",
  "Waarneming",
  "Actie",
  "Materiaal_Vast"
)
interim_data <-
  interim_data |>
  dplyr::select(dplyr::all_of(relevant_cols)) |>
  janitor::clean_names() # Convert to snake_case

# CONVERT COORDINATES
# Convert Lambert UTM to latitude & longitude
coordinates <-
  interim_data |>
  sf::st_as_sf(coords = c("x", "y"), crs = 31370) |>
  sf::st_transform(crs = 4326) |>
  sf::st_coordinates() |>
  dplyr::as_tibble() |>
  dplyr::rename(
    latitude = X,
    longitude = Y
  )
interim_data <-
  dplyr::bind_cols(interim_data, coordinates) |>
  dplyr::relocate(latitude, longitude, .after = y)

# Round coordinates
interim_data <-
  interim_data |>
  # Round UTM to 1m
  dplyr::mutate(
    x = round(x),
    y = round(y)
  ) |>
  # Round lat/lon to 5 decimals
  dplyr::mutate(
    latitude = round(latitude, 5),
    longitude = round(longitude, 5)
  )

# TRIM VALUES
str_clean <- function(string) {
  string <-
    # Trim + use single spaces
    stringr::str_squish(string) |>
    # Remove space after ; separator
    stringr::str_replace_all("; ", ";") |>
    # Remove last ;
    stringr::str_remove("[;|:]$")
  return(string)
}
interim_data <-
  interim_data |>
  dplyr::mutate(
    soort = str_clean(soort),
    waarneming = str_clean(waarneming),
    actie = str_clean(actie),
    materiaal_vast = str_clean(materiaal_vast),
    global_id = stringr::str_remove_all(global_id, "\\{|\\}")
  )

# ADD CONFIRMED OBSERVATION AND CATCH
# This is based on specific "waarneming" or "actie" values
interim_data <-
  interim_data |>
  mutate(confirmed_observation = case_when(
    str_detect(waarneming, "Haard vastgesteld = [1-9]") ~ TRUE,
    str_detect(waarneming, "Vastgesteld = [1-9]") ~ TRUE,
    str_detect(waarneming, "Vastgesteld \\(aantal\\) = [1-9]") ~ TRUE,
    str_detect(waarneming, "Vastgesteld \\(in m²\\) = [1-9]") ~ TRUE,
    str_detect(actie, "Eieren geschud \\(aantal\\) = [1-9]") ~ TRUE,
    str_detect(actie, "Gevangen = [1-9]") ~ TRUE,
    str_detect(actie, "Gevangen juveniel \\(aantal\\) = [1-9]") ~ TRUE,
    str_detect(actie, "Gevangen volwassenen \\(aantal\\) = [1-9]") ~ TRUE,
    str_detect(actie, "Hoeveelheid = [1-9]") ~ TRUE,
    str_detect(actie, "Vangst \\(aantal\\) = [1-9]") ~ TRUE,
    str_detect(actie, "Vastgesteld = [1-9]") ~ TRUE,
    str_detect(actie, "Verwijderd \\(aantal m²\\) = [1-9]") ~ TRUE
    # Note on some values we do not include:
    # "Beverdam": does not imply animal was seen
    # "Nevenvangst": is not an observation of the main species
  )) |>
  mutate(catch = case_when(
    confirmed_observation & str_detect(actie, "Gevangen") ~ TRUE,
    confirmed_observation & str_detect(actie, "Vangst") ~ TRUE,
  ))

# ORDER DATA
interim_data <-
  interim_data |>
  dplyr::arrange(
    dossier_id,
    laatst_bewerkt_datum
  )

# SELECT SPECIES
# Join with species reference data and filter on species we want to include
species <- readr::read_csv(here::here("data", "reference", "species.csv"))
interim_data <-
  interim_data |>
  dplyr::left_join(species, by = "soort") |>
  dplyr::relocate(kingdom, scientific_name, taxon_rank, .after = "soort") |>
  dplyr::filter(include) |>
  dplyr::select(-include)

# WRITE DATA
readr::write_csv(interim_data, here::here("data", "interim", "interim.csv"), na = "")
