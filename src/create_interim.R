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

# PROCESS TO INTERIM DATA

# Exclude "Werken"
interim_data <-
  raw_data |>
  dplyr::filter(Domein != "Werken")

# Select relevant columns and clean their names
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
  "Materiaal_Vast",
  "Materiaal_Consumptie"
)
interim_data <-
  interim_data |>
  dplyr::select(dplyr::all_of(relevant_cols)) |>
  janitor::clean_names()

# Transform Lambert coordinates to WGS84
coordinates <-
  interim_data |>
  sf::st_as_sf(coords = c("x", "y"), crs = 31370) |>
  sf::st_transform(crs = 4326) |>
  sf::st_coordinates() |>
  dplyr::as_tibble() |>
  dplyr::rename(
    latitude = Y,
    longitude = X
  )
interim_data <-
  dplyr::bind_cols(interim_data, coordinates) |>
  dplyr::relocate(latitude, longitude, .after = y)

# Clean values
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
    materiaal_consumptie = str_clean(materiaal_consumptie),
    global_id = stringr::str_remove_all(global_id, "\\{|\\}")
  )

# Filter out no-relevant species
exclude_species <- c(
  "Duiven",
  "Kippen",
  "Neerhofdier(en)",
  "Zwerfkatten"
)
interim_data <-
  interim_data |>
  dplyr::filter(!soort %in% exclude_species)

# Process property columns (Waarneming, Actie, Materiaal Vast, Materiaal Consumptie)
interim_data <-
  interim_data |>
  # Separate values of property columns into columns
  tidyr::separate(
    waarneming,
    into = c("p_waarneming_1", "p_waarneming_2", "p_waarneming_3", "p_waarneming_4", "p_waarneming_5", "p_waarneming_6"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    actie,
    into = c("p_actie_1", "p_actie_2", "p_actie_3", "p_actie_4", "p_actie_5"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    materiaal_vast,
    into = c("p_materiaal_vast_1", "p_materiaal_vast_2", "p_materiaal_vast_3", "p_materiaal_vast_4", "p_materiaal_vast_5"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    materiaal_consumptie,
    into = c("p_materiaal_consumptie_1", "p_materiaal_consumptie_2", "p_materiaal_consumptie_3", "p_materiaal_consumptie_4", "p_materiaal_consumptie_5", "p_materiaal_consumptie_6"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = FALSE,
    extra = "drop"
  ) |>
  # Convert property columns into rows
  tidyr::pivot_longer(
    cols = tidyr::starts_with("p_"),
    values_to = "p_key_value",
    names_to = "p_field",
    values_drop_na = TRUE,
    names_repair = "unique"
  ) |>
  dplyr::mutate(p_field = stringr::str_remove(p_field, "_\\d$")) |>
  tidyr::separate(
    p_key_value,
    into = c("p_key", "p_value"),
    sep = " = ",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  )

# WRITE INTERIM DATA
readr::write_csv(interim_data, here::here("data", "interim", "interim.csv"), na = "")
