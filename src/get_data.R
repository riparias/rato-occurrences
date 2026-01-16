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

# CONVERT PROPERTY COLUMNS TO ROWS
# These columns are: waarneming, actie, materiaal_vast
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
