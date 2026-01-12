library(ratatouille)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(here)

# GET LIVE DATA
raw_data <- ratatouille::ratatouille()

# PROCESS TO INTERIM DATA

# Exclude "Werken"
interim_data <- dplyr::filter(raw_data, Domein != "Werken")

# Select and reorder relevant columns
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
interim_data <- dplyr::select(interim_data, dplyr::all_of(relevant_cols))

# Create function to clean ; in string values
str_clean <- function(string) {
  string <-
    # Trim + use single spaces
    stringr::str_squish(string) |>
    # Remove space after ; separator
    stringr::str_replace_all("; ", ";") |>
    # Remove last ;
    stringr::str_remove(";$")
  return(string)
}

# Process property columns (Waarneming, Actie, Materiaal Vast, Materiaal Consumptie)
interim_data <-
  interim_data |>
  # Remove stray spaces, semicolons, etc.
  dplyr::mutate(
    Waarneming = str_clean(Waarneming),
    Actie = str_clean(Actie),
    Materiaal_Vast = str_clean(Materiaal_Vast),
    Materiaal_Consumptie = str_clean(Materiaal_Consumptie),
    GlobalID = stringr::str_remove_all(GlobalID, "\\{|\\}")
  ) |>
  # Separate values of property columns into columns
  tidyr::separate(
    Waarneming,
    into = c("p_waarneming_1", "p_waarneming_2", "p_waarneming_3", "p_waarneming_4", "p_waarneming_5", "p_waarneming_6"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    Actie,
    into = c("p_actie_1", "p_actie_2", "p_actie_3", "p_actie_4", "p_actie_5"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    Materiaal_Vast,
    into = c("p_materiaal_vast_1", "p_materiaal_vast_2", "p_materiaal_vast_3", "p_materiaal_vast_4", "p_materiaal_vast_5"),
    sep = "\\s*\\;\\s*",
    remove = TRUE,
    convert = TRUE,
    extra = "drop"
  ) |>
  tidyr::separate(
    Materiaal_Consumptie,
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

# Filter out no-relevant species
exclude_species <- c(
  "Duiven",
  "Kippen",
  "Kippen:",
  "Neerhofdier(en)",
  "Neerhofdier(en):",
  "Zwerfkatten"
)
interim_data <- dplyr::filter(interim_data, !Soort %in% exclude_species)

# WRITE INTERIM DATA
readr::write_csv(interim_data, here::here("data", "interim", "interim.csv"), na = "")
