library(ratatouille)
library(dplyr)
library(stringr)
library(janitor)
library(sf)
library(readr)
library(here)

# GET LIVE DATA
## raw_data <- ratatouille(source = "rato")

# SELECT DATA FOR ANIMALS & PLANTS
interim_data <-
  raw_data |>
  filter(Domein %in% c("Dier", "Plant"))

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
  select(all_of(relevant_cols)) |>
  janitor::clean_names() # Convert to snake_case

# SELECT SPECIES
# Join with species reference data and filter on species we want to include
species <- read_csv(here("data", "reference", "species.csv"))
interim_data <-
  interim_data |>
  left_join(species, by = "soort") |>
  relocate(kingdom, scientific_name, taxon_rank, .after = "soort") |>
  filter(include) |>
  select(-include)

# CONVERT COORDINATES
# Convert Lambert UTM to latitude & longitude
coordinates <-
  interim_data |>
  sf::st_as_sf(coords = c("x", "y"), crs = 31370) |>
  sf::st_transform(crs = 4326) |>
  sf::st_coordinates() |>
  as_tibble() |>
  rename(
    latitude = X,
    longitude = Y
  )
interim_data <-
  bind_cols(interim_data, coordinates) |>
  relocate(latitude, longitude, .after = y)

# Round coordinates
interim_data <-
  interim_data |>
  # Round UTM to 1m
  mutate(
    x = round(x),
    y = round(y)
  ) |>
  # Round lat/lon to 5 decimals
  mutate(
    latitude = round(latitude, 5),
    longitude = round(longitude, 5)
  )

# TRIM VALUES
str_clean <- function(string) {
  string <-
    str_squish(string) |> # Trim + use single spaces
    str_replace_all("; ", ";") |> # Remove space after ; separator
    str_remove(";$") # Remove last ;
  return(string)
}
interim_data <-
  interim_data |>
  mutate(
    waarneming = str_squish(waarneming),
    actie = str_squish(actie),
    materiaal_vast = str_squish(materiaal_vast),
    global_id = str_remove_all(global_id, "\\{|\\}")
  )

# ADD CONFIRMED OBSERVATION
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
  ))

# ADD CATCH
interim_data <-
  interim_data |>
  mutate(catch = case_when(
    confirmed_observation & str_detect(actie, "Gevangen") ~ TRUE,
    confirmed_observation & str_detect(actie, "Vangst") ~ TRUE,
  ))

# TRANSLATE MATERIAL
# Separate values
interim_data <-
  interim_data |>
  mutate(material = str_remove_all(materiaal_vast, " = [0-9]*")) |> # Remove numbers, in many cases they likely refer to dropdown value codes
  mutate(material = str_remove(material, ";$")) |> # Remove last ";"
  separate(
    material,
    into = c("material_1", "material_2", "material_3", "material_4", "material_5"),
    sep = "; ",
    remove = TRUE,
    convert = TRUE,
    extra = "merge"
  )

# Map values
material <- read_csv(here("data", "reference", "material.csv"))
mapped_material <- as.character(pull(material, mapped_value))
names(mapped_material) <- pull(material, input_value)
interim_data <-
  interim_data |>
  mutate(
    # See also https://github.com/tidyverse/dplyr/issues/6856
    material_1 = recode(material_1, !!!mapped_material),
    material_2 = recode(material_2, !!!mapped_material),
    material_3 = recode(material_3, !!!mapped_material),
    material_4 = recode(material_4, !!!mapped_material),
    material_5 = recode(material_5, !!!mapped_material)
  )

# Concatenate values (unique, sorted, no NA)
interim_data <-
  interim_data |>
  rowwise() |>
  mutate(
    material = list(sort(na.omit(unique(c(material_1, material_2, material_3, material_4, material_5))))),
    material = paste(material, collapse = " | ")
  ) |>
  select(-starts_with("material_"))

# ORDER DATA
interim_data <-
  interim_data |>
  arrange(
    dossier_id,
    laatst_bewerkt_datum
  )

# WRITE DATA
write_csv(interim_data, here("data", "interim", "interim.csv"), na = "")
