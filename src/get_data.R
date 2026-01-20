# GET LIVE DATA
raw_data <- ratatouille::ratatouille(source = "rato")

# FILTER ON DATA FOR ANIMALS & PLANTS
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

# FILTER ON SPECIES
# Join with species reference data and filter on species we want to include
species <- readr::read_csv(here::here("data", "reference", "species.csv"))
interim_data <-
  interim_data |>
  dplyr::mutate(soort = stringr::str_remove(stringr::str_squish(soort), ":$")) |> # Remove trailing ":"
  dplyr::left_join(species, by = "soort") |>
  dplyr::relocate(kingdom, scientific_name, taxon_rank, .after = "soort") |>
  dplyr::filter(include) |>
  dplyr::select(-include)

# CONVERT COORDINATES
# Convert Lambert UTM to latitude & longitude
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
interim_data <-
  interim_data |>
  dplyr::mutate(
    waarneming = stringr::str_squish(waarneming),
    actie = stringr::str_squish(actie),
    materiaal_vast = stringr::str_squish(materiaal_vast),
    global_id = stringr::str_remove_all(global_id, "\\{|\\}")
  )

# ADD CONFIRMED OBSERVATION
# This is based on specific "waarneming" or "actie" values
interim_data <-
  interim_data |>
  dplyr::mutate(confirmed_observation = dplyr::case_when(
    stringr::str_detect(waarneming, "Haard vastgesteld = [1-9]") ~ TRUE,
    stringr::str_detect(waarneming, "Vastgesteld = [1-9]") ~ TRUE,
    stringr::str_detect(waarneming, "Vastgesteld \\(aantal\\) = [1-9]") ~ TRUE,
    stringr::str_detect(waarneming, "Vastgesteld \\(in m²\\) = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Eieren geschud \\(aantal\\) = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Gevangen = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Gevangen juveniel \\(aantal\\) = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Gevangen volwassenen \\(aantal\\) = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Hoeveelheid = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Vangst \\(aantal\\) = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Vastgesteld = [1-9]") ~ TRUE,
    stringr::str_detect(actie, "Verwijderd \\(aantal m²\\) = [1-9]") ~ TRUE
    # Note on some values we do not include:
    # "Beverdam": does not imply animal was seen
    # "Nevenvangst": is not an observation of the main species
  ))

# FILTER ON CONFIRMED OBSERVATIONS
interim_data <-
  interim_data |>
  dplyr::filter(confirmed_observation)

# ADD CATCH
interim_data <-
  interim_data |>
  dplyr::mutate(catch = dplyr::case_when(
    confirmed_observation & stringr::str_detect(actie, "Gevangen") ~ TRUE,
    confirmed_observation & stringr::str_detect(actie, "Vangst") ~ TRUE,
  ))

# TRANSLATE MATERIAL
# Separate values
interim_data <-
  interim_data |>
  dplyr::mutate(material = stringr::str_remove_all(materiaal_vast, " = [0-9]*")) |> # Remove numbers, in many cases they likely refer to dropdown value codes
  dplyr::mutate(material = stringr::str_remove(material, ";$")) |> # Remove trailing ";"
  tidyr::separate_wider_delim(
    material,
    delim = "; ",
    names = c("material_1", "material_2", "material_3", "material_4", "material_5"),
    too_few = "align_start",
    too_many = "merge",
    cols_remove = TRUE
  )

# Map values
material <- readr::read_csv(here::here("data", "reference", "material.csv"))
mapped_material <- as.character(dplyr::pull(material, mapped_value))
names(mapped_material) <- dplyr::pull(material, input_value)
interim_data <-
  interim_data |>
  dplyr::mutate(
    # See also https://github.com/tidyverse/dplyr/issues/6856
    material_1 = dplyr::recode(material_1, !!!mapped_material),
    material_2 = dplyr::recode(material_2, !!!mapped_material),
    material_3 = dplyr::recode(material_3, !!!mapped_material),
    material_4 = dplyr::recode(material_4, !!!mapped_material),
    material_5 = dplyr::recode(material_5, !!!mapped_material)
  )

# Concatenate values (unique, sorted, no NA)
interim_data <-
  interim_data |>
  dplyr::rowwise() |>
  dplyr::mutate(
    material = list(sort(na.omit(unique(c(material_1, material_2, material_3, material_4, material_5))))),
    material = paste(material, collapse = " | ")
  ) |>
  dplyr::select(-dplyr::starts_with("material_"))

# ORDER DATA
interim_data <-
  interim_data |>
  dplyr::arrange(
    dossier_id,
    laatst_bewerkt_datum
  )

# WRITE DATA
readr::write_csv(interim_data, here::here("data", "interim", "confirmed_observations.csv"), na = "")
