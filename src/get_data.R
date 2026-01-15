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
  dplyr::filter(Domein %in% c("Dier", "Plant")

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
  "Materiaal_Vast",
  "Materiaal_Consumptie"
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
    materiaal_consumptie = str_clean(materiaal_consumptie),
    global_id = stringr::str_remove_all(global_id, "\\{|\\}")
  )

# CONVERT PROPERTY COLUMNS TO ROWS

# These columns are: Waarneming, Actie, Materiaal Vast, Materiaal Consumptie
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

# ORDER DATA

interim_data <-
  interim_data |>
  dplyr::arrange(
    dossier_id,
    laatst_bewerkt_datum,
    p_field
  )

# ADD SCIENTIFIC NAME

interim_data <-
  interim_data |>
  dplyr::mutate(
    scientific_name = dplyr::case_match(
      # We match on "soort" since "gbif_code" is not reliable and can differ for same "soort"
      soort,
      "Amerikaanse Nerts" ~ "Neovison vison",
      "Amerikaanse Stierkikker" ~ "Lithobates catesbeianus",
      "Andere (soort vermelden)" ~ NA_character_,
      "Andere (soort vermelden): Eend" ~ NA_character_,
      "Andere (soort vermelden): Kraaien" ~ NA_character_,
      "Aziatische hoornaar" ~ "Vespa velutina",
      "Aziatische hoornaar actie" ~ "Vespa velutina",
      "Bever" ~ "Castor fiber",
      "Beverrat" ~ "Myocastor coypus",
      "Boerengans" ~ "Anser anser f. domesticus",
      "Brandgans" ~ "Branta leucopsis",
      "Bruine rat" ~ "Rattus norvegicus",
      "Bruine rat bak/buis" ~ "Rattus norvegicus",
      "Canadese Gans" ~ "Branta canadensis",
      "Duiven" ~ NA_character_,
      "Exotische Eekhoorn" ~ NA_character_,
      "Ganzenactie" ~ NA_character_,
      "gedomesticeerde gans" ~ "Anser anser f. domesticus",
      "Grauwe Gans" ~ "Anser anser",
      "Grote Waternavel" ~ "Hydrocotyle ranunculoides",
      "Halsbandparkiet" ~ "Psittacula krameri",
      "Japanse Duizendknoop" ~ "Fallopia japonica",
      "Kippen" ~ NA_character_,
      "Konijnen" ~ "Oryctolagus cuniculus",
      "Leidse Plant" ~ "Saururus cernuus",
      "Lettersierschildpad" ~ "Trachemys scripta",
      "Mantsjoerese wilde rijst" ~ "Zizania latifolia",
      "Mollen" ~ "Talpa europaea",
      "Muizen" ~ "Muridae",
      "Muskusrat" ~ "Ondatra zibethicus",
      "Neerhofdier(en)" ~ NA_character_,
      "Nijlgans" ~ "Alopochen aegyptiaca",
      "Parelvederkruid" ~ "Myriophyllum aquaticum",
      "Reuzenbalsemien" ~ "Impatiens glandulifera",
      "Reuzenberenklauw" ~ "Heracleum mantegazzianum",
      "Rivierkreeft" ~ NA_character_,
      "Steenmarter" ~ "Martes foina",
      "Watercrassula" ~ "Crassula helmsii",
      "Watersla" ~ "Pistia stratiotes",
      "Waterteunisbloem" ~ "Ludwigia",
      "Zwarte rat" ~ "Rattus rattus",
      "Zwerfkatten" ~ "Felis catus"
    )
  ) |>
  dplyr::relocate(scientific_name, .after = "soort")

# SELECT RELEVANT SPECIES TO PUBLISH

select_species <- c(
  "Rattus norvegicus",
  "Rattus rattus",
  "Myocastor coypus",
  "Ondatra zibethicus"
)
interim_data <-
  interim_data |>
  dplyr::filter(scientific_name %in% select_species)

# WRITE DATA

readr::write_csv(interim_data, here::here("data", "interim", "interim.csv"), na = "")
