# Create an interim dataset that can be deposited and used as a source for the
# mappings.


# Fetch raw data from RATO ------------------------------------------------

rato_raw <- ratatouille::ratatouille()


# Filter out "Werken" -----------------------------------------------------
interim_data <-
  dplyr::filter(rato_raw, Domein != "Werken") |>
# Select and order relevant columns ---------------------------------------

  dplyr::select(dplyr::all_of(c("Dossier_ID", "OBJECTID", "Soort", "Waarneming", "Actie", "Materiaal_Vast", "Materiaal_Consumptie", "X", "Y", "Gemeente", "Laatst_Bewerkt_Datum", "GlobalID"))) |>
  dplyr::relocate(Dossier_ID, Soort, Gemeente, X, Y, OBJECTID, GlobalID, Laatst_Bewerkt_Datum, Waarneming, Actie, Materiaal_Vast, Materiaal_Consumptie) |>
# Remove stray spaces, semicolons, etc. -----------------------------------

  dplyr::mutate(
    Waarneming = stringr::str_replace_all(stringr::str_remove(Waarneming, ";$"), "; ", ";"),
    Actie = stringr::str_replace_all(stringr::str_remove(Actie, ";$"), "; ", ";"),
    Materiaal_Vast = stringr::str_replace_all(stringr::str_remove(Materiaal_Vast, ";$"), "; ", ";"),
    Materiaal_Consumptie = stringr::str_replace_all(stringr::str_remove(Materiaal_Consumptie, ";$"), "; ", ";"),
    GlobalID = stringr::str_remove_all(GlobalID, "\\{|\\}")
  ) |>
# Separate values of property columns -------------------------------------
  # values of property columns (Waarneming, Actie, Materiaal Vast, Materiaal Consumptie) into columns
  tidyr::separate(Waarneming, into = c("p_waarneming_1", "p_waarneming_2", "p_waarneming_3", "p_waarneming_4", "p_waarneming_5", "p_waarneming_6"), sep = "\\s*\\;\\s*", remove = TRUE, convert = TRUE, extra = "drop") |>
  tidyr::separate(Actie, into = c("p_actie_1", "p_actie_2", "p_actie_3", "p_actie_4", "p_actie_5"), sep = "\\s*\\;\\s*", remove = TRUE, convert = TRUE, extra = "drop") |>
  tidyr::separate(Materiaal_Vast, into = c("p_materiaal_vast_1", "p_materiaal_vast_2", "p_materiaal_vast_3", "p_materiaal_vast_4", "p_materiaal_vast_5"), sep = "\\s*\\;\\s*", remove = TRUE, convert = TRUE, extra = "drop") |>
  tidyr::separate(Materiaal_Consumptie, into = c("p_materiaal_consumptie_1", "p_materiaal_consumptie_2", "p_materiaal_consumptie_3", "p_materiaal_consumptie_4", "p_materiaal_consumptie_5", "p_materiaal_consumptie_6"), sep = "\\s*\\;\\s*", remove = TRUE, convert = FALSE, extra = "drop") |>
# Convert property columns into rows --------------------------------------

  tidyr::pivot_longer(cols = dplyr::starts_with("p_"), values_to = "p_key_value", names_to = c("p_field"), values_drop_na = TRUE, names_repair = "unique") |>
  dplyr::mutate(p_field = stringr::str_remove(p_field, "_\\d$")) |>
  tidyr::separate(p_key_value, into = c("p_key", "p_value"), sep = " = ", remove = TRUE, convert = TRUE, extra = "drop") |>
# Filter out non-relevant species -----------------------------------------

  dplyr::filter(!Soort %in% c("Duiven", "Kippen", "Kippen:", "Neerhofdier(en)", "Neerhofdier(en):", "Zwerfkatten"))
# Write out ---------------------------------------------------------------

write_csv(interim_data,
    here::here("inst", "extdata", "processed", "interim.csv"),
    na = ""
  )
