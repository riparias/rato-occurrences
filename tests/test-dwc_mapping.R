library(testthat)

# Get Darwin Core data
occurrence <- readr::read_csv(here::here("data", "processed", "occurrence.csv"))

test_that("Columns are expected Darwin Core terms", {
  expected_cols <- c(
    "type",
    "license",
    "rightsHolder",
    "datasetID",
    "institutionCode",
    "datasetName",
    "basisOfRecord",
    "occurrenceID",
    "recordedBy",
    "occurrenceStatus",
    "eventID",
    "parentEventID",
    "eventType",
    "eventDate",
    "samplingProtocol",
    "countryCode",
    "municipality",
    "decimalLatitude",
    "decimalLongitude",
    "geodeticDatum",
    "coordinateUncertaintyInMeters",
    "verbatimLatitude",
    "verbatimLongitude",
    "verbatimCoordinateSystem",
    "verbatimSRS",
    "identificationVerificationStatus",
    "kingdom",
    "scientificName",
    "taxonRank"
  )
  expect_named(occurrence, expected_cols)
})

test_that("occurrenceID is unique", {
  expect_equal(
    nrow(occurrence),
    nrow(dplyr::distinct(occurrence, occurrenceID))
  )
})

test_that("samplingProtocol has expected values", {
  expected_material <- c(
    "bins and tubes",
    "cage",
    "camera trap",
    "chemical control",
    "clamp",
    "conibear trap",
    "decoy",
    "fyke",
    "net",
    "other",
    "trap",
    NA_character_
  )
  sampling_protocol_values <-
    occurrence |>
    tidyr::separate_wider_delim(
      samplingProtocol,
      delim = " |",
      names = c("samp_1", "samp_2", "samp_3"), # Assume max 3
      too_few = "align_start",
      too_many = "merge",
      cols_remove = TRUE
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("samp_"),
      names_to = NULL,
      values_to = "samp"
    ) |>
    dplyr::select(samp) |>
    dplyr::distinct(samp) |>
    dplyr::pull()

  # No unexpected values
  expect_in(
    sampling_protocol_values,
    expected_material
  )
})

test_that("scientificName has expected values", {
  expected_species <- c(
    "Anser anser",
    "Alopochen aegyptiaca",
    "Branta canadensis",
    "Branta leucopsis",
    "Castor fiber",
    "Crassula helmsii",
    "Heracleum mantegazzianum",
    "Hydrocotyle ranunculoides",
    "Impatiens glandulifera",
    "Ludwigia",
    "Martes foina",
    "Myriophyllum aquaticum",
    "Neovison vison",
    "Ondatra zibethicus",
    "Oryctolagus cuniculus",
    "Pistia stratiotes",
    "Psittacula krameri",
    "Rattus norvegicus",
    "Rattus rattus",
    "Reynoutria japonica",
    "Saururus cernuus",
    "Talpa europaea",
    "Trachemys scripta",
    "Zizania latifolia"
  )
  # No unexpected values
  expect_in(
    dplyr::pull(dplyr::distinct(occurrence, scientificName)),
    expected_species
  )
})
