# load libraries
library(testthat, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)

# read raw data received from RATO
raw_data <- readr::read_csv(
  here::here("data", "raw", "rato_data.csv"),
  guess_max = 10000, show_col_types = FALSE)

# tests
testthat::test_that("Opmerkingen fields are removed from raw_data.csv", {
  testthat::expect_false(any(stringr::str_starts(colnames(raw_data), "Opmerkingen")))
})

testthat::test_that("raw_data.csv has the expected columns", {
  testthat::expect_named(
    raw_data,
    c(
      "Dossier_ID",
      "OBJECTID",
      "Dossier_Status",
      "Domein",
      "Soort",
      "Waarneming",
      "Actie",
      "Materiaal_Vast",
      "Melder_Naam",
      "Melder_Klant",
      "Planning_Datum",
      "X",
      "Y",
      "Gemeente",
      "Aard_Locatie",
      "GBIF_Code",
      "Dossier_Link",
      "Dossier_Link_ID",
      "Hoofddossier_ID",
      "Aangemaakt_Datum",
      "Laatst_Bewerkt_Datum",
      "Datum_Van",
      "Geometrie_Type",
      "Shape"
    )
  )
})
