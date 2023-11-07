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
testthat::test_that("Opmerkingen fields are encrypted in raw_data.csv", {
  # all NA values should be salted and encrypted
  testthat::expect_false(any(is.na(raw_data$Opmerkingen)))
  testthat::expect_false(any(is.na(raw_data$Opmerkingen_admin)))
  # there are no spaces in the encrypted strings
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen, " "))
    )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen_admin, " "))
    )
  # test for some common words that occur in the Opmerkingen
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen, " de "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen, " en "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen, " op "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen, " na "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen_admin, " de "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen_admin, " en "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen_admin, " op "))
  )
  testthat::expect_false(
    any(stringr::str_detect(raw_data$Opmerkingen_admin, " na "))
  )
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
      "Opmerkingen_admin",
      "Opmerkingen",
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
