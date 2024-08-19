# load libraries
library(testthat, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)

# read proposed new version of the DwC mapping
occs_path <- here::here("data", "processed", "occurrence.csv")
dwc_occurrence <-
  readr::read_csv(occs_path, guess_max = 10000, show_col_types = FALSE)

# tests

testthat::test_that("The occurrence output exists", {
  testthat::expect_true(file.exists(occs_path))
})

testthat::test_that("Right columns in right order", {
  columns <- c(
    "type",
    "language",
    "license",
    "rightsHolder",
    "datasetID",
    "institutionCode",
    "datasetName",
    "basisOfRecord",
    "eventID",
    "occurrenceID",
    "recordedBy",
    "organismQuantity",
    "organismQuantityType",
    "occurrenceStatus",
    "samplingProtocol",
    "samplingEffort",
    "eventDate",
    "countryCode",
    "municipality",
    "verbatimLatitude",
    "verbatimLongitude",
    "verbatimCoordinateSystem",
    "verbatimSRS",
    "decimalLatitude",
    "decimalLongitude",
    "geodeticDatum",
    "coordinateUncertaintyInMeters",
    "scientificName",
    "kingdom",
    "taxonRank"
  )
  testthat::expect_equal(names(dwc_occurrence), columns)
})

testthat::test_that("datasetID is always present and is equal to DOI dataset", {
  testthat::expect_true(all(!is.na(dwc_occurrence$datasetID)))
  testthat::expect_equal(unique(dwc_occurrence$datasetID),
                         "https://doi.org/10.15468/fw2rbx")
})

testthat::test_that("occurrenceID is always present and is unique", {
  testthat::expect_true(all(!is.na(dwc_occurrence$occurrenceID)))
  testthat::expect_equal(length(unique(dwc_occurrence$occurrenceID)),
                         nrow(dwc_occurrence))
})

testthat::test_that("eventID is always present", {
  testthat::expect_true(all(!is.na(dwc_occurrence$eventID)))
})

testthat::test_that(
  "organismQuantity is always an integer equal or greater than 0 if present", {
    organismQuantity_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantity)) %>%
      dplyr::distinct(organismQuantity) %>%
      dplyr::pull(organismQuantity)
    testthat::expect_equal(
      as.numeric(organismQuantity_values), as.integer(organismQuantity_values)
    )
    testthat::expect_true(
      all(as.numeric(organismQuantity_values) >= 0)
    )
})

testthat::test_that(
  "an organismQuantity must have a corresponding organismQuantityType", {
    organismQuantityType_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantity)) %>%
      dplyr::distinct(organismQuantityType) %>%
      dplyr::pull(organismQuantityType)
    testthat::expect_true(all(!is.na(organismQuantityType_values)))
  })

testthat::test_that(
  "an organsimQuantityType must have a corresponding organsimQuantity",
  {
    # there should be no cases where there is an organismQuantityType and no
    # organismQuantity
    testthat::expect_identical(
      nrow(dplyr::filter(
        dwc_occurrence,
        !is.na(organismQuantityType) & is.na(organismQuantity)
      )),
      0L
    )
  }
)

testthat::test_that(
  "organismQuantityType is one of the predefined values if not NA", {
    values <- c("individual(s)", "square meter(s)", "nest")
    organismQuantityType_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantityType)) %>%
      dplyr::distinct(organismQuantityType) %>%
      dplyr::pull(organismQuantityType)
    testthat::expect_true(all(organismQuantityType_values %in% values))
  })

testthat::test_that("coordinates and uncertainties are always filled in", {
  # decimalLatitude
  testthat::expect_true(
    all(!is.na(dwc_occurrence$decimalLatitude))
  )
  # decimalLongitude
  testthat::expect_true(
    all(!is.na(dwc_occurrence$decimalLongitude))
  )
  # verbatimLatitude
  testthat::expect_true(
    all(!is.na(dwc_occurrence$verbatimLatitude))
  )
  # verbatimLongitude
  testthat::expect_true(
    all(!is.na(dwc_occurrence$verbatimLongitude))
  )
  # coordinateUncertaintyInMeters
  testthat::expect_true(
    all(!is.na(dwc_occurrence$coordinateUncertaintyInMeters))
  )
})

testthat::test_that("decimalLatitude is within Flemish boundaries", {
  testthat::expect_true(all(dwc_occurrence$decimalLatitude < 51.65))
  testthat::expect_true(all(dwc_occurrence$decimalLatitude > 50.63))
})

testthat::test_that("decimalLongitude is within Flemish boundaries", {
  testthat::expect_true(all(dwc_occurrence$decimalLongitude < 5.95))
  testthat::expect_true(all(dwc_occurrence$decimalLongitude > 2.450))
})

testthat::test_that("verbatimLongitude is always positive", {
  testthat::expect_true(
    all(dwc_occurrence$verbatimLongitude  > 0)
  )
})

testthat::test_that("verbatimLatitude is always positive", {
  testthat::expect_true(
    all(dwc_occurrence$verbatimLatitude > 0)
  )
})

testthat::test_that("eventDate is always filled in", {
  testthat::expect_true(all(!is.na(dwc_occurrence$eventDate)))
})

testthat::test_that("scientificName is never NA and one of the list", {
  species <- c(
    "Ondatra zibethicus",
    "Fallopia japonica",
    "Castor fiber",
    "Gallus gallus domesticus",
    "Myriophyllum aquaticum",
    "Alopochen aegyptiaca",
    "Ludwigia peploides",
    "Martes foina",
    "Hydrocotyle ranunculoides",
    "Vespa velutina",
    "Heracleum mantegazzianum",
    "Rattus norvegicus",
    "Cairina moschata",
    "Anser anser domesticus",
    "Neovison vison",
    "Trachemys scripta",
    "Psittacula krameri",
    "Oryctolagus cuniculus",
    "Branta canadensis",
    "Branta leucopsis",
    "Anatidae",
    "Anser anser",
    "Impatiens glandulifera",
    "Myocastor coypus",
    "Lysichiton americanus",
    "Procambarus clarkii",
    "Ludwigia grandiflora",
    "Sciurus",
    "Crassula helmsii",
    "Zizania latifolia",
    "Procambarus",
    "Columba livia domestica"
  )
  testthat::expect_true(all(!is.na(dwc_occurrence$scientificName)))
  testthat::expect_true(all(dwc_occurrence$scientificName %in% species))
})

testthat::test_that("kingdom is always equal to Plantae or Animalia", {
  testthat::expect_true(all(!is.na(dwc_occurrence$kingdom)))
  testthat::expect_true(
    all(dwc_occurrence$kingdom %in% c("Plantae", "Animalia"))
  )
})

testthat::test_that("taxonRank is always filled in and one of the list", {
  taxon_ranks <-
    c("species", "genus", "form", "unknown", "kingdom", "family")
  testthat::expect_true(all(!is.na(dwc_occurrence$taxonRank)))
  testthat::expect_true(
    all(dwc_occurrence$taxonRank %in% taxon_ranks)
  )
})

testthat::test_that("known test objects are removed from output", {
  testthat::expect_identical(
    nrow(filter(dwc_occurrence, occurrenceID %in%
                  c(
                    "432883",
                    "432884",
                    "432887",
                    "432896",
                    "437303",
                    "449283",
                    "449284",
                    "449285",
                    "449317",
                    "450596",
                    "596279"
                  ))),
    0L
  )
})

testthat::test_that(
  "There is at least one record for every year since the beginning of the data",{
    testthat::expect_equal(
      sort(unique(lubridate::year(dwc_occurrence$eventDate))),
      seq(2021, as.double(format(Sys.Date(), "%Y"))),
      tolerance = 0 # so it will allow comparing doubles and integers
    )
  })

