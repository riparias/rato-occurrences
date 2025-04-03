# read proposed new version of the DwC mapping
dwc_occurrence <- get_current()

# tests

test_that("The occurrence output exists", {
  expect_true(
    file.exists(
      system.file("extdata/processed/occurrence.csv",
                  package = "rato.occurrences")
      )
    )
})

test_that("Right columns in right order", {
  columns <- c(
    "occurrenceID",
    "occurrenceStatus",
    "eventID",
    "scientificName",
    "eventDate",
    "organismQuantity",
    "organismQuantityType",
    "samplingProtocol",
    "samplingEffort",
    "municipality",
    "verbatimLatitude",
    "verbatimLongitude",
    "verbatimCoordinateSystem",
    "verbatimSRS",
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "countryCode",
    "geodeticDatum",
    "type",
    "language",
    "license",
    "rightsHolder",
    "datasetID",
    "institutionCode",
    "datasetName",
    "basisOfRecord",
    "recordedBy",
    "kingdom",
    "taxonRank"
  )
  expect_named(dwc_occurrence, columns)
})

test_that("datasetID is always present and is equal to DOI dataset", {
  expect_true(all(!is.na(dwc_occurrence$datasetID)))
  expect_equal(unique(dwc_occurrence$datasetID),
                         "https://doi.org/10.15468/fw2rbx")
})

test_that("occurrenceID is always present and is unique", {
  expect_true(all(!is.na(dwc_occurrence$occurrenceID)))
  expect_equal(length(unique(dwc_occurrence$occurrenceID)),
                         nrow(dwc_occurrence))
})

test_that("eventID is always present", {
  expect_true(all(!is.na(dwc_occurrence$eventID)))
})

test_that(
  "organismQuantity is always an integer equal or greater than 0 if present", {
    organismQuantity_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantity)) %>%
      dplyr::distinct(organismQuantity) %>%
      dplyr::pull(organismQuantity)
    expect_equal(
      as.numeric(organismQuantity_values), as.integer(organismQuantity_values)
    )
    expect_true(
      all(as.numeric(organismQuantity_values) >= 0)
    )
  })

test_that(
  "an organismQuantity must have a corresponding organismQuantityType", {
    organismQuantityType_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantity)) %>%
      dplyr::distinct(organismQuantityType) %>%
      dplyr::pull(organismQuantityType)
    expect_true(all(!is.na(organismQuantityType_values)))
  })

test_that(
  "an organsimQuantityType must have a corresponding organsimQuantity",
  {
    # there should be no cases where there is an organismQuantityType and no
    # organismQuantity
    expect_identical(
      nrow(dplyr::filter(
        dwc_occurrence,
        !is.na(organismQuantityType) & is.na(organismQuantity)
      )),
      0L
    )
  }
)

test_that(
  "organismQuantityType is one of the predefined values if not NA", {
    values <- c("individual(s)", "square meter(s)", "nest")
    organismQuantityType_values <-
      dwc_occurrence %>%
      dplyr::filter(!is.na(organismQuantityType)) %>%
      dplyr::distinct(organismQuantityType) %>%
      dplyr::pull(organismQuantityType)
    expect_true(all(organismQuantityType_values %in% values))
  })

test_that("coordinates and uncertainties are always filled in", {
  # decimalLatitude
  expect_true(
    all(!is.na(dwc_occurrence$decimalLatitude))
  )
  # decimalLongitude
  expect_true(
    all(!is.na(dwc_occurrence$decimalLongitude))
  )
  # verbatimLatitude
  expect_true(
    all(!is.na(dwc_occurrence$verbatimLatitude))
  )
  # verbatimLongitude
  expect_true(
    all(!is.na(dwc_occurrence$verbatimLongitude))
  )
  # coordinateUncertaintyInMeters
  expect_true(
    all(!is.na(dwc_occurrence$coordinateUncertaintyInMeters))
  )
})

test_that("decimalLatitude is within Flemish boundaries", {
  expect_true(all(dwc_occurrence$decimalLatitude < 51.65))
  expect_true(all(dwc_occurrence$decimalLatitude > 50.63))
})

test_that("decimalLongitude is within Flemish boundaries", {
  expect_true(all(dwc_occurrence$decimalLongitude < 5.95))
  expect_true(all(dwc_occurrence$decimalLongitude > 2.450))
})

test_that("verbatimLongitude is always positive", {
  expect_true(
    all(dwc_occurrence$verbatimLongitude  > 0)
  )
})

test_that("verbatimLatitude is always positive", {
  expect_true(
    all(dwc_occurrence$verbatimLatitude > 0)
  )
})

test_that("eventDate is always filled in", {
  expect_true(all(!is.na(dwc_occurrence$eventDate)))
})

test_that("scientificName is never NA and one of the list", {
  # species is an exported data object (a character vector) that contains
  # a list of species names known to be included in the RATO dataset.
  # When new species are added, you can edit it by updating data/species.rda

  scientific_names_present <- unique(dwc_occurrence$scientificName)

  expect_true(all(!is.na(dwc_occurrence$scientificName)))

  expect(
    all(scientific_names_present %in% species),
    glue::glue(
      "New species detected: {missing_species_names}",
      missing_species_names = glue::glue_collapse(
        glue::backtick(
          scientific_names_present[!scientific_names_present %in% species]
        ),
        sep = ", ",
        last = " and "
      )
    )
  )

})

test_that("kingdom is always equal to Plantae or Animalia", {
  expect_true(all(!is.na(dwc_occurrence$kingdom)))
  expect_true(
    all(dwc_occurrence$kingdom %in% c("Plantae", "Animalia"))
  )
})

test_that("taxonRank is always filled in and one of the list", {
  taxon_ranks <-
    c("subspecies", "species", "genus", "form", "unknown", "kingdom", "family")
  expect_true(all(!is.na(dwc_occurrence$taxonRank)))
  expect_true(
    all(dwc_occurrence$taxonRank %in% taxon_ranks)
  )
})

test_that("known test objects are removed from output", {
  expect_identical(
    nrow(dplyr::filter(dwc_occurrence, occurrenceID %in%
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

test_that(
  "There is at least one record for every year since the start of the data", {
    expect_equal(
      sort(unique(lubridate::year(dwc_occurrence$eventDate))),
      seq(2021, as.double(format(Sys.Date(), "%Y"))),
      tolerance = 0 # so it will allow comparing doubles and integers
    )
  })

test_that("Less than 1% of records were removed since last publication", {
  # Total size has not decreased by more than 1%
  expect_gte(nrow(dwc_occurrence) / nrow(get_reference()), 0.99)

  # Proportion of deleted records is lower than 1% of the total size
  deleted_records <-
    dplyr::setdiff(
      get_reference()$occurrenceID,
      dwc_occurrence$occurrenceID
    )

  ## Compared to the new updated dataset
  expect_lte(
    length(deleted_records) / nrow(dwc_occurrence),
    0.01
  )

  ## Compared to the published dataset
  expect_lte(
    length(deleted_records) / nrow(get_reference()),
    0.01
  )

})
