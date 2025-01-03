expected_column_order <-
  c(
    "occurrenceID", "occurrenceStatus", "eventID", "scientificName",
    "eventDate", "organismQuantity", "organismQuantityType", "samplingProtocol",
    "samplingEffort", "municipality", "verbatimLatitude", "verbatimLongitude",
    "verbatimCoordinateSystem", "verbatimSRS", "decimalLatitude",
    "decimalLongitude", "coordinateUncertaintyInMeters", "countryCode",
    "geodeticDatum", "type", "language", "license", "rightsHolder",
    "datasetID", "institutionCode", "datasetName", "basisOfRecord",
    "recordedBy", "kingdom", "taxonRank"
  )


# Create two versions of the occurrence table, one hard coded with the columns
# in the wrong order, and one with the current occurrence table with the columns
# in a random order

# This should always flag an error
occ_col_wrong_order <- dplyr::tibble(
  verbatimCoordinateSystem = character(0),
  samplingEffort = character(0),
  eventDate = character(0),
  datasetName = character(0),
  recordedBy = character(0),
  occurrenceID = character(0),
  verbatimLongitude = character(0),
  organismQuantity = character(0),
  occurrenceStatus = character(0),
  samplingProtocol = character(0),
  language = character(0),
  municipality = character(0),
  eventID = character(0),
  decimalLongitude = character(0),
  scientificName = character(0),
  verbatimLatitude = character(0),
  decimalLatitude = character(0),
  institutionCode = character(0),
  basisOfRecord = character(0),
  taxonRank = character(0),
  geodeticDatum = character(0),
  organismQuantityType = character(0),
  kingdom = character(0),
  verbatimSRS = character(0),
  type = character(0),
  license = character(0),
  datasetID = character(0),
  coordinateUncertaintyInMeters = character(0),
  countryCode = character(0),
  rightsHolder = character(0)
)

# This is in case a future added or removed column causes issues
occ_col_scramble <-
  readr::read_csv(
    file.path(
      system.file("data/processed", package = "rato.occurrences"),
      "occurrence.csv"
    ),
    progress = FALSE,
    show_col_types = FALSE,
    n_max = 5
  ) %>%
  .[, sample(seq(ncol(.)))]

test_that("set_dwc_order changes the order of columns to the correct one", {
  expect_identical(colnames(set_dwc_order(occ_col_wrong_order)), expected_column_order)

  expect_named(
    ignore.order = FALSE,
    ignore.case = FALSE,
    object = set_dwc_order(occ_col_wrong_order),
    expected = expected_column_order
  )

  expect_identical(
    colnames(set_dwc_order(occ_col_scramble)),
    expected_column_order
  )

  expect_named(
    ignore.order = FALSE,
    ignore.case = FALSE,
    object = set_dwc_order(occ_col_scramble),
    expected = expected_column_order
  )
})

test_that("set_dwc_order() returns error when input is not a data.frame", {
  expect_error(set_dwc_order(1),
    regexp = "x is not a data frame",
    fixed = TRUE
  )
})
