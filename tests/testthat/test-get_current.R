test_that("get_current() returns a data.frame", {
  expect_s3_class(get_current(), "data.frame")
})

test_that("get_current() returns a data.frame with the expected columns", {
  expected_columns <-
    c(
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
  expect_named(get_current(), expected_columns)
})
