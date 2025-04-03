test_that("get_reference() returns a data.frame", {
  skip_if_offline()
  expect_s3_class(get_reference(), "data.frame")
})

test_that("get_reference() returns the expected columns", {
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
  expect_named(get_reference(), expected_columns)
})
