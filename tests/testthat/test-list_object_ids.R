test_that("list_object_ids() works for default RATO oost-vlaanderen API", {
  expect_type(list_object_ids(), "integer")
  # there are more than 10.000 records in the RATO Oost-Vlaanderen database
  expect_gte(length(list_object_ids()), 10000)
})
