test_that("list_object_ids() works for default RATO oost-vlaanderen API", {
  expect_type(list_object_ids(), "integer")
  # there are more than 10.000 records in the RATO Oost-Vlaanderen database
  expect_gte(length(list_object_ids()), 10000)
})

test_that("list_object_ids() can retreive ids for West-Vlaanderen", {
  expect_type(
    list_object_ids(
      token = get_token(username = "Ecosystem2_INBO",
                        password = askpass::askpass(),
                        domain = "https://gwadmin.west-vlaanderen.be/gwportal"),
      domain = "https://gwadmin.west-vlaanderen.be/gwserver/rest/services",
      table_path = c("Ecosystem2","AGS_ES2_Dossiers_Publiek","MapServer")
    ),
    "integer"
  )
})
