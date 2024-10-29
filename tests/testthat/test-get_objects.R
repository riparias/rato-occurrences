test_that("get_objects() can fetch a known RATO Oost-Vlaanderen object", {
  expect_s3_class(
    get_objects(
      2011,
      token = get_token()
    ),
    "data.frame"
  )

  # Expect the right columns
  expect_named(
    get_objects(
      2011,
      token = get_token()
    ),
    c(
      "Dossier_ID",
      "OBJECTID",
      "Dossier_Status",
      "Domein",
      "Soort",
      "Materiaal_Consumptie",
      "Opmerkingen_admin",
      "Opmerkingen",
      "Planning_Datum",
      "X",
      "Y",
      "Gemeente",
      "Aard_Locatie",
      "NIS_Code",
      "GBIF_Code",
      "Hoofddossier_ID",
      "Laatst_Bewerkt_Datum",
      "Geometrie_Type",
      "GlobalID"
    )
  )
})

test_that("get_objects() can retreive objects from the West-Vlaanderen API", {
  expect_s3_class(
    get_objects(
      383421,
      token = get_token(username = "Ecosystem2_INBO",
                        password = askpass::askpass(),
                        domain = "https://gwadmin.west-vlaanderen.be/gwportal"),
      domain = "https://gwadmin.west-vlaanderen.be/gwserver/rest/services",
      table_path = c("Ecosystem2","AGS_ES2_Dossiers_Publiek","MapServer")
    ),
    "data.frame"
  )
})
