test_that("get_token() can fetch a valid token from RATO Oost-Vlaanderen", {
  skip_on_ci()
  expect_type(get_token(), "character")
  expect_no_warning(get_token())
})

test_that("get_token() supports the West-Vlaanderen domain", {
  skip_on_ci()
  expect_type(get_token(username = "Ecosystem2_INBO",
                        password = askpass::askpass(),
                        domain = "https://gwadmin.west-vlaanderen.be/gwportal"),
              "character")
})
