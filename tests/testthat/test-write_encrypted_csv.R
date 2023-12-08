test_that("write_encrypted_csv() fails on bad inputs", {
  expect_error(write_encrypted_csv(NULL))
  withr::with_envvar(
    new = c("encryption_key" = NA),
    {
      expect_error(
        write_encrypted_csv(
          dplyr::tibble(a = 1:5),
          outfile = tempfile()
        ),
        regex = "key"
      )
    },
    action = "replace"
  )
  expect_error(
    write_encrypted_csv(
      x = dplyr::tibble(a = 1:5),
      outfile = tempfile(),
      key = c("seven", "of", "nine")
    ),
    regex = "key is not a string (a length one character vector)",
    fixed = TRUE
  )
  expect_error(
    write_encrypted_csv(
      x = dplyr::tibble(a = 1:5),
      outfile = tempfile(),
      key = ""
    ),
    regexp = 'key not not equal to ""',
    fixed = TRUE
  )
})
