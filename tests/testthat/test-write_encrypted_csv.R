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

test_that("write_encrypted_csv() can write an encrypted csv", {
  withr::with_tempfile(
    "encrypted_csv",
    {
      write_encrypted_csv(
        dplyr::tibble(a = seq(1,50), b = seq(50,1)),
        outfile = encrypted_csv,
        key = "super public key"
      )
      expect_true(file.exists(encrypted_csv))
    }
  )
})

