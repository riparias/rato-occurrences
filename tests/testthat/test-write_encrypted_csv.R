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

test_that("write_encrypted_csv() can write a file that can be decrypted", {
  withr::with_tempfile(
    c("encrypted_csv", "decrypted_csv"),
    {
      object_to_encrypt <- dplyr::tibble(a = seq(1,50), b = seq(50,1))
      testing_key <- "super public key for testing and not for actual use"

      write_encrypted_csv(
        object_to_encrypt,
        outfile = encrypted_csv,
        key = testing_key
      )

      safer::decrypt_file(
        encrypted_csv,
        key = testing_key,
        outfile = decrypted_csv
      )
      # decryption works
      expect_true(file.exists(decrypted_csv))
      # object is about the same after decryption, readr parsing will change
      # colclasses
      expect_equal(
        readr::read_csv(decrypted_csv, show_col_types = FALSE),
        object_to_encrypt
      )
    }
  )
})

test_that("write_encrypted_csv output files can not be read by readr", {
  testing_key <- "testing key for testing"
  withr::local_envvar(.new = list("encryption_key" = testing_key,
                                  action = "replace"))
  withr::with_tempfile(
    c("encrypted_csv","decrypted_csv"),
    {
      object_to_encrypt <- dplyr::tibble(month.name, month.abb)
      write_encrypted_csv(
        object_to_encrypt,
        outfile = encrypted_csv
      )

      safer::decrypt_file(
        encrypted_csv,
        key = testing_key,
        outfile = decrypted_csv
      )

      expect_identical(
        readr::read_csv(decrypted_csv, show_col_types = FALSE),
        object_to_encrypt
      )
    }
  )
})
