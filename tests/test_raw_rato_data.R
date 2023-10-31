# load libraries
library(testthat, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)

# read raw data received from RATO
raw_data <- readr::read_csv(
  here::here("data", "raw", "rato_data.csv"),
  guess_max = 10000, show_col_types = FALSE)

# tests
testthat::test_that("Opmerkingen fields are removed from raw_data.csv", {
  testthat::expect_false(any(stringr::str_starts(colnames(raw_data), "Opmerkingen")))
})

