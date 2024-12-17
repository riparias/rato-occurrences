skip_if_offline("raw.githubusercontent.com")

test_current_path <-
  "https://github.com/riparias/rato-occurrences/raw/e50ee3a3132bc459b65521b3f2fd0760978ae7b3/data/processed/occurrence.csv"
test_reference_path <-
  "https://raw.githubusercontent.com/riparias/rato-occurrences/7ca5cb5cd80008c19718352813f669b662f6ea37/data/processed/occurrence.csv"

test_that("get_modified_lines() can return a tibble with the modified lines", {
  expect_s3_class(
    get_modified_lines(test_current_path, test_reference_path),
    "data.frame"
  )
})

test_that("get_modified_lines() can return a list of lines with the modified lines", {
  expect_type(
    get_modified_lines(test_current_path, test_reference_path, as_df = FALSE),
    "character"
  )
})

test_that("get_modified_lines() can drop the new records and only show modified records", {
  expect_less_than(
    nrow(get_modified_lines(test_current_path, test_reference_path, as_df = TRUE, drop_new_records = TRUE)),
    149255
  )
})

test_that("get_modified_lines() can return modified lines, new records included", {
  expect_identical(
    nrow(get_modified_lines(test_current_path, test_reference_path, as_df = TRUE, drop_new_records = FALSE)),
    149255
  )
})

test_that("get_modified_lines() does not repeat the header in the first line", {
  df_modified_lines <- get_modified_lines(test_current_path, test_reference_path, as_df = TRUE)
  expect_false(
    dplyr::setequal(
      colnames(df_modified_lines),
      dplyr::slice_head(df_modified_lines, n = 1)
          )
  )
})
