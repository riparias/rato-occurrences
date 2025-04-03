#' Get the version of the dataset as currently present locally. Called current
#' in tests.
#'
#' This version might be different from the reference dataset [get_reference()]
#' if new data was fetched and mapped but not yet published to Github. For
#' example during an automatic publishing run, or while a new verion of the
#' dataset is still in review.
#'
#' @return A data.frame with DwC mapping of the dataset, as present on the repo.
#' @export
#'
#' @examplesIf interactive()
#' get_current()
get_current <- function() {

  # `occurrence.csv` is stored in inst/data/processed/occurrence.csv, everything
  # stored in inst is placed into the package root after installing. Thus in
  # data/processed/occurrence.csv, but when directly navigating the repo, the
  # file still remains in inst.

  data_object_path <-
    system.file("extdata/processed", package = "rato.occurrences")

  # Make sure data_object_path exists
  assertthat::assert_that(
    dir.exists(data_object_path)
  )

  readr::read_csv(
    file.path(data_object_path, "occurrence.csv"),
    show_col_types = FALSE,
    progress = FALSE
  )
}
