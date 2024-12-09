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

  readr::read_csv(
    file.path(here::here(), "data/processed/occurrence.csv"),
    show_col_types = FALSE,
    progress = FALSE
  )
}
