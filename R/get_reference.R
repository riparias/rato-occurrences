#' Get the version of the dataset as published on Github, called reference in
#' tests
#'
#' This version might be different from the one on GBIF, as the IPT only fetches
#' the data from GITHUB periodically (once a week) and not necessarily in phase
#' with the updates on Github.
#'
#' @return A data.frame with DwC mapping of the dataset, as currently published
#'   on Github.
#' @export
#'
#' @examplesIf interactive()
#' get_reference()
get_reference <- function() {
  github_csv_url <-
    paste0(
      "https://raw.githubusercontent.com/riparias/rato-occurrences/main/",
      "inst/data/processed/occurrence.csv"
    )

  httr2::request(github_csv_url) %>%
    httr2::req_retry() %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    readr::read_csv(show_col_types = FALSE,
                    progress = FALSE)
}
