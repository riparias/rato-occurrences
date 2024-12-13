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
    httr2::req_cache(path = tempdir(), max_age = 600) %>%
    httr2::req_perform() %>%
    httr2::resp_body_raw() %>%
    rawToChar() %>%
    readr::read_csv(show_col_types = FALSE,
                    progress = FALSE,
                    col_types = 
                      readr::cols(
                        occurrenceID = readr::col_character(),
                        occurrenceStatus = readr::col_character(),
                        eventID = readr::col_double(),
                        scientificName = readr::col_character(),
                        eventDate = readr::col_datetime(format = ""),
                        organismQuantity = readr::col_double(),
                        organismQuantityType = readr::col_character(),
                        samplingProtocol = readr::col_character(),
                        samplingEffort = readr::col_character(),
                        municipality = readr::col_character(),
                        verbatimLatitude = readr::col_double(),
                        verbatimLongitude = readr::col_double(),
                        verbatimCoordinateSystem = readr::col_character(),
                        verbatimSRS = readr::col_character(),
                        decimalLatitude = readr::col_double(),
                        decimalLongitude = readr::col_double(),
                        coordinateUncertaintyInMeters = readr::col_double(),
                        countryCode = readr::col_character(),
                        geodeticDatum = readr::col_character(),
                        type = readr::col_character(),
                        language = readr::col_character(),
                        license = readr::col_character(),
                        rightsHolder = readr::col_character(),
                        datasetID = readr::col_character(),
                        institutionCode = readr::col_character(),
                        datasetName = readr::col_character(),
                        basisOfRecord = readr::col_character(),
                        recordedBy = readr::col_character(),
                        kingdom = readr::col_character(),
                        taxonRank = readr::col_character()
                      ))
}
