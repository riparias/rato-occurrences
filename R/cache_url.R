#' Cache URL content
#'
#' This function fetches the content of a given URL and caches it for future use.
#' If the provided path is not an URL, it returns the path so it can be used directly by reading functions.
#'
#' @param url The URL to fetch the content from.
#' @inheritParams httr2::req_cache
#' @return The content of the URL, either fetched as a string. If the provided path is not a url, it is returned as is.
#' @export
cache_url <- function(url, debug = FALSE) {
  # If the provided path is not an URL, return it so it can be used directly by reading functions
  if (!R.utils::isUrl(url)) {
    return(url)
  }

  # Otherwise, fetch the content and cache it
  httr2::request(url) |>
    httr2::req_retry() |>
    httr2::req_cache(
      path = file.path(tools::R_user_dir("pkgdown", "cache"), "rato.occurrences"),
      max_age = 600,
      debug = debug
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}
