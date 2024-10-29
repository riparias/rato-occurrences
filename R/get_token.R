#' Request an access token from the GIS API services
#'
#' @param username ArcGIS Enterprise username
#' @param password ArcGIS Enterprise password
#' @param expires In minutes, how long should the token remain valid?
#'
#' @return Character. An access token for future API calls.
#'
#' @export
get_token <- function(username = "RATO_INBO",
                      password = Sys.getenv("ratopwd"),
                      expires = 5,
                      domain = "https://gis.oost-vlaanderen.be/portal") {
  # Check that username and password are strings if provided
  assertthat::assert_that(assertthat::is.string(username))
  assertthat::assert_that(assertthat::is.string(password))

  # If the pwd variable isn't set, prompt for password when session interactive
  if(password == ""){
    Sys.setenv(ratopwd = askpass::askpass())
  }

  # Build request for the API
  token_request <-
    httr2::request(domain) %>%
    httr2::req_url_path_append("sharing", "rest", "generateToken") %>%
    httr2::req_body_form(
      username = username,
      password = password,
      # NOTE MUST USE CLIENT `referer`, otherwise you'll get a token but it will
      # not work!
      client = "referer",
      referer = dirname(domain),
      expiration = expires,
      f = "json"
    )

  # Parse the API response
  token_response <-
    token_request %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  # If unable to login, reset the password so one is requested next time.
  if(
    purrr::pluck(token_response, "error", "code", .default = FALSE)
  ) {
    Sys.setenv(ratopwd = "")
    stop(
      glue::glue(purrr::chuck(token_response, "error", "message"),
                 purrr::map_chr(
                   purrr::chuck(token_response, "error", "details"),
                   ~.x)))
  } else {
    ## If there was no error, return the token
    return(token_response$token)
  }


}
