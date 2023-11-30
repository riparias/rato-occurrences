# experimental script to retreive rato data behind authentication, via a map
# service layer


# load libraries ----------------------------------------------------------


library(httr2)




# get an access token -----------------------------------------------------

get_token <- function(username = "RATO_INBO", password = Sys.getenv("ratopwd")) {
  assertthat::assert_that(assertthat::is.string(username))
  assertthat::assert_that(assertthat::is.string(password))

  if(password == ""){
    Sys.setenv(ratopwd = askpass::askpass())
  }

  token_request <-
    request("https://gis.oost-vlaanderen.be/portal/sharing/rest/generateToken") %>%
    req_body_form(
      username = username,
      password = Sys.getenv("ratopwd"),
      client = "referer", #MUST USE CLIENT referer, otherwise you'll get a token but it will not work!
      referer = "https://gis.oost-vlaanderen.be",
      expiration = 5,
      f = "json"
    )

  token_response <-
    token_request %>%
    req_perform() %>%
    resp_body_json()

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
    return(token_response$token)
  }


}

# get a list of all object id's -------------------------------------------

get_all_object_ids <- function(token = get_token()) {
  object_ids_response <-
    request("https://gis.oost-vlaanderen.be/server/rest/services/RATO/RATO_Private_Data/MapServer/0/query?where=1%3D1") %>%
    req_url_query(
      returnIdsOnly = "true",
      f = "pjson",
      token = token
    ) %>%
    req_perform() %>%
    resp_body_json(check_type = FALSE)

  object_ids_response %>%
    purrr::chuck("objectIds") %>%
    unlist() %>%
    return()
}

# get featrues via objectid -----------------------------------------------

object_ids_to_query <- get_all_object_ids()

fetch_objects <- function(object_ids, token = get_token()) {
  assertthat::assert_that(assertthat::not_empty(object_ids))

  object_id_query <- glue::glue(
    "OBJECTID IN ({object_ids_collated})",
    object_ids_collated = glue::glue_collapse(object_ids,
      sep = ","
    )
  ) %>%
    curl::curl_escape()

  objects_request <-
    request(
      glue::glue("https://gis.oost-vlaanderen.be/server/rest/services/RATO/RATO_Private_Data/MapServer/0/query?where={object_id_query}")
    ) %>%
    req_url_query(
      outFields = "*",
      f = "json",
      token = token
    )

  objects_response <-
    objects_request %>%
    req_perform() %>%
    resp_body_json()

  assertthat::assert_that(
    isTRUE(purrr::pluck(objects_response, "error", "message", .default = TRUE)),
    msg = purrr::pluck(objects_response, "error", "message")
  )

  objects_response %>%
    purrr::chuck("features") %>%
    purrr::map(~ purrr::pluck(.x, "attributes")) %>%
    purrr::map_dfr(~.x) %>%
    return()
}


# split up the ids in batches of 100 --------------------------------------
object_ids_to_query <- get_all_object_ids()

# we get an error if we query for more than 100 objects at a time
raw_data <-
  split(object_ids_to_query, ceiling(seq_along(object_ids_to_query)/100)) %>%
  purrr::map_dfr(fetch_objects)
