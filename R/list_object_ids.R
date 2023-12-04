list_object_ids <- function(token = get_token()) {

  object_ids_response <-
    httr2::request("https://gis.oost-vlaanderen.be/server/rest/services/RATO/RATO_Private_Data/MapServer/0/query?where=1%3D1") %>%
    httr2::req_url_query(
      returnIdsOnly = "true",
      f = "pjson",
      token = token
    ) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(check_type = FALSE)

  object_ids_response %>%
    purrr::chuck("objectIds") %>%
    unlist() %>%
    return()
}
