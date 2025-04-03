#' Set the order of the columns to a specific order.
#'
#' This order is relevant because it is easier to read in a text editor, as well
#' as increasing the chances that git will correctly detect a change, addition
#' or removal based on the identifiable columns being at the front.
#'
#' @details
#'
#' A notable difference from default DWC-A field order is that identifing fields
#' are placed at the front to increase the chances of GIT correctly detecting
#' changes as additions, changes or deletions. Columns that are less likely to
#' change are placed at the back.
#'
#' @param x Input tibble as produced by the dwc mapping
#'
#' @return A tibble with the columns in the default order of the dwc mappings on
#'   the IPT, except identiable columns are placed at the front.
#' @export
set_dwc_order <- function(x) {
  assertthat::assert_that(is.data.frame(x))
  dplyr::relocate(
    x,
    "occurrenceID",
    "occurrenceStatus",
    "eventID",
    "scientificName",
    "eventDate",
    "organismQuantity",
    "organismQuantityType",
    "samplingProtocol",
    "samplingEffort",
    "municipality",
    "verbatimLatitude",
    "verbatimLongitude",
    "verbatimCoordinateSystem",
    "verbatimSRS",
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "countryCode",
    "geodeticDatum",
    "type",
    "language",
    "license",
    "rightsHolder",
    "datasetID",
    "institutionCode",
    "datasetName",
    "basisOfRecord",
    "recordedBy",
    "kingdom",
    "taxonRank"
  )
}
