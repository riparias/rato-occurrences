# READ INTERIM DATA
interim_data <- readr::read_csv(
  here::here("data", "interim", "confirmed_observations.csv"),
  show_col_types = FALSE
)

# MAP TO DARWIN CORE
occurrence <-
  interim_data |>
  dplyr::mutate(
    .keep = "none",
    type = "Event",
    license = "CC0-1.0",
    rightsHolder = "RATO",
    datasetID = "https://doi.org/10.15468/fw2rbx",
    institutionCode	= "RATO",
    datasetName	= "RATO - Daily operations commissioned by the province East Flanders, Belgium",
    basisOfRecord	= "HumanObservation",
    occurrenceID = global_id,
    recordedBy = "RATO",
    # No reliable data for individualCount
    occurrenceStatus = "present",
    eventID = global_id, # Alternatively objectid
    parentEventID = dossier_id,
    eventType = dplyr::if_else(catch, "catch", "observation"),
    eventDate = laatst_bewerkt_datum, # readr will write as YYYY-MM-DDTHH:MM:SSZ
    samplingProtocol = material,
    # No reliable data for samplingEffort
    countryCode = "BE",
    municipality = gemeente,
    decimalLatitude	= latitude,
    decimalLongitude = longitude,
    geodeticDatum	= "WGS84",
    coordinateUncertaintyInMeters	= 30, # Use of GPS assumed
    verbatimLatitude = y,
    verbatimLongitude	= x,
    verbatimCoordinateSystem = "Lambert coordinates",
    verbatimSRS	= "Belgian Datum 1972",
    identificationVerificationStatus = "unverified",
    kingdom = kingdom,
    scientificName = scientific_name,
    taxonRank = taxon_rank
  ) |>
  dplyr::relocate(kingdom, .before = scientificName)

# WRITE DATA
readr::write_csv(occurrence, here::here("data", "processed", "occurrence.csv"), na = "")
