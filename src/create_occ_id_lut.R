# Build a lookup table for John Waller between the old and the new occurrence
# ID's (since the switch to GUIDs)



# fetch the datasets ------------------------------------------------------


old_dataset_version <- "v1.74"
new_dataset_version <- "v1.75"

old_dataset_url <-
  glue::glue("https://ipt.inbo.be/archive.do?r=rato-occurrences&v={old_dataset_version}")
new_dataset_url <-
  glue::glue("https://ipt.inbo.be/archive.do?r=rato-occurrences&v={new_dataset_version}")

# Fetch locally

old_dataset <-
  rgbif::as.download("~/Downloads/dwca-rato-occurrences-v1.74.zip") |>
  rgbif::occ_download_import()

new_dataset <-
  rgbif::as.download("~/Downloads/dwca-rato-occurrences-v1.75.zip") |>
  rgbif::occ_download_import()


# calc a content based hash as id -----------------------------------------

# Function to create an identifier based on identifying columns for this
# dataset: https://gist.github.com/PietrH/247f4e1bc6620a91055237176c913275

create_id <- function(df){
  dplyr::mutate(
    df,
    concat_id_cols = paste0(
      occurrenceStatus,
      eventDate,
      verbatimLatitude,
      verbatimLongitude,
      scientificName,
      samplingProtocol
    )
  ) |>
    dplyr::mutate(
      new_id =
        purrr::map_chr(concat_id_cols, ~ digest::digest(paste0(.x), algo = "crc32")
        )
    )
}

# Check that this create id function actually produces unique id's
assertthat::assert_that(nrow(old_dataset) == data.table::uniqueN(create_id(old_dataset)$new_id))
assertthat::assert_that(nrow(new_dataset) == data.table::uniqueN(create_id(new_dataset)$new_id))

# Look at content based collisions.
create_id(new_dataset) %>%
  dplyr::filter(duplicated(new_id)) %>%
  # select(occurrenceStatus, eventDate, verbatimLatitude, verbatimLongitude, scientificName) %>%
  View()

# -> These records only differ in their identifier, I can not distinguish these

# add content based identifiers to the data -------------------------------


# drop any rows that are duplicated, if they only differ in the id, then I can't
# reconcile them. I'm dropping very few records this way.

new_dataset_identified <-
  create_id(new_dataset) %>%
  filter(!duplicated(new_id))

old_dataset_identified <-
  create_id(old_dataset) %>%
  filter(!duplicated(new_id))


# merge the datasets, find records in common ------------------------------


left_join(old_dataset_identified,
          new_dataset_identified,
          by = join_by("new_id"),
          suffix = c(glue::glue("_{old_dataset_version}"),
                     glue::glue("_{new_dataset_version}"))
) %>%
  select(contains("occurrenceID")) %>%
  data.table::fwrite("data/processed/occurrenceID_LUT.csv")

