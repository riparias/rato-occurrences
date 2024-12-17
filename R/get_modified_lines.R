line_which <- function(x, fixed_patterns) {
  dplyr::left_join(
    dplyr::tibble(x),
    dplyr::tibble(
      fixed_patterns,
      present = TRUE
    ),
    dplyr::join_by("x" == "fixed_patterns")
  ) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::filter(present) %>%
    dplyr::pull(row_number)
}

#' Get the lines that changed between two DwC occurrence files
#'
#' Optionally you can exclude new records (based on `occurrenceID`) from the
#' result. If `drop_new_records` is set to `FALSE`, this function should work
#' for any two text files. It's based on ``xxhash32`` hashes compared line by
#' line.
#'
#' @param current_path The current file path to compare against. By default, this is the
#'   file in the `data/processed` directory.
#' @param reference_path The reference file path to compare against. By default, this is
#'   the file in the GitHub repository.
#' @param drop_new_records Should new records (based on `occurrenceID`) be
#'   excluded from the result?
#' @param as_df By default, return the modified lines as a data frame. If FALSE,
#'   return as a character vector of lines.
#'
#' @return
#' @export
#'
#' @examples
get_modified_lines <-
  function(current_path = "data/processed/occurrence.csv",
           reference_path = paste0(
             "https://raw.githubusercontent.com/riparias/rato-occurrences/main/",
             "data/processed/occurrence.csv"
           ),
           drop_new_records = TRUE,
           as_df = TRUE) {
    # Read the input files
    current <- readr::read_lines(cache_url(current_path), progress = FALSE)
    reference <- readr::read_lines(cache_url(reference_path), progress = FALSE)

    # Check that current differs from reference
    assertthat::assert_that(
      digest::digest(current, algo = "spookyhash") !=
        digest::digest(reference, algo = "spookyhash"),
      msg = "The current file is identical as the reference file."
    )

    # Remove new lines based on occurrenceID
    if (drop_new_records) {
      current_csv <- readr::read_csv(
        file = paste0(current, sep = "\n"),
        col_select = "occurrenceID",
        show_col_types = FALSE,
        progress = FALSE
      )
      reference_csv <- readr::read_csv(
        file = paste0(reference, sep = "\n"),
        col_select = "occurrenceID",
        show_col_types = FALSE,
        progress = FALSE)

      new_records_id <- dplyr::setdiff(
        current_csv,
        reference_csv
      ) %>%
        dplyr::pull("occurrenceID")

      new_lines <-
        line_which(
          current_csv$occurrenceID,
          new_records_id
        )
    }

    # Calculate hashes line by line for current and reference
    ## Create hashing function for vectorised input
    xxhash32 <- digest::getVDigest(algo = "xxhash32")

    cur_hash <- xxhash32(current)
    ref_hash <- xxhash32(reference)

    # Now find all lines that have changed between current and reference based
    # on the hashes.
    ## If the `drop_new_records` flag is `TRUE`, drop any lines that are completely new
    modified_lines <-
      line_which(cur_hash, ref_hash) %>%
      dplyr::setdiff(new_lines) %>%
      current[.]

    # Return the modified lines
    if (as_df) {
      return(
        readr::read_csv(
          file = paste0(c(head(current, 1), modified_lines), collapse = "\n"),
          show_col_types = FALSE
        )
      )
    } else {
      return(modified_lines)
    }
  }
