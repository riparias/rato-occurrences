#' Write an encrypted csv
#'
#' @param x A data frame or tibble to write to disk in an encrypted binary file.
#' @param overwrite TRUE by default: the outfile will be removed before attempting to write.
#' @param ... passed on to [readr::read_csv()]
#'
#' @inheritParams readr::read_csv
#' @inheritParams safer::decrypt_file
#'
#' @return A binary file, the function returns TRUE invisibly on success.
#' @export
#'
write_encrypted_csv <- function(x,
                                outfile,
                                key = Sys.getenv("encryption_key"),
                                overwrite = TRUE,
                                ...) {
  # check inputs
  assertthat::assert_that(is.data.frame(x))
  assertthat::assert_that(assertthat::is.writeable(dirname(outfile)))
  assertthat::assert_that(assertthat::is.string(key))
  assertthat::assert_that(key != "")

  # remove the outfile if it exists
  if (file.exists(outfile)) {
    file.remove(outfile)
  }

  # write out
  withr::with_tempfile(
    "decrypted",
    {
      readr::write_csv(
        x = x,
        file = decrypted,
        ...
      )

      safer::encrypt_file(
        infile = decrypted,
        key = key,
        outfile = outfile
      )
    }
  )
  invisible(TRUE) # return TRUE on success
}
