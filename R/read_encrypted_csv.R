#' Read an encrypted csv
#'
#' The decryption key is read from the system environment by default, but can
#' be provided as a length one character vector.
#'
#' @param ... passed on to [readr::read_csv()]
#'
#' @inheritParams readr::read_csv
#' @inheritParams safer::decrypt_file
#'
#' @return A [tibble()]. As per [readr::read_csv()]
#'
read_encrypted_csv <- function(file, key = Sys.getenv("encryption_key"), ...) {
  withr::with_tempfile(
    "decrypted",
    {
      safer::decrypt_file(
        file,
        key = key,
        outfile = decrypted
      )

      decrypted_tibble <- readr::read_csv(decrypted, ...)
    }
  )
  return(decrypted_tibble)
}
