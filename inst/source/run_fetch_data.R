#' R code to automatically run all chunks of fetch_data.Rmd

# load required packages (install them if needed)
installed <- rownames(installed.packages())
required <- c("knitr")
if (!all(required %in% installed)) {
  install.packages(required[!required %in% installed])
}
library(knitr)

# look for files relative to the location of this script
here::i_am("inst/source/run_fetch_data.R")

# create temporary R file
tempR <- tempfile(fileext = ".R")

# execute them
knitr::purl(here::here("inst/source/fetch_data.Rmd"), output=tempR)
source(tempR)
unlink(tempR)
