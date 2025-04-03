#' R code to automatically run all chunks of fetch_data.Rmd
library(knitr)

# create temporary R file
tempR <- tempfile(fileext = ".R")
# look for files relative to the location of this script
here::i_am("inst/source/run_dwc_mapping.R")

# execute them
knitr::purl(here::here("inst/source/dwc_mapping.Rmd"), output=tempR)
source(tempR)
unlink(tempR)
