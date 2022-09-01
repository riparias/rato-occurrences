#' Download data from rato vzw's WFS as csv

# install needed packages
installed <- rownames(installed.packages())
required <- c("httr", "ows4R", "here")
if (!all(required %in% installed)) {
  pkgs_to_install <- required[!required %in% installed]
  print(paste("Packages to install:", paste(pkgs_to_install, collapse = ", ")))
  install.packages(pkgs_to_install)
}

# load libraries
library(httr) # generic webservice package
library(ows4R) # interface for OGC webservices
library(here) # to work with paths

# WFS link
wfs <- "https://geodiensten.oost-vlaanderen.be/arcgis/services/MIL/RATO_Public_Data/MapServer/WFSServer"

# create WFS client
rato_client <- ows4R::WFSClient$new(wfs, serviceVersion = "2.0.0")

# get overview of the layers available
layers <- rato_client$getFeatureTypes(pretty = TRUE)
print(layers)

# request data from layer RATO_Public_Data
url <- httr::parse_url(wfs)
url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "RATO_Public_Data",
                  outputFormat = "CSV"
)
request <- httr::build_url(url)

# Download requested data as csv file
file <- here::here("data", "raw", "rato_data.csv")
httr::GET(url = request, write_disk(file, overwrite = TRUE))
