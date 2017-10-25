# Read site names and USGS station IDs
library(dplyr)
library(dataRetrieval)

get_sites <- function(sites_file) {
  
  sites_file_path <- "./10_load_data/raw"
  sites <- read.csv(file.path(sites_file_path,sites.file),colClasses = c("character","character"))
  from_NWIS <- readNWISsite(sites$STAID)
  dfSites <- left_join(from_NWIS, sites, by=c("site_no"="STAID"))
  
  return(dfSites)
}
