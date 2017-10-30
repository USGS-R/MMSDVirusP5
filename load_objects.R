# Load objects into the local environment from the remake process
library(remake)

make()

dates <- make("dates")
siteInfo <- make("siteInfo")
parmInfo <- make("parmInfo")
NWIS <- make("NWIS")

