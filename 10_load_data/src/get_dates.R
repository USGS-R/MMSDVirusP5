# Define dates of interest for retrieving data

get_dates <- function() {
  beginDate <- "2017-01-01"
  endDate <- as.character(Sys.Date())
  dates <- c(beginDate,endDate)
  return(dates)
}
         
