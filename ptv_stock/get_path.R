#
# FILE: get_path.R
#
# 3/30/2018 tso   New File
# 4/29/2018 tso   Compose the path URL to retrieve data from alphavantage.co website.
#

# library(dplyr)
# library(lubridate)
# library(ggplot2)

get_path <- function(apikey, 
                     sym = "AAPL", 
                     site = "alphavantage", 
                     func = "TIME_SERIES_DAILY_ADJUSTED", 
                     outputsize = "compact", 
                     datatype = "csv") {
  if (site != "alphavantage") {
    stop("Don't know how to interpret for now", call. = FALSE)
  }
  
  # https://www.alphavantage.co/documentation/
  http <- "https://"
  siteURL <- "www.alphavantage.co/"
  query <- "query?"
  
  url <- paste0(http, siteURL, query, 
                "function=", func,
                "&",
                "symbol=", sym,
                "&",
                "apikey=", apikey,
                "&",
                "outputsize=", outputsize,
                "&",
                "datatype=", datatype)
  sprintf("url=%s", url)
  return (url)
}