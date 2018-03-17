#
# input: Billing PDF files locating in ../data/home_energy_bill/{year}/*.pdf
# output: a list of data frame billing info' from 2009 to 2017
#
# 3/11/2018 - tso First Revision
#
# This function create_billing_df_list will in turn call function create_df_pdf
# to read the billing PDF file and store it in the corresponding year data frame
#

source("create_df_pdf.R")

# Set the path to the data folder
data_path <- "./data/home_energy_bill/"

create_billing_df_list <- function(path=data_path, start_year=2009, end_year=2017) {

  # frontLink <- "../data/home_energy_bill/"
  frontLink <- path
print(frontLink)
  middleLink <- "/PaloAltoUtilities"
  # Only care about Decemeber month
  monthLink <- "12"
  # using wild card to find the December data fil
  backLink <- "*.pdf"
  start <- start_year
  finish <- end_year
print(start)
print(finish)
  
  billing_df_list <- list() # prepare result

  for (i in start:finish) {
    year <- i
    fullLink <-
      paste(frontLink, year, middleLink, year, monthLink, backLink, sep = '') # create link based on year
  print(fullLink)
    filenames <- Sys.glob(fullLink)
  print(filenames)
    billing_df_list[[as.character(year)]] <- create_df_pdf(filenames)
    # DEBUG BEGIN
    # str(filenames)
    # print(filenames)
    # DEBUG END
  }
  return (billing_df_list)
}

# billing_df <- create_df_pdf("../data/home_energy_bill/2009/PaloAltoUtilities20091217.pdf")
