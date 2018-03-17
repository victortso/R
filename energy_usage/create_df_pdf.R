#
# input: utility bill in PDF format from City of Palo Alto 
# output: data frame consists of the the year summary of Electricity, Gas and Water usuage
# 3/11/2018 - tso   First revision
# 
create_df_pdf <- function(utility_pdf) {
  #
  # https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
  #
  
  # QUESTION: Where is the appropriate place to put these library declarations?
  # need for %>%
  library(dplyr)
  library(tm)
  
  if (!file.exists(utility_pdf)) {
    stop("File does not exist!", call. = FALSE)
  }
  
  read_pdf <- readPDF(control = list(text = "-layout"))
  
  document2 <-
    Corpus(
      URISource(
        #        "./data/home_energy_bill/2016/PaloAltoUtilities20161221.pdf"
        utility_pdf
      ),
      readerControl = list(reader = read_pdf)
    )
  doc2 <- content(document2[[1]])
  # doc2
  # head(doc2)
  doc2_2 <- strsplit(doc2, "\n")
  
  # DEBUG BEGIN
  # doc2_2[[3]]
  # str(doc2_2)
  # write.table(doc2_2[[3]], "/Users/tso/proj/fuhsd/R/home_energy_bill/PaloAltoUtilities20180123_tm.txt", sep = "\t")
  # DEBUG END
  
#if you the entire usage bill, then it is page 3
#  (tbl <- doc2_2[[3]])
# One page extracted from the original bill
  (tbl <- doc2_2[[1]])
  # tbl[2]
  
  # use grep instead of grepl
  # https://stackoverflow.com/questions/27015180/error-in-if-argument-is-of-length-zero-in-r
  j <- 0
  for (i in 1:length(tbl)) {
    # print(tbl[i])
    # Find the block of the billing info' starting from the line with identifier "SERVICE"
    #   followed by a line with the identifier "MONTH"
    if (grepl("SERVICE", tbl[i]) == TRUE &&
        grepl("MONTH", tbl[i + 1]) == TRUE) {
      # DEBUG BEGIN
      # print(tbl[i])
      # print(tbl[i + 1])
      # DEBUG END
      j <- i + 2
      
      # DEBUG BEGIN
      # print(j)
      # DEBUG END
      break
    }
  }
  
  # DEBUG BEGIN
  # if (j != 0) {
  #   # print(paste0("FOUND ", j))
  #   # message(sprintf("FOUND %d\n", j))
  #   message("FOUND ", j)
  # }
  #
  # Iterate through the block, not including the last row containing the identifier "Page" info'
  # for (i in j:(length(tbl) - 1)) {
  #   print(tbl[i])
  # }
  # DEBUG END
  
  df <- data.frame() # create dataframe to store raw data
  billinfo_header <-
    c(
      "MONTH",
      "SERVICE DAYS",
      "ELECTRIC KWHS PER DAY",
      "ELECTRIC $ PER DAY",
      "GAS THERMS PER DAY",
      "GAS $ PER DAY",
      "WATER GAL PER DAY",
      "WATER $ PER DAY"
    )
  
  for (i in j:(length(tbl) - 1)) {
    temp <- tbl[i]
    # # message("k= ", k) # DEBUG
    # # df[k,1] <- as.Date(mdy(paste("01",substr(temp, 4, 10),sep="/")))
    # df[k, 1] <- paste("01", substr(temp, 3, 10), sep = "/")
    # df[k, 2] <- as.numeric(substr(temp, 14, 15))
    # df[k, 3] <- as.numeric(substr(temp, 24, 27))
    # df[k, 4] <- as.numeric(substr(temp, 38, 41))
    # df[k, 5] <- as.numeric(substr(temp, 52, 55))
    # df[k, 6] <- as.numeric(substr(temp, 65, 68))
    # df[k, 7] <- as.numeric(substr(temp, 75, 80))
    # df[k, 8] <- as.numeric(substr(temp, 89, 92))

    # extract billing info details from each months (line)
    data <- list()
    t <- strsplit(temp, " ")
    k <- 1
    for (m in 1:length(t[[1]])) {
      if (t[[1]][m] != "") {
        # DEBUG BEGIN
        # print(t[[1]][m])
        # DEBUG END
        data[k] <- t[[1]][m]
        k <- k + 1
      }
    }
    if (length(billinfo_header) != (k-1)) {
      message("Incorrect number of billing info", (k-1))
    }
    # transfer the billing info' details from temporary t to data frame df[k, *]
    k <- i - j + 1
    df[k, 1] <- paste("01", data[[1]], sep = "/")
    for (m in 2:length(billinfo_header)) {
      df[k, m] <- as.numeric(data[[m]])
    }
  }
  colnames(df) <- billinfo_header
  
  # CLEAN UP: Combining the cleaned up Month and Year to Date
  # Somehow the column needs to be created as a vector for subsequent as.Date
  df$DATE <- as.Date("1841-01-01")
  for (i in 1:nrow(df)) {
    # https://www.stat.berkeley.edu/~s133/dates.html
    # Ways to convert to standard date format
    df$DATE[i] <-
      as.Date(df[i, "MONTH"], format = '%d/%m/%Y')
  }
  
  # Rearrange columns
  # Make sure you have library(dplyr) in order for %>% to work!
  df <- df %>% select("DATE", everything())
  # Drop the Year and Month
  df <-
    df[c(1:nrow(df)), c(1, 3:ncol(df))]
  
  # str(df)
  
  # Calculate total consumption of Electricity + Gas + Water in $ a day
  df <- df %>%
    mutate(BILLING = (`ELECTRIC $ PER DAY` + `GAS $ PER DAY` + `WATER $ PER DAY`))
  
  # remove one entry from last year
  # and renumber the row #
  df <- df[-12, ]
  # sort
  ranks <- order(df$DATE,decreasing = FALSE)
  df <- df[ranks,]
  # renumber
  rownames(df) <- seq(length=nrow(df))
  
  return(df)
}