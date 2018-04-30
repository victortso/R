#
# FILE: create_subset_df.R
#
# 4/14/2018 tso   New File
# 4/29/2018 tso   Create subset data frame (30, 60, 90 days) out from the larger set of data frame
#
#------ Requirement# 1
# Given n is number of months to be used to compute the SD of the last n months
# Create a subset data frame that contains the past n month(s)

create_subset_df <- function(df, n) {
  # https://stats.stackexchange.com/questions/12980/subset-data-by-month-in-r
  # using subset to get the specific month data
  df$month <- as.numeric(substr(df$timestamp, 6, 7))
  m <- df$month[1]
  first <- subset(df, month == m)
  
  # DEBUG: BEGIN
  # n <-7
  # df <- df_save
  # subset of df
  # DEBUG: END
  df_s <- data.frame()
  for (i in 1:n) {
    m <- df$month[1]
    first <- subset(df, month == m)
    if (dim(first)[1] == 0) {
      # Check if there is anything left to do.
      # print("DONE early")
      break;
    }
    df <- subset(df, month != m)
    df_s <- rbind(df_s, first)
  }
  
  (df_s <- df_s[,c(1:7)])
  # Compute the Range, Range_SD_range and SD
  df_s <- df_s %>%
    mutate(Range = high - low)
  # Compute the SD
  (sd_range <- sd(df_s$Range))
  
  df_s <- df_s %>%
    mutate(SD = sd_range)
  # Compute the Range - SD_range
  df_s <- df_s %>%
    mutate(Range_SD_range = Range - sd_range)
  
  return(df_s)
}