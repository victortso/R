# Task 4 part of the assignment
# 2/3/2018
# https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849
#
library(rvest)
library(RCurl)
library(XML)
library(dplyr)
library(ggplot2)
# Combining year and month into standard date format
# For as.yearmon
library(zoo)

webpage <- read_html("https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849")

# First method: monthly <- html_nodes(webpage, "table")[[1]]
# Second method: monthly <- webpage %>% html_nodes("table") %>% .[[1]] %>% html_table(fill = TRUE)

# 3rd method
html_doc <- webpage %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)

monthly <- html_doc[[1]]
dim(monthly)
class(monthly)

#Remove the header
monthly <- monthly[c(1:nrow(monthly)), c(1,2,3,4)]
class(monthly)
monthly

# Extract the Notes column
notes <- monthly[c(1:nrow(monthly)), c(6)]

# CLEAN UP: The problem child is the Month column!!

# https://stackoverflow.com/questions/10128617/test-if-characters-in-string-in-r
# grepl
# with fixed = TRUE, is faster
# grepl("mar", tolower(monthly$Month[1]), fixed = TRUE)

# string replace
# str_replace() or str_replace_all
# http://stringr.tidyverse.org/reference/str_replace.html

# For each month, generate a df based on the month vector and the logical vector
for (i in month.abb) {
  # When can I use fixed = TRUE?!
  #month_mask <- grepl(i, monthly$Month, ignore.case = TRUE)
  month_mask <- grepl(i, monthly$Month, ignore.case = TRUE)
  # construct data frame
  df <- data.frame(d = monthly$Month, j = month_mask)
  monthly$Month <- replace(df$d, df$j, i)
  # print(monthly$Month)
}

# CLEAN UP: Take care of the special/bad data on row 41 "Maj" & "Okt" is supposed to be "May" & "Oct"
# Convert this if the data set is big!
month_mask <- grepl("Maj", monthly$Month, ignore.case = TRUE)
df <- data.frame(d = monthly$Month, j = month_mask)
monthly$Month <- replace(df$d, df$j, "May")

month_mask <- grepl("Okt", monthly$Month, ignore.case = TRUE)
df <- data.frame(d = monthly$Month, j = month_mask)
monthly$Month <- replace(df$d, df$j, "Oct")

# Remove the na / na bad data!
# Just remove the twelveth row!
# monthly <- monthly[-12,]

# CLEAN UP: Convert to numeric so that NA is not of type character for use in is.na/na.omit
# Warning is expected here!
for (i in names(monthly)[3:4]) {
  monthly[[i]] <- as.numeric(monthly[[i]])
}

# Make sure the field is of right type as mentioned above!
#monthly$Births <- replace(monthly$Births, is.na(monthly$Births), 0)
#monthly$Deaths <- replace(monthly$Deaths, is.na(monthly$Deaths), 0)

# CLEAN UP: Replace Year(NA) with the Year indicated above
yr <- monthly$Year[1]
for (i in 2:nrow(monthly)) {
  if (is.na(monthly$Year[i])) {
    monthly$Year[i] <- yr
  }
  else {
    # Save the year for subsequent rows
    yr <- monthly$Year[i]
  }
}

# Now get rid of the NA row.
# I guess another way is to compute the mean of the entire (or surrounding rows) column, but I'm lazy! :-)
monthly <- na.omit(monthly)

# Compute the proportion deaths over births.
monthly <- mutate(monthly, proportion_deaths = (Deaths / Births))
head(monthly)

# CLEAN UP: Combining the cleaned up Month and Year to Date
# Somehow the column needs to be created as a vector for subsequent as.Date
monthly$Date <- as.Date("1841-01-01")
for(i in 1:nrow(monthly)) {
  monthly$Date[i] <- as.Date(as.yearmon(paste(monthly$Month[i], monthly$Year[i], sep=" ")))
}

# Rearrange columns
monthly <- monthly %>% select("Date", everything())
# Drop the Year and Month
monthly <- monthly[c(1:nrow(monthly)), c(1,4,5,6)]

# Plot the graph
# There is a difference in between putting the mapping as part of the ggplot parameter
# vs telling geom_line taking the mapping parameter!  The latter shows with legend!
#prop_plot <- ggplot(data = monthly) +
#  geom_line(mapping = aes(x = Date, y = proportion_deaths, size=1, color = "blue"))
prop_plot <- ggplot(data = monthly, mapping = aes(x = Date, y = proportion_deaths)) +
  geom_line(size=1, color = "blue")

prop_plot + ggtitle("Death Rate of New Born") +
  # ylab("Proportion Deaths Deaths/Births") +
  labs(x ="Time", y = "Deaths/Births") +
  theme(
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="slateblue4", size=14, face="bold") 
  ) +
  geom_point(mapping = aes(x = as.Date("1841-12-01"), y = .215), 
             size = 3, color = "red") +
  geom_text(label = "Missing data for Dec 1841", x = as.Date("1843-04-01"), 
            y = .23, size = 3, colour = "red", alpha = .02)
  #+
  #geom_text(label = "Line Thickness = Number of Births", 
  #          x = as.Date("1846-04-01"), y = .3, size = 3.5, colour = "purple") 




