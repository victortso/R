# assignment: handwash-instructions.docx
# Webscrape the data from the web site about Dr. Semmelweiss
# 2/3/2018
#
library(rvest)
library(RCurl)
library(XML)
library(dplyr)
library(ggplot2)

#df_html <- read_html("https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849")
#df_table1 <- html_nodes(df_html, "table")[[1]]
#df_table2 <- html_nodes(df_html, "table")[[2]]
#df_table3 <- html_nodes(df_html, "table")[[3]]
#df_table4 <- html_nodes(df_html, "table")[[4]]
#df_table5 <- html_nodes(df_html, "table")[[5]]

head(df_table1)

webpage <- read_html("https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849")
#tbls <- html_nodes(webpage, "table")
#tbls



#webpage <- getURL("https://en.wikipedia.org/wiki/Historical_mortality_rates_of_puerperal_fever#Monthly_mortality_rates_for_birthgiving_women_1841%E2%80%931849")
#htmldoc <- htmlParse(webpage)
#htmldoc2 <- html_nodes(df_html, "table")
#htmldoc2
#head(htmldoc2)
#htmldoc2

yearly <- webpage %>% html_nodes("table") %>% .[[2]] %>% html_table(fill = TRUE)
yearly
head(yearly, n=5L)
str(yearly)
dim(yearly)
headers <- paste(yearly[1,], yearly[2,])
headers <- paste(yearly[1,])
headers
yearly[1,]
yearly
colnames(yearly) <- headers
yearly
yearly[-1, ]
yearly

# Jayleen's post
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
html_doc <- webpage %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE)
yearly <- html_doc[[1]]
dim(yearly)
class(yearly)

#Remove the first two header lines
yearly <- yearly[c(3:nrow(yearly)), c(1,2,3,6,7)]
class(yearly)
yearly

#Add column names
colnames(yearly) <- c("Year", "C1_Births", "C1_Deaths", "C2_Births", "C2_Deaths")
yearly

#typeof(yearly[1,'C1_Births'])

#Convert character to numeric to allow math functions
#Remove the commas to make it numeric
#w = yearly$C1_Births
#yearly$C1_Births <- as.numeric(gsub(",", "", w))

#x = yearly$C1_Deaths
#yearly$C1_Deaths <- as.numeric(gsub(",", "", x))

#y = yearly$C2_Births
#yearly$C2_Births <- as.numeric(gsub(",", "", y))

#z = yearly$C2_Deaths
#yearly$C2_Deaths <- as.numeric(gsub(",", "", z))

# reduce to the following
# https://stackoverflow.com/questions/18462736/loop-through-columns-and-add-string-lengths-as-new-columns
for (i in names(yearly)[2:5]) {
  yearly[[i]] <- as.numeric(gsub(",", "", yearly[[i]]))
}

#Checking the data frame
yearly
class(yearly)

# Ensure that the proportions are known objects
yearly <- mutate(yearly, 
                 C1_Pro = C1_Deaths / C1_Births,
                 C2_Pro = C2_Deaths / C2_Births)

# Look at the data in the data frame
yearly

#Plot the data
#ggplot(data = yearly) +
#  geom_line(mapping = aes(x = Year, y = C1_Pro, group=1, size = 1.5, color = "blue")) +
#  geom_line(mapping = aes(x = Year, y = C2_Pro, group=1, size = 1.5, color = "red"))

prop_plot <- ggplot(data = yearly) +
  geom_line(mapping = aes(x = Year, y = C1_Pro, group=1, color = "blue")) +
  geom_line(mapping = aes(x = Year, y = C2_Pro, group=1, color = "red"))

prop_plot + ggtitle("Proportional Plot by Year") + ylab("Proportion")

yearly
