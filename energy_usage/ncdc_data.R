#
# Read NCDC weather data
# https://www.climate.gov/maps-data/dataset/past-weather-zip-code-data-table


library(dplyr)

# Set working directory
# setwd("./energy_usage")

# start <- 2009
# finish <- 2011
# weather_data <- read.csv("../data/ncdc/raw/1246037_2009_2011.csv")

# start <- 2012
# finish <- 2015
# weather_data <- read.csv("../data/ncdc/raw/1246003_2012_2015.csv")

start <- 2016
finish <- 2017
weather_data <- read.csv("./data/ncdc/raw/1243541_2016_2017.csv")
# head(weather_data)

# extract the relevant part
pa_data <- subset(weather_data, NAME == "PALO ALTO, CA US")
str(pa_data)

# cleanse them
pa_data$NAME <- as.character(pa_data$NAME)
pa_data$DATE <- as.Date(pa_data$DATE)
# length(pa_data)
# pa_data
# summary(pa_data)
# nrow(pa_data)
# head(pa_data)

# Take care of NA in the TMAX, TMIN and TOBS
# Just use previous entry
for (i in 1:nrow(pa_data)) {
  if (is.na(pa_data$TMAX[i])) {
    if (is.na(pa_data$TMAX[i-1]) == FALSE) {
      # use previous entry
      pa_data$TMIN[i] <- pa_data$TMAX[i-1]
    }
  }
  if (is.na(pa_data$TMIN[i])) {
    if (is.na(pa_data$TMIN[i-1]) == FALSE) {
      pa_data$TMIN[i] <- pa_data$TMIN[i-1]
    }
  }
  if (is.na(pa_data$TOBS[i])) {
    if (is.na(pa_data$TOBS[i-1]) == FALSE) {
      pa_data$TOBS[i] <- pa_data$TOBS[i-1]
    }
  }
}

#---- CLEANSE UP done

# 3 column vectors: Date, Mean_max_min, Mean_TOBS
# intermediate column vectors: Date, Sum_TMAX, Sum_TMIN, Sum_TOBS

same_year <- start
same_month <- 1
tmax_vector <- 0
tmin_vector <- 0
tobs_vector <- 0

# DEBUG:
# ff <- 0
# DEBUG END
compute_mean_values <- function(weather, pa_data, index, year, tmax_vector, tmin_vector, tobs_vector, counter) {
# DEBUG BEGIN
  # if (ff == 0) {
  #   print(counter)
  #   print(tmax_vector)
  #   print(tmin_vector)
  #   print(tobs_vector)
  #   # this is to make sure the change propagate to the global variable ff!
  #   ff <<- 1
  # }
# DEBUG END
  mean_tmax_tmin <- (tmax_vector - tmin_vector)/counter
  mean_tmax_tmin <- mean_tmax_tmin/2. + tmin_vector/counter
  mean_tobs <- tobs_vector / counter
  # date <- as.Date("1841-01-01")
  # date <- as.Date(pa_data[i, "DATE"])
  date <- pa_data[index, "DATE"]  # get the last entry of the month
  # print(date)
  # str(date)
  year_char <- as.character(year)
  # str(same_year_char) ##
  # print(same_year_char) ##
  
  weather[[year_char]] <- rbind(weather[[year_char]], data.frame(date, mean_tmax_tmin, mean_tobs))
  return(weather)
}

# weather <- list()
# d <- as.Date("2016-01-01")
# date <- as.Date("2016-01-1")
# mean_tmax_tmin <- 0
# mean_tobs <- 0
# data <- data.frame(date, mean_tmax_tmin, mean_tobs)
# data <- rbind(data, c("2016-02-1", 1, 2))
# str(data)
# str(pa_data)
# weather$"2007" <- data
  
# rm(weather)
weather <- list()
date <- as.Date("2016-01-1")
mean_tmax_tmin <- 0
mean_tobs <- 0
##weather$"2006" <- data.frame(date, mean_tmax_tmin, mean_tobs)
# mean_tmax_tmin <- 0
# mean_tobs <- 0
# data <- data.frame(date, mean_tmax_tmin, mean_tobs)
# data <- rbind(data, c("2016-02-1", 1, 2))
counter <- 0
same_year <- start
for (i in 1:nrow(pa_data)) {
#for (i in 1:32) {
  y_char <- format(pa_data[i,"DATE"], "%Y")
  y <- as.integer(y_char)
  m <- as.integer(format(pa_data[i,"DATE"], "%m"))
  d <- as.integer(format(pa_data[i,"DATE"], "%d"))
  if (same_year == y) {
    if (same_month == m) {
      #print("same_month == m")
      tmax_vector <- pa_data[i, "TMAX"] + tmax_vector
      tmin_vector <- pa_data[i, "TMIN"] + tmin_vector
      tobs_vector <- pa_data[i, "TOBS"] + tobs_vector
      counter <- counter + 1
    }
    else {
      weather <- compute_mean_values(weather, pa_data, i-1, same_year, tmax_vector, tmin_vector, tobs_vector, counter)
      tmax_vector <- pa_data[i, "TMAX"]
      tmin_vector <- pa_data[i, "TMIN"]
      tobs_vector <- pa_data[i, "TOBS"]
      counter <- 1
      same_month <- m
    }
  }
  else {
    # ### ----- repeat the same as above
    weather <- compute_mean_values(weather, pa_data, i-1, same_year, tmax_vector, tmin_vector, tobs_vector, counter)
    tmax_vector <- pa_data[i, "TMAX"]
    tmin_vector <- pa_data[i, "TMIN"]
    tobs_vector <- pa_data[i, "TOBS"]
    counter <- 1
    same_month <- m
    same_year <- y;
  }
}

# Output the last set of data
weather <- compute_mean_values(weather, pa_data, i, same_year, tmax_vector, tmin_vector, tobs_vector, counter)

# counter
# str(weather)

# weather$"2016"
# weather$"2017"

# NOW Combining home energy bill with the data
# from NCDC data
# Use the date coming from weather report
# use only TOBS for now

# TODO:
# energy_weather[[]]
# billing_df_list[[as.character(year)]] <- create_df_pdf(filenames)
# str(weather[[1]])

energy_weather2016 <-
  data.frame(
    weather$"2016"$date,
    billing_df_list$`2016`$"ELECTRIC KWHS PER DAY",
    billing_df_list$`2016`$"GAS THERMS PER DAY",
    weather$`2016`$mean_tobs
  )
colnames(energy_weather2016) <-
  c("DATE", "ELECTRIC KWHS PER DAY", "GAS THERMS PER DAY", "MEAN TOBS")
energy_weather2016

energy_weather2017 <-
  data.frame(
    weather$"2017"$date,
    billing_df_list$`2017`$"ELECTRIC KWHS PER DAY",
    billing_df_list$`2017`$"GAS THERMS PER DAY",
    weather$`2017`$mean_tobs
  )
colnames(energy_weather2017) <-
  c("DATE", "ELECTRIC KWHS PER DAY", "GAS THERMS PER DAY", "MEAN TOBS")
energy_weather2017

# Combined the 2 years info' into 1
# Build Weather Report for 2016 and 2017
energy_weather <- rbind(energy_weather2016, energy_weather2017)

# use lm
lm_energy <- lm (energy_weather$`GAS THERMS PER DAY` ~ energy_weather$`MEAN TOBS`)
summary(lm_energy)

# Plot

# "Sigh"!  Can't get to pause in between plotting each graph
# Tried using a function.  It worked for 2 plots but not > 2 plots!!!

# pause_for_gas <- function() {
  temp <- readline(prompt = "Ready for next plot? Hit Return > ")
  
  # plot here
# }
# pause_for_gas()

  plot_energy_gas <-
    ggplot(data = energy_weather,
           mapping = aes(x = `MEAN TOBS`, y = `GAS THERMS PER DAY`)) +
    # geom_line(size = 1, color = "indianred4") +
    geom_point() +
    ggtitle("Temp vs Gas Usage") +
    labs(x = "Temp", y = "Gas THERMS") +
    theme(
      axis.title.x = element_text(color = "firebrick3", size = 14, face = "bold"),
      axis.title.y = element_text(color = "firebrick3", size = 14, face = "bold")
    ) +
    geom_smooth(method = 'lm')
  plot_energy_gas

# Wait for user input

  plot_energy_electricity <-
    ggplot(data = energy_weather,
           mapping = aes(x = `MEAN TOBS`, y = `ELECTRIC KWHS PER DAY`)) +
    geom_point() +
    #geom_line(size = 1, color = "goldenrod3") +
    ggtitle("Temp vs Electricity Usage") +
    labs(x = "Temp", y = "Electricity KWHS") +
    theme(
      axis.title.x = element_text(color = "gold2", size = 14, face = "bold"),
      axis.title.y = element_text(color = "gold2", size = 14, face = "bold")
    ) +
    geom_smooth(method = 'lm')
  plot_energy_electricity
  
# PAUSE
  
  # Temperature over time
  plot_temperature_time <-
    ggplot(data = energy_weather,
           mapping = aes(x = DATE, y = `MEAN TOBS`)) +
    geom_line(size = 1, color = "springgreen4") +
    geom_point(color = "black") +
    ggtitle("Temperature vs Time ") +
    labs(x = "Time", y = "MEAN TOBS") +
    theme(
      axis.title.x = element_text(color = "green3", size = 14, face = "bold"),
      axis.title.y = element_text(color = "green3", size = 14, face = "bold")
    )
  plot_temperature_time

