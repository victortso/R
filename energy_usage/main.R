#
# Some interesting data from 
# http://data.cityofpaloalto.org/home
# http://data.cityofpaloalto.org/dataviews/244906/electricity-consumption-information/
# http://data.cityofpaloalto.org/dashboards/7572/utilities-electric/
# http://xmap.cityofpaloalto.org/OpenGisData/

# 3/16/2018 - tso Make sure you have the following libraries installed for this project
# tidyverse, pdftools, lubridate, dplyr, ggplot2, tm
# 3/10/2018
# MAIN FILE
#
# for PDF
# Use the tm package
install.packages("tm")
library(tidyverse)
library(pdftools)
# for date function
library(lubridate)
# need for %>%
library(dplyr)
# ggplot
library(ggplot2)

# Set your working directory!
# Set working directory
# setwd("/Users/tso/proj/fuhsd/R/energy_usage_temperature")

source("ncdc_data.R")

# output: billing_df_list - list of data frame contains all the years usage
source("create_billing_df_list.R")
billing_df_list <- create_billing_df_list(start_year = 2016, end_year = 2017) 

# Create all the usage graphs
# plot_e - electricity usage
# plot_g - gas usage
# plot_w - water usage
# plot_amount - total amount in $ spent over the year
source("create_usage_graphs.R")

# just plot one year data
billing_df <- billing_df_list$"2016"
plots <- create_usage_graphs(billing_df)

# Function to draw the graph
source("multiplot.R")

# This plots 4 graphs
# 1. Monthly Electric Usage
# 2. Monthly Gas Usage
# 3. Monthly Water Usage
# 4. Billing in $ over time
#multiplot(plot_e, plot_g, plot_w, plot_amount, cols=1)
multiplot(plots$plot_e, plots$plot_g, plots$plot_w, plots$plot_amount, cols=1)

