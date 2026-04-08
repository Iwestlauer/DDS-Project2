# install.packages("arrow")
library(arrow)
library(dplyr)

# data dictionary
# https://www.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf

taxi <- read_parquet(file.choose(), header = TRUE)
head(taxi)

# How to append all the data from several months?