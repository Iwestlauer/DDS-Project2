#skim the data from parquet files

# install.packages("arrow")
library(arrow)
library(dplyr)
library(skimr)
# data dictionary
# https://www.nyc.gov/assets/tlc/downloads/pdf/data_dictionary_trip_records_yellow.pdf

taxi <- read_parquet(file.choose(), header = TRUE)
# how many observations in the file
dim(taxi)[1]

head(taxi)
summary(taxi)
skim(taxi)

library(DataExplorer)
plot_intro(taxi)
plot_missing(taxi)
