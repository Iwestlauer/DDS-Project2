install.packages("arrow")
library(arrow)
library(dplyr)

taxi <- read_parquet(file.choose(), header = TRUE)
head(taxi)
