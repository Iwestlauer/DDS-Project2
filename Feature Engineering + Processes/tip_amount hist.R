# plot tip_amount distribution

library(ggplot2)
library(arrow)
library(dplyr)

dataset <- open_dataset("data")

# Only pulls data when needed
combined_df <- dataset %>% 
  collect()

# get the number of observations
dim(combined_df)[1] #24M+ for July-Dec

# select n random observations
sliced_df<- combined_df %>% slice_sample(n = 1000)

hist(sliced_df$tip_amount)
