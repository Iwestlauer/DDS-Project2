# combine all the taxi data from several months

library(arrow)
library(dplyr)

dataset <- open_dataset("data")

# Only pulls data when needed
combined_df <- dataset %>% 
  collect()

# get the number of observations
dim(combined_df)[1] #24M+ for July-Dec

# select n random observations
sliced_df<- combined_df %>% slice_sample(n = 10)

skimr::skim(sliced_df)
