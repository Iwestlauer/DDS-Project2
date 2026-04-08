# get borough from location id

library(dplyr)

#read in taxi zone lookup
zone_lookup <- read.csv("NYC_Taxi_Zones.csv") # "Location ID",	"Borough"
# zone_lookup <- zone_lookup %>%
#   rename(borough = Borough)  # adjust if needed
# zone_lookup <- zone_lookup %>%
#   rename(LocationID = Location.ID)  # adjust if needed

# Join borough into your dataset
sliced_df <- sliced_df %>%
  left_join(zone_lookup, by = c( "PULocationID" ="Location.ID"))
head(sliced_df$Borough.x)
head(sliced_df$Borough.y)
