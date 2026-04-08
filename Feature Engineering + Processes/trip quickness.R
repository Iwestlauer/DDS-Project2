# plot trip quickness
# trip_quickness = trip_distance/(dropoff time - pickup time)

library(lubridate)

sliced_df <- sliced_df %>%
  dplyr::mutate(
    pickup_time  = ymd_hms(tpep_pickup_datetime),
    dropoff_time = ymd_hms(tpep_dropoff_datetime),
    # Calculate difference in minutes
    trip_minutes = interval(pickup_time, dropoff_time) / minutes(1),
    # trip quickness in miles per hour
    trip_quickness = 60* trip_distance / trip_minutes
  )

hist(sliced_df$trip_quickness)


