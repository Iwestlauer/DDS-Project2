# split tpep_pickup_datetime into separate parts
library(lubridate)
pickup_hours <- hour(sliced_df$tpep_pickup_datetime)

hist(pickup_hours)

# split tpep_pickup_datetime into day of the week
pickup_dates <- ymd_hms(sliced_df$tpep_pickup_datetime)
weekday_nums <- wday(pickup_dates)
# 1 = Sunday by default
weekday_names <- wday(pickup_dates, label = TRUE)

hist(weekday_nums)
table(weekday_names)

