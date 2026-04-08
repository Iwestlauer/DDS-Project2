# plot tip_amount vs fare_amount
library(ggplot2)
sliced_df %>%

  ggplot(aes(x = fare_amount,
             y = tip_amount)) + 
  geom_smooth()

library(DataExplorer)
plot_density(sliced_df)
