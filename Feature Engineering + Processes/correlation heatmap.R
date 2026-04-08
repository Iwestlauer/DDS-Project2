# correlation heatmap to see what correlates with tip

# filter out the categorical and keep only numerical
# ignore RatecodeID, VendorID, filter to only payment_type =1
# convert tpep datetimes to a number for day of week and a num for hour

library(DataExplorer)
plot_correlation(final_data, plotly = TRUE)