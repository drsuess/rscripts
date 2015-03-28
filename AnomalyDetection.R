#Load required libraries
library(devtools)
##### can install anomaly detector from github
# devtools::install_github("twitter/AnomalyDetection")
#####
library(AnomalyDetection)
#default raw data
data(raw_data)
res= AnomalyDetectionTs(raw_data, max_anoms=0.2, direction="both", plot=TRUE)
res$plot

#Change the ticker symbol for different stocks
y <- "TSLA"
x <- as.character(y)

#Plot for stock ticker symbol above
df = data.frame(timestamp = as.POSIXct(as.Date(index(x), "%Y-%m-%d")), count = x$x.Volume)
res = AnomalyDetectionTs(df, max_anoms=0.2, alpha=0.01, direction='both', plot=TRUE)
res$plot

