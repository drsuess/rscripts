install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
data(raw_data)
res= AnomalyDetectionTs(raw_data, max_anoms=0.2, direction="both", plot=TRUE)
res$plot
df = data.frame(timestamp = as.POSIXct(as.Date(index(TSLA), "%Y-%m-%d")), count = TSLA$TSLA.Volume)
> res = AnomalyDetectionTs(df, max_anoms=0.2, alpha=0.01, direction='both', plot=TRUE)
> res$plot