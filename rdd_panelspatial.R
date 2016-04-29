library(dplyr)
library(data.table)
library(lfe)
# Loading R function to compute Conley SEs:
source("~/Dropbox/ConleySEs/ConleySEs_17June2015.R")

#Changes in the varialbes: distances in kilometers and lat (y) and lon (x).

merge_rasters_dataframes_long <- mutate(merge_rasters_dataframes_long, dist_p_km = dist_p / 1000)
setnames(merge_rasters_dataframes_long, c("y", "x"), c("lat", "lon"))

m <-felm(log(0.01 + dm) ~ I(dist_p/1000) + as.factor(t) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
         | municode + year | 0 | lat + lon + ID + municode + community_id,
         data = merge_rasters_long_bw[["5000"]])

coefficients(m) %>%round(3) # Same as the STATA result.


ptm <-proc.time()
SE <-ConleySEs(reg = m,
               unit = "ID",
               time = "year",
               lat = "lat", lon = "lon",
               dist_fn = "SH", dist_cutoff = 500, 
               lag_cutoff = 5,
               cores = 4, 
               verbose = T) 

sapply(SE, sqrt) %>%round(3) # Same as the STATA results.
coeftest(m, SE)

#With my data

reg <- felm(log(0.01 + dm) ~ factor(t) + I(dist_p/1000) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
         + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | municode + year, 
          data = merge_rasters_dataframes_long, keepCX = T)







