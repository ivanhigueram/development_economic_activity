# Loading sample data:
dta_file <- "~/Dropbox/ConleySEs/Data/new_testspatial.dta"
DTA <-data.table(read.dta(dta_file))
setnames(DTA, c("latitude", "longitude"), c("lat", "lon"))

# Loading R function to compute Conley SEs:
source("~/Dropbox/ConleySEs/ConleySEs_17June2015.R")


# We use the felm() from the lfe package to estimate model with year and county fixed effects.
# Two important points:
# (1) We specify our latitude and longitude coordinates as the cluster variables, so that they are included in the output (m).
# (2) We specify keepCx = TRUE, so that the centered data is included in the output (m).


merge_rasters_dataframes_long <- mutate(merge_rasters_dataframes_long, dist_p_km = dist_p / 1000)
setnames(merge_rasters_dataframes_long, c("y", "x"), c("lat", "lon"))

m <-felm(log(0.01 + dm) ~ t dist_p_km | year + ID | 0 | lat + lon,
         data = merge_rasters_dataframes_long, keepCX = TRUE)

residuals <- m$residuals

moran.test

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
          data = merge_rasters_dataframes_long, keepCX = T, exactDOF = T)


RD <- RDestimate(log(0.01 + dm1997) ~ dist_p_km | altura_mean_30arc + roughness + slope + dist_colonial + dist_capital + dist_coast + colds00ag,
                 data = merge_rasters_dataframes,
                 cutpoint = 0,
                 cluster = c(merge_rasters_dataframes$municode),
                 model = T,
                 frame = T)

plot(RD)






