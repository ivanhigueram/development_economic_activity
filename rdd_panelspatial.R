library(plyr)
library(dplyr)
library(data.table)
library(lfe)
library(plm)
library(multiwayvcov)
# Loading R function to compute Conley SEs:
source("~/Dropbox/ConleySEs/ConleySEs_17June2015.R")

#Changes in the varialbes: distances in kilometers and lat (y) and lon (x).

merge_rasters_dataframes_long <- mutate(merge_rasters_dataframes_long, dist_p_km = dist_p / 1000)
setnames(merge_rasters_dataframes_long, c("y", "x"), c("lat", "lon"))


m1 <-felm(log(0.01 + dm) ~  as.factor(t) | ID + year + dptocode + municode + community_id | 0 | ID + year,
         data = merge_rasters_dataframes_long, exactDOF = T, subset = dist_p_km <= 5) 
m2 <-felm(log(0.01 + dm) ~ poly(dist_p_km, 2) + as.factor(t) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | ID + year ,
          data = merge_rasters_dataframes_long, exactDOF = T, subset = dist_p_km <= 2.5) 
m3 <-felm(log(0.01 + dm) ~ poly(dist_p_km, 3) + as.factor(t) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | ID + year  ,
          data = merge_rasters_dataframes_long, exactDOF = T, subset = dist_p_km <= 2.5)
m4 <-felm(log(0.01 + dm) ~ poly(dist_p_km, 4) + as.factor(t) + altura_mean_30arc + roughness + slope + colds00ag + dist_coast + dist_capital + dist_colonial
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 |  ID + year  ,
          data = merge_rasters_dataframes_long,  exactDOF = T, subset = dist_p_km <= 2.5)


stargazer(m1, m2, m3, 
          title = "Estimación paramétrica panel (1997 - 2013)",
          dep.var.caption = "Densidad de luz en todo el litoral", dep.var.labels = "Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado"), notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)

#Cities
merge_rasters_dataframes_long_cities <- subset(merge_rasters_dataframes_long, dist_capital < quantile(dist_capital, c(.09)))
merge_rasters_dataframes_long_colonial <- subset(merge_rasters_dataframes_long, dist_colonial < quantile(dist_colonial, c(.09)))


m1_cities <-felm(log(0.01 + dm) ~ poly(dist_p_km, 1) + factor(t) + altura_mean_30arc + roughness + slope + colds00ag 
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | municode + year,
          data = merge_rasters_dataframes_long_cities, subset = dist_p_km <= 2.5, exactDOF = TRUE)
m2_cities <-felm(log(0.01 + dm) ~ poly(dist_p_km, 2) + factor(t) + altura_mean_30arc + roughness + slope + colds00ag 
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | municode + year,
          data = merge_rasters_dataframes_long_cities, subset = dist_p_km <= 2.5, exactDOF = TRUE)
m3_cities <-felm(log(0.01 + dm) ~ poly(dist_p_km, 3) + factor(t) + altura_mean_30arc + roughness + slope + colds00ag 
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | municode + year,
          data = merge_rasters_dataframes_long_cities, subset = dist_p_km <= 2.5, exactDOF = TRUE)
m4_cities <-felm(log(0.01 + dm) ~ poly(dist_p_km, 4) + factor(t) + altura_mean_30arc + roughness + slope + colds00ag 
          + factor(sq1) + factor(sq2) + factor(sq3) + factor(sq4) + factor(sq5) + factor(sq6) + factor(sq7) | year + municode + dptocode | 0 | municode + year,
          data = merge_rasters_dataframes_long_cities, subset = dist_p_km <= 2.5, exactDOF = TRUE)


stargazer(m1_cities, m2_cities, m3_cities, m4_cities, 
          title = "Estimación paramétrica panel (1997 - 2013): Percentil 10 más cercano a la capital de departamento",
          dep.var.caption = "Densidad de luz en el $h$ óptimo", dep.var.labels = "Tratamiento",
          column.labels = c("Primer grado", "Segundo grado", "Tercer grado"), notes.label = "Nota: ", notes.align = "c",
          initial.zero = F
)


#rdd package panel

rd_nonpara_ik_panel <- RDestimate(log(0.01 + dm) ~ dist_p_km | altura_mean_30arc + roughness + slope + colds00ag + municode + dptocode
                                  + sq1 + sq2 + sq3 + sq4 + sq5 + sq6 + sq7,
             data = merge_rasters_dataframes_long_cities,
             cutpoint = 0,
             model = TRUE)
plot(rd_nonpara_ik_panel)



ptm <-proc.time()
SE <-ConleySEs(reg = m,
               unit = "ID",
               time = "year",
               lat = "lat", lon = "lon",
               dist_fn = "SH", dist_cutoff = 500, 
               lag_cutoff = 5,
               cores = 2, 
               verbose = T) 

sapply(SE, sqrt) %>%round(3) # Same as the STATA results.
coeftest(m, SE)

#With my data


# Table 3 - Panel for the bw suggested by RD
merge_rasters_dataframes_long$dist_p_km <- merge_rasters_dataframes_long$dist_p / 1000
merge_rasters_dataframes_long_bw <- filter(merge_rasters_dataframes_long, dist_p_km < rd2013_controles_fx$bws[1, 1], dist_p_km > -rd2013_controles_fx$bws[1, 1])
merge_rasters_dataframes_long_capital <- filter(merge_rasters_dataframes_long_bw, dist_capital > 48827.96)


reg <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 | 0, data = merge_rasters_dataframes_long_bw)
reg_clust <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 |  ID + year, data = merge_rasters_dataframes_long_bw)
reg_clust_capitals <- felm(log(0.01 + dm) ~ as.factor(t) | ID + year | 0 | ID + year, data = merge_rasters_dataframes_long_capital)


stargazer(reg_clust, reg_clust_capitals)

