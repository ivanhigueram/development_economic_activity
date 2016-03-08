library(RDDtools)
library(rdd)
library(rdrobust)
library(plm)
library(stargazer)
library(xtable)

#Data frames by distance
merge_rasters_bw <- list()
for(i in c(100, 200, 300, 400, 500, 1000, 2500, 2000)){
  merge_rasters_bw[[str_c(i)]] <- filter(merge_rasters_dataframes, dist_p < i, dist_p > -i) 
}

#RDD Tools
discontinuity_data <- RDDdata(x = dist_p,
                              y = dm1997,
                              data = merge_rasters_dataframes,
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data, order = 3)
reg_nonpara <- RDDreg_np(discontinuity_data)

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)
plotSensi(reg_nonpara, from = 1000, to = 10000, by = 500)
plotPlacebo(reg_nonpara)

#rdrobust pacakge

#Variables <- cut-off (dist_p) and outcome (all the dm)
dist <- merge_rasters_dataframes$dist_p
light <- cbind(merge_rasters_dataframes[1:22])

#Estimation for all years
rd_nonpara <- list()
for(i in c(1:22)){
  rd_nonpara[[str_c(i)]] <- rdrobust(x = dist, y = light[, i])
}

rd_nonpara_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_nonpara)){
  rd_nonpara_table[[i]] <- rd_nonpara[[i]]$tabl3.str[1, ]
}

rd_nonpara_table <- ldply(rd_nonpara_table) #Reshape and create a table of LATE's
rd_nonpara_table$year <- c(1992:2013)

for(i in 1:length(rd_nonpara_table)){
  rd_nonpara_table[, i] <- as.numeric(rd_nonpara_table[, i])
}

#Graph LATE for all years with IC's 
g1 <- ggplot(rd_nonpara_table, aes(year)) +
  geom_line(aes(y = Coef), colour = "blue") +
  geom_ribbon(aes(ymin = rd_nonpara_table$`CI Lower`, ymax = rd_nonpara_table$`CI Upper`), alpha = 0.2)
g1 <- g1 + scale_x_continuous(breaks=c(1992:2013))
g1 <- g1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10),
                 axis.text.y = element_text(size = 10))
g1 <- g1 + labs(x="Año", y=expression(paste("Diferencia (LATE)")), title="Discontinuidad por año \n(1992- 2013)") 
g1 <- g1 + theme(plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 10, 10, 10)))

#rdlocrand package download (from Cattaneo's web-page)
url <- "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, ".R")]
filenames_list <- as.list(filenames)

l_ply(filenames_list, download,
      baseurl = "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/",
      folder = "rdlocrand"
)

setwd("rdlocrand")
source("rdwinselect.R")
source("rdrandinf.R")
source("rdsensitivity.R")
source("rdrbounds.R")






#Models 
#(IK and CT non-parametric estimators - Sharp RD)
rd_allyearsSRD <- lapply(merge_rasters_dataframes[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_dataframes$dist_f, c = 0, all = T))


#(IK and CT non-parametric estimators - Fuzzy RD)
rd_allyearsFRD <- lapply(merge_rasters_1km[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_1km$dist_f, fuzzy = merge_rasters_1km$slope,c = 0, all = T))


#RD graph approach


attach(merge_rasters_dataframes) 
rdplot_2000 <- rdplot(y = dm2013, x = dist_p, c = 0, p = 3, binselect = "esmv",
                 x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 2), 
                 x.lim = c(-2000, 2000), lowerend = -50000,
                 title = "Discontinuidad en la actividad económica - 1997")
detach(merge_rasters_dataframes)




