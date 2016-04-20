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
                              y = log(1 + dm2010),
                              data = merge_rasters_dataframes,
                              covar = merge_rasters_dataframes[, 1:10],
                              cutpoint = 0) 

reg_para <- RDDreg_lm(discontinuity_data, order = 1, covariates = merge_rasters_dataframes[, 1:10])
reg_nonpara <- RDDreg_np(discontinuity_data, covariates = merge_rasters_dataframes[, 1:10])

reg_para <- RDDreg_lm(discontinuity_data, order = 1)
bw_ik <- RDDbw_IK(discontinuity_data)
reg_nonpara <- RDDreg_np(RDDobject = discontinuity_data, bw = bw_ik)
plotSensi(reg_nonpara, from = -1000, to = 1000, by = 200)
plotPlacebo(reg_nonpara)

#rdrobust pacakge

#Variables <- cut-off (dist_p) and outcome (all the dm)
dist <- merge_rasters_dataframes$dist_p
light <- cbind(merge_rasters_dataframes[1:22])

#Estimation for all years
rd_nonpara <- list()
for(i in c(1:22)){
  for(j in )
  rd_nonpara[[str_c(i)]] <- rdrobust(x = dist, y = light[, i])
}

rdrobust(y = merge_rasters_dataframes$dm1997,
         x = merge_rasters_dataframes$dist_p,
         )


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
theme_set(theme_gray(base_size = 13))
g1 <- ggplot(rd_nonpara_table, aes(year)) +
  geom_line(aes(y = Coef), colour = "blue") +
  geom_ribbon(aes(ymin = rd_nonpara_table$`CI Lower`, ymax = rd_nonpara_table$`CI Upper`), alpha = 0.2)
g1 <- g1 + scale_x_continuous(breaks=c(1992:2013))
g1 <- g1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10),
                 axis.text.y = element_text(size = 10))
g1 <- g1 + labs(x="Año", y=expression(paste("Diferencia (LATE)")), title="Discontinuidad por año - Estimación no paramétrica \n(1992- 2013)") 
g1 <- g1 + theme(plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 10, 10, 10)))
g1 <- g1 +  geom_vline(xintercept=1997, linetype = 2) 


png("LATE_local.png", width = 13, height = 9, units = 'in', res = 800)
g1
dev.off()

#rdlocrand package download (from Cattaneo's web-page)
url <- "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, ".R")]
filenames_list <- as.list(filenames)

l_ply(filenames_list, download,
      baseurl = "http://www-personal.umich.edu/~cattaneo/software/rdlocrand/R/",
      folder = "rdlocrand"
)

#Loading source commands of the package
setwd("rdlocrand")
source("rdwinselect.R")
source("rdrandinf.R")
source("rdsensitivity.R")
source("rdrbounds.R")

#Define a window of treatment
covariates <- cbind(merge_rasters_dataframes$slope, merge_rasters_dataframes$roughness, 
                    merge_rasters_dataframes$hill, merge_rasters_dataframes$colds00ag,
                    merge_rasters_dataframes$sq1)

window <- rdwinselect(dist, covariates)
window <- rdrandinf(light[, 2], dist ,statistic = "all", 
                 covariates = covariates, wmin = 10, wstep = 1, 
                 rdwreps=10000, p = 2)

rd_nonpara_locrand <- list()
for(i in c(1:22)){
  rd_nonpara_locrand[[str_c(i)]] <- rdrobust(x = dist, y = light[, i], h = 23, matches = 8) 
}

rd_nonpara_locrand_table <- list() #Table of LATE and p-values
for(i in 1:length(rd_nonpara_locrand)){
  rd_nonpara_locrand_table[[i]] <- rd_nonpara_locrand[[i]]$tabl3.str[1, ]
}

rd_nonpara_locrand_table <- ldply(rd_nonpara_locrand_table) #Reshape and create a table of LATE's
rownames(rd_nonpara_locrand_table) <- c(1992:2013)
rd_nonpara_locrand_table$year <- c(1992:2013)

for(i in 1:length(rd_nonpara_locrand_table)){
  rd_nonpara_locrand_table[, i] <- as.numeric(rd_nonpara_locrand_table[, i])
}

stargazer(rd_nonpara_locrand_table[,c(1:6)], summary = F,  
          title = "Estimación no paramétrica para todos los años con supuestos de aletoriedad",
          coef = rd_nonpara_locrand_table$`Coef `, se = rd_nonpara_locrand_table$`Std. Err.`,
          p = rd_nonpara_locrand_table$`P>|z|`)


#Graph LATE for all years with IC's for random selected windows
theme_set(theme_gray(base_size = 13))
g2 <- ggplot(rd_nonpara_locrand_table, aes(year)) +
  geom_line(aes(y = Coef), colour = "blue") +
  geom_ribbon(aes(ymin = rd_nonpara_locrand_table$`CI Lower` , ymax = rd_nonpara_locrand_table$`CI Upper`), alpha = 0.2)
g2 <- g2 + scale_x_continuous(breaks=c(1992:2013))
g2 <- g2 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size = 10, face = "bold"),
                 axis.text.y = element_text(size = 10, face = "bold"))
g2 <- g2 + labs(x="Año", y=expression(paste("Diferencia (LATE)")), title="Discontinuidad por año - Estimación no paramétrica\n(1992- 2013)") 
g2 <- g2 + theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 10, 10, 10)),
                 axis.title = element_text(face="bold"))
g2 <- g2 + geom_vline(xintercept=1997, linetype = 2) 

png("LATE_locrand.png", width = 13, height = 7, units = 'in', res = 800)
g2
dev.off()


g3 <- g2 + annotate("text", x = 1998, y = 0.1, label = "-0.5426") #Add label for 1998 LATE
png("LATE_locrand_98.png", width = 13, height = 7, units = 'in', res = 800)
g3
dev.off()


#rdplot
rdplot

#Models 
#(IK and CT non-parametric estimators - Sharp RD)
rd_allyearsSRD <- lapply(merge_rasters_dataframes[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_dataframes$dist_f, c = 0, all = T))


#(IK and CT non-parametric estimators - Fuzzy RD)
rd_allyearsFRD <- lapply(merge_rasters_1km[, 1:20], function(x) rdrobust(y = x, x = merge_rasters_1km$dist_f, fuzzy = merge_rasters_1km$slope,c = 0, all = T))


#RD graph approach


attach(merge_rasters_dataframes) 
png("rd_1997.png", width = 13, height = 9, units = 'in', res = 800)
rdplot_1997 <- rdplot(y = dm1997, x = dist_p, c = 0, p = 2, binselect = "esmv",
                 x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 2), 
                 lowerend = -6000, upperend = 6000, col.lines = c(size = 10, color = "blue"),
                 title = "Discontinuidad en la actividad económica - 1997")
dev.off()
detach(merge_rasters_dataframes)


attach(merge_rasters_dataframes) 
png("rd_2013.png", width = 13, height = 9, units = 'in', res = 800)
rdplot_2013 <- rdplot(y = dm2013, x = dist_p, c = 0, p = 2, binselect = "esmv",
                      x.label = "Distancia", y.label = "Actividad económica", y.lim = c(0, 2), 
                      lowerend = -6000, upperend = 6000, col.lines = c(size = 10), 
                      title = "Discontinuidad en la actividad económica - 2013")
dev.off()
detach(merge_rasters_dataframes)



