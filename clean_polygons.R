library(devtools)
install_github("eblondel/cleangeo") 
library(cleangeo)

#get a report of geometry validity & issues for a sp spatial object
report <- clgeo_CollectionReport(black_territories_join_holefree)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]

#get suspicious features (indexes)
nv <- clgeo_SuspiciousFeatures(report)

#try to clean data
black_territories_clean <- clgeo_Clean(layers_reprojected[[3]], print.log = TRUE)

#check if they are still errors
report.clean <- clgeo_CollectionReport(black_territories_clean)
summary.clean <- clgeo_SummaryReport(report.clean)
