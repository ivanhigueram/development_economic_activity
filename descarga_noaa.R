library(raster)
library(landsat)
library(rgdal)
library(XML)
library(sp)
library(stringr)
library(RCurl)
library(plyr)
library(R.utils)

setwd("~/Procesamiento_OLS")
#Descargar datos de luces#

#1. Get URL's
url <- "http://www.ngdc.noaa.gov/eog/data/web_data/v4composites/"
links <- getHTMLLinks(url)
filenames <- links[str_detect(links, ".v4.tar")]
filenames_list <- as.list(filenames)

#2. Create a download function (we use the option write binary "wb" in mode, this option is valid if you're working on Windows)
downloadTIFF <- function(filename, baseurl, folder) {
  dir.create(folder, showWarnings = F)
  fileurl <- str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    download.file(fileurl, 
                  destfile = str_c(folder,"/", filename), mode="wb")
    Sys.sleep(1)
  }
}


#3. Apply to list of filenames

l_ply(filenames_list, downloadTIFF,
      baseurl = "http://www.ngdc.noaa.gov/eog/data/web_data/v4composites/",
      folder = "NOAA2"
)



#4. Open .tar files into the same folder (only extract the stable lights .tif and the .tfw file)
untar_tiff <- function(filename, folder) { 
  dir.create(folder, showWarnings = F)
  list <- untar(filename, list = T)
  untar(filename, files = str_c(list[str_detect(list, "web.stable")]), exdir = folder)
  
  lapply(filenames_list, untar_tiff,
         folder = "TIFF")
  
}

#5.Decompress the .tif files (as you may note the .tif remain compressed)
list_tiff <- list.files("TIFF")
list_tiff <- list_tiff[str_detect(list_tiff, ".tif.gz")]
lapply(str_c("TIFF", "/", list_tiff), gunzip)


#6. Open .tif files as a raster (the raster package allow to read these files in the disk and not in the memory, this improves the efficiency of functions in R)
r <- readGDAL("TIFF/F182012.v4c_web.stable_lights.avg_vis.tif")
colombia_regiones1 <- getData("GADM", country="CO", level=1)
colombia_regiones2 <- getData("GADM", country="CO", level=2)

plot(colombia_regiones2, axes=T)
colombia_ext <- extent(-79, -67, -4, 12.5)
r_colombia <- crop(r, colombia_ext)
plot(r_colombia, add=T)

plot(r_colombia, axes=T, col=terrain.colors(100))
plot(colombia_regiones1, add=T)



#4. Open .tar files into the same folder (only extract the stable lights .tif and the .tfw file)
untar_tiff <- function(filename, folder) { 
  dir.create(folder, showWarnings = F)
  list <- untar(filename, list = T)
  untar(filename, files = str_c(list[str_detect(list, "web.stable")]), exdir = folder)
} 



