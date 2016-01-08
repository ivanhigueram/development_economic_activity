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
}  


lapply(filenames_list, untar_tiff, folder = "TIFF")
  

#5.Decompress the .tif files (as you may note the .tif remain compressed)
list_tiff <- list.files("TIFF")
list_tiff <- list_tiff[str_detect(list_tiff, ".tif.gz")]
lapply(str_c("TIFF", "/", list_tiff), gunzip)


#6. Open .tar files into the same folder (only extract the stable lights .tif and the .tfw file)
untar_tiff <- function(filename, folder) { 
  dir.create(folder, showWarnings = F)
  list <- untar(filename, list = T)
  untar(filename, files = str_c(list[str_detect(list, "web.stable")]), exdir = folder)
} 

#7. Delete .tfw files
list_tfw <- list.files("TIFF")
list_tfw <- list_tfw[str_detect(list_tfw, ".tfw")]
lapply(list_tfw, file.remove)




