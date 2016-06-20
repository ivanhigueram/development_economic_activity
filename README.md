#Persistencias históricas y discontinuidades espaciales: territorios comunitarios en el Pacífico colombiano

Este repositorio contiene los códigos para obtener la información de luces satelitares nocturas para colombia así como los códigos para replicar los resultados mostrados en el artículo. 

1. `descarga_noaa.R` tiene los códigos para descargar lor archivos .tif de la página de NOAA. Estos archivos ocupan alrededor de 30 Gb y vienen comprimidos. El script incluye los códigos para descomprimirlos. 
2. `rasters.R` usa el paquete `raster` de R para leer los archivos .fit de luces, altura y demás datos satelitales. En este script también se incluyen códigos de descarga para los datos de altura, población y calidad del suelo. 
3. `shapefiles.R` contiene los codigos de procesamiento de los arhivos .shp (ESRI Shapefile) para las comunidades y los municipios colombianos, los cuales puede descargar [aqui](http://sigotn.igac.gov.co/sigotn/).
4. `replication_codes.R` contiene los codigos para replicar los resultados por tabla, así como las imágenes y mapas mostrados en el documento. 

To do: Mejorar la velocidad del comando `raster::mask()` usando el paquete `snow` y usar las API de NASA/USGS/NOAA para exportar los archivos .tif de forma más eficiente. 
