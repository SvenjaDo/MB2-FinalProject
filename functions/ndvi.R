##################### NDVI for MODIS ###########################################

#### NDVI Normalized Difference Vegetation Index
ndvi <- function(raster){
  ndvi <- ((raster$b2_NIR - raster$b1_red) / (raster$b2_NIR + raster$b1_red))
}


