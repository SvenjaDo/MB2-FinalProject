##################### BSI for MODIS ###########################################

#### BSI Bare Soil Index
#
bsi <- function(raster){
  bsi <- ((raster$b1_red+raster$b5_SWIR1) - (raster$b2_NIR+raster$b3_blue)) / 
    ((raster$b1_red+raster$b5_SWIR1) + (raster$b2_NIR+raster$b3_blue))
}



