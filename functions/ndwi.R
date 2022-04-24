##################### ndwi for MODIS ###########################################

#### NDWI Normalized Difference Water Index
#
ndwi <- function(raster){
  ndwi <- (raster$b2_NIR) - raster$b5_SWIR1 / (raster$b2_NIR + raster$b5_SWIR1)
  return(ndwi)
}

