##################### Assign Classnames for classified raster ##################


assignClassnames <- function(raster){
raster$class <- matrix(getValues(raster))
raster$class[raster$class == 1] <- "Forest"
raster$class[raster$class == 2] <- "Bare Soil"
raster$class[raster$class == 3] <- "Mine"
raster$class[raster$class == 4] <- "Grassland"
return(raster)}


assignClassnames_CD <- function(raster){
  raster$class <- matrix(getValues(raster))
  raster$class[raster$class == 11] <- "Forest-Forest"
  raster$class[raster$class == 12] <- "Forest-Bare Soil"
  raster$class[raster$class == 13] <- "Forest-Mine"
  raster$class[raster$class == 14] <- "Forest-Grassland"
  raster$class[raster$class == 21] <- "Bare Soil-Forest"
  raster$class[raster$class == 22] <- "Bare Soil-Bare Soil"
  raster$class[raster$class == 23] <- "Bare Soil-Mine"
  raster$class[raster$class == 24] <- "Bare Soil-Grassland"
  raster$class[raster$class == 31] <- "Mine-Forest"
  raster$class[raster$class == 32] <- "Mine-Bare Soil"
  raster$class[raster$class == 33] <- "Mine-Mine"
  raster$class[raster$class == 34] <- "Mine-Grassland"
  raster$class[raster$class == 41] <- "Grassland-Forest"
  raster$class[raster$class == 42] <- "Grassland-Bare Soil"
  raster$class[raster$class == 43] <- "Grassland-Mine"
  raster$class[raster$class == 44] <- "Grassland-Grassland"
  return(raster)}

raster <- unsupclass02_11
raster$class <- assignClassnames_CD(raster)
