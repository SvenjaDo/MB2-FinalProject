##################### load and stack .tiff files ###############################


loadandstack <- function(filenames,years,crs,crop){
for(i in 1:length(years)){
  current_filenames <- grep(years[i], filenames, value=T) # list filenames for current year
  current_raster <- stack(current_filenames) # stack raster files
  crs(current_raster) <- CRS(crs) # set coordinate system
  raster_crop <- crop(current_raster,crop) # crop raster to aoi
  names(raster_crop) <- bandsel # clean up the band names for neater plotting
  assign(paste("raster",years[i],sep="_"),raster_crop) # save current raster
  rm(current_filenames,current_raster,raster_crop)}} # remove layers 

  

