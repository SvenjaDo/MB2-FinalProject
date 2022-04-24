##################### convert .hdf to .tiff files ##############################
hdf2tiff <- function(filenames, bandsel){
for(i in 1:length(filenames)){
  sds <- get_subdatasets(filenames[i])
  for (x in 1:length(bandsel)){
    if(!file.exists(paste(filenames[i],"_",bandsel[x],".tif",sep=""))){
      gdal_translate(sds[x],dst_dataset = paste(filenames[i],"_",bandsel[x],".tif",sep=""))
      print(paste(filenames[i],"_",bandsel[x],".tif", " file converted",sep=""))}
    else 
      print(paste(filenames[i],"_",bandsel[x],".tif", " file already exists",sep=""))}}
  }