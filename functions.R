############################################################################################################################
############################### Functions ##################################################################################
############################################################################################################################

##################### installing and loading packages ##########################
loadandinstall <- function(mypkg) {
  for (i in 1:length(mypkg)) {
    if (!is.element(mypkg[i], installed.packages()[,1]))
    {install.packages(mypkg[i])}; 
    library(mypkg[i], character.only=TRUE)}
  }

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

################ create spatial polygon from given coordinates #################
coords2spatpoly <- function(x_coords,y_coords,crs){
  poly <- sp::Polygon(cbind(x_coords,y_coords))%>% # create Polygon from coordinates
    list()%>% sp::Polygons( ID = "A") 
  aoi <- sp::SpatialPolygons(list(poly))
  crs(aoi) <- CRS(crs)
  aoi
}

##################### plot raster as RGB image  ################################
plot_rgb <- function(raster,title){
  ggRGB(raster,
        r=1, g=4, b=3,
        stretch = "none")+
    labs(title = title ,y="Latitude",x="Longitude") +
    coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm")) +
    my_theme
}

##################### NDVI for MODIS ###########################################
ndvi <- function(raster){
  ndvi <- ((raster$b2_NIR - raster$b1_red) / (raster$b2_NIR + raster$b1_red))
}

##################### BSI for MODIS ###########################################
bsi <- function(raster){
  bsi <- ((raster$b1_red+raster$b5_SWIR1) - (raster$b2_NIR+raster$b3_blue)) / 
    ((raster$b1_red+raster$b5_SWIR1) + (raster$b2_NIR+raster$b3_blue))
}

##################### NDWI for MODIS ###########################################
ndwi <- function(raster){
  ndwi <- (raster$b2_NIR) - raster$b5_SWIR1 / (raster$b2_NIR + raster$b5_SWIR1)
  return(ndwi)
}

##################### plot Indices as RGB image  ################################
plot_indic <- function(raster,title){
  ggRGB(raster,
        r=2, g=1, b=3,
        stretch = "lin")+
    labs(title = title,
         subtitle="R = BSI, G = NDVI, B = NDWI",y="Latitude",x="Longitude") +  coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm")) + my_theme
}

##################### Assign Classnames for classified raster ##################
assignClassnames <- function(raster){
  raster$class <- matrix(getValues(raster))
  raster$class[raster$class == 1] <- "Forest"
  raster$class[raster$class == 2] <- "Bare Soil"
  raster$class[raster$class == 3] <- "Mine"
  raster$class[raster$class == 4] <- "Grassland"
  return(raster)}

##################### Assign Classnames for Postclassification #################
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


##################### plot Classification results  #############################
plot_LC <- function(raster,title){
  ggplot() + ggR(raster$class, geom_raster = T, ggLayer = T) +
    scale_fill_discrete(type="viridis", name = "Land Cover") +
    labs(title = title, x="Longitude", y= "Latitude")+
    coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm")) +  my_theme
}

##################### plot change detection results  ###########################
plot_CD <- function(raster,title){
  ggplot() + ggR(raster$class, geom_raster = T, ggLayer = T) +
    scale_fill_manual(values = rep(c("#00BFC4","#7CAE00","#C77CFF","#F8766D"),times=4))+
    labs(x="Longitude", y= "Latitude",title = title)+
    coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm"))  + theme_minimal()+
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),legend.position = "bottom")
}


##################### extract legend from ggplot ###############################
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

##################### plot CVA results  ########################################
plot_CVAang <- function(raster,title){
  ggplot() + ggR(raster, geom_raster = T, ggLayer = T) +
    scale_fill_continuous(type="viridis", name = "",limits=c(0,360)) +
    labs(title = title, x="Longitude", y= "Latitude",
         subtitle = "Angle")+
    coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm"))+
    theme_minimal()+
    theme(text = element_text(size = 8),
          axis.text.x = element_text(angle=45, vjust=1, hjust=1))
}

plot_CVAmag <- function(raster,title){
  ggplot() + ggR(raster, geom_raster = T, ggLayer = T) +
    scale_fill_continuous(type="gradient", name = "",limits=c(0,1)) +
    labs(title = title, x="Longitude", y= "Latitude",
         subtitle = "Magnitude")+
    coord_sf(crs = (3857))+
    annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                     pad_y = unit(-0, "cm"))+
    theme_minimal()+
    theme(text = element_text(size = 8),
          axis.text.x = element_text(angle=45, vjust=1, hjust=1))
}

############################### Mask out classes ###############################
mask_one <- function(raster, class) { # mask out single classes 
  
  mask <- raster
  mask$bin <- raster$class
  mask$bin[getValues(raster$class) == class] <- 1
  mask$bin[getValues(raster$class) != class] <- 0
  
  return(mask$bin)
}

mask <- function(raster,classification,classes){ # mask all classes
  for (k in 1:length(classes)){
    classes_new <- sub(classes,pattern=" ", replacement = ""  ) # remove blanks to avoid trouble while plotting
    if (k ==1){
      current_mask <- mask_one(classification,classes[k])
      ofile <- current_mask
      names(ofile[[k]]) <- classes_new[k]}
    else {
      current_mask <- mask_one(classification,classes[k])
      ofile <- stack(ofile,current_mask)
      names(ofile[[k]]) <- classes_new[k]}}
  rm(current_mask)
  return(ofile)}


############################### Calculate Area for each class ##################
area <- function(BinLayer,classes){
  res <-  res(BinLayer)
  area <- data.frame(class=classes,area=rep(NA,4))
  for (i in 1:length(classes)){
    ncell <- ncell(BinLayer[BinLayer[[i]] == 1])
    current_area <- ncell * res[1] * res[2] * 1e-6 
    area$area[i] <- current_area}
  return(area)
}

#####################  save ggplot #############################################
save_plot <- function(filename,plot){
  ggsave(filename= paste(outdir,filename,sep=""),plot=plot, width = 20 ,
         height = 20,units="cm", device = "png")
}






















