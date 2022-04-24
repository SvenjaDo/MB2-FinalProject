############################################################################################################################
##################### Landcover Classification in Change detection for the Adaro Coal mine in Indonesia ####################
############################################################################################################################
# Land degradation from coal mining 
############################################################################################################################
# Final submission for the MB2 course, EAGLE Master Würzburg
# Author: Svenja Dobelmann
# Mtr. Number: 2767870
# Contact: svenja.dobelmann@stud-mail.uni-wuerzburg.de 
# Date: April 2022


# RStudio version: 2021.09.1
# R version: 4.1.1
# operating system: Windows 11

# Overview:
## - 1.) convert MODIS .hdf data into .TIFF files
## - 2.) Pre-Process data: load, reproject, clip, stack ,cloud filter
## - 3.) Process data: Classification
## - 4.) Process data: Change detection 
## - 5.) Visualization: animation, ggplot

# requirements: 
## - MODIS files for time and area of interest in .hdf or .tiff format
## - additionnal cloud mask products 
## - GDAL has to be installed on your system! 



############################################################################################################################
############################### 0. Install and Load required packages & define working directory ###########################
############################################################################################################################

#### set working directory 
workdir <- getwd() # add path here! 


#### load functions
source("functions/loadandinstall.R")
source("functions/hdf2tiff.R")
source("functions/coords2spatpoly.R")
source("functions/loadandstack.R")
source("functions/ndvi.R")
source("functions/bsi.R")
source("functions/ndwi.R")
source("functions/assignClassnames.R")
source("functions/mask.R")


#### load required packages 
loadandinstall("raster")
loadandinstall("ggplot2")
loadandinstall("gganimate")
loadandinstall("sp")
loadandinstall("sf")
loadandinstall("rgdal")
loadandinstall("GDAL")
loadandinstall("gdalUtils")
loadandinstall("RStoolbox")
#loadandinstall("nightmares")
loadandinstall("terra")
loadandinstall("tidyverse")

############################################################################################################################
############################### 1. transform .hdf files to .TIFF ###########################################################
############################################################################################################################
#if you already have .tiff files this step can be skipped.
# GDAL has to be installed on your system!!!

#### set directory to downloaded MODIS .hdf files 
download_dir <- paste0(getwd(),"/download","_datasets","/modis_mod09a1_v6") # add path here! 

#### grep only required .hdf files 
product_list <- grep("MOD09A1", list.files(download_dir, pattern="*.hdf$"), value=T) 

filenames <- paste(download_dir,product_list,sep="/")


#### character with bandnames, for naming the output files
bandsel <- c("b1_red","b2_NIR","b3_blue","b4_green","b5_SWIR1","b6_SWIR2","b7_SWIR3") 

#### get subdatasets and transform .hdf into .tif
hdf2tiff(filenames, bandsel)



############################################################################################################################
############################### 2. data pre-processing  ####################################################################
###########################################################################################################################

#### Area of interest: Adaro coal mine, Indonesia ##############################

##### set coordinates for aoi (EPSG:3857):
x_coords <- c(12819301.718,12819301.718,12882276.032,12882276.032,12819301.718)
y_coords <- c(-214152.945,-273341.918,-273341.918,-214152.945,-214152.945) 
crs <- "EPSG:3857"

aoi <- coords2spatpoly(x_coords,y_coords,crs)



bnd <- raster::getData("GADM", country='IDN', level=1,path= download_dir) # borders of Indonesia
#bnd <- st_as_sf(bnd) # convert to sf 
kalimS <- bnd[bnd$HASC_1=="ID.KS",] # Division of South Kalimantan
kalimS <- spTransform(kalimS,CRSobj = CRS("EPSG:3857"))


# visual check
plot(kalimS)
plot(aoi,add=T)


#### Load and stack raster files ###############################################

filenames <- paste0(download_dir,"/",list.files(download_dir, pattern="*.tif$"))
years <- c("2002","2011","2021") # enter years of analysis

loadandstack(filenames,years,crs,aoi)

for(i in 1:length(years)){
  current_filenames <- grep(years[i], filenames, value=T) # list filenames for current year
  current_raster <- stack(current_filenames) # stack raster files
  crs(current_raster) <- CRS("EPSG:3857") # set coordinate system
  raster_crop <- crop(current_raster,aoi) # crop raster to aoi
  names(raster_crop) <- bandsel # clean up the band names for neater plotting
  assign(paste("raster",years[i],sep="_"),raster_crop) # save current raster
  rm(current_filenames,current_raster,raster_crop)}# remove layers that are not needed anymore 



##############################################################################################################
################################ 3. Calculate Indices ########################################################
##############################################################################################################
#### NDVI Normalized Difference Vegetation Index

ndvi_2002 <- ndvi(raster_2002)
ndvi_2011 <- ndvi(raster_2011)
ndvi_2021 <- ndvi(raster_2021)

#### BSI Bare Soil Index

bsi_2002 <- bsi(raster_2002)
bsi_2011 <- bsi(raster_2011)
bsi_2021 <- bsi(raster_2021)


### Normalized difference water index

ndwi_2002 <- ndwi(raster_2002)
ndwi_2011 <- ndwi(raster_2011)
ndwi_2021 <- ndwi(raster_2021)

# bring indices to same unit [0,1]

#### stack indices together

indices_2002 <- stack(ndvi_2002,bsi_2002,ndwi_2002)
indices_2011 <- stack(ndvi_2011,bsi_2011,ndwi_2002)
indices_2021 <- stack(ndvi_2021,bsi_2021,ndwi_2021)

#### plot results in RGB image
plotRGB(indices_2002,
        r = 2, g = 1, b = 3,
        stretch = "hist",
        main = "R = BSI, G = NDVI, B = NDWI",
        axes = TRUE)

plotRGB(indices_2011,
        r = 2, g = 1, b = 3,
        stretch = "hist",
        main = "R = BSI, G = NDVI, B = NDWI",
        axes = TRUE)

plotRGB(indices_2021,
        r = 2, g = 1, b = 3,
        stretch = "hist",
        main = "R = BSI, G = NDVI, B = NDWI",
        axes = TRUE)

##############################################################################################################
################################ 4. Landcover Classification #################################################
##############################################################################################################

## using the calculated indices to perform a unsupervised classification 

#### Unsupervised classification ###############################################

unsupclass02 <- unsuperClass(indices_2002,nClasses = 4, nSamples = 100000, nStarts = 6, nIter=100,algorithm = "Hartigan-Wong",
                             trainPartition = 0.6, responseCol = "class")
# use this model to predict classes for 2011 and 2021
unsupclass11 <- predict(unsupclass02,indices_2011)
unsupclass21 <-  predict(unsupclass02,indices_2021)


#### assign classnames 
unsupclass02$class <- assignClassnames(unsupclass02$map)[[1]]
unsupclass11$class <- assignClassnames(unsupclass11$layer)[[2]]
unsupclass21$class <- assignClassnames(unsupclass21$layer)[[2]]



##### plot results

ggplot() + ggR(unsupclass02$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))
ggplot() + ggR(unsupclass11$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))
ggplot() + ggR(unsupclass21$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))


#### animation

class_all <- c(unsupclass02$class,unsupclass11$class,unsupclass21$class)

ggplot() + ggR(class_all$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))

levelplot(class_all[[1]])
          
          
          
          
          
          
 r <- raster(nrow=10, ncol=10)
 r[] = 1
 r[51:100] = 3
 r[3:6, 1:5] = 5
 r <- ratify(r)
 
 rat <- levels(r)[[1]]
 rat$landcover <- c('Pine', 'Oak', 'Meadow')
 rat$class <- c('A1', 'B2', 'C3')
 levels(r) <- rat
 
 levelplot(r, col.regions=c('palegreen', 'midnightblue', 'indianred1'))

##############################################################################################################
################################ 5. Change Detection  ########################################################
##############################################################################################################

#### Postclassification ########################################################
# using the results frum unsupervised classification
unsupclass11x10 <- unsupclass11$layer*10

unsupclass02_11 <- unsupclass11x10 + unsupclass02$map
unsupclass11_21 <- unsupclass11x10 + unsupclass21$layer


#### plot results
ggplot() + ggR(unsupclass02_11, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))

ggplot() + ggR(unsupclass11_21, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Land Cover") +
  coord_sf(crs = (3857))


#### Change Vector Analysis ####################################################

# tasseled cap calculated brightness greenness and wetness to perform CVA  
tc2002 <- tasseledCap(raster_2002, sat= "MODIS")
tc2011 <- tasseledCap(raster_2011, sat= "MODIS")
tc2021 <- tasseledCap(raster_2021, sat= "MODIS")


changevec02_11 <- rasterCVA(tc2002[[1:2]],tc2011[[1:2]]) # only select brightness and greeness 
changevec11_21 <- rasterCVA(tc2011[[1:2]],tc2021[[1:2]])


#display results
ggplot() + ggR(changevec02_11$angle, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Angle") +
  coord_sf(crs = (3857))
ggplot() + ggR(changevec02_11$magnitude, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Magnitude") +
  coord_sf(crs = (3857))


ggplot() + ggR(changevec11_21$angle, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Angle") +
  coord_sf(crs = (3857))
ggplot() + ggR(changevec11_21$magnitude, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "Magnitude") +
  coord_sf(crs = (3857))

##############################################################################################################
################################ 5. Binary maps / Calculate extent of land degradation  ######################
##############################################################################################################


##### generate binary layer for classes
classes <- unique(unsupclass02$class)

Binary2002 <- mask_all(raster_2002,unsupclass02,classes)
Binary2011 <- mask_all(raster_2011,unsupclass11,classes)
Binary2021 <- mask_all(raster_2021,unsupclass21,classes)


##### calculate area for each class 

area2002 <- area(Binary2002,classes)
area2011 <- area(Binary2011,classes)
area2021 <- area(Binary2021,classes)

#### animate results

areas <- rbind(area2002,area2011,area2021)
areas$frame <- rep(c("02","11","21"),c(nrow(area2002),nrow(area2011),nrow(area2021)))

ggplot(areas,aes(class,area,fill=class))+
  geom_col()+
  transition_states(frame,transition_length = 4, state_length = 1)



################################### END OF ANALYSIS ##########################################################################

a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)

plot(raster11_crop[[4]], 
     main="Band 2\n NEON Harvard Forest Field Site",
     col=grayscale_colors)

grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)   #Null=colors are not transparent

plotRGB(indices_2021,
        r = 1, g = 2, b = 3,
        stretch = "hist",
        main = "Pre-fire RGB image with cloud\n Cold Springs Fire",
        axes = TRUE)
