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
workdir <- getwd() # add path here! (maybe use interactive folder selection)

#### load functions sourc(c(a,b,c,))
source("functions/loadandinstall.R")
source("functions/hdf2tiff.R")
source("functions/coords2spatpoly.R")
source("functions/ndvi.R")
source("functions/bsi.R")
source("functions/ndwi.R")
source("functions/assignClassnames.R")
source("functions/mask.R")
source("functions/area.R")
source("functions/extract_legend.R")

#### load required packages use pacman or loadandinstall(c(a,b,..))
loadandinstall("raster")
loadandinstall("ggplot2")
loadandinstall("gganimate")
loadandinstall("patchwork")
loadandinstall("ggthemes")
loadandinstall("gridExtra")
loadandinstall("sp")
loadandinstall("sf")
loadandinstall("rgdal")
loadandinstall("GDAL")
loadandinstall("gdalUtils")
loadandinstall("RStoolbox")
loadandinstall("dplyr")
loadandinstall("terra")
loadandinstall("tidyverse")
loadandinstall("ggspatial")
loadandinstall("gifski")

############################################################################################################################
############################### 1. transform .hdf files to .TIFF ###########################################################
############################################################################################################################
#if you already have .tiff files this step can be skipped.
# GDAL has to be installed on your system!!!

#### set directory to downloaded MODIS .hdf files 
download_dir <- paste0(getwd(),"/download","_datasets","/modis_mod09a1_v6") # add path here! 

#### grep only required .hdf files 
product_list <- grep("MOD09A1", list.files(download_dir, pattern="*.hdf$"), value=T) 

###
filenames <- paste(download_dir,product_list,sep="/")

#### character with bandnames, for naming the output files
bandsel <- c("b1_red","b2_NIR","b3_blue","b4_green","b5_SWIR1","b6_SWIR2","b7_SWIR3") 

#### get subdatasets and transform .hdf into .tif
hdf2tiff(filenames, bandsel)

############################################################################################################################
############################### 2. data pre-processing  ####################################################################
###########################################################################################################################

#### Area of interest: Adaro coal mine, Indonesia ##############################

##### set coordinates for AOI (Long, Lat) -> use leaflet to draw AOI
x_coords <- c(115.158,115.158,115.723,115.723,115.158)
y_coords <- c(-1.923,-2.455,-2.455,-1.923,-1.923) 
crs <- "EPSG:4326"

aoi <- coords2spatpoly(x_coords,y_coords,crs)

bnd <- raster::getData("GADM", country='IDN', level=1,path= download_dir) # borders of Indonesia
#bnd <- st_as_sf(bnd) # convert to sf 
kalim_subdiv <- grep("Kalimantan", bnd$NAME_1) # grap all Divisions of Kalimantan 

kalimantan <- bnd[kalim_subdiv,] # Division of Kalimantan
kalimantan <- spTransform(kalimantan,CRSobj = CRS("EPSG:4326"))

# visual check
kalimantan_sf <- st_as_sf(kalimantan) # transform st to sf object 
aoi_sf <- st_as_sf(aoi)


plot_aoi <- ggplot(kalimantan_sf)+
  geom_sf(fill="darkgreen",colour="black")+
  geom_sf(data=aoi_sf,fill="red")+
  labs(title="Island of Borneo (Indonesia)")+
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(color = 'lightblue'))


plot_aoi

#### Load and stack raster files ###############################################

filenames <- paste0(download_dir,"/",list.files(download_dir, pattern="*.tif$"))
years <- c("2002","2011","2021") # enter years of analysis
aoi_proj <- spTransform(aoi,CRS("EPSG:3857")) # transform aoi to coordinate system of MODIS files 

for(i in 1:length(years)){
  current_filenames <- grep(years[i], filenames, value=T) # list filenames for current year
  current_raster <- stack(current_filenames) # stack raster files
  crs(current_raster) <- CRS("EPSG:3857") # set coordinate system
  raster_crop <- crop(current_raster,aoi_proj) # crop raster to aoi
  names(raster_crop) <- bandsel # clean up the band names for easier plotting
  assign(paste("raster",years[i],sep="_"),raster_crop) # save current raster
  rm(current_filenames,current_raster,raster_crop)}# remove layers that are not needed anymore 

#### plot raster as RGB images
# save theme for following plots
my_theme <- theme_fivethirtyeight() +
  theme(axis.title = element_text())+
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0))+
  theme(plot.margin = margin(t = 1, r = 1, b = 3, l = 1, unit = "cm"),
        axis.text.x = element_text(angle=40, vjust=1, hjust=1))
  

rgb_02 <- ggRGB(raster_2002,
                r=1, g=4, b=3,
                stretch = "none")+
  labs(title = paste("True Colour Image:",years[1],sep=" "),y="Latitude",x="Longitude") +
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme


rgb_11 <- ggRGB(raster_2011,
                 r=1, g=4, b=3,
                 stretch = "none")+
  labs(title = paste("True Colour Image:",years[2],sep=" "),y="Latitude",x="Longitude") +
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme

rgb_21 <- ggRGB(raster_2021,
                 r=1, g=4, b=3,
                 stretch = "none")+
  labs(title = paste("True Colour Image:",years[3],sep=" "),y="Latitude",x="Longitude") +
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme

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

#### stack indices together

indices_2002 <- stack(ndvi_2002,bsi_2002,ndwi_2002)
indices_2011 <- stack(ndvi_2011,bsi_2011,ndwi_2002)
indices_2021 <- stack(ndvi_2021,bsi_2021,ndwi_2021)

#### plot results as RGB image
indic_02 <- ggRGB(indices_2002,
      r=2, g=1, b=3,
      stretch = "lin")+
  labs(title = paste("Indices ",years[1],":",sep=""),
       subtitle="R = BSI, G = NDVI, B = NDWI",y="Latitude",x="Longitude") +  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme

indic_11 <- ggRGB(indices_2011,
      r=2, g=1, b=3,
      stretch = "lin")+
  labs(title = paste("Indices ",years[2],":",sep=""),
       subtitle="R = BSI, G = NDVI, B = NDWI",y="Latitude",x="Longitude") +  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme


indic_21 <- ggRGB(indices_2021,
      r=2, g=1, b=3,
      stretch = "lin")+
  labs(title = paste("Indices ",years[3],":",sep=""),
                     subtitle="R = BSI, G = NDVI, B = NDWI",y="Latitude",x="Longitude") +
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme

##############################################################################################################
################################ 4. Landcover Classification #################################################
##############################################################################################################

## using the calculated indices to perform a unsupervised classification 

#### Unsupervised classification ###############################################
set.seed(42)
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
LC_02 <- ggplot() + ggR(unsupclass02$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  labs(title = paste("Land Cover Classification",years[1],sep=" "), x="Longitude", y= "Latitude")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) +  my_theme


LC_11 <-ggplot() + ggR(unsupclass11$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  labs(title =  paste("Land Cover Classification",years[2],sep=" "), x="Longitude", y= "Latitude")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme

LC_21 <-ggplot() + ggR(unsupclass21$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "Land Cover") +
  labs(title =  paste("Land Cover Classification",years[3],sep=" "), x="Longitude", y= "Latitude")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm")) + my_theme


##############################################################################################################
################################ 5. Change Detection  ########################################################
##############################################################################################################

#### Postclassification ########################################################
# using the results from unsupervised classification
unsupclass11x10 <- unsupclass11$layer*10

#
unsupclass02_11 <- unsupclass11x10 + unsupclass02$map
unsupclass11_21 <- unsupclass11x10 + unsupclass21$layer

### assign classnames
unsupclass02_11$class <- assignClassnames_CD(unsupclass02_11)[[2]]
unsupclass11_21$class <- assignClassnames_CD(unsupclass11_21)[[2]]

#### plot results
CD_02.11 <- ggplot() + ggR(unsupclass02_11$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "LC-Change") +
  labs(x="Longitude", y= "Latitude",
       title = paste(years[1],"to",years[2],sep=" "))+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm"))  + theme_minimal()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),legend.position = "bottom")


CD_11.21 <- ggplot() + ggR(unsupclass11_21$class, geom_raster = T, ggLayer = T) +
  scale_fill_discrete(type="viridis", name = "LC-Change") +
  labs(x="Longitude", y= "Latitude",
       title =  paste(years[2],"to",years[3],sep=" "))+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br",pad_x = unit(0.25, "cm"),
                   pad_y = unit(-0, "cm"))  + theme_minimal()+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 3))

### extract legend from second plot
CD_legend <- extract_legend(CD_11.21)

### draw both plots with shared legend
CD_plot <- ((plot_aoi+ theme_void()) + CD_legend) / ((CD_02.11+theme(legend.position = "none"))  + 
  (CD_11.21+theme(legend.position = "none"))) + 
    plot_annotation(
                    title = 'Land Cover Change in Indonesia',
                    subtitle = 'Due to coal exploitation in the Adaro Coal mine',
                    caption = 'Based on Postclassification analysis - April 2022 - Author: Svenja Dobelmann'
                  )&
  theme(plot.title = element_text(hjust = .5,size = 16),
        plot.subtitle = element_text(hjust = .5,size = 14))
  

#### Change Vector Analysis ####################################################

# tasseled cap calculated brightness greenness and wetness to perform CVA  
tc2002 <- tasseledCap(raster_2002, sat= "MODIS")
tc2011 <- tasseledCap(raster_2011, sat= "MODIS")
tc2021 <- tasseledCap(raster_2021, sat= "MODIS")


changevec02_11 <- rasterCVA(tc2002[[1:2]],tc2011[[1:2]]) # only select brightness and greeness 
changevec11_21 <- rasterCVA(tc2011[[1:2]],tc2021[[1:2]])


#display results
CVA02.11_ang <- ggplot() + ggR(changevec02_11$angle, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "") +
  labs(title = paste(years[1],"to",years[2],sep= " "),
       x="Longitude", y= "Latitude",
       subtitle = "Angle")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br")+
  theme_minimal()+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  

CVA02.11_mag <- ggplot() + ggR(changevec02_11$magnitude, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="gradient", name = "") +
  labs(title = paste(years[1],"to",years[2],sep= " "),
       x="Longitude", y= "Latitude",
       subtitle = "Magnitude")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br")+
  theme_minimal()+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  


CVA11.21_ang <- ggplot() + ggR(changevec11_21$angle, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="viridis", name = "") +
  labs(title = paste(years[2],"to",years[3],sep= " "),
       x="Longitude", y= "Latitude",
       subtitle = "Angle")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br")+
  theme_minimal()+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  

CVA11.21_mag <- ggplot() + ggR(changevec11_21$magnitude, geom_raster = T, ggLayer = T) +
  scale_fill_continuous(type="gradient", name = "") +
  labs(title = paste(years[2],"to",years[3],sep= " "),
       x="Longitude", y= "Latitude",
       subtitle = "Magnitude")+
  coord_sf(crs = (3857))+
  annotation_scale(location = "br")+
  theme_minimal()+
  theme(text = element_text(size = 8),
       axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  



(CVA02.11_ang + CVA02.11_mag) / (CVA11.21_ang + CVA11.21_mag) +
  plot_annotation('Change Vector Analysis of the Adaro Coal mine in Indonesia',
                  tag_levels = 'A',caption = 'Author: Svenja Dobelmann',
                  theme=theme(plot.title = element_text(hjust = 0.5)))
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

#### animate area per class in a barplot

areas <- rbind(area2002,area2011,area2021)
areas$area <- round(areas$area,1)
areas$year <- rep(c(years[1],years[2],years[3]),c(nrow(area2002),nrow(area2011),nrow(area2021))) # assign year as frame for animation

p <- ggplot(areas,aes(class,area,fill=class))+
  geom_col(width=0.8)+
  geom_label(aes(label=round(area,1)), vjust=-0.3, color="white", size=3.5)+
  my_theme+
  transition_states(year,transition_length = 3, state_length = 3)+
  labs(x="",y=expression(paste("area ", (m^2),sep=" ")),
       title="Area per LC-type",subtitle = "Year: {closest_state}")

##############################################################################################################
################################ 7. Visualizations  ##########################################################
##############################################################################################################

#### create directory for output files 
outdir <- paste(workdir, "/plots/",sep="") 
dir.create(outdir) 

## save animated barplot
anim_save(filename = "plots/area_anim.gif", animation = p)

########### Map 1: land cover animation ########################################

### animate normal rgb rasterlayer 

#### save all 3 plots as .png and create a GIFF

png(paste0(outdir,"rgb_2002.png"),width = 800, height = 800, units = "px") 
print(rgb_02)
dev.off()

png(paste0(outdir,"rgb_2011.png"),width = 800, height = 800, units = "px") 
print(rgb_11)
dev.off()

png(paste0(outdir,"rgb_2021.png"),width = 800, height = 800, units = "px") 
print(rgb_21)
dev.off()


##### create GIF
rgb_files <- paste0(outdir,"",list.files(outdir, pattern="*rgb*"))

gifski(rgb_files, gif_file = paste0(outdir,"rgb_animation.gif"),
       width = 800,
       height = 800,)

### same procedure for Indices 

### safe all 3 plots as .png and create a GIFF


png(paste0(outdir,"indic_2002.png"),width = 800, height = 800, units = "px") 
print(indic_02)
dev.off()

png(paste0(outdir,"indic_2011.png"),width = 800, height = 800, units = "px") 
print(indic_11)
dev.off()

png(paste0(outdir,"indic_2021.png"),width = 800, height = 800, units = "px") 
print(indic_21)
dev.off()


##### create GIF
indic_files <- paste0(outdir,"",list.files(outdir, pattern="*indic*"))

gifski(indic_files, gif_file = paste0(outdir,"indic_animation.gif"),
       width = 800,
       height = 800,)


### same procedure for Landcover 

### safe all 3 plots as .png and create a GIFF


png(paste0(outdir,"LC_2002.png"),width = 800, height = 800, units = "px") 
print(LC_02)
dev.off()

png(paste0(outdir,"LC_2011.png"),width = 800, height = 800, units = "px") 
print(LC_11)
dev.off()

png(paste0(outdir,"LC_2021.png"),width = 800, height = 800, units = "px") 
print(LC_21)
dev.off()


##### create GIF
indic_files <- paste0(outdir,"",list.files(outdir, pattern="*LC*"))

gifski(indic_files, gif_file = paste0(outdir,"LC_animation.gif"),
       width = 800,
       height = 800,)




################################### END OF ANALYSIS ##########################################################################