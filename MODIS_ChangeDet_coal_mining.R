############################################################################################################################
##################### Landcover Classification and Change detection for the Adaro Coal mine in Indonesia ###################
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
## - 2.) Pre-Process data: load, re-project, clip, stack
## - 3.) Process data:
#### - 3.1) Calculate Indices
#### - 3.2) Classification
#### - 3.3) Change detection (Postclassif, CVA) 
#### - 3.4) Mask classes 
## - 4) Export Visualizations: animation, ggplot


# Requirements: 
## - MODIS files for time and area of interest in .hdf or .tiff format
####(can be downloaded from the Earth explorer of the UDGS: https://earthexplorer.usgs.gov/ )
## - GDAL has to be installed on your system! 

############################################################################################################################
############################### 0. Install and Load required packages & define working directory ###########################
############################################################################################################################   
#### set working directory 
workdir <- getwd() # add path here!

#### load functions 
source("functions.R")

#### load required packages
loadandinstall(c("raster","ggplot2","gganimate","patchwork","ggthemes","sp","sf","gdalUtils","RStoolbox","dplyr","ggspatial",
  "gifski"))

############################################################################################################################
############################### 1. transform .hdf files to .TIFF ###########################################################
############################################################################################################################
# if you already have .tiff files this step can be skipped.
# GDAL has to be installed on your system!!!

#### set directory to downloaded MODIS .hdf files 
download_dir <- paste0(getwd(),"/download","_datasets","/modis_mod09a1_v6") # add path here! 

#### grep only required .hdf files 
product_list <- grep("MOD09A1", list.files(download_dir, pattern="*.hdf$"), value=T) 

#### write list with all files 
filenames <- paste(download_dir,product_list,sep="/")

#### character with band-names, for naming the output files
bandsel <- c("b1_red","b2_NIR","b3_blue","b4_green","b5_SWIR1","b6_SWIR2","b7_SWIR3") 

#### get subdatasets and transform .hdf into .tif
hdf2tiff(filenames, bandsel)

############################################################################################################################
############################### 2. data pre-processing  ####################################################################
###########################################################################################################################

#### Area of interest: Adaro coal mine, Indonesia ##############################

#### set coordinates for AOI (Long, Lat) 
x_coords <- c(115.158,115.158,115.723,115.723,115.158)
y_coords <- c(-1.923,-2.455,-2.455,-1.923,-1.923) 
crs <- "EPSG:4326"

#### create spatial polygon from coordinates
aoi <- coords2spatpoly(x_coords,y_coords,crs)

#### download GADM borders for country of interest 
bnd <- raster::getData("GADM", country='IDN', level=1,path= download_dir) # borders of Indonesia
kalim_subdiv <- grep("Kalimantan", bnd$NAME_1) # grap all Divisions of Kalimantan 

#### Subset divisions of Kalimantan (Island of Borneo)
kalimantan <- bnd[kalim_subdiv,] %>%
  spTransform(CRSobj = CRS("EPSG:4326"))

#### visual check
##### transform to sf object
kalimantan_sf <- st_as_sf(kalimantan) 
aoi_sf <- st_as_sf(aoi)

plot_aoi <- ggplot(kalimantan_sf)+
  geom_sf(fill="darkgreen",colour="black")+
  geom_sf(data=aoi_sf,fill="red")+
  labs(title="Island of Borneo (Indonesia)")+
  theme(panel.background = element_rect(fill = 'lightblue'),
        panel.grid.major = element_line(color = 'lightblue'))

plot_aoi

#### Load and stack raster files ###############################################

#### list all .tif files 
filenames <- paste0(download_dir,"/",list.files(download_dir, pattern="*.tif$"))

#### years of analysis                    
years <- c("2002","2011","2021") 

#### transform aoi to coordinate system of MODIS files 
aoi_proj <- spTransform(aoi,CRS("EPSG:3857")) 

#### stack, transform and crop files 
for(i in 1:length(years)){
  current_filenames <- grep(years[i], filenames, value=T) # list filenames for current year
  current_raster <- stack(current_filenames) # stack raster files
  crs(current_raster) <- CRS("EPSG:3857") # set coordinate system
  raster_crop <- crop(current_raster,aoi_proj) # crop raster to aoi
  names(raster_crop) <- bandsel # clean up the band names for easier plotting
  assign(paste("raster",years[i],sep="_"),raster_crop) # assign name for current raster in local environment 
  rm(current_filenames,current_raster,raster_crop)}# remove layers that are not needed anymore 

#### plot raster as RGB images #################################################

#### save theme for following plots
my_theme <- theme_fivethirtyeight() +
  theme(axis.title = element_text())+
  theme(text = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0))+
  theme(plot.margin = margin(t = 1, r = 1, b = 3, l = 1, unit = "cm"),
        axis.text.x = element_text(angle=40, vjust=1, hjust=1))
  
#### plot scenes
rgb_02 <- plot_rgb(raster_2002,"True Colour Image: 2002")
rgb_11 <- plot_rgb(raster_2011,"True Colour Image: 2011")
rgb_21 <- plot_rgb(raster_2021,"True Colour Image: 2021")

rgb_02
rgb_11
rgb_21

##############################################################################################################
################################ 3.1 Calculate Indices #######################################################
##############################################################################################################

#### calculate NDVI, BSI and NDWI and stack them for each year:
indices_2002 <- stack(ndvi(raster_2002),bsi(raster_2002),ndwi(raster_2002))
indices_2011 <- stack(ndvi(raster_2011),bsi(raster_2011),ndwi(raster_2011))
indices_2021 <- stack(ndvi(raster_2021),bsi(raster_2021),ndwi(raster_2021))

#### plot results as RGB image
indic_02 <- plot_indic(indices_2002,"Indices 2002")
indic_11 <- plot_indic(indices_2011,"Indices 2011")
indic_21 <- plot_indic(indices_2021,"Indices 2021")

indic_02
indic_11
indic_21

##############################################################################################################
################################ 3.2 Landcover Classification ################################################
##############################################################################################################
## using the calculated indices to perform a unsupervised classification 

#### Unsupervised classification ###############################################
set.seed(42)
unsupclass02 <- unsuperClass(indices_2002,nClasses = 4, nSamples = 100000, nStarts = 6, nIter=100,algorithm = "Hartigan-Wong",
                             trainPartition = 0.6, responseCol = "class")
#### use this model to predict classes for 2011 and 2021
unsupclass11 <- predict(unsupclass02,indices_2011)
unsupclass21 <-  predict(unsupclass02,indices_2021)

#### assign classnames 
unsupclass02$class <- assignClassnames(unsupclass02$map)[[1]]
unsupclass11$class <- assignClassnames(unsupclass11$layer)[[2]]
unsupclass21$class <- assignClassnames(unsupclass21$layer)[[2]]

##### plot results
LC_02 <- plot_LC(unsupclass02, "Land Cover Classification 2002")
LC_11 <- plot_LC(unsupclass11, "Land Cover Classification 2011")
LC_21 <- plot_LC(unsupclass21, "Land Cover Classification 2021")

LC_02
LC_11
LC_21

##############################################################################################################
################################ 3.3 Change Detection  #######################################################
##############################################################################################################

#### Postclassification ########################################################
# using the results from unsupervised classification

#### multiply results form 2011 times 10
unsupclass11x10 <- unsupclass11$layer*10

#### add up classifications to get change classes 
unsupclass02_11 <- unsupclass11x10 + unsupclass02$map
unsupclass11_21 <- unsupclass11x10 + unsupclass21$layer

### assign change classnames
unsupclass02_11$class <- assignClassnames_CD(unsupclass02_11)[[2]]
unsupclass11_21$class <- assignClassnames_CD(unsupclass11_21)[[2]]

#### plot results
CD_02.11 <- plot_CD(unsupclass02_11, "2002 to 2011")
CD_11.21 <- plot_CD(unsupclass11_21, "2011 to 2021")

#### extract legend from plot
CD_legend <- extract_legend(CD_11.21)

### draw both plots with shared legend and AOI
CD_plot <- (plot_aoi+ theme_void()+labs(title="")) + ((CD_02.11+theme(legend.position = "none")  + 
                                                       (CD_11.21+theme(legend.position = "none"))) / CD_legend) + 
  plot_layout(widths=c(2,5)) +
  plot_annotation(
    title = 'Land Cover Change in Indonesia',
    subtitle = 'Due to coal exploitation in the Adaro Coal mine',
    caption = 'Based on Postclassification analysis - April 2022 - Author: Svenja Dobelmann'
  )&
  theme(plot.title = element_text(hjust = .5,size = 16),
        plot.subtitle = element_text(hjust = .5,size = 14))

CD_plot

#### Change Vector Analysis ####################################################

#### tasseled cap calculated brightness greenness and wetness to perform CVA  
tc2002 <- tasseledCap(raster_2002, sat= "MODIS")
tc2011 <- tasseledCap(raster_2011, sat= "MODIS")
tc2021 <- tasseledCap(raster_2021, sat= "MODIS")

#### perform CVA from brightness and greeness 
changevec02_11 <- rasterCVA(tc2002[[1:2]],tc2011[[1:2]]) # only select brightness and greeness 
changevec11_21 <- rasterCVA(tc2011[[1:2]],tc2021[[1:2]])

#### plot results (Angle and Magnitude of change)
CVA02.11_ang <- plot_CVAang(changevec02_11$angle,"2002 to 2011")
CVA02.11_mag <- plot_CVAmag(changevec02_11$magnitude,"2002 to 2011")

CVA11.21_ang <- plot_CVAang(changevec11_21$angle,"2011 to 2021")
CVA11.21_mag <- plot_CVAmag(changevec11_21$magnitude,"2011 to 2021")

#### layout with all four plots
CVA_plot <- (plot_aoi+ theme_void()+labs(title=""))+(CVA02.11_ang + theme(legend.position = "none") + CVA11.21_ang) / 
  (CVA02.11_mag + theme(legend.position = "none") + CVA11.21_mag) +
  plot_layout(widths=c(2,5)) +
  plot_annotation('Change Vector Analysis of the Adaro Coal mine in Indonesia',
                   caption = 'April 2022 - Author: Svenja Dobelmann',
                  theme=theme(plot.title = element_text(hjust = 0.5)))
CVA_plot
##############################################################################################################
################################ 3.4 Binary maps / Calculate extent of land degradation  #####################
##############################################################################################################

#### character with occurring classes 
classes <- unique(unsupclass02$class)

##### generate binary layer for classes
Binary2002 <- mask(raster_2002,unsupclass02,classes)
Binary2011 <- mask(raster_2011,unsupclass11,classes)
Binary2021 <- mask(raster_2021,unsupclass21,classes)

##### calculate area for each class and assign in data.frame 
areas <- rbind(area(Binary2002,classes), area(Binary2011,classes),area(Binary2021,classes))%>%
  mutate(year=rep(c(years[1],years[2],years[3]),times=length(classes)))

#### animate area per class in a barplot
p <- ggplot(areas,aes(class,area,fill=class))+
  geom_col(width=0.8)+
  geom_label(aes(label=round(area,1)), vjust=-0.3, color="white", size=3.5)+
  my_theme+
  theme(legend.position = "none")+
transition_states(year,transition_length = 3, state_length = 3)+
  labs(x="",y=expression(paste("area ", (m^2),sep=" ")),
       title="Area per LC-type",subtitle = "Year: {closest_state}")

p

##############################################################################################################
################################ 4. Export Plots  ############################################################
##############################################################################################################

#### create directory for output files 
outdir <- paste(workdir, "/plots/",sep="") 
dir.create(outdir) 

## save animated barplot
anim_save(filename = "plots/area_anim.gif", animation = p)

### animate normal rgb rasterlayer 

#### save all 3 plots as .png 
save_plot("rgb_2002.png",rgb_02)
save_plot("rgb_2011.png",rgb_11)
save_plot("rgb_2021.png",rgb_21)

#### create GIF
#### list plots 
rgb_files <- paste0(outdir,"",list.files(outdir, pattern="*rgb*"))

gifski(rgb_files, gif_file = paste0(outdir,"rgb_animation.gif"),
       width = 800,
       height = 800,)

### same procedure for Indices 
save_plot("indic_2002.png",indic_02)
save_plot("indic_2011.png",indic_11)
save_plot("indic_2021.png",indic_21)

indic_files <- paste0(outdir,"",list.files(outdir, pattern="*indic*"))

gifski(indic_files, gif_file = paste0(outdir,"indic_animation.gif"),
       width = 800,
       height = 800,)

### same procedure for Landcover 
save_plot("LC_2002.png",LC_02)
save_plot("LC_2011.png",LC_11)
save_plot("LC_2021.png",LC_21)

indic_files <- paste0(outdir,"",list.files(outdir, pattern="*LC*"))

gifski(indic_files, gif_file = paste0(outdir,"LC_animation.gif"),
       width = 800,
       height = 800,)

#### export change detection plot
ggsave(filename= paste(outdir,"change_det.png",sep=""),plot=CD_plot, width = 30 ,height = 20,units="cm", device = "png")

#### export CVA plot
ggsave(filename= paste(outdir,"CVA.png",sep=""),plot=CVA_plot, width = 30 ,height = 20,units="cm", device = "png")


################################### END OF ANALYSIS ##########################################################################
