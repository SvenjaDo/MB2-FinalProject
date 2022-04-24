############################### Mask ###########################################

#### Mask out classes 
#
mask <- function(raster, class) {
  
  mask <- raster
  mask$bin <- raster$class
  mask$bin[getValues(raster$class) == class] <- 1
  mask$bin[getValues(raster$class) != class] <- 0
  
  return(mask$bin)
}

mask_all <- function(raster,classification,classes){
  for (k in 1:length(classes)){
    classes_new <- sub(classes,pattern=" ", replacement = ""  ) # remove blanks to avoid trouble while plotting
    if (k ==1){
    current_mask <- mask(classification,classes[k])
    ofile <- current_mask
    names(ofile[[k]]) <- classes_new[k]}
    else {
      current_mask <- mask(classification,classes[k])
      ofile <- stack(ofile,current_mask)
      names(ofile[[k]]) <- classes_new[k]}}
    rm(current_mask)
  return(ofile)}


