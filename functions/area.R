############################### Area ###########################################

#### Claculate Area for each class


area <- function(BinLayer,classes){
  res <-  res(BinLayer)
  area <- data.frame(class=classes,area=rep(NA,4))
  for (i in 1:length(classes)){
      ncell <- ncell(BinLayer[BinLayer[[i]] == 1])
      current_area <- ncell * res[1] * res[2] * 1e-6 
      area$area[i] <- current_area}
  return(area)
}


