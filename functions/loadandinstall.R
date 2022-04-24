##################### installing and loading packages ##########################

loadandinstall <- function(mypkg) {
  if (!is.element(mypkg, installed.packages()[,1]))
  {install.packages("mypkg")}; 
  library(mypkg, character.only=TRUE)}

