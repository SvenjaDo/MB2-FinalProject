################ create spatial polygon from given coordinates #################


coords2spatpoly <- function(x_coords,y_coords,crs){
  poly <- sp::Polygon(cbind(x_coords,y_coords))%>% # create Polygon from coordinates
    list()%>% sp::Polygons( ID = "A") 
  aoi <- sp::SpatialPolygons(list(poly))
  crs(aoi) <- CRS(crs)
  aoi
}
  

