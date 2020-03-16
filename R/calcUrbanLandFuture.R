#' @title calcUrbanLandFuture
#' @description Urban land in Mha on 0.5deg grid
#' @param cellular TRUE for results on 0.5 degree grid.
#' @return List of magpie objects with results on 0.5deg grid level, weights NULL, unit and description.
#' @author David Chen

calcUrbanLandFuture <-function(cellular = TRUE){
  
out <- readSource("UrbanLandFuture",convert=FALSE)

return(list(
  x=out,
  weight=NULL,
  unit="Mha",
  isocountries=(!cellular & (nregions(out)!=1)),
  description="Amount of Urban land expansion for various SSPs"))
}