#' @title calcUrbanLandFuture
#' @description Urban land in Mha on 0.5deg grid
#' @param cellular TRUE for results on 0.5 degree grid.
#' @param timestep 5year or yearly
#' @return List of magpie objects with results on 0.5deg grid level, weights NULL, unit and description.
#' @author David Chen
#' @importFrom magpiesets findset

calcUrbanLandFuture <-function(timestep = "5year", cellular = TRUE){
  
out <- readSource("UrbanLandFuture", subtype="LUH2v2",convert=FALSE)



if(timestep == "5year"){
  out <- out[,seq(2015,2100,5),]
  out <- toolHoldConstant(out, seq(2105,2150, 5))
}

else if(timestep=="yearly"){
  out <- toolHoldConstant(out, c(2101:2150))
}

return(list(
  x=out,
  weight=NULL,
  unit="Mha",
  isocountries=(!cellular & (nregions(out)!=1)),
  description="Amount of Urban land expansion for various SSPs"))
}