#' @title calcMulticropping 
#' @description calculates the ratio between area harvested and physical cropland area. Can be larger or smaller, depending on fallow land and double cropping.
#'
#' @param selectyears "time": the full period in 5 year timesteps. "past": only past. "past_all" past with all years. otherwhise, any vector of years.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcFAOLand}},
#' \code{\link{calcCroparea}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("")
#' }
#' 
calcMulticropping <- function(selectyears="time") {
  
  # kcr <- findset("kcr")
  # newproducts<-c("betr","begr")
  # kcr_red<-setdiff(kcr,newproducts)
  past<-findset("past")
  
  if (selectyears[[1]]%in%c("time","past")) {
    selection<-past
  } else if (selectyears[[1]]%in%c("past_all")){
    selection<-paste0("y",1961:2011)
  } else {selection = selectyears}
  
  phys <- collapseNames(calcOutput("FAOLand", aggregate=FALSE)[,,"6620|Arable land and Permanent crops"][,selection,])
  # read in area harvested
  area <- collapseNames(dimSums(calcOutput("Croparea", physical=FALSE, aggregate=FALSE, sectoral="kcr"),dim=3.1)[,selection,])
  

  multi <- area/phys
  multi[is.na(multi)]<-0
  multi[multi==Inf]<-0

  if (selectyears[[1]]=="time"){
    multi<-toolHoldConstantBeyondEnd(multi)
    phys<-toolHoldConstantBeyondEnd(phys)
  }
  
  return(list(x=multi,
              weight=phys,
              unit="area harvested per physical area (ratio)",
              description="values above one indicate multicropping, below one fallow land"
  ))
}