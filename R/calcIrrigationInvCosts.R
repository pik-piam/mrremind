#' Calculation of IrrigationInvCosts
#' 
#' This function calculates values for each country until the year 2050. A
#' linear convergence is implemented, which means that every country will have
#' the value of Germany (1995) in 2050.
#' 
#' 
#' @return MAgPIE object
#' @author Nele Steinmetz
#' @seealso \code{\link{calcOutput}}, \code{\link{readWBirrigation}},
#' \code{\link{convertWBirrigation}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("IrrigationInvCosts")
#' 
#' 
#' }
#' @importFrom magclass getNames
calcIrrigationInvCosts <- function() {
  WBirrigation   <- readSource("WBirrigation")
  
  data <- new.magpie(getRegions(WBirrigation), 1995:2050, "ad_unit_cost")
  
  # data does not change until 2015
  yNull <- 1995
  yStop <- 1995   #2015
  years <- yStop-yNull
  for(row in 1:nrow(data)){
    value <- WBirrigation[row][,1]
    for(i in 0:years){
      cell = row + i * nrow(data)
    data[cell] <- value*1000*1.19  # multiplied by 1.19 for conversion $1995 to $2004
  }
  }
  
  # linear convergence of the data to the value of Germany until 2050
  yMax <- 2050
  years <- yMax-yNull
  value_germany <- as.numeric(data["DEU", "y1995",])
  
  for(row in 1:nrow(data)){
    value_yNull <- data[row][,1]
    for(i in which(getYears(data)==paste0("y",yStop)):years)
      {
        cell = row + i * nrow(data)
        year <- i+yNull
        data[cell] <- ((year-yStop)/(yMax-yStop)*value_germany)+((yMax-year)/(yMax-yStop)*value_yNull)
      }
  }

  # expand until 2150
  time_extend <- paste0("y",seq(2055,2150,5))
  data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
  
  getNames(data) <- NULL
  
  w <- readSource("FAO",subtype="Land")[,1995,"6610|Agricultural area.area"]
  
  return(list(x=data,weight=w,unit="US$2004", description="unit cost for irrigation expansion"))
}
