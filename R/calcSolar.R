#' calcSolar
#' calculate Area, Capacity and Energy for photovoltaics (PV) and contentrated solar power (CSP)
#' 
#' @return magpie object
#'
#' @author Julian Oeser, modified by Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="Solar")
#' }
#' 
#'
#' @importFrom utils head
#' @importFrom magclass where


calcSolar <- function() {
  
  x <- readSource("DLR")
  
  # calculate area values for CSP technology using formula: CSP capacity / (176.19 MW therm / km?)
  x[,,"CSP"][,,"area"] <- (x[,,"capacity"][,,"CSP"]/176.19)
  
  # adjust CSP capacities using two correction factors: csp capacity * 0.37 (efficiency thermal -> electric) * 0.33 (scale down for solar multiple 3)
  
  x[,,"capacity"][,,"CSP"] <- (x[,,"capacity"][,,"CSP"]*0.37*0.33)
  
  # correction for PV capacity in Japan in bin 1020
  
  x["JPN",,"PV"][,,"capacity"][,,"1020"] <- (x["JPN",,"PV"][,,"capacity"][,,"1020"]+100000)
  
  # correction for area in Japan in bin 1020
  
  x["JPN",,"area"][,,"1020"] <- (x["JPN",,"area"][,,"1020"]+100000)/90
  
  # calculate distance classes 50-100 and 100-inf based on differences between classes
  
  x <- add_columns(x, addnm = c("50-100", "100-inf"), dim = 3.3)
  x[,,"50-100"] <- x[,,"0-100"]-x[,,"0-50"]
  x[,,"100-inf"] <- x[,,"0-inf"]-x[,,"0-100"]
  
  # adjust for negative values in differences
  
  # print warning for countries where negative values make up more than 1% of positive values
  x.pos <- x
  x.pos[x.pos<0]=0
  x.neg <- x
  x.neg[x.neg>0]=0
  
  y  <- dimSums(x.neg[,,"50-100"], dim=3.4) / dimSums(x.pos[,,c("50-100", "0-50")], dim=c(3.4, 3.3))
  countries.neg <- where(y< -0.01)$true$regions
  vcat(2, paste0("In the following countries negative values made up more than 1% in newly created distance bin 50-100: ", countries.neg))
  
  # set negative values to 0
  x[,,"50-100"][x[,,"50-100"]<0] <- 0
  x[,,"100-inf"][x[,,"100-inf"]<0] <- 0
  
  return(list(x=x,
              weight=calcOutput("FE", aggregate = FALSE)[,"y2015","FE|Electricity (EJ/yr)"],
              unit="Area in km2; Capacity factor in share of year; Energy in EJ",
              description="Area (limitGeopot), Capacity factor (nur) and Energy (maxprod) for photovoltaics (spv) and contentrated solar power (csp)",
              aggregationFunction=toolSolarFunctionAggregate
  ))
  
}
