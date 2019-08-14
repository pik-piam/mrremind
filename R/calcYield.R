#' calcYield
#' 
#' @description calculates the yield based on FAO data
#' @param physical physical area or havested area
#' @param attributes in dm, wm, ge, nr, p, k
#' @param cellular if TRUE value is calculate on cellular level
#' @param irrigation distinguish irrigation or not
#' @return MAgPIE object of yields
#' @author Debbora Leip, Jan Philipp Dietrich
#' @importFrom stats quantile

calcYield <- function(physical = TRUE, attributes="dm", irrigation=FALSE, cellular=FALSE){
  
  years <- findset("past")
  
  area       <- calcOutput("Croparea", sectoral="kcr", physical = physical, 
                           irrigation=irrigation, aggregate=FALSE,cellular=cellular)[,years,]
  production <- calcOutput("Production", aggregate=FALSE, attributes=attributes,
                           irrigation=irrigation, cellular=cellular, products="kcr")[,years,]
  
  yield      <- collapseNames(production)/area
  yield[yield==Inf | yield==-Inf | is.nan(yield) | yield==0] <- NA
  
  #if no data for begr and betr is available just take highest observed yields for the given country as replacement
  max <- as.magpie(suppressWarnings(apply(yield,1:2,max,na.rm=TRUE)))
  max[max==-Inf] <- NA
  for(b in c("begr","betr")) {
    if(all(is.na(yield[,,b]))) {
      yield[,,b] <- max
    }
  }
  
  # use lower end yield values as replacements for missing data points
  tmp <- as.magpie(apply(yield,2:3,quantile,probs=0.2,na.rm=TRUE))
  low <- yield; low[,,] <- tmp
  na  <- which(is.na(yield),arr.ind=TRUE)
  yield[na] <- low[na]
  
  
  return(list(x=yield,
              weight=area + 10^-10,
              min=0,
              unit="t/ha",
              description="Calculates the yield based on FAO",
              isocountries=!cellular))
}

