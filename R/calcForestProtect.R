#' Aggregate forest area with protected areas values from source
#' FAO_forestry_protection
#' 
#' Aggregates forest area within protected areas values from source
#' FAO_forestry_protection over the magpie-regions. NAs get replaced by zero. A
#' weight for the aggregation is not implemented
#' 
#' 
#' @return writes a magpie object with the new regions to the output-folder
#' defined in the file "exampleconfig.cfg"
#' @author Nele Steinmetz
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}},
#' \code{\link{readFAO_forestry}}, \code{\link{calcOutput}}
#' @importFrom magclass getNames nyears

calcForestProtect <- function() {
  
  x   <- readSource("FAO_forestry","protection")
  e   <- readSource("FAO_forestry","extent")
  
  p <- time_interpolate(round(x/e,3), seq(1995,2010,5))
  p[is.nan(p)] <- 0
  p[p>1] <- 1
  
  years <- seq(1995,2150,5)
  
  .target <- function(target,type,save_fore_protect,years,prod_shr) {
    tmp <- time_interpolate(save_fore_protect,years)
    for (y in 5:nyears(tmp)) {
      tmp[,y,] <- setYears(tmp[,4,],getYears(tmp[,y,]))
    }
    if (type == "absolut") {
      aim <- target-prod_shr
    } else if (type == "relative") {
      aim <- save_fore_protect[,4,]*target
    }
    tmp <- convergence(tmp,aim,start_year="y2010",end_year="y2100",type="linear")
    for (y in years) {
      tmp[which(tmp[,y,] > 1-prod_shr),y,] <- 1-prod_shr[which(tmp[,y,] > 1-prod_shr),,]
    }
    tmp[tmp < 0] <- 0
    return(tmp)
  }
  
  ps <- calcOutput("ForestProd",aggregate=FALSE)
  
  #SSP1/SSP4/B1/B2 - high forest protection
  high <- .target(4,"relative",p,years,ps)
  getNames(high) <- "high"
    
  #SSP2 - medium forest protection
  med <- .target(1.5,"relative",p,years,ps)
  getNames(med) <- "medium"
  
  #SSP3/SSP5/A1/A2 - low forest protection
  low <- .target(1,"relative",p,years,ps)
  getNames(low) <- "low"
  
  x <- mbind(high,med,low)
  
  e <- time_interpolate(e,getYears(x),extrapolation_type="constant")
  
  return(list(x=x,weight=e,unit="-",description="Share of total forest area designated for protection"))
}
