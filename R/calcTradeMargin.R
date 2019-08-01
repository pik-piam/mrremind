#' @title calcTradeMargin
#' @description calculate total value of trade margins from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' }
#' @param producer_price which producer price should be used
#' @param bilateral  whether bilateral trade margin should be calculated
#' 
#' @return Trade margins as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#'     \dontrun{
#'     x <- calcTradeMargin("GTAP7")
#'     }
#' @importFrom magpiesets findset





calcTradeMargin <- function(gtap_version ="GTAP7", bilateral = FALSE,producer_price ="FAO"){
  stopifnot(gtap_version %in% c("GTAP7","GTAP8")) 
  viws <- calcOutput(type = "GTAPTrade",subtype = paste(gtap_version,"VIWS",sep="_"),bilateral = bilateral,aggregate = FALSE)
  vxwd <- calcOutput(type = "GTAPTrade",subtype = paste(gtap_version,"VXWD",sep="_"),bilateral = bilateral,aggregate = FALSE)
  vtwr <- viws - vxwd
  
  vtwr[vtwr < 0] <- 0
  

  vxmd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXMD",sep="_"),bilateral = bilateral,aggregate = FALSE)
  vom <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOM",sep="_"),bilateral = bilateral,aggregate = FALSE)
  voa <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOA",sep="_"),bilateral = bilateral,aggregate = FALSE)
  
  y <- setYears(vtwr/vxmd* (vom/voa),NULL)
  y[is.infinite(y)] <- NA
  
  fillMean <- function(x){
    stopifnot(is.magpie(x))
    for (k in getNames(x)){
      mean <- mean(x[,,k],na.rm = TRUE)
      x[,,k][is.na(x[,,k])] <- mean
    }
   return(x)
  }
  
  y <- fillMean(y)
  
  if(is.null(producer_price)){
    producer_price <- "FAOp"
  } 
  if (producer_price %in% c("IMPACT3.2.2World_Price" , "FAO", "FAOp","WBGEM")){
    p <- collapseNames(calcOutput("PriceAgriculture",datasource =producer_price,aggregate = FALSE))[,2005,]
  } else (stop("Valid food price is required"))
  
  k_trade <- findset("k_trade")
  k_trade <- intersect(intersect(getNames(p),getNames(y)),k_trade)
  out <- y[,,k_trade]*p[,,k_trade]
  out <- toolCountryFill(out,0)
 
  
  weight <- setYears(vxmd *voa,NULL)[,,k_trade]
  weight <- toolCountryFill(weight,0)
  unit <- "US$05"
  description <- "Trade margins"
 
  return(list(x=out,
              weight = weight,
              unit = unit,
              description = description))
}
