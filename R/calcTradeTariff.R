#' @title calcTradeTariff
#' @description calculate tarde tariffs from GTAP dataset
#' @param gtap_version type of GTAP data version
#' \itemize{
#' \item \code{GTAP7}
#' \item \code{GTAP8}
#' }
#'
#' @param type_tariff which producer price should be used
#' \itemize{
#' \item \code{type_tariff}
#' }
#' 
#' @return Trade tariffs as an MAgPIE object
#' @author Xiaoxi Wang
#' @examples
#'     \dontrun{
#'     x <- calcTradeTariff("GTAP7")
#'     }
#' @importFrom madrat toolAggregate toolMappingFile
#' @importFrom reshape2 acast
#' @importFrom magclass as.data.frame add_columns
#' @importFrom magpiesets findset
#' 

calcTradeTariff<- function(gtap_version = "GTAP7", type_tariff = "total"){
  stopifnot(gtap_version %in% c("GTAP7","GTAP8"))
  vom  <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOM", sep ="_"),aggregate = FALSE, bilateral = F)
  voa  <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VOA", sep ="_"),aggregate = FALSE, bilateral = F)
  vxmd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXMD", sep ="_"),aggregate = FALSE, bilateral = F)
  viws <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIWS", sep ="_"),aggregate = FALSE, bilateral = F)
  x <- vom[,,getNames(viws)]/voa[,,getNames(viws)]
  fillMean <- function(x){
    stopifnot(is.magpie(x))
    for (k in getNames(x)){
      mean <- mean(x[,,k],na.rm = TRUE)
      x[,,k][is.na(x[,,k])] <- mean
    }
    return(x)
  }
  
  x <- fillMean(x)
  if (type_tariff == "export"){
    #export tax: positive values are taxes, negative values are export subsidies
    
    vxwd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXWD",sep="_"), aggregate = FALSE, bilateral = F)
    y <- (vxwd-vxmd)/vxmd
    y[is.nan(y)] <-NA
  }else if (type_tariff == "import"){
    #import tariff: positive values are tariffs
    
    vims <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIMS",sep="_"), aggregate = FALSE, bilateral = F)
    y <- (vims - viws)/vxmd
    y[is.nan(y)] <- NA
    y[is.infinite(y)] <- NA
  }else if (type_tariff == "total"){
    vxwd <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VXWD",sep="_"), aggregate = FALSE, bilateral = F)
    vims <- calcOutput(type ="GTAPTrade",subtype = paste(gtap_version,"VIMS",sep="_"), aggregate = FALSE, bilateral = F)
    y<- (vxwd-vxmd + vims - viws)/vxmd
    y[is.nan(y)] <- NA
    y[is.infinite(y)] <- NA
  }else  {stop(paste("type_tariff",type_tariff, "is not supported!"))}
  
  y <- fillMean(y)*x
  
  k_trade <- findset("k_trade")
  missing <- setdiff(k_trade,getNames(y))
  y <- add_columns(y,dim = 3.1,addnm = missing)
  y[,,missing]=0
  y<- y[,,setdiff(getNames(y),k_trade),invert=TRUE]
  
  p <- collapseNames(calcOutput("PriceAgriculture",datasource = "FAO",aggregate = FALSE))[,2005,]
  
  p_glo<-calcOutput("IniFoodPrice", aggregate=FALSE, products="k_trade")
  
  missing2 <- setdiff(getNames(p_glo),getNames(p))
  p <- add_columns(p,dim=3.1,addnm =missing2 )
  
  for (i in missing2){
    p[,,i] <- as.numeric(p_glo[,,i])
    }


  out <- setYears(y[,,k_trade],NULL)*p[,,k_trade]
  out <- toolCountryFill(out,0)
  
  # set trade tariffs of ruminant meant in Inida high to prevent exports; Japan too
  out["IND",,"livst_rum"]   <- 10^5
  out["JPN",,"livst_rum"]   <- 10^5
  out["JPN",,"livst_pig"]   <- 10^5
  out["JPN",,"livst_chick"] <- 10^5
  out["JPN",,"livst_egg"]   <- 10^5
  out["JPN",,"livst_milk"]  <- 10^5
  # set trade tariffs of alcohol in Japan high to prevent unrealistic exports
  out["JPN",,"alcohol"]  <- 10^5
  
  # use sugar tariffs as a surrogate for sugar crops
  out[,,"sugr_cane"] <- setNames(out[,,"sugar"],"sugr_cane")
  out[,,"sugr_beet"] <- setNames(out[,,"sugar"],"sugr_beet")


  weight <- setYears(toolCountryFill(vxmd*voa[,,getNames(viws)],0),NULL)
  missing <- setdiff(k_trade,getNames(weight))
  weight <- add_columns(weight,dim=3.1,addnm = missing)
  weight[,,missing] <- 1
  weight <- weight[,,setdiff(getNames(weight),k_trade),invert=TRUE]
  
  
  description <- paste0(type_tariff," trade tariff")
  unit <- "USD05MER/tDM"
  
  return(list(x=out,
              weight = weight,
              unit = unit,
              description = description))
  
}
