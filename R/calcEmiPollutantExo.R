#' calcEmiPollutantExo
#' calculate EmiPollutantExo based on RCP data
#' 
#' @return magpie object
#' @author Julian Oeser
#' @param subtype Either 'Waste' or 'AviationShipping'
#' @param aviationshippingsource Defines source for aviation and shipping emissions. Either 'RCP' or 'LeeGAINS'.
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="EmiPollutantExo")
#' }
#' 
#'
#' @importFrom magclass getSets getYears time_interpolate complete_magpie


calcEmiPollutantExo <- function(subtype, aviationshippingsource="RCP"){
  
  if (subtype=="Waste") {
    
    x <- readSource("RCP", subtype="Waste")
    
    # scale to EDGAR data
    ch4 <- readSource("EDGAR", subtype="ch4_history")[,2005,c("6A","6B","6C","6D")] / 1000  # convert from Gg to Tg  
    so2 <- readSource("EDGAR", subtype="SO2")[,2005,c("6A","6B","6C","6D")]         / 1000  # convert from Gg to Tg 
    bc  <- setNames(collapseNames(readSource("CEDS", subtype="BC")[,2005,c("5A_Solid-waste-disposal",
                                                                           "5C_Waste-incineration",
                                                                           "5D_Wastewater-handling",
                                                                           "5E_Other-waste-handling")]),c("6A","6B","6C","6D")) / 1000  # convert from kt to Tg 
    getSets(bc) <- getSets(ch4)
    oc  <- setNames(collapseNames(readSource("CEDS", subtype="OC")[,2005,c("5A_Solid-waste-disposal",
                                                                           "5C_Waste-incineration",
                                                                           "5D_Wastewater-handling",
                                                                           "5E_Other-waste-handling")]),c("6A","6B","6C","6D")) / 1000  # convert from kt to Tg 
    getSets(oc) <- getSets(ch4)
    co  <- readSource("EDGAR", subtype="CO")[,2005,c("6A","6B","6C","6D")]          / 1000  # convert from Gg to Tg 
    voc <- readSource("EDGAR", subtype="VOC")[,2005,c("6A","6B","6C","6D")]         / 1000  # convert from Gg to Tg 
    #nh3 <- readSource("EDGAR", subtype="NH3")[,2005,c("6A","6B","6C","6D")]         / 1000  # convert from Gg to Tg 
    nox <- readSource("EDGAR", subtype="NOx")[,2005,c("6A","6B","6C","6D")]         / 1000  # convert from Gg to Tg 
    
    ch4 <- add_dimension(ch4, dim=3.1, "type", "ch4")
    so2 <- add_dimension(so2, dim=3.1, "type", "so2")
    bc  <- add_dimension(bc,  dim=3.1, "type", "bc")
    oc  <- add_dimension(oc,  dim=3.1, "type", "oc")
    co  <- add_dimension(co,  dim=3.1, "type", "CO")
    voc <- add_dimension(voc, dim=3.1, "type", "VOC")
    #nh3 <- add_dimension(nh3, dim=3.1, "type", "NH3")
    nox <- add_dimension(nox, dim=3.1, "type", "NOx")
    
    scale_data           <- collapseNames(mbind(ch4,so2,bc,oc,co,voc,nox))
    scale_data           <- dimSums(scale_data,dim=3.2)
    getYears(scale_data) <- NULL
    getSets(scale_data)  <- c("region","year","type")
    x                 <- complete_magpie(x,fill=0)
    scale_x           <- x[,2005,]
    getYears(scale_x) <- NULL
    
    x <- x * scale_data / scale_x
    x[is.na(x)] <- 0
    
    # interpolate time
    y <- getYears(x, as.integer = TRUE)
    required_year <- seq(2000, 2150, by=5)
    interpolated_year <- required_year[!(required_year %in% y)]
    out <- time_interpolate(x, interpolated_year, TRUE, extrapolation_type = "constant")
    
    getSets(out) <- getSets(x)
    
    
  } else if (subtype=="AviationShipping") {
    
    if (aviationshippingsource == "RCP") {
      x <- readSource("RCP", subtype="AviationShipping", convert=FALSE)
    } else if (aviationshippingsource == "LeeGAINS") {
      shipping <- readSource("ECLIPSE", subtype="shipping.emi", convert = FALSE)
      avi <- readSource("Lee", subtype="emi")
      x <- mbind(shipping, avi)
      getSets(x) <- c("region", "year", "source", "type", "scenario")
      dimnames(x)[[3]] <- gsub(pattern = "CH4", replacement = "ch4", dimnames(x)[[3]])
      dimnames(x)[[3]] <- gsub(pattern = "BC", replacement = "bc", dimnames(x)[[3]])
      dimnames(x)[[3]] <- gsub(pattern = "SO2", replacement = "so2", dimnames(x)[[3]])
    }
    
    # scale to EDGAR data
    ch4 <- readSource("EDGAR", subtype="ch4_history")[,2005,c("1C1","1C2")] / 1000  # convert from Gg to Tg 
    so2 <- readSource("EDGAR", subtype="SO2")[,2005,c("1C1","1C2")]         / 1000  # convert from Gg to Tg 
    bc  <- setNames(collapseNames(readSource("CEDS", subtype="BC")[,2005,c("1A3ai_International-aviation","1A3di_International-shipping")]),c("1C1","1C2")) / 1000  # convert from kt to Tg 
    getSets(bc) <- getSets(ch4)
    oc  <- setNames(collapseNames(readSource("CEDS", subtype="OC")[,2005,c("1A3ai_International-aviation","1A3di_International-shipping")]),c("1C1","1C2")) / 1000  # convert from kt to Tg 
    getSets(oc) <- getSets(ch4)
    co  <- readSource("EDGAR", subtype="CO")[,2005,c("1C1","1C2")]          / 1000  # convert from Gg to Tg 
    voc <- readSource("EDGAR", subtype="VOC")[,2005,c("1C1","1C2")]         / 1000  # convert from Gg to Tg 
    nox <- readSource("EDGAR", subtype="NOx")[,2005,c("1C1","1C2")]         / 1000  # convert from Gg to Tg
    
    ch4 <- add_dimension(ch4, dim=3.1, "type", "ch4")
    so2 <- add_dimension(so2, dim=3.1, "type", "so2")
    bc  <- add_dimension(bc,  dim=3.1, "type", "bc")
    oc  <- add_dimension(oc,  dim=3.1, "type", "oc")
    co  <- add_dimension(co,  dim=3.1, "type", "CO")
    voc <- add_dimension(voc, dim=3.1, "type", "VOC")
    nox <- add_dimension(nox, dim=3.1, "type", "NOx")
    
    scale_data           <- collapseNames(mbind(ch4,so2,bc,oc,co,voc,nox))
    scale_data           <- dimSums(scale_data,dim=1)
    getNames(scale_data) <- gsub("1C1","Aviation",getNames(scale_data))
    getNames(scale_data) <- gsub("1C2","InternationalShipping",getNames(scale_data))
    getYears(scale_data) <- NULL
    getSets(scale_data)  <- c("region","year","type","source")
    x                 <- complete_magpie(x,fill=0)
    scale_x           <- x[,2005,]
    getYears(scale_x) <- NULL
    
    x <- x * scale_data / scale_x
    x[is.na(x)] <- 0
    
    # interpolate time
    y <- getYears(x, as.integer = TRUE)
    required_year <- seq(2000, 2150, by=5)
    interpolated_year <- required_year[!(required_year %in% y)]
    out <- time_interpolate(x, interpolated_year, TRUE, extrapolation_type = "constant")
    
    getSets(out) <- getSets(x)
    
    
  } else {
    stop("Invalid subtype. Must be 'Waste' or 'AviationShipping'")
  }
  
  
  return(list(x=out,
              weight=NULL,
              unit="Tg/yr",
              description="Emissions from Aviation and International Shipping from 2000 to 2150"))
  
}