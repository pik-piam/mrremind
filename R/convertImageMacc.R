#' Convert subtypes of the ImageMacc data
#' 
#' Convert subtypes from ImageMacc to data on ISO country level. Correct values
#' for N2O of the subtype "baseline_sources" from N to N2O (factor: 44/28.
#' 
#' 
#' @param x MAgPIE object containing ImageMacc data mixed on region level
#' @param subtype data subtype. Either CH4_Energy_Industry", "CH4_Landuse",
#' "N2O_Energy_Industry", "N2O_Landuse", "HFC_tot", "SF6_tot", "PFC_tot" or
#' "baseline_sources"
#' @return ImageMacc data as MAgPIE object for all subtypes aggregated to
#' country level
#' @author Nele Steinmetz
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("ImageMacc","CH4_Energy_Industry")
#' a <- readSource("ImageMacc","CH4_Landuse")
#' a <- readSource("ImageMacc","N2O_Energy_Industry")
#' a <- readSource("ImageMacc","N2O_Landuse")
#' a <- readSource("ImageMacc","HFC_tot")
#' a <- readSource("ImageMacc","SF6_tot")
#' a <- readSource("ImageMacc","PFC_tot")
#' a <- readSource("ImageMacc","baseline_sources")
#' }
#' 
convertImageMacc <- function(x,subtype) {
   map <- "regionmappingImageMacc.csv"
  
  if(subtype=="CH4_Energy_Industry"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="CH4_Landuse") {
    y <- toolAggregate(x,map)
  }
  if(subtype=="N2O_Energy_Industry"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="N2O_Landuse"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="HFC_tot"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="SF6_tot"){
    y <- toolAggregate(x,map)
  }
  if(subtype=="PFC_tot"){
    y <- toolAggregate(x,map)
  }
  else if(subtype=="baseline_sources"){
    # values for N2O have to be corrected by the factor 44/28 (N -> N2O)
    x[,,"N2O Transport"] <- x[,,"N2O Transport"]*(44/28)
    x[,,"N2O Adipic acid production"] <- x[,,"N2O Adipic acid production"]*(44/28)
    x[,,"N2O Nitric acid production"] <- x[,,"N2O Nitric acid production"]*(44/28)
    x[,,"N2O Fertilizer"] <- x[,,"N2O Fertilizer"]*(44/28)
    x[,,"N2O Animal waste"] <- x[,,"N2O Animal waste"]*(44/28)
    x[,,"N2O Domestic sewage"] <- x[,,"N2O Domestic sewage"]*(44/28)
    y <- toolAggregate(x,map)
  }
  
  return(y)
}
