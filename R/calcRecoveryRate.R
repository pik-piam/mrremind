#' Calculate RecoveryRate
#' 
#' Provides MAgPIE-FEED data for Recovery Rate. Aggregated to MAGPIE-sectors.
#' Usually no weight needed as the data will beused in MAgPIE-FEED model
#' country based.
#' 
#' 
#' @return MAgPIE-FEED data for Recovery Rate and corresonding weights as a
#' list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("RecoveryRate")
#' 
#' }
#' @importFrom magclass getNames setNames
#' @importFrom utils read.csv

calcRecoveryRate <- function() {
  x <- readSource("WirseniusPHD")[,,"3_17",pmatch=TRUE]   
  # load sectoral mapping
  map <- read.csv(toolMappingFile("sectoral","Wirsenius_magpieFEED_mapping.csv")) 
  map <- map[map$Table=="3_17",]
  # rename sectors
  x <- x[,,as.character(map$wirsenius_items)]
  getNames(x) <- map$MAgPIE_FEED_items
  # delete sector "aussortiert", "foddr_gras", "foddr_maiz"
  x <- x[,,c("aussortiert","foddr_gras","foddr_maiz"),invert=TRUE]
  # add values for potato_cropby and cassav_cropby:
  #Recovery rates for those crop flows not displayed in the table were assumed to be close to 100 percent.(Wirsenius thesis, p. 94) 
  x <- mbind(x,setNames(x[,,1],"potato_cropby"),setNames(x[,,1],"cassav_cropby"))
  x[,,"potato_cropby"] <- 0.9
  x[,,"cassav_cropby"] <- 0.9
  
  # load weight for the aggregation
  w <- calcOutput("FAOCrop_aggr",aggregate=FALSE)
  w <- dimSums(w[,2000,"production"],dim=3)
  
  return(list(x=x,
              weight=w,
              unit="-",
              description="amount of plant mass recovered as share of the amount of plant mass generated"
               ))
}
