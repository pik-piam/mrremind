#' Read WBirrigation
#' 
#' Read-in an WBirrigation data .csv file as magclass object
#' from Jones, William I. 1995. "World Bank and Irrigation." Washington, D.C.: World Bank.
#' 
#' @return magpie object of the WBirrigation data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WBirrigation")
#' }
#' 
readWBirrigation <- function() {    
      irr <- read.csv("irrigation.csv", sep=";", row.names=1)
      regions <- c("East and South Asia"="ESA","East Asia"="EAS", "South Asia"="SAS", "India"="IND", "Europe"="ERP", "Middle East"="MET", "Africa" ="AFR", "North Africa"="NAF", "Sub-Saharan Africa"="SAF", "Latin America and Caribbean"="LAC")
      row.names(irr) <- regions #better: independent ordering"
      irr <- as.magpie(irr)
      irr <- irr[,,"ad_unit_cost"]
      getYears(irr) <- "y1995"
      return(irr)
}  
