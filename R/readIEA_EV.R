
#' Read IEA_EV
#' 
#' Read-in IEA_EV
#' 
#' 
#' @return IEA data on electric vehicles as magpie object
#' @author Lavinia Baumstark, Christoph Bertram
#' @seealso \code{\link{readSource}}
#' @param subtype Which data: combSales, combStock, bevSales, bevStock, phevSales, or phevStock
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IEA_EV")
#' }
#' @importFrom readxl read_excel
#' 
#' 
readIEA_EV <- function(subtype) {
  file <- "GlobalEVOutlook2019_TableA1-A6.xlsx"
  #subtype = combSales, combStock, bevSales, bevStock, phevSales, phevStock
  ev <- read_excel(file, sheet=subtype,skip=1,col_types = c("text",rep("numeric",14)))
  names(ev)[1] <- "region"
  #names(ev)  <- gsub('X','y',names(ev))
  ev <- as.data.frame(ev)
  ev <- as.magpie(ev,spatial=1,datacol=2)
  return(ev)
}  
