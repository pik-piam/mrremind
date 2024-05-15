#' Read Macknic Intensities
#' 
#' Read in Macknick (2011) data on water consumption and withdrawal
#' coefficients per electricity technology
#' 
#' 
#' @param subtype Type of Macknick data that should be read. Available types
#' are: \itemize{ \item \code{data}: The original Macknick source data \item
#' \code{missingAssumed}: Additional data to fill gaps }
#' @return MAgPIE object of the Macknick (2011) data
#' @author Ioanna Mouratiadou
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="MacknickIntensities", convert = FALSE)
#' }
readMacknickIntensities <- function(subtype) {  
  
  sheets <- c(data=1, missingAssumed=2)
  sheet <- toolSubtypeSelect(subtype,sheets)
  
  x <- as.data.frame(read_excel("MacknickIntensities.xlsx", sheet = sheet))
  x <- x[,!is.na(names(x))]
  
  return(as.magpie(x))
}  
