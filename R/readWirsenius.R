#' Read Wirsenius
#' 
#' Read-in an Wirsenius data .csv file as magclass object
#' 
#' 
#' @return magpie object of the Wirsenius data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Wirsenius")
#' }
#' 
readWirsenius <- function() {
      wir <- read.csv("prod_system_ratio.csv", sep=";", header=TRUE)
      wir <- as.magpie(wir, datacol=3)
      getYears(wir) <- "y2000"
      return(wir)
}  
