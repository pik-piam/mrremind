#' Read dataset with population numbers completed for small islands states
#' 
#' Based on data review of werton Arauju
#' 
#' 
#' @param subtype Type: gdppast,gdpfuture,poppast,popfuture
#' @return magpie object of the completed WDI data
#' @author Ewerton Arauju
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="WDI",subtype="SP.POP.TOTL")
#' }
#' 

readWDIextended <- function(subtype="gdppast"){
  if (subtype == "gdppast") {
    data <- readRDS("gdppast.rds")
  }else if (subtype == "gdpfuture") {
    data <- readRDS("gdpfuture.rds")
  }else if (subtype == "poppast") {
    data <- readRDS("poppast.rds")
  } else if (subtype=="popfuture"){
    data <- readRDS("popfuture.rds")
  }
  return(data)  
}
