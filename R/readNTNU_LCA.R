#' Read NTNU_LCA
#' 
#' Read-in an NTNU_LCA data .xlsx file as magclass object
#' 
#' 
#' @return magpie object of the NTNU_LCA data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="NTNU_LCA")
#' }
#' @importFrom magclass read.report
readNTNU_LCA <- function() {
  x <- read.report("NTNU_LCA.csv",as.list=FALSE)
  return(x)
}  
