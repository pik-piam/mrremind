#' CapacityEV
#' calculate capacity of EV
#' 
#' @return magpie object
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="CapacityEV")
#' }
#' 


calcCapacityEV <- function() {
 
  x <- readSource("IEA_EV",subtype="combStock")
  # convert unit
  # 1Mio cars =	1/650 cap = 0.00154
  x <- x/650000
  # set all NA to 0
  x[is.na(x)] <- 0
  
  getNames(x) <- NULL
 
  return(list(x      = x,
              weight = NULL,
              unit        = "TWa",
              description = "capacity of electric vehicles"))
  
}
