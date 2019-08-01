#' Calculate ProdSystRatio for 2000
#' 
#' Provides MAgPIE-FEED data for ProdSystRatio.Usually no weight needed as the
#' data will be used in MAgPIE-FEED model country based. Data aslo used as
#' input for MAgPIE, then an aggregation to regions is needed
#' 
#' 
#' @return MAgPIE[-FEED] data for ProdSystRatio and corresonding weights as a
#' list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ProdSystRatio_2000")
#' 
#' }
#' 
calcProdSystRatio_2000 <- function() {
  x <- readSource("Wirsenius")
  
  # load weight for the aggregation
  w <- calcOutput("FAOLivePrim",aggregate=FALSE)[,2000,]  #FIXME - which sector?
  
  return(list(x=x,
              weight=w,
              unit="-",
              description="share of livestock products generated in different livestock subsystems"
               ))
}
