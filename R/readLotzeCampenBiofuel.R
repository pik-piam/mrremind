#' Read LotzeCampenBiofuel
#' 
#' Read-in Future trends in first generation bioenergy demand from the publication 
#' Lotze Campen et al. 2014. "Impacts of increased bioenergy demand on global food
#' markets: an AgMIP economic model intercomparison" Agricultural Economics 45 (103-116).
#' doi:10.1111/agec.12092.
#' from a .csv file to a magclass object
#' 
#' 
#' @return Future trends in first generation bioenergy demand in ExaJoule as magpie object
#' @author Ewerton Araujo
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("LotzeCampenBiofuel")
#' }
#' 
#' 

readLotzeCampenBiofuel <- function() {
  dataa <- read.csv("AgMIP_1stgen_bio_dem.csv",sep=";",stringsAsFactors=FALSE)
  return(as.magpie(dataa, spatial=1))
}