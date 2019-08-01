#' Aggregate forest area designated for production values from source
#' FAO_forestry_production
#' 
#' Aggregates forest area designated for production values from source
#' FAO_forestry_production over the magpie-regions. A
#' weight for the aggregation is not implemented
#' 
#' 
#' @return writes a magpie object with the new regions to the output-folder
#' defined in the file "exampleconfig.cfg"
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}},
#' \code{\link{readFAO_forestry}}, \code{\link{calcOutput}}

calcForestProdArea <- function() {
  
  x   <- readSource("FAO_forestry","production")
  
  ## calculate data for 1995 as the mean between 1990 and 1995, and keep values constant after 2010
  dat <- new.magpie(getRegions(x), seq(1990,2150,5))
  dat[,getYears(x),] <- x[,,]
  dat[,1995,] <- rowMeans(dat[,c(1990,2000),])
  dat[,seq(2015,2150,5),] <- setYears(dat[,2010,])
  
  return(list(x=dat,weight=NULL,unit="Mha",description="Forest area designated for production from 1990-2010 (FRA2010). Constant thereafter"))
}