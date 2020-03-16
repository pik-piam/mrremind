#' Calculate FAO Livestock Primary aggregated
#' 
#' Provides the FAO Livestock Primary data aggregated to MAgPIE categories.
#' 
#' 
#' @return FAO Livestock Primary data and corresonding weights as a list of two
#' MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{calcFAOLivePrim}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FAOLivePrim_aggr")
#' 
#' }
#' @importFrom utils read.csv
#' @importFrom magclass fulldim

calcFAOLivePrim_aggr <- function() {
  
  ## read in the file
  FAOLive <- readSource("FAO", "LivePrim")
  
  if (any( grepl("+ (Total)",getNames(FAOLive, fulldim=T)[[1]], fixed = TRUE))) {
    FAOLive <- FAOLive[,,"+ (Total)", pmatch=T, invert=T]
  }
  
  aggregation <- toolGetMapping("FAOitems.csv", type = "sectoral", where="mappingfolder")

  FAOLive[is.na(FAOLive)] <- 0
  
  FAO_out <- toolAggregate(FAOLive, rel=aggregation, from="ProductionItem", to="k", dim=3.1, partrel = TRUE)
  
  
  if(any(fulldim(FAO_out)[[2]][[3]]=="")) {
    if (sum(FAO_out[,,""]) == 0) {
      FAO_out <- FAO_out[,,"", invert=T]
    } else  {
      cat('Aggregation created entries without name (""), but containing information')
    }
  }
  
  if(any(fulldim(FAO_out)[[2]][[3]]=="remaining")) {
    remain_prod <- mean( dimSums(FAO_out[,,"remaining.production"], dim=1)/dimSums(dimSums(FAO_out[,,"production"], dim=3), dim=1) )
    if (remain_prod > 0.02) cat("Remaining production is", round(remain_prod,digits = 3)*100, "% of total \n")
    FAO_out <- FAO_out[,,"remaining", invert=T]  
  }
  
  
  return(list(x=FAO_out,weight=NULL, unit="if not specified differently in the dimension name in tonnes", description="FAO Livestock Primary Production information aggregated to magpie categories"))
}
