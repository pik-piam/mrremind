#' @title convertExpertGuess
#' @description Converts data from expert guess
#' @param x unconverted magpie object from read-script
#' @param subtype Type of data that are converted. 
#' 
#' @return magpie object with a completed dataset.
#' 
#' @seealso
#' \code{\link{convertExpertGuess}}


convertExpertGuess <- function(x,subtype) 
{
  
  if (subtype == "costsTradePeFinancial"){
    # use data for each country that belongs to a region
    # No weighting for spatial aggregation
    out <- toolAggregate(x, toolGetMapping(type = "regional", name = "regionmappingH12.csv", returnPathOnly = TRUE, where = "mappingfolder"),
                         weight=NULL)
  } else { 
    out <- x
  }

  return(out)
}
