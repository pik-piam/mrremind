#' Calculate FAO Livestock Production
#' 
#' Provides the FAO Livestock Production data aggregated to MAgPIE categories
#' in million ton wet matter.
#' 
#' 
#' @return FAO Livestock Production (WM) and corresonding weights as a list of
#' two MAgPIE objects
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LivstProduction")
#' 
#' }
#' 
calcLivstProduction <- function() {

  prod <- collapseNames(calcOutput("Production",products="kli",attributes="wm",aggregate = F)) 

  prod[is.nan(prod)] <- 0
  prod[prod < 0] <- 0

  return(list(x=prod,
              weight=NULL,
              unit="mio ton WM",
              description="FAO livestock production aggregated to MAgPIE categories")
  )
}
