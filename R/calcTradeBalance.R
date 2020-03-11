#' Calculate imports/exports
#' 
#' Calculate the difference between production and
#' domestic_supply. Numbers till 2010 are derived from FAO.
#' Numbers after 2010 are hold constant
#' 
#' @return regional trade balances
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcTradeBalance()
#' }
#' 
calcTradeBalance <- function() {
  
  kall <- findset("kall")
  x <- calcOutput("FAOmassbalance",aggregate = FALSE)[,,kall]

  balance <- collapseNames(x[,,"production.dm"]) - collapseNames(x[,,"domestic_supply.dm"])
  
  t <- findset("time")
  out <- time_interpolate(balance,t,extrapolation_type="constant")
  
  return(list(x=out,
              weight=NULL,
              unit="mio. ton dm",
              description="Trade balances derived from FAO for historic values and hold constant after 2010")
  )
}
