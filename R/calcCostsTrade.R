#' Calculate trade costs
#'
#' Provides REMIND data for PE tradecosts (energy losses on import).
#'
#'
#' @return REMIND data forPE tradecosts (energy losses on import) and
#' corresonding weights (1) as a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("calcCostsTrade")
#' }
#'
calcCostsTrade <- function() {
  x <- readSource("REMIND_11Regi", "tradecost")
  w <- new.magpie(getRegions(x), "y2005", fill = 1)

  return(list(
    x = x,
    weight = w,
    unit = "share",
    description = "energy costs in share (0..1)"
  ))
}
