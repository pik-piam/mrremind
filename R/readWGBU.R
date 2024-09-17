#' Read WGBU
#'
#' Read-in an WGBU xlsx file as magclass object
#'
#'
#' @return magpie object of WGBU
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "WGBU")
#' }
#'
readWGBU <- function() {
  # read in data
  x <- as.data.frame(read_excel("Global Technical Potential of Hydro Power_WGBU_adjusted.xlsx", sheet = 1))
  x <- x[!is.na(x[[1]]), ]
  # delete last not meaningful dimension
  x <- x[-73, ]
  # substitute . in region names
  x$Land <- gsub("\\.", ",", x$Land)
  # turn into a magclass object
  x <- as.magpie(x, spatial = 1)
  # clean up the names
  getNames(x) <- gsub("\\.", " ", getNames(x))
  return(x)
}
