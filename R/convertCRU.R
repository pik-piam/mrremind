#' @title convertCRU
#' @description Convert CRU content
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readCRU}},
#' \code{\link{read.LPJ_input}}
#' @examples
#'
#' \dontrun{
#'   readSource("CRU", subtype="precipitation", convert=TRUE)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input

convertCRU <- function(x){

  return(x)
}
