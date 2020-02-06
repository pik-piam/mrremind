#' @title correctCRU
#' @description Correct CRU content
#' @param x magpie object provided by the read function
#' @param subtype Switch between diffrent input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readCRU}},
#' \code{\link{read.LPJ_input}}
#' @examples
#'
#' \dontrun{
#'   readSource("CRU", subtype="precipitation", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input

correctCRU <- function(x, subtype){
  toolConditionalReplace <- function(){}
  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)
  if(subtype %in% c("precipitation", "potential_evap")){
    x <- toolConditionalReplace(x, conditions = c("<0"), replaceby = 0)
  }
  x <- toolCell2isoCell(x)

  return(x)
}
