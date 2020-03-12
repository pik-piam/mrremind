#' @title correctLPJmL
#' @description Correct LPJmL content
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{correctLPJmL}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL", subtype="soilc", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass readLPJ

correctLPJmL <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
