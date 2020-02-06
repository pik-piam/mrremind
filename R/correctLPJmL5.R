#' @title correctLPJmL5
#' @description Correct LPJmL 5 content
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{correctLPJmL5}}
#' @examples
#'
#' \dontrun{
#' readSource("LPJmL5", subtype="soilc", convert="onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass readLPJ

correctLPJmL5<-function(x){
  toolConditionalReplace <- function(){}
  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
