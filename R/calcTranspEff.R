#' calcTranspEff
#' calculate Transport efficiency
#' 
#' @return magpie object
#' @author Julian Oeser
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="TranspEff")
#' }
#' 


calcTranspEff <- function(){
 
  x <- readSource("REMIND_11Regi", subtype = "transpEff")
  weights <- calcOutput("FE", aggregate = FALSE, years=2005)[,,"FE|Transport (EJ/yr)"]
  
  return(list(x           = x,
              weight      = weights,
              unit        = "range 0..1, bn pkm/EJ",
              description = "regionalized transport mode shares and efficiencies"))
}
  