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
  ## the data is provided for energy service per "fossil-fuel equivalents".
  ## to obtain the ES for a given motive energy, we have to divide by
  ## tank-to-wheel efficiencies
  #x[,, "Eff_Pass_LDV"] <- x[,, "Eff_Pass_LDV"] / 0.22
  #x[,, c("Eff_Pass_nonLDV", "Eff_Freight")] <- x[,, c("Eff_Pass_nonLDV", "Eff_Freight")] / 0.24
  weights <- calcOutput("FE", aggregate = FALSE, years=2005)[,,"FE|Transport (EJ/yr)"]


  
  return(list(x           = x,
              weight      = weights,
              unit        = "range 0..1, bn pkm/EJ",
              description = "regionalized transport mode shares and efficiencies"))
}
