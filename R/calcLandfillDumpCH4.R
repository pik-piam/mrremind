#' @title calcLandfillDumpCH4
#' @description Mt CH4 from landfills and dumps (potential for biogas), based on What a waste projections and emissions
#' @return Magpie object of CH4 from global landfills and dumps
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="LandfillDumpCH4")
#' }

calcLandfillDumpCH4 <- function(){
  
x<-calcOutput("WasteEmissions",treatment="swds",yearly = FALSE, aggregate=F)
x<-dimSums(x, dim=3.1)
out <- x/24 #for CO2eq back to CH4


return(list(
  x=out,
  weight=NULL,
  unit="Mt CH4",
  description="CH4 emissions from landfills and dumps"))

}