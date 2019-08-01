#' @title calcBiogasProdMSW
#' @description Mt CH4 captured from landfills and dumps 
#' @return Magpie object of CH4 captured
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="BiogasProdMSW")
#' }

calcBiogasProdMSW <- function(){

x <- readSource("Waste", subtype="Treatment")
x <- x[,,"sanitary_landfill_landfill_gas_system"]/dimSums(x, dim=3, na.rm=T)

a <- readSource("LandfillCH4Capture")

y <- calcOutput("LandfillDumpCH4", aggregate=F)

out <- setYears(a, NULL)*setYears(x, NULL)*y
return(list(
  x=out,
  weight=NULL,
  unit="Mt CH4",
  description="CH4 emissions from landfills and dumps"))
}