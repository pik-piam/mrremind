#' @title calcEmisNitrogenAWMSPast
#' @description calculates nitrogenous emissions from animal waste management systems in the historical period
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenAWMSPast")
#' }
#' 


calcEmisNitrogenAWMSPast<-function(){
  past<-findset("past")
  excretion<-collapseNames(calcOutput("Excretion",aggregate = FALSE)[,,"confinement"][,,"nr"])
  awms_shr<-collapseNames(calcOutput("AWMSconfShr",aggregate = FALSE)[,past,"ssp2"])
  ef<-calcOutput("EF3confinement",selection=NULL,aggregate = FALSE)
  out<-excretion*awms_shr*ef
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr in various forms",
    description="Nitrogen emissions from animal waste management systems"))
}