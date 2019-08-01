#' @title calcEmisNitrogenPast
#' @description 
#' Emission factors from cropland soils.
#'
#' @param method If IPCC, using the ipcc emission factors as share of applied N inputs. If Nloss, as share of cropland budget surplus.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenPast")
#' }
#' 


calcEfNSoil<-function(method="IPCC"){
  EfNSoil<-setYears(readSource("IPCC","efnsoil",convert = FALSE),NULL)
  weight=NULL
  if (method=="Nloss") {
    surplus<-dimSums(calcOutput("NitrogenBudgetCropland")[,,"surplus"],dim=1)
    emis<-dimSums(calcOutput("EmisNitrogenCroplandPast",method="IPCC"),dim=c(1,3.2))
    EfNSoil<-emis/surplus
  }else if (method!="IPCC"){stop("mtehod unknown")}
  return(list(
    x=EfNSoil,
    weight=NULL,
    unit="Share",
    description="Emission factors from cropland soils. If IPCC, using the ipcc emission factors as share of applied N inputs. If Nloss, as share of cropland budget surplus."))
}