#' @importFrom magclass setNames

calcResFeedAvailability<-function(){
  
  past           <- findset("past")
  kres           <- findset("kres")
  mapping.res    <- toolGetMapping("kcr_kres.csv",type="sectoral")
  dev_state_past <- collapseNames(calcOutput("DevelopmentState",aggregate = F)[,past,"SSP2"])
  
  biomass.kcr  <- collapseNames(calcOutput("ResBiomass",aggregate = FALSE)[,,"ag"][,,"dm"])
  biomass.kres <- toolAggregate(biomass.kcr, rel = mapping.res, from = "kcr", to = "kres", dim = 3)[,,kres]
  
  feed <- biomass.kres * (dev_state_past*0.2 + (1-dev_state_past)*0.4)
  feed <- feed * calcOutput("Attributes",aggregate = F)[,,kres]
  
  return(list(x=feed,
              weight=NULL,
              unit="Mt DM, PJ Energy, Mt K, Mt Nr, Mt P, Mt WM",
              description="Crop Residues available for feed, but not necessarily used for feed"))
}
