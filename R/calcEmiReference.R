#' @author Falk Benke
#' @importFrom quitte calc_addVariable

calcEmiReference <-  function(){
  eea.emi <- readSource("EEA_EuropeanEnvironmentAgency", subtype="historical")
  eea.emi <- eea.emi[,2005,c('Emi|GHG|ETS', 'Emi|GHG|ES'), pmatch=TRUE]
  eea.emi <- as.data.frame(eea.emi)
  eea.emi <- cbind(model=c("EEA"), eea.emi)

  eurostat.emi <- readSource("EuropeanEnergyDatasheets")
  eurostat.emi <- eurostat.emi[,1990,'Emi|GHGtot', pmatch=TRUE]
  eurostat.emi <- as.data.frame(eurostat.emi)
  eurostat.emi <- cbind(model=c("Eurostat"), eurostat.emi)

  emi <- rbind(eea.emi, eurostat.emi)
  emi$Cell <- NULL
  colnames(emi) <- c("model", "region", "period", "variable", "value")
  emi <- emi[!is.na(emi$value),]

  emi <- emi %>% calc_addVariable(
    "`Emi|GHGtot|target|40% (Mt CO2-equiv/yr)`" = "`Emi|GHGtot (Mt CO2-equiv/yr)` * 0.6",
    "`Emi|GHGtot|target|55% (Mt CO2-equiv/yr)`" = "`Emi|GHGtot (Mt CO2-equiv/yr)` * 0.45",
    "`Emi|GHGtot|target|65% (Mt CO2-equiv/yr)`" = "`Emi|GHGtot (Mt CO2-equiv/yr)` * 0.35",
    "`Emi|GHG|ETS|target|40% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ETS (Mt CO2-equiv/yr)` * 0.6",
    "`Emi|GHG|ETS|target|55% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ETS (Mt CO2-equiv/yr)` * 0.45",
    "`Emi|GHG|ETS|target|65% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ETS (Mt CO2-equiv/yr)` * 0.35",
    "`Emi|GHG|ES|target|40% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ES (Mt CO2-equiv/yr)` * 0.6",
    "`Emi|GHG|ES|target|55% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ES (Mt CO2-equiv/yr)` * 0.45",
    "`Emi|GHG|ES|target|65% (Mt CO2-equiv/yr)`" = "`Emi|GHG|ES (Mt CO2-equiv/yr)` * 0.35",
    units=c("Mt CO2-equiv/yr")
  )

  x <- as.magpie(emi)
  x <- x[,,"target", pmatch=TRUE] %>% toolCountryFill(fill=0)
  return(list(x=x,weight=NULL,unit="Mt CO2-equiv/yr",description="Emission reduction targets"))
}