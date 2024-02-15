#' fullREMIND
#'
#' Function that produces the complete regional data set required for the
#' REMIND model.
#' @importFrom madrat madratAttach
#' @importFrom magrittr %>%
#' @importFrom quitte cartesian madrat_mule
#' @author Lavinia Baumstark
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullREMIND()
#' }
#'
fullREMIND <- function() {

  rem_years <- seq(2005, 2150, 5)
  rem_years_hist <- seq(1990, 2150, 5)

  madratAttach("edgeTransport")   # enable madrat caching for edgeTransport

  #-------------- macro-economic parameters -----------------------------------------------------------
  calcOutput("Population", years = rem_years_hist,    round = 8,  file = "f_pop.cs3r")
  calcOutput("Labour",     years = rem_years,         round = 8,  file = "f_lab.cs3r")
  calcOutput("GDP",     years = rem_years_hist,       round = 8,  file = "f_gdp.cs3r")
  calcOutput("RatioPPP2MER",                          round = 8,  file = "pm_shPPPMER.cs4r")
  calcOutput("MacroInvestments",                      round = 8,  file = "p01_boundInvMacro.cs4r")
  calcOutput("FETaxes", subtype = "taxes",            round = 2,  file = "f21_tau_fe_tax.cs4r")
  calcOutput("FETaxes", subtype = "subsidies",        round = 2,  file = "f21_tau_fe_sub.cs4r")
  calcOutput("TaxConvergence",                        round = 2,  file = "f21_tax_convergence.cs4r")
  calcOutput("TaxLimits", subtype = "maxFeSubsidy",   round = 2,  file = "f21_max_fe_sub.cs4r")
  calcOutput("TaxLimits", subtype = "maxPeSubsidy",   round = 2,  file = "f21_max_pe_sub.cs4r")
  calcOutput("TaxLimits", subtype = "propFeSubsidy",  round = 2,  file = "f21_prop_fe_sub.cs4r")
  calcOutput("PETaxes", subtype = "subsidies",        round = 2,  file = "f21_tau_pe_sub.cs4r")
  calcOutput("TaxXport",                              round = 2,  file = "p21_tau_xpres_tax.cs4r")   # not default, overwritten with 0
  calcOutput("Capital", signif = 4,                               file = "f29_capitalQuantity.cs4r")
  calcOutput("ExogDemScen",                           round = 8,  file = "p47_exogDemScen.cs4r") # exogenous demand scenarios activated by cm_exogDem_scen
  calcOutput(
    type = "Steel_Projections", subtype = "secondary.steel.max.share",
    file = "p37_steel_secondary_max_share.cs4r",
    match.steel.historic.values = TRUE, match.steel.estimates = "IEA_ETP",
    China_Production = readSource(type = "ExpertGuess",
                                  subtype = "Chinese_Steel_Production",
                                  convert = FALSE) %>%
      madrat_mule())


  calcOutput("FEdemand", signif = 4,                                   file = "f_fedemand.cs4r")
  calcOutput("FeDemandBuildings", subtype = "FE_buildings", round = 8, file = "f_fedemand_build.cs4r")
  calcOutput("FeDemandBuildings", subtype = "UE_buildings", round = 8, file = "f36_uedemand_build.cs4r")

  calcOutput("ChemicalFeedstocksShare",                     round = 2, file = "p37_chemicals_feedstock_share.cs4r")
  calcOutput("Floorspace", onlyTotal = TRUE,                round = 1, file = "p36_floorspace_scen.cs4r")
  calcOutput("Floorspace",                                  round = 1, file = "f36_floorspace_scen.cs4r")
  calcOutput("WeightNash",                                  round = 6, file = "p80_eoWeights_fix.cs4r")
  calcOutput("IntertempElastSubst",                         round = 6, file = "pm_ies.cs4r")
  calcOutput("TimePref",                                    round = 6, file = "p23_prtp.cs4r")
  calcOutput("CO2Prices",                                   round = 2, file = "pm_taxCO2eqHist.cs4r")
  calcOutput("RiskPremium",                                 round = 6, file = "pm_risk_premium.cs4r")
  calcOutput("NetForeignAsset",                             round = 6, file = "pm_nfa_start.cs4r")
  calcOutput("Theil",                                       round = 8, file = "f_ineqTheil.cs4r")
  calcOutput("DevelopmentState",                            round = 4, file = "f_developmentState.cs3r")
  calcOutput("Population", years = rem_years_hist,          round = 8, file = "f50_pop.cs3r", aggregate = FALSE)
  calcOutput("GDP", years = rem_years_hist,                 round = 8, file = "f50_gdp.cs3r", aggregate = FALSE)
  calcOutput("TCdamage", subtype = "const",                 round = 8, file = "f50_TC_df_const.cs4r", aggregate = FALSE)
  calcOutput("TCdamage", subtype = "tasK",                  round = 8, file = "f50_TC_df_tasK.cs4r", aggregate = FALSE)

  #-------------- emission parameter ------------------------------------------------------------------
  calcOutput("EconometricEmiParameter",                             round = 5, file = "p_emineg_econometric.cs3r")
  calcOutput("EmissionsTe",                                         round = 5, file = "p_boundEmi.cs4r")
  calcOutput("HistEmissions", subtype = "sector",                   round = 8, file = "p_histEmiSector.cs4r")
  calcOutput("HistEmissions", subtype = "MAC",                      round = 8, file = "p_histEmiMac.cs4r")
  calcOutput("EmiCO2LandUse",                                       round = 5, file = "p_macPolCO2luc.cs4r")
  calcOutput("MacBaseLandUse", subtype = "DirectlyFromMAgPIE",      round = 5, file = "f_macBaseMagpie.cs4r")
  calcOutput("MacBaseLandUse", subtype = "Exogenous",               round = 5, file = "f_macBaseExo.cs4r")
  calcOutput("MACCsCO2",                                            round = 5, file = "p_abatparam_CO2.cs4r", aggregate = FALSE)
  calcOutput("EmiMac",                                              round = 5, file = "p_macBase2005.cs4r")
  calcOutput("EmiMac1990",                                          round = 5, file = "p_macBase1990.cs4r")
  calcOutput("MACCbaseN2O",                                         round = 5, file = "p_macBaseVanv.cs4r")
  calcOutput("MACCsCH4",                                            round = 6, file = "p_abatparam_CH4.cs4r")
  calcOutput("MACCsN2O",                                            round = 6, file = "p_abatparam_N2O.cs4r")
  calcOutput("FGas",                                                round = 6, file = "f_emiFgas.cs4r")
  calcOutput("EmiFossilFuelExtr",                                   round = 6, file = "p_emiFossilFuelExtr.cs4r")
  calcOutput("Region2MAGICC",                                       round = 6, file = "p_regi_2_MAGICC_regions.cs3r")
  calcOutput("EmiPollutantExo", subtype = "AviationShipping",       round = 6, file = "f11_emiAPexoGlob.cs4r", aggregate = FALSE)
  calcOutput("EmiPollutantExo", subtype = "Waste",                  round = 6, file = "f11_emiAPexo.cs4r")
  calcOutput("EmiAirPollLandUse",                                   round = 6, file = "f11_emiAPexoAgricult.cs4r")
  calcOutput("GAINSEmi", subtype = "emissions",                     round = 5, file = "emi_gains.cs4r")
  calcOutput("GAINSEmi", subtype = "emission_factors",              round = 5, file = "ef_gains.cs4r")
  calcOutput("GAINSEmi", subtype = "emissions_starting_values",     round = 5, file = "f11_emiAPexsolve.cs4r")
  calcOutput("EmissionFactors", subtype = "emission_factors",       round = 5, file = "f11_emiFacAP.cs4r")
  calcOutput("EmissionFactorsFeedstocks",                           round = 5, file = "f_nechem_emissionFactors.cs4r")
  calcOutput("EmiLULUCFCountryAcc", subtype = "UNFCCC",             round = 5, file = "p_EmiLULUCFCountryAcc.cs4r")

  #-------------- energy/technology parameters ---------------------------------------------------------
  calcOutput("PotentialHydro",                        round = 3,  file = "f_maxProdGradeRegiHydro.cs3r")
  calcOutput("PotentialWindOn",                       round = 3,  file = "f_maxProdGradeRegiWindOn.cs3r")
  calcOutput("PotentialWindOff",                      round = 3,  file = "f_maxProdGradeRegiWindOff.cs3r")
  calcOutput("PotentialGeothermal",                   round = 3,  file = "f_maxProdGeothermal.cs3r")
  calcOutput("PotentialWeathering",                   round = 3,  file = "f33_maxProdGradeRegiWeathering.cs3r")
  calcOutput("CostsWeathering",                       round = 8,  file = "p33_transportCostsWeathering.cs4r")
  calcOutput("CostsTrade",                            round = 5,  file = "pm_costsPEtradeMp.cs4r")
  calcOutput("CostsTradePeFinancial",                 round = 5,  file = "pm_costsTradePeFinancial.cs3r")
  calcOutput("ShareCHP",                              round = 3,  file = "f32_shCHP.cs4r")
  calcOutput("CapacityOffset",                        round = 5,  file = "p_adj_deltacapoffset.cs4r")
  calcOutput("CoolingSharesAll",                      round = 2,  file = "CoolingShares_time.cs4r")
  calcOutput("WaterConsCoef",                         round = 3,  file = "WaterConsCoef.cs4r", aggregate = FALSE)
  calcOutput("WaterWithCoef",                         round = 3,  file = "WaterWithCoef.cs4r", aggregate = FALSE)
  calcOutput("IO",   subtype = "output",              round = 8,  file = "f04_IO_output.cs4r")
  calcOutput("IO",   subtype = "input",               round = 8,  file = "f04_IO_input.cs4r")
  calcOutput("IO",   subtype = "trade",               round = 8,  file = "f_IO_trade.cs4r")
  calcOutput("ShareIndFE",                            round = 3,  file = "p37_shIndFE.cs3r")
  calcOutput("nonEnergyIndFE",                        round = 8,  file = "f37_fedemand_NonEnergyIndst.cs4r")
  calcOutput("Clinker_to_cement_ratio",               round = 2,  file = "p37_clinker-to-cement-ratio.cs3r")
  # delete the 'dummy' line
  system(paste0('sed -i "/dummy/d" ', getConfig()$outputfolder, "/p37_clinker-to-cement-ratio.cs3r"))

  calcOutput("Capacity", subtype = "capacityByTech",                   round = 6,  file = "p_histCap.cs3r")  # will be deleted after the merge of REMIND-EU
  calcOutput("Capacity", subtype = "capacityByTech",                   round = 6,  file = "pm_histCap.cs3r")
  calcOutput("Capacity", subtype = "capacityByTech_windoff",           round = 6,  file = "pm_histCap_windoff.cs3r")
  calcOutput("Capacity", subtype = "capacityByPE",                     round = 6,  file = "p_PE_histCap.cs3r")
  calcOutput("CapacityFactor",                                         round = 6,  file = "f_cf.cs3r")
  calcOutput("StorageFactor",                                          round = 6,  file = "f32_factorStorage.cs4r")
  calcOutput("GridFactor",                                             round = 6,  file = "p32_grid_factor.cs4r")
  calcOutput("FEShares", subtype = "ind_coal",                         round = 5,  file = "p_share_ind_fesos.cs4r")
  calcOutput("FEShares", subtype = "ind_bio",                          round = 5,  file = "p_share_ind_fesos_bio.cs4r")
  calcOutput("FEShares", subtype = "ind_liq",                          round = 5,  file = "p_share_ind_fehos.cs4r")
  calcOutput("Solar",                                                  round = 5,  file = "f_dataRegiSolar.cs3r")
  calcOutput("CapacityEV",                                             round = 8,  file = "pm_boundCapEV.cs4r")
  calcOutput("CapacityNuclear",                                        round = 5,  file = "pm_NuclearConstraint.cs4r")
  calcOutput("CCScapacity",                                            round = 8,  file = "pm_boundCapCCS.cs4r")
  calcOutput("CCSbounds",                                              round = 8,  file = "p_boundCapCCSindicator.cs4r")
  calcOutput("LimitCCS",                                               round = 8,  file = "pm_dataccs.cs3r")
  calcOutput("TranspEff",                                              round = 8,  file = "f35_transp_eff.cs3r")
  calcOutput("VintagesTransport",                                      round = 8,  file = "f35_factorVintages.cs3r")
  calcOutput("BiomassPrices",                                          round = 6,  file = "f30_bioen_price.cs4r")
  calcOutput("ResFor2ndBioengery", years = rem_years,                  round = 5,  file = "p30_biolcResidues.cs3r")
  calcOutput("1stBioDem", subtype = "ethanol_oils", years = rem_years, round = 5,  file = "p30_bio1stgen.cs3r")
  calcOutput("MAgPIEReport", subtype = "ProductionBiomass",            round = 8,  file = "p30_biolcProductionLookup.cs4r")
  calcOutput("MAgPIEReport", subtype = "CostTotal",                    round = 8,  file = "p26_totLUcostLookup.cs4r")
  calcOutput("MAgPIEReport", subtype = "CostMAC",                      round = 8,  file = "p26_macCostLuLookup.cs4r")
  calcOutput("FossilPolyRent",                                         round = 8,  file = "f31_ffPolyRent.cs4r")
  calcOutput("FossilPolyCumEx",                                        round = 8,  file = "f31_ffPolyCumEx.cs4r")
  calcOutput("FossilExtraction", subtype = "FossilExtraction",         round = 9,  file = "f31_ffPolyCoeffs.cs3r")
  calcOutput("FossilExtraction", subtype = "UraniumExtraction",        round = 5,  file = "f31_costExPoly.cs3r")
  calcOutput("RLDCCoefficients", subtype = "LoB",                      round = 6,  file = "f32_RLDC_Coeff_LoB.cs3r")
  calcOutput("RLDCCoefficients", subtype = "Peak",                     round = 6,  file = "f32_RLDC_Coeff_Peak.cs3r")
  calcOutput("EarlyRetirementAdjFactor",                                           file = "p_earlyRetirementAdjFactor.cs3r")
  calcOutput("DiffInvestCosts",  subtype = "Invest_Costs",             round = 4,  file = "p_inco0.cs4r")
  calcOutput("DiffInvestCosts",  subtype = "Efficiency",               round = 4,  file = "pm_eff.cs4r")
  calcOutput("CapacityFactorHist", subtype = "wind",                   round = 4,  file = "p_histCapFac.cs4r")
  calcOutput("CapacityFactorHist", subtype = "windoff",                round = 4,  file = "p_histCapFac_windoff.cs4r")
  calcOutput("GEA2012", subtype = "coal",                              round = 8,  file = "p31_grades_coal.cs4r")
  calcOutput("GEA2012", subtype = "gas",                               round = 8,  file = "p31_grades_gas.cs4r")
  calcOutput("GEA2012", subtype = "oil",                               round = 8,  file = "p31_grades_oil.cs4r")
  calcOutput("industry_specific_FE_limits", aggregate = FALSE,                     file = "pm_energy_limit.csv")
  calcOutput("PlasticsEoL",                                            round = 5,  file = "f_incinerationShares.cs4r")

  #--------------- EDGE Transport ---------------------------------------------------------------------
  calcOutput("TransportGDPshare", round = 6,                                       file = "f35_transportGDPshare.cs4r")

  calcOutput("EDGETransport", subtype = "value_time",                              file = "value_time.cs4r")
  calcOutput("EDGETransport", subtype = "harmonized_intensities",                  file = "harmonized_intensities.cs4r")
  calcOutput("EDGETransport", subtype = "price_nonmot",                            file = "price_nonmot.cs4r")
  calcOutput("EDGETransport", subtype = "pref",                                    file = "pref.cs4r")
  calcOutput("EDGETransport", subtype = "UCD_NEC_iso",                             file = "UCD_NEC_iso.cs4r")
  calcOutput("EDGETransport", subtype = "loadFactor",                              file = "loadFactor.cs4r")
  calcOutput("EDGETransport", subtype = "fe_demand_tech",                          file = "fe_demand_tech.cs4r")
  calcOutput("EDGETransport", subtype = "fe2es",                                   file = "fe2es.cs4r")
  calcOutput("EDGETransport", subtype = "esCapCost",                               file = "esCapCost.cs4r")
  calcOutput("EDGETransport", subtype = "pm_trp_demand",                           file = "pm_trp_demand.cs4r")
  calcOutput("EDGETransport", subtype = "pm_fe_demand_EDGETbased",                 file = "pm_fe_demand_EDGETbased.cs4r")
  calcOutput("EDGETransport", subtype = "f35_bunkers_fe",                          file = "f35_bunkers_fe.cs4r")
  calcOutput("EDGETransport", subtype = "annual_mileage",                          file = "annual_mileage.cs4r")

  # not to be aggregated as global
  calcOutput("EDGETransport", subtype = "logit_exponent", aggregate = FALSE,       file = "logit_exponent.cs4r")
  calcOutput("EDGETransport", subtype = "ptab4W", aggregate = FALSE,               file = "ptab4W.cs4r")

  #---------------policy parameters--------------------------------------------------------------------
  calcOutput("EmiTarget", sources = "UNFCCC_NDC", subtype = "Ghgshare2005", round = 4, file = "fm_2005shareTarget.cs3r")
  calcOutput("EmiTarget", sources = "UNFCCC_NDC", subtype = "Ghgfactor",    round = 4, file = "fm_factorTargetyear.cs3r")
  calcOutput("EmiTarget", sources = "UNFCCC_NDC", subtype = "Ghghistshare", round = 4, file = "fm_histShare.cs3r")
  calcOutput("CapTarget", sources = "UNFCCC_NDC",                           round = 4, file = "f40_NDC.cs3r")
  calcOutput("CapTarget", sources = "REN21",                                round = 4, file = "f40_REN21.cs4r")
  calcOutput("CapTarget", sources = "UNFCCC_NDC+REN21+CHN_NUC",             round = 4, file = "f40_NDC+REN21+CHN_NUC.cs3r")
  calcOutput("SharedTarget", subtype = "FErenewablesShare",                 round = 3, file = "f40_FE_RenShare.cs4r")
  calcOutput("EffortSharingTarget",                                         round = 3, file = "p47_ESR_target.cs4r")
  calcOutput("EffortSharingRefEmi", subtype = "EEA_GHG",                    round = 6, file = "p47_ESR_GHG_referenceEmissions.cs4r")
  calcOutput("EffortSharingRefEmi", subtype = "REMIND_CO2",                 round = 6, file = "p47_ESR_CO2_referenceEmissions.cs4r")
  calcOutput("ETSRefEmi", subtype = "EEA_GHG",                              round = 6, file = "p47_ETS_GHG_referenceEmissions.cs4r")
  calcOutput("TransportSubsidies",                                          round = 8, file = "f21_vehiclesSubsidies.cs4r")
}
