#' fullREMIND
#'
#' Function that produces the complete regional data set required for the REMIND model.
#'
#' @author Lavinia Baumstark
#' @seealso
#' \code{\link[madrat]{readSource}}, \code{\link[madrat]{getCalculations}}, \code{\link[madrat]{calcOutput}}
#' @export
#' @examples
#' \dontrun{
#' fullREMIND()
#' }
#'
fullREMIND <- function() {

  rem_years <- seq(2005, 2150, 5)
  rem_years_hist <- seq(1990, 2150, 5)
  gdpPopScen <- c("SSPs", "SSP2IndiaDEAs")
  feDemScen <- c(gdpPopScen, "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")

  #-------------- macro-economic parameters -----------------------------------------------------------
  calcOutput("Population", scenario = gdpPopScen, years = rem_years_hist, round = 8, file = "f_pop.cs3r")
  calcOutput("Population",
             scenario = gdpPopScen,
             aggregate = FALSE,
             years = rem_years_hist,
             round = 8,
             file = "f50_pop.cs3r")
  calcOutput("Labour", scenario = gdpPopScen, years = rem_years_hist, round = 8, file = "f_lab.cs3r")
  calcOutput("GDP", scenario = gdpPopScen, years = rem_years_hist, round = 8, file = "f_gdp.cs3r")
  calcOutput("GDP", scenario = gdpPopScen, years = rem_years_hist, round = 8, aggregate = FALSE, file = "f50_gdp.cs3r")

  calcOutput("RatioPPP2MER",                          round = 8,  file = "pm_shPPPMER.cs4r")
  calcOutput("MacroInvestments",                      round = 8,  file = "p01_boundInvMacro.cs4r")
  calcOutput("FETaxes", subtype = "taxes",            round = 2,  file = "f21_tau_fe_tax.cs4r")
  calcOutput("FETaxes", subtype = "subsidies",        round = 2,  file = "f21_tau_fe_sub.cs4r")

  calcOutput("ExpertGuess", subtype = "taxConvergence", round = 2, file = "f21_tax_convergence.cs4r")
  calcOutput("ExpertGuess", subtype = "taxConvergenceRollback", round = 2, file = "f21_tax_convergence_rollback.cs4r")
  calcOutput("ExpertGuess", subtype = "subConvergenceRollback", round = 2, file = "f21_sub_convergence_rollback.cs4r")

  calcOutput("TaxLimits", subtype = "maxFeSubsidy",   round = 2,  file = "f21_max_fe_sub.cs4r")
  calcOutput("TaxLimits", subtype = "propFeSubsidy",  round = 2,  file = "f21_prop_fe_sub.cs4r")
  calcOutput("PETaxes", subtype = "subsidies",        round = 2,  file = "f21_tau_pe_sub.cs4r")
  calcOutput("Capital", scenario = gdpPopScen,        signif = 4, file = "f29_capitalQuantity.cs4r")

  # Exogenous demand scenarios activated by cm_exogDem_scen
  calcOutput("ExogDemScen",                           round = 8,  file = "p47_exogDemScen.cs4r")
  calcOutput(
    type = "Steel_Projections",
    scenarios = mrdrivers::toolReplaceShortcuts(gdpPopScen),
    subtype = "secondary.steel.max.share",
    file = "p37_steel_secondary_max_share.cs4r",
    match.steel.historic.values = TRUE,
    match.steel.estimates = "IEA_ETP"
  )
  calcOutput("FEdemand", scenario = feDemScen, signif = 4, file = "f_fedemand.cs4r")
  calcOutput("FeDemandBuildings",
             subtype = "FE_buildings",
             scenario = feDemScen,
             round = 8,
             file = "f_fedemand_build.cs4r")
  calcOutput("FeDemandBuildings",
             subtype = "UE_buildings",
             scenario = feDemScen,
             round = 8,
             file = "f36_uedemand_build.cs4r")
  calcOutput("ChemicalFeedstocksShare",                     round = 2, file = "p37_chemicals_feedstock_share.cs4r")
  calcOutput("AllChemicalRoutes_2020", CCS=FALSE,           round = 8, file = "pm_outflowPrcHist_chemicals.cs4r")
  calcOutput("AllChemicalMat2Ue_2020to2150",                round = 8, file = "p37_mat2ue_chemicals.cs4r")
  calcOutput("AllChemicalUeShares_2020to2150",              round = 8, file = "p37_ue_share_chemicals.cs4r")
  calcOutput("AllChemicalEnergyDemand_2005to2020", CCS=FALSE, round = 8, file = "p37_demFePrcHist_chemicals.cs4r")
  calcOutput("AllChemicalSpecFeDemand_2005to2020", CCS=FALSE, round = 8, file = "pm_specFeDem_chemicals.cs4r")
  calcOutput("Floorspace", scenario = feDemScen, onlyTotal = TRUE, round = 1, file = "p36_floorspace_scen.cs4r")
  calcOutput("Floorspace", scenario = feDemScen,            round = 1, file = "f36_floorspace_scen.cs4r")
  calcOutput("IntertempElastSubst",                         round = 6, file = "pm_ies.cs4r")
  calcOutput("TimePref",                                    round = 6, file = "p23_prtp.cs4r")
  calcOutput("CO2Prices",                                   round = 2, file = "pm_taxCO2eqHist.cs4r")
  calcOutput("RiskPremium",                                 round = 6, file = "pm_risk_premium.cs4r")
  calcOutput("NetForeignAsset",                             round = 6, file = "pm_nfa_start.cs4r")
  calcOutput("Theil", scenario = gdpPopScen,                round = 8, file = "f_ineqTheil.cs4r")
  calcOutput("DevelopmentState",                            round = 4, file = "f_developmentState.cs3r")
  calcOutput("TCdamage", subtype = "const",                 round = 8, file = "f50_TC_df_const.cs4r", aggregate = FALSE)
  calcOutput("TCdamage", subtype = "tasK",                  round = 8, file = "f50_TC_df_tasK.cs4r", aggregate = FALSE)
  calcOutput("KLWdamage", subtype = "beta1",                round = 8, file = "f50_KLW_df_beta1.cs4r", aggregate = FALSE)
  calcOutput("KLWdamage", subtype = "beta2",                round = 8, file = "f50_KLW_df_beta2.cs4r", aggregate = FALSE)
  calcOutput("KLWdamage", subtype = "maxGMT",               round = 8, file = "f50_KLW_df_maxGMT.cs4r", aggregate = FALSE)

  #-------------- emission parameter ------------------------------------------------------------------
  calcOutput("EmissionsTe",                                                     round = 5, file = "p_boundEmi.cs4r")
  calcOutput("HistEmissions", subtype = "sector",                               round = 8, file = "p_histEmiSector.cs4r")
  calcOutput("HistEmissions", subtype = "MAC",                                  round = 8, file = "p_histEmiMac.cs4r")
  calcOutput("EmiCO2LandUse",                                                   round = 5, file = "p_macPolCO2luc.cs4r")
  calcOutput("MacBaseLandUse", subtype = "DirectlyFromMAgPIE",                  round = 8, file = "f_macBaseMagpie.cs4r")
  calcOutput("MacBaseLandUse", subtype = "Exogenous",                           round = 5, file = "f_macBaseExo.cs4r")
  calcOutput("MACCsCO2",                                                        round = 5, file = "p_abatparam_CO2.cs4r", aggregate = FALSE)
  calcOutput("EmiMac",                                                          round = 5, file = "p_macBase2005.cs4r")
  calcOutput("EmiMac1990",                                                      round = 5, file = "p_macBase1990.cs4r")
  calcOutput("EmiMacCEDS", baseyear = 2005,                                     round = 5, file = "p_macBaseCEDS2005.cs4r")
  calcOutput("EmiMacCEDS", baseyear = 2020,                                     round = 5, file = "p_macBaseCEDS2020.cs4r")
  calcOutput("MACCbaseN2O",  source = "PBL_2007",                               round = 5, file = "p_macBaseVanv.cs4r")
  calcOutput("MACCbaseN2O",  source = "PBL_2022",                               round = 5, file = "p_macBaseHarmsen2022.cs4r")
  calcOutput("MACCsCH4", source = "ImageMacc",                                  round = 6, file = "p_abatparam_CH4.cs4r")
  calcOutput("MACCsN2O", source = "ImageMacc",                                  round = 6, file = "p_abatparam_N2O.cs4r")
  calcOutput("MACCsCH4", source = "PBL_MACC_SSP2_2022",                         round = 6, file = "p_abatparam_SSP22022_CH4.cs4r")
  calcOutput("MACCsN2O", source = "PBL_MACC_SSP2_2022",                         round = 6, file = "p_abatparam_SSP22022_N2O.cs4r")
  calcOutput("FGas",                                                            round = 6, file = "f_emiFgas.cs4r")
  calcOutput("EmiFossilFuelExtr", source = "EDGAR",                             round = 6, file = "p_emiFossilFuelExtr.cs4r")
  calcOutput("EmiFossilFuelExtr", source = "CEDS2025",                          round = 6, file = "p_emiFossilFuelExtr2020.cs4r")
  calcOutput("Region2MAGICC",                                                   round = 6, file = "p_regi_2_MAGICC_regions.cs3r")
  calcOutput("EmissionFactorsFeedstocks",                                       round = 5, file = "f_nechem_emissionFactors.cs4r")
  calcOutput("EmiLULUCFCountryAcc",                                             round = 5, file = "p_EmiLULUCFCountryAcc.cs4r")

  #-------------- air pollution parameters - outdated but currently still needed ----------------------
  calcOutput("EmiAirPollLandUse",                                               round = 6, file = "f11_emiAPexoAgricult.cs4r")

  #-------------- air pollution parameters - refactored -----------------------------------------------
  calcOutput("GAINS2025", weight_source = "CEDS2025",    outsectors = "REMIND",     outunit = "Tg/TWa",                round = 8, file = "f11_emifacs_sectREMIND_sourceCEDS.cs4r")
  calcOutput("GAINS2025", weight_source = "GAINS2025",   outsectors = "REMIND",     outunit = "Tg/TWa",                round = 8, file = "f11_emifacs_sectREMIND_sourceGAINS.cs4r")
  calcOutput("GAINS2025", weight_source = "CEDS2025",    outsectors = "GAINS2025",  outunit = "Tg/TWa",                round = 8, file = "emifacs_sectGAINS_sourceCEDS.cs4r")
  calcOutput("GAINS2025", weight_source = "GAINS2025",   outsectors = "GAINS2025",  outunit = "Tg/TWa",                round = 8, file = "emifacs_sectGAINS_sourceGAINS.cs4r")
  calcOutput("AirPollBaseyearEmi", data_source = "CEDS2025",  outsectors = "GAINS", baseyear = 2020, CEDS.5yearmean = TRUE, round = 8, file = "emi2020_sectGAINS_sourceCEDS.cs4r")
  calcOutput("AirPollBaseyearEmi", data_source = "GAINS2025", outsectors = "GAINS", baseyear = 2020, CEDS.5yearmean = TRUE, round = 8, file = "emi2020_sectGAINS_sourceGAINS.cs4r")

  #-------------- energy/technology parameters ---------------------------------------------------------
  calcOutput("PotentialHydro",                        round = 3,  file = "f_maxProdGradeRegiHydro.cs3r")
  calcOutput("PotentialWindOn",                       round = 3,  file = "f_maxProdGradeRegiWindOn.cs3r")
  calcOutput("PotentialWindOff",                      round = 3,  file = "f_maxProdGradeRegiWindOff.cs3r")
  calcOutput("PotentialGeothermal",                   round = 3,  file = "f_maxProdGeothermal.cs3r")
  calcOutput("PotentialWeathering",                   round = 3,  file = "f33_maxProdGradeRegiWeathering.cs3r")
  calcOutput("CostsWeathering",                       round = 8,  file = "p33_transportCostsWeathering.cs4r")
  calcOutput("EEZdistribution",                       round = 4,  file = "p33_EEZdistribution.cs4r")
  calcOutput("ExpertGuess", subtype = "biocharPrices",            file = "p33_BiocharPricePath.cs4r", aggregate = FALSE)
  calcOutput("BiocharBounds",                         round = 2,  file = "p_boundCapBiochar.cs4r")
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
  calcOutput("ClinkerToCementRatio",                  round = 2,  file = "p37_clinker-to-cement-ratio.cs4r")

  calcOutput("Capacity", subtype = "capacityByTech",                   round = 6,  file = "pm_histCap.cs3r",
             # for period 2025, only use the year 2024 value (drop 2023, 2025-2027 are not in data anyways)
             temporalmapping = filter(quitte::remind_timesteps, .data$year != 2023))
  calcOutput("Capacity", subtype = "capacityByPE",                     round = 6,  file = "p_PE_histCap.cs3r")
  calcOutput("CapacityFactor",                                         round = 6,  file = "f_cf.cs3r")
  calcOutput("SeProduction",                                           round = 8,  file = "p_histProdSe.cs3r")
  calcOutput("StorageFactor",                                          round = 6,  file = "f32_factorStorage.cs4r")
  calcOutput("GridFactor",                                             round = 6,  file = "p32_grid_factor.cs4r")
  # Pass the same scenarios to FEShares as to FEDemand to optimize madrat cache usage.
  calcOutput("FEShares", subtype = "ind_coal", scenario = feDemScen,   round = 5,  file = "p_share_ind_fesos.cs4r")
  calcOutput("FEShares", subtype = "ind_bio", scenario = feDemScen,    round = 5,  file = "p_share_ind_fesos_bio.cs4r")
  calcOutput("FEShares", subtype = "ind_liq", scenario = feDemScen,    round = 5,  file = "p11_share_ind_fehos.cs4r")
  calcOutput("Solar",                                                  round = 5,  file = "f_dataRegiSolar.cs3r")
  calcOutput("CapacityNuclear",                                        round = 5,  file = "pm_NuclearConstraint.cs4r")
  calcOutput("CCScapacity", subtype = "pipeline",                      round = 8,  file = "p_boundCapCCS.cs4r")
  calcOutput("ExpertGuess", subtype = "ccsBounds",                     round = 8,  file = "p_boundCapCCSindicator.cs4r")
  calcOutput("LimitCCS",                                               round = 8,  file = "pm_dataccs.cs3r")

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
  calcOutput("DiffInvestCosts",                                        round = 4,  file = "p_inco0.cs4r")
  calcOutput("CapacityFactorHist",                                     round = 4,  file = "p_histCapFac.cs4r")
  calcOutput("GEA2012", subtype = "coal",                              round = 8,  file = "p31_grades_coal.cs4r")
  calcOutput("GEA2012", subtype = "gas",                               round = 8,  file = "p31_grades_gas.cs4r")
  calcOutput("GEA2012", subtype = "oil",                               round = 8,  file = "p31_grades_oil.cs4r")
  calcOutput("industry_specific_FE_limits", aggregate = FALSE,                     file = "pm_energy_limit.csv")
  calcOutput("PlasticsEoL",                                            round = 5,  file = "f_incinerationShares.cs4r")

  #--------------- EDGE Transport ---------------------------------------------------------------------

  calcOutput("EDGETransport", subtype = "f35_esCapCost",                           file = "f35_esCapCost.cs4r")
  calcOutput("EDGETransport", subtype = "f35_fe2es",                               file = "f35_fe2es.cs4r")
  calcOutput("EDGETransport", subtype = "f35_demByTech",                           file = "f35_demByTech.cs4r")
  calcOutput("EDGETransport", subtype = "f29_trpdemand",                           file = "f29_trpdemand.cs4r")


  #---------------policy parameters--------------------------------------------------------------------
  calcOutput("EmiTarget", sources = "UNFCCC_NDC", subtype = "Ghgfactor", scenario = gdpPopScen, round = 4, file = "fm_factorTargetyear.cs3r")
  calcOutput("EmiTarget", sources = "UNFCCC_NDC", subtype = "Ghgshare2015", scenario = gdpPopScen, round = 4, file = "fm_2015shareTarget.cs3r")

  calcOutput("EmiTarget", sources = "NewClimate", subtype = "Ghgfactor", scenario = gdpPopScen, round = 4, file = "fm_NC_factorTargetyear.cs3r")
  calcOutput("EmiTarget", sources = "NewClimate", subtype = "Ghgshare2015", scenario = gdpPopScen, round = 4, file = "fm_NC_2015shareTarget.cs3r")

  calcOutput("CapTarget", sources = "UNFCCC_NDC+REN21+CHN_NUC", round = 4, file = "f40_NDC+REN21+CHN_NUC.cs3r")
  calcOutput("CapTarget", sources = "NewClimate", round = 4, file = "f40_NewClimate.cs3r")

  calcOutput("SharedTarget", subtype = "FErenewablesShare", round = 3, file = "f40_FE_RenShare.cs4r")
  calcOutput("ExpertGuess", subtype = "tradeConstraints", aggregate = FALSE, file = "p24_trade_constraints.cs4r")

  #---------------files used in reporting-------------------------------------------------------------

  calcOutput("OtherFossilInElectricity", round = 6,  file = "se_otherfoss.cs4r", aggregate = "reg+glo")
  calcOutput("WasteEnergyUseShares", round = 6, file = "emi_waste_shares.cs4r")
  calcOutput("Emissions4ReportExtra", sectors = "CEDS", round = 9, file = "p_emissions4ReportExtraCEDS.cs4r")
  calcOutput("Emissions4ReportExtra", sectors = "IAMC", round = 9, file = "p_emissions4ReportExtraIAMC.cs4r")
  calcOutput("AirPollBaseyearEmi", data_source = "CEDS2025",  outsectors = "INT",   baseyear = 2020, CEDS.5yearmean = TRUE, round = 8, file = "emi2020_sectNOGAINS_sourceCEDS.cs4r")

  #---------------no longer used in REMIND develop-----------------------------------------------------

  calcOutput("Industry_CCS_limits",
             scenarios = feDemScen,
             a1 = 0.3, a2 = 0.15, installation_minimum = 1,
             stage_weight = c("Operational"          = 1,
                              "In construction"      = 1,
                              "Advanced development" = 0.5,
                              "Early development"    = 0.2),
             signif = 3, file = "f37_indCCSlimit_default.cs4r",
             years = seq(2005, 2050, 5))

  calcOutput("Industry_CCS_limits",
             scenarios = feDemScen,
             a1 = 0.5, a2 = 0.25, installation_minimum = 1,
             stage_weight = c("Operational"          = 1,
                              "In construction"      = 1,
                              "Advanced development" = 0.8,
                              "Early development"    = 0.5),
             signif = 3, file = "f37_indCCSlimit_high.cs4r",
             years = seq(2005, 2050, 5))

}
