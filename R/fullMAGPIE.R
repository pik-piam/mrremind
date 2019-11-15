#' fullMAgPIE
#' 
#' Function that produces the complete regional data set required for running the
#' MAgPIE model.
#' 
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @param debug if TRUE, the script does not stop upon an error.
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' 
#' \dontrun{ 
#' fullMAgPIE(revision=12, mainfolder="pathtowhereallfilesarestored")
#' }

fullMAGPIE <- function(rev=0.1, debug=FALSE) {
  
    mag_years <- findset("time")
    mag_years_past <- findset("past")
    short_years <- findset("t_all")
    
    cellsregions <- function(reg_revision=0) {
      # function which calculates the name vector for spatial 0.5 degree MAgPIE data sets
      # containing MAgPIE cell number and corresponding region
      cwd <- getwd()
      if(!file.exists(getConfig("outputfolder"))) dir.create(getConfig("outputfolder"),recursive = TRUE)
      setwd(getConfig("outputfolder"))
      map <- toolMappingFile("regional",getConfig("regionmapping"),readcsv = TRUE)
      regionscode <- regionscode(map)
      spatial_header <- spatial_header(map)
      save(spatial_header,regionscode,map,reg_revision,file="spatial_header.rda",compress="xz")
      setwd(cwd)
    }
    cellsregions(rev)
    
    # data fully agrees with the data currently used in MAgPIE and new data set is implemented
    calcOutput("TauTotal",  years=1995,        round=2, file="fm_tau1995.cs4", try = debug)
  
    # 09 drivers
    calcOutput("CollectProjectionDrivers", driver="gdp",aggregate=FALSE, years=mag_years, round=1, file="f09_gdp_ppp_iso.csv", try = debug)
    calcOutput("CollectProjectionDrivers", driver="gdpmer",aggregate=FALSE, years=mag_years, round=1, file="f09_gdp_mer_iso.csv", try = debug)
    calcOutput("CollectProjectionDrivers", driver="pop",aggregate=FALSE, years=mag_years, round=1, file="f09_pop_iso.csv", try = debug)
    calcOutput("CollectProjectionDrivers", driver="urban",aggregate=FALSE, years=mag_years, round=1, file="f09_urban_iso.csv", try = debug)
    calcOutput("DevelopmentState", round=4, file="f09_development_state.cs3", try = debug)
    calcOutput("Demography",education=FALSE,aggregate=FALSE, file="f09_demography.cs3", try = debug)
    calcOutput("PhysicalInactivity",aggregate = FALSE,years=mag_years, round=3, file="f09_physical_inactivity.cs3", try = debug)
    
    # 13 tc
    calcOutput("ExoTcDummy",       round=4, file="f13_tau_scenario.csv", try = debug)
    calcOutput("TCguess",          round=3, file="f13_tcguess.cs4", try = debug)
    
    # 14 yields
    calcOutput("CalibrationDummy", round=0, file="f14_yld_calib.csv", try = debug)
    calcOutput("PastureYield",round=3,file="f14_pasture_yields_hist.csv", try = debug)
    calcOutput("Yield", cut=0.95, years=mag_years_past, round=2, file="f14_region_yields.cs3", try = debug)
    
    # 15 food
    calcOutput("BodyHeight",aggregate = FALSE,years=mag_years_past, round=2, file="f15_bodyheight_historical.cs3", try = debug)
    calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_schofield_parameters.cs3", regression="Schofield", try = debug)
    calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_schofield_parameters_height.cs3", regression="FAO_WHO_UNU1985", try = debug)
    calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_bmi_shr_regr_paras.cs3", regression="bmi_shr", try = debug)
    #calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_intake_regression_parameters.cs3", regression="intake_regression", try = debug)
    calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_demand_regression_parameters.cs3", regression="demand_regression", try = debug)
    calcOutput("RegressionParameters",aggregate = FALSE, round=3, file="f15_bodyheight_regr_paras.cs3", regression="bodyheight_regression", try = debug)
    
    calcOutput("Intake",modelinput="age_groups_hist", standardize=FALSE, method="FAO_WHO_UNU1985", aggregate=FALSE,years=mag_years_past, round=1, file="f15_intake_pc_observed_iso.cs3", try = debug)
    calcOutput("FoodSupplyPast",per_capita=TRUE, products=NULL, product_aggr=FALSE, populationweight="PopulationPast",attributes="kcal",aggregate=FALSE,years=mag_years_past, round=1, file="f15_kcal_pc_iso.csv", try = debug)
    calcOutput("Household_balanceflow",    years=mag_years, round=4, file="f15_household_balanceflow.cs3", try = debug)
    calcOutput("NutritionAttributes",      years=mag_years, round=4, file="f15_nutrition_attributes.cs3", aggregate=FALSE, try = debug)
    calcOutput("IniFoodPrice", datasource="FAO", years=NULL, round=4, file="f15_prices_initial.csv", aggregate=FALSE, year="y2005", try = debug)
    calcOutput("BMIshr", convert=TRUE,years=mag_years_past, round=4, file="f15_bmi_shr_past.cs3",aggregate = FALSE, try = debug)
    calcOutput("BMI",file="f15_bmi.cs3",aggregate = FALSE)
    
    calcOutput("EATLancetDiets",aggregate = TRUE, round=4, file="f15_intake_EATLancet.cs3", attributes = "kcal", calib = TRUE, FAOcountr = TRUE, try = debug)
    calcOutput("EATLancetWaste",aggregate = TRUE, round=4, file="f15_supply2intake_ratio_bottomup.cs3", out_type="ratio_detailed_FAO", try = debug)
    calcOutput("EATLancetWaste",aggregate = TRUE, round=4, file="f15_calib_factor_FAOfsupply.cs4", out_type="calib", try = debug)
    
    # 16 demand
    calcOutput("Attributes", round=4, aggregate = FALSE,        file="fm_attributes.cs3", try = debug)
    calcOutput("SeedShare", years=mag_years, round=4,           file="f16_seed_shr.csv", try = debug)
    calcOutput("LossShare", years=mag_years, round=4,           file="f16_waste_shr.csv", try = debug)
    calcOutput("DomesticBalanceflow", years=mag_years, round=4, file="f16_domestic_balanceflow.csv", try = debug)
    calcOutput("TimberDemandExt", file = "f16_forestry_demand.csv", try = debug)
    
    # 18 residues
    calcOutput("Multicropping", round=4, file="f18_multicropping.csv",aggregate = TRUE, try = debug)
    calcOutput("ResCombustEff", round=4, file="f18_res_combust_eff.cs4",aggregate = F, try = debug)
    
    # 20 processing
    calcOutput("Processing_shares",             years=mag_years, round=4, file="f20_processing_shares.cs3", try = debug)
    calcOutput("Processing_conversion_factors", years=mag_years, round=4, file="f20_processing_conversion_factors.cs3", aggregate = FALSE, try = debug)
    calcOutput("Processing_balanceflow",        years=mag_years, round=4, file="f20_processing_balanceflow.cs3", try = debug)
    
    # 21 trade
    calcOutput("TradeSelfSuff",    years=mag_years, round=2, file="f21_trade_self_suff.cs3", try = debug)
    calcOutput("TradeExportShr",   years=mag_years, round=2, file="f21_trade_export_share.cs3", try = debug)
    calcOutput("TradeBalanceflow", years=mag_years, round=4, file="f21_trade_balanceflow.cs3", aggregate=FALSE, try = debug)
    calcOutput("TradeMargin",      years=2005,      round=4, file="f21_trade_margin.cs3", try = debug)
    calcOutput("TradeTariff",      years=2005,      round=4, file="f21_trade_tariff.cs3", try = debug)
    calcOutput("TradeTariff", type_tariff="export",    round=4, file="f21_trade_tariff_export.cs3", try = debug)
    calcOutput("TradeTariff", type_tariff="import",    round=4, file="f21_trade_tariff_import.cs3", try = debug)

    # 32 forestry
    calcOutput("AfforestCosts", years=2001,        round=0, file="f32_fac_req_ha.csv", try = debug)
    calcOutput("ManagementFactor",file = "f32_forestry_management.csv",try = debug) 
    calcOutput("TimberHarvestCost",file = "f32_harvestingcost.cs4",try = debug)
    calcOutput("ForestryProductionRatio",file = "f32_production_ratio.csv",round=3,try = debug)
    
    #38 factor costs
    calcOutput("Yield", cut=0.95, years=1995, round=2, file="f38_region_yield.csv", try = debug)
    
    #41 Area Equipped for Irrigation
    #f41_irrig(j) should be read out of calcAreaEquippedForIrrigation()
    calcOutput("IrrigationInvCosts", years=short_years, round=0, file="f41_c_irrig.csv", try = debug)
    
    #45 climate
    #f45_koeppengeiger(j,clcl)
    
    #50 n soil budget
    calcOutput("SNUpE",years=mag_years, round=4, file="f50_snupe.cs4", try = debug)
    calcOutput("NitrogenBudgetBalanceflow",years=mag_years, round=4, file="f50_nitrogen_balanceflow.cs4", try = debug)
    calcOutput("NitrogenFixationNdfa",years=mag_years, round=4, file="f50_ndfa.cs4", try = debug)
    calcOutput("NitrogenFixationFreeliving", round=4, file="f50_fixation_freeliving.cs4", aggregate = FALSE, try = debug)
    calcOutput("AtmosphericDepositionRates",round=4, file="f50_atmospheric_deposition_rates.cs4", try = debug)
    
    calcOutput("NuePasture",years=mag_years, round=4, file="f50_nue_pasture.cs4", try = debug)
    calcOutput("NitrogenBudgetPastureBalanceflow",years=mag_years, round=4, file="f50_nitrogen_balanceflow_pasture.cs4", try = debug)
    calcOutput("NitrogenFixationRatePasture",years=mag_years, round=5, file="f50_nitrogen_fixation_rates_pasture.cs4", try = debug)
    
    #51 nitrogen pollution
    calcOutput("EfNSoil",  round=4, file="f51_ef_n_soil.cs3", aggregate = FALSE, try = debug)
    calcOutput("EF3confinement",round=4, file="f51_ef3_confinement.cs4", try = debug)
    calcOutput("EF3prp",round=4, file="f51_ef3_prp.cs4", try = debug)
    
    #53 methane
    calcOutput("EFch4Rice",years=mag_years,round=4, file="f53_EFch4Rice.cs4", try = debug)
    calcOutput("EFch4AWMS",years=mag_years,round=4, file="f53_EFch4AWMS.cs4", try = debug)
    
    #55 awms
    calcOutput("ManureFuelShr",      years=mag_years,  round=4, file="f55_manure_fuel_shr.cs4", try = debug)
    calcOutput("AWMSconfShr",        years=mag_years,  round=4, file="f55_awms_shr.cs4", try = debug)
    calcOutput("EF3confinement", selection="recycling",round=4, file="f55_awms_recycling_share.cs4", try = debug)
  
    # 56_ghg_policy
    if(rev<4) {
      calcOutput("GHGPrices",emissions="pollutants", years=short_years, round=2, file="f56_pollutant_prices.cs3", try = debug)
    } else {
      calcOutput("GHGPrices",datasource="SSP_and_REM", years=short_years, round=2, file="f56_pollutant_prices.cs3", try = debug)
    }
    
    # 57 maccs
    short_years_from_2010 <- short_years[as.integer(sub("y","",short_years))>=2010]
    calcOutput("MACCsN2O",sector="landuse",years=short_years_from_2010, round=4, file="f57_maccs_n2o.cs3", try = debug)
    calcOutput("MACCsCH4",sector="landuse",years=short_years_from_2010, round=4, file="f57_maccs_ch4.cs3", try = debug)
    

    #60 bioenergy
    calcOutput("1stBioDem",years = mag_years,round=3, file="f60_1stgen_bioenergy_dem.cs3", try = debug)
    calcOutput("2ndBioDem",datasource="SSP_and_REM",years=short_years, round=3, file="f60_bioenergy_dem.cs3", try = debug)
    calcOutput("ResFor2ndBioengery", products="kres",product_aggr=TRUE, add_off=TRUE, years=mag_years, round=3, file="f60_2ndgenBE_residue_dem.cs3", try = debug)
    
    #62 Material
    calcOutput("DemMaterial", years=mag_years_past, round=4, file="f62_dem_material.cs3", try = debug)
  
    #70 livestock 
    calcOutput("FeedBaskets",           years=mag_years,  round=4, file="f70_feed_baskets.cs3", try = debug)
    calcOutput("FeedBalanceflow",       years=mag_years,  round=4, file="f70_feed_balanceflow.cs3", try = debug)
    calcOutput("LivestockProductivity", years=mag_years,  round=4, file="f70_livestock_productivity.cs3", try = debug)
    calcOutput("SlaughterFeedShare",    years=mag_years,  round=4, file="f70_slaughter_feed_share.cs4", try = debug)
    calcOutput("PYieldSlope",                             round=2, file="f70_pyld_slope_reg.cs4", try = debug)
}
