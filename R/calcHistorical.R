#' @importFrom magclass setNames getNames getSets add_columns
#' @importFrom luscale rename_dimnames


calcHistorical <- function() {
  
  .fillZeros <- function(data){
    Non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
    tmp <- data[Non28EUcountries,,]
    tmp[is.na(tmp)] <- 0
    data[Non28EUcountries,,] <- tmp[Non28EUcountries,,]
    return(data)
  }
  
  # Final Energy
  fe_iea <- calcOutput("FE",source="IEA",aggregate=FALSE)
  fe_iea <- add_dimension(fe_iea, dim=3.1, add="model",nm="IEA")
  
  fe_weo <- calcOutput("FE",source="IEA_WEO",aggregate = F)
  fe_weo <- fe_weo[,,"Current Policies Scenario",pmatch=T]
  fe_weo <- collapseNames(fe_weo)
  fe_weo <- add_dimension(fe_weo, dim=3.1, add="model",nm="IEA_WEO")

  # Final Energy
  fe_proj_ssp1 <- calcOutput("FE", source = "EDGE_projections", scenario_proj = "SSP1",aggregate=FALSE)
  fe_proj_ssp1 <- add_dimension(fe_proj_ssp1, dim=3.1, add="model",nm="EDGE_SSP1")
  fe_proj_ssp2 <- calcOutput("FE", source = "EDGE_projections", scenario_proj = "SSP2",aggregate=FALSE)
  fe_proj_ssp2 <- add_dimension(fe_proj_ssp2, dim=3.1, add="model",nm="EDGE_SSP2")
  fe_proj = mbind(fe_proj_ssp1,fe_proj_ssp2)
  fe_proj <- fe_proj[,getYears(fe_proj,T)[which(getYears(fe_proj,T) <= 2100)],] # get rid of periods after 2100

  # Final Energy - Heat Roadmap Europe
  fe_hre <- calcOutput("HRE", aggregate=FALSE)

  # Primary Energy
  pe_iea <- calcOutput("PE",subtype="IEA",aggregate=FALSE)
  pe_iea <- add_dimension(pe_iea, dim=3.1, add="model",nm="IEA")
  
  pe_weo <- calcOutput("PE",subtype="IEA_WEO",aggregate=FALSE)
  pe_weo <- pe_weo[,,"Current Policies Scenario",pmatch=T]
  pe_weo <- collapseNames(pe_weo)
  pe_weo <- add_dimension(pe_weo, dim=3.1, add="model",nm="IEA_WEO")
  
  # fossil trade
  trade <- calcOutput("Trade",aggregate=FALSE)
  trade <- add_dimension(trade, dim=3.1, add="model",nm="IEA")
 
  # Population
  pop <- calcOutput("PopulationPast",aggregate=FALSE)
  unit=strsplit(grep("unit",attributes(pop)$comment,value=TRUE),split = ": ")[[1]][[2]]
  getNames(pop) <- paste0("Population (",unit,")")
  pop <- add_dimension(pop, dim=3.1, add="model",nm="WDI")
  
  # GDP in ppp
  gdpp_James <- calcOutput("GDPPast",aggregate=FALSE) / 1000
  getNames(gdpp_James) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_James <- add_dimension(gdpp_James, dim=3.1, add="model",nm="James_IHME")
  
  gdpp_WB <- calcOutput("GDPPast",GDPPast="WB_USD05_PPP_pc",aggregate=FALSE) / 1000
  getNames(gdpp_WB) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_WB <- add_dimension(gdpp_WB, dim=3.1, add="model",nm="James_WB")
  
  gdpp_IMF <- calcOutput("GDPPast",GDPPast="IMF_USD05_PPP_pc",aggregate=FALSE) / 1000
  getNames(gdpp_IMF) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_IMF <- add_dimension(gdpp_IMF, dim=3.1, add="model",nm="James_IMF")
  
  # Historical emissions from CEDS data base
  ceds <- calcOutput("Emissions", datasource="CEDS2021", aggregate=FALSE)
  
  # Add GHG total (removed while Land-Use Change is not available)
  #ceds <- add_columns(ceds,"Emi|GHGtot (Mt CO2-equiv/yr)",dim=3.1)
  #ceds[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] <- ceds[,,"Emi|CO2 (Mt CO2/yr)"] +
  #  ceds[,,"Emi|CH4 (Mt CH4/yr)"]*28 +
  #  ceds[,,"Emi|N2O (kt N2O/yr)"]/1000*265
  ceds <- add_dimension(ceds, dim=3.1, add="model", nm="CEDS")

  # Historical emissions from EDGAR v5.0 and v6.0
  edgar6 <- calcOutput("Emissions", datasource="EDGAR6", aggregate=FALSE)
  edgar6 <- add_dimension(edgar6, dim=3.1, add="model", nm="EDGAR6")

  # Historical emissions from PRIMAPhist data base
  primap <- readSource("PRIMAPhist","hist")[,,"CAT0"]  # select total
  primap <- primap[,,c("co2_c","kyotoghgar4_co2eq_c")] / 12*44  # select CO2 and total GHG and convert into Co2
  getNames(primap) <- c("Emi|CO2 (Mt CO2/yr)","Emi|GHGtot (Mt CO2-equiv/yr)")
  primap <- add_dimension(primap, dim=3.1, add="model",nm="PRIMAPhist")

  # Historical emissions from CDIAC data base
  cdiac <- calcOutput("Emissions",datasource="CDIAC",aggregate=FALSE)
  getNames(cdiac) <- gsub("Emissions","Emi",getNames(cdiac))
  getNames(cdiac) <- gsub("Mt/yr","Mt CO2/yr",getNames(cdiac))
  cdiac <- add_dimension(cdiac, dim=3.1, add="model",nm="CDIAC")

  # Historical land use emissions (taken from "mrvalidation/R/fullVALIDATION.R")
  LU_EDGAR_LU    <- calcOutput(type="LandEmissions", datasource="EDGAR_LU", aggregate=FALSE, try=TRUE)
  LU_CEDS        <- calcOutput(type="LandEmissions", datasource="CEDS", aggregate=FALSE, try=TRUE)
  LU_FAO_EmisLUC <- calcOutput(type="LandEmissions", datasource="FAO_EmisLUC", aggregate=FALSE, try=TRUE)
  LU_FAO_EmisAg  <- calcOutput(type="LandEmissions", datasource="FAO_EmisAg", aggregate=FALSE, try=TRUE)
  LU_PRIMAPhist  <- calcOutput(type="LandEmissions", datasource="PRIMAPhist", aggregate=FALSE, try=TRUE)
  
  # remove scenario dimension (will be added below as also for remind variables)
  LU_EDGAR_LU    <- collapseNames(LU_EDGAR_LU   , collapsedim=1)
  LU_CEDS        <- collapseNames(LU_CEDS       , collapsedim=1)
  LU_FAO_EmisLUC <- collapseNames(LU_FAO_EmisLUC, collapsedim=1)
  LU_FAO_EmisAg  <- collapseNames(LU_FAO_EmisAg , collapsedim=1)
  LU_PRIMAPhist  <- collapseNames(LU_PRIMAPhist , collapsedim=1)
  #LU_IPCC        <- collapseNames(LU_IPCC       , collapsedim=1)
  #LU_Nsurplus2   <- collapseNames(LU_Nsurplus2  , collapsedim=1)
  
  # give ceds emissions from calcValidEmissions (magpie) a name that is different from ceds emissions from calcEmissions (remind)
  getNames(LU_CEDS,dim=1) <- "ceds_lu"
  
  # remove duplicates from LU_FAO_EmisAg
  LU_FAO_EmisAg <- LU_FAO_EmisAg[,,which(!duplicated(getNames(LU_FAO_EmisAg)))]
  
  
  #====== Capacities historical data ===================
  
  #IRENA capacities - technologies: "csp", "geohdr", "hydro", "spv", "wind"
  IRENAcap <- readSource(type="IRENA",subtype="Capacity")[,,c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind")] # Read IRENA renewables capacity data
  IRENAcap <- IRENAcap * 1E-03 # converting MW to GW
  mapping <- data.frame( IRENA_techs=c("Concentrated solar power", "Geothermal", "Hydropower", "Solar photovoltaic", "Wind"),
                         REMIND_var=c("Cap|Electricity|Solar|CSP (GW)", "Cap|Electricity|Geothermal (GW)", "Cap|Electricity|Hydro (GW)", "Cap|Electricity|Solar|PV (GW)", "Cap|Electricity|Wind (GW)"), stringsAsFactors = FALSE)
  IRENAcap <- rename_dimnames(IRENAcap, dim = 3, query = mapping, from = "IRENA_techs", to="REMIND_var") # renaming technologies to REMIND naming convention
  IRENAcap <- mbind(IRENAcap, setNames(IRENAcap[,,"Cap|Electricity|Solar|CSP (GW)"] + IRENAcap[,,"Cap|Electricity|Solar|PV (GW)"], "Cap|Electricity|Solar (GW)")) 
  IRENAcap <- add_dimension(IRENAcap, dim=3.1, add="model",nm="IRENA")
  
  #====== Region specific historical data ===================
  # European Eurostat data
  eurostat <- readSource("EuropeanEnergyDatasheets")
  EUcountries <- c("ALA","AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FRO","FIN","FRA","DEU","GIB","GRC","GGY","HUN","IRL","IMN","ITA","JEY","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR")
  eurostatEU <- eurostat[EUcountries,,]
  eurostatEU[is.na(eurostatEU)] <- 0
  eurostat[EUcountries,,] <- eurostatEU[EUcountries,,]
  eurostat <- add_dimension(eurostat, dim=3.1, add="model",nm="Eurostat")
  
  # Emissions market data
  # emiMktES <- setNames(readSource("Eurostat_EffortSharing",subtype="emissions"),"Emi|GHG|ESR (Mt CO2-equiv/yr)") # Effort Sharing
  # emiMktETS <- setNames(dimSums(readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2019), c("2_ Verified emissions.20-99 All stationary installations","3_ Estimate to reflect current ETS scope for allowances and emissions.20-99 All stationary installations")]),"Emi|GHG|ETS (Mt CO2-equiv/yr)") #ETS without aviation
  # # national aviation is not included in REMIND ETS yet
  # # aviation <- readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2018),c("2_ Verified emissions.10 Aviation")]
  # #set all non EU values to NA (by doing this we are excluding from the ETS the non EU28 countries - Norway, Liechtenstein and Iceland - because REMIND is not including them in the ETS)
  # emiMktES[getRegions(emiMktES)[-which(getRegions(emiMktES) %in% EUcountries)],,] <- NA 
  # emiMktES <- add_dimension(emiMktES, dim=3.1, add="model",nm="Eurostat")
  # ETScountries <- c(EUcountries,"GRL","ISL","LIE","NOR","SJM","CHE")
  # emiMktETS[getRegions(emiMktETS)[-which(getRegions(emiMktETS) %in% ETScountries)],,] <- NA 
  # emiMktETS <- add_dimension(emiMktETS, dim=3.1, add="model",nm="EEA_historical")
  # # set remaining emissions to other market - it is missing lulucf (Land use, land-use change, and forestry)
  # totalGHG <- dimSums(eurostat[,,c("Emi|GHGtot (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Aviation (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Maritime Transport (Mt CO2-equiv/yr)")])
  # years <- Reduce(intersect, list(getYears(totalGHG),getYears(emiMktES[,,"Emi|GHG|ESR (Mt CO2-equiv/yr)"]),getYears(emiMktETS[,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"])))
  # emiMktESOthers <- setNames(collapseNames(totalGHG[,years,] - emiMktES[,years,"Emi|GHG|ESR (Mt CO2-equiv/yr)"] - emiMktETS[,years,"Emi|GHG|ETS (Mt CO2-equiv/yr)"]),"Emi|GHG|other - Non ETS and ESR (Mt CO2-equiv/yr)")
  # emiMktESOthers <- add_dimension(emiMktESOthers, dim=3.1, add="model",nm="Eurostat")
  
  # EEA GHG Projections
  EEA_GHGProjections <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype="projections"))
  
  # EEA GHG Sectoral Historical Data
  EEA_GHGSectoral <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype="sectoral"))
  EEA_GHGSectoral <- add_dimension(EEA_GHGSectoral, dim=3.1,add="model",nm="EEA_historical")
  
  EEA_GHGTotal <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype="total"))
  EEA_GHGTotal <- add_dimension(EEA_GHGTotal, dim=3.1,add="model",nm="EEA_historical")

  # EEA_GHGES <- .fillZeros(readSource("EEA_EuropeanEnvironmentAgency", subtype="ESR"))
  # EEA_GHGES <- add_dimension(EEA_GHGES, dim=3.1,add="model",nm="EEA_historical")

  # EU Reference Scenario
  EU_ReferenceScenario <- calcOutput("EU_ReferenceScenario", aggregate = F)

  # ARIADNE Reference Scenario
  ARIADNE_ReferenceScenarioGdp <- readSource("ARIADNE", subtype = "gdp")
  ARIADNE_ReferenceScenarioGdp <- add_dimension(ARIADNE_ReferenceScenarioGdp,
                                                dim = 3.1, add = "model", nm = "ARIADNE")

  ARIADNE_ReferenceScenarioGdpCorona <- readSource("ARIADNE", subtype = "gdp_corona")
  ARIADNE_ReferenceScenarioGdpCorona <- add_dimension(ARIADNE_ReferenceScenarioGdpCorona,
                                                      dim = 3.1, add = "model", nm = "ARIADNE - Corona")

  ARIADNE_ReferenceScenarioPop <- readSource("ARIADNE", subtype = "population")
  ARIADNE_ReferenceScenarioPop <- add_dimension(ARIADNE_ReferenceScenarioPop, 
                                                dim = 3.1, add = "model", nm = "ARIADNE")

  IEA_ETP <- calcOutput("IEA_ETP", aggregate = F)

  # Calculate Emission Reference Values
  Emi_Reference <- .fillZeros(calcOutput("EmiReference", aggregate=FALSE))
  Emi_Reference <- add_dimension(Emi_Reference, dim=3.1,add="model",nm="EEA") 
  
  # Eurostat emissions
  eurostatEmi <- readSource(type="Eurostat",subtype="emissions")
  eurostatEmi[getRegions(eurostatEmi)[-which(getRegions(eurostatEmi) %in% EUcountries)],,] <- NA 
  emiEurostatEU <- eurostatEmi[EUcountries,,]
  emiEurostatEU[is.na(emiEurostatEU)] <- 0
  emiEurostat <- NULL
  emiEurostat <- mbind(
    setNames(eurostatEmi[,,"CH4.All sectors (excluding memo items)"],"Emi|CH4 (Mt CH4/yr)")/28,
    setNames(eurostatEmi[,,"N2O.All sectors (excluding memo items)"],"Emi|N2O (kt N2O/yr)")/(265 * 44 / 28)*1000 
  )
  emiEurostat <- add_dimension(emiEurostat, dim = 3.1, add = "model", nm = "Eurostat")
  
  # INNOPATHS data
  INNOPATHS <- readSource("INNOPATHS")
  INNOPATHS <- add_dimension(INNOPATHS, dim = 3.1, add = "model", nm = "INNOPATHS")
  
  # JRC IDEES data
  JRC_Industry <- calcOutput("JRC_IDEES", subtype = "Industry", aggregate = FALSE)
  JRC_Industry <- add_dimension(JRC_Industry, dim = 3.1, add = "model", nm = "JRC")

  JRC_Transport <- calcOutput("JRC_IDEES", subtype = "Transport", aggregate = FALSE)
  JRC_Transport <- add_dimension(JRC_Transport, dim = 3.1, add = "model", nm = "JRC")

  JRC_ResCom <- calcOutput("JRC_IDEES", subtype = "ResCom", aggregate = FALSE)
  JRC_ResCom <- add_dimension(JRC_ResCom, dim = 3.1, add = "model", nm = "JRC")

  # AGEB final energy data
  AGEB_FE <- calcOutput("AGEB", aggregate = FALSE)
  AGEB_FE <- add_dimension(AGEB_FE, dim = 3.1, add = "model", nm = "AGEB")
  
  # UBA Emission data
  UBA_emi <- calcOutput("UBA", aggregate = FALSE)
  UBA_emi <- add_dimension(UBA_emi, dim = 3.1, add = "model", nm = "UBA")
  
  # UNFCCC emission data
  UNFCCC <- calcOutput("UNFCCC", aggregate = FALSE)
  UNFCCC <- add_dimension(UNFCCC, dim = 3.1, add = "model", nm = "UNFCCC")
  
  # BP data
  BP_Emi <- calcOutput("BP", subtype = "Emission", aggregate = FALSE)
  BP_Emi <- add_dimension(BP_Emi, dim = 3.1, add = "model", nm = "BP")
  BP_Cap <- calcOutput("BP", subtype = "Capacity", aggregate = FALSE)
  BP_Cap <- add_dimension(BP_Cap, dim = 3.1, add = "model", nm = "BP")
  BP_Gen <- calcOutput("BP", subtype = "Generation", aggregate = FALSE)
  BP_Gen <- add_dimension(BP_Gen, dim = 3.1, add = "model", nm = "BP")
  BP_Consump <- calcOutput("BP", subtype = "Consumption", aggregate = FALSE)
  BP_Consump <- add_dimension(BP_Consump, dim = 3.1, add = "model", nm = "BP")
  BP_Trad <- calcOutput("BP", subtype = "Trade", aggregate = FALSE)
  BP_Trad <- add_dimension(BP_Trad, dim = 3.1, add = "model", nm = "BP")
  BP_Price <- calcOutput("BP", subtype = "Price", aggregate = FALSE)
  BP_Price <- add_dimension(BP_Price, dim = 3.1, add = "model", nm = "BP")

  WEO_2021 <- calcOutput("IEA_WEO_2021", subtype = "GLO", aggregate = F)
  WEO_2021_reg <- calcOutput("IEA_WEO_2021", subtype = "regional", aggregate = F)

  #====== start: blow up to union of years ===================
  # find all existing years (y) and variable names (n) 
  
  varlist <- list(fe_iea, fe_weo, fe_proj, fe_hre, pe_iea, pe_weo, trade, pop, gdpp_James, gdpp_WB, gdpp_IMF, ceds, edgar6, primap, cdiac,
                  LU_EDGAR_LU, LU_CEDS, LU_FAO_EmisLUC, LU_FAO_EmisAg, LU_PRIMAPhist, IRENAcap, eurostat, #emiMktES, emiMktETS, emiMktESOthers, 
                  EU_ReferenceScenario, emiEurostat, ARIADNE_ReferenceScenarioGdp, ARIADNE_ReferenceScenarioGdpCorona,
                  ARIADNE_ReferenceScenarioPop, EEA_GHGSectoral, EEA_GHGTotal, EEA_GHGProjections, Emi_Reference, #, EEA_GHGES
                  IEA_ETP, INNOPATHS, JRC_Industry, JRC_Transport, JRC_ResCom, AGEB_FE, UBA_emi, UNFCCC,
                  BP_Emi, BP_Cap, BP_Gen, BP_Consump, BP_Trad, BP_Price, WEO_2021, WEO_2021_reg)

  y <- Reduce(union,lapply(varlist,getYears))
  n <- Reduce(c,lapply(varlist,getNames))
  y <- sort(y)
  
  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getRegions(fe_iea),y,n,fill=NA)
  getSets(data)[3]<- "model"
  getSets(data)[4]<- "variable"

  # transfer data of existing years
  for (i in varlist) {
    data[,getYears(i),getNames(i)] <- i
  }
  #====== end: blow up to union of years ===================

  # add scenario dimension
  data <- add_dimension(data,dim=3.1,add="scenario",nm="historical")
  # rename dimension "data" into "variable"
  getSets(data)[5] <- "variable"
  
  # rename emission variables generated by calcValidEmissions (magpie) to the names generated by calcEmissions (remind)
  # note: spelling for the same gas might be different across historical sources
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land Use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CO2|Land|+|Land-use Change", "Emi|CO2|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|Agriculture",       "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|CH4|Land|+|Agriculture",     "Emi|CH4|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|Agriculture",       "Emi|N2O|Land Use", getNames(data), fixed = TRUE)
  getNames(data) <- gsub("Emissions|N2O|Land|+|Agriculture",     "Emi|N2O|Land Use", getNames(data), fixed = TRUE)
  
  # change unit from Mt to kt for N2O from calcValidEmissions (magpie) 
  vars_with_unit_Mt <- getNames(data[,,"(Mt N2O/yr)",pmatch=T])
  data[,,vars_with_unit_Mt] <- data[,,vars_with_unit_Mt] * 1000
  getNames(data) <- gsub("(Mt N2O/yr)", "(kt N2O/yr)", getNames(data), fixed = TRUE)
  
  return(list(x=data,weight=NULL,unit="Various",description="Historical Data"))
}
