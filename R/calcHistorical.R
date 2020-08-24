#' @importFrom magclass setNames getNames getSets add_columns
#' @importFrom luscale rename_dimnames


calcHistorical <- function() {
  
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
  gdpp_James <- calcOutput("GDPpppPast",aggregate=FALSE) / 1000
  getNames(gdpp_James) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_James <- add_dimension(gdpp_James, dim=3.1, add="model",nm="James_IHME")
  
  gdpp_WB <- calcOutput("GDPpppPast",GDPpppPast="WB_USD05_PPP_pc",aggregate=FALSE) / 1000
  getNames(gdpp_WB) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_WB <- add_dimension(gdpp_WB, dim=3.1, add="model",nm="James_WB")
  
  gdpp_IMF <- calcOutput("GDPpppPast",GDPpppPast="IMF_USD05_PPP_pc",aggregate=FALSE) / 1000
  getNames(gdpp_IMF) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp_IMF <- add_dimension(gdpp_IMF, dim=3.1, add="model",nm="James_IMF")
  
  # Historical emissions from CEDS data base (Steve Smith)
    # ceds16 <- calcOutput("Emissions",datasource="CEDS16")
    # map_CEDS16toCEDS9  <- read.csv(toolMappingFile("sectoral", "mappingCEDS16toCEDS9.csv"), stringsAsFactors=FALSE)
    # # aggregate from CEDS16 to CEDS9 sectors
    # ceds9 <- groupAggregate(ceds16,vectorfunction = "sum",dim=3.1,query = map_CEDS16toCEDS9, from="CEDS16",to="CEDS9")
    # # Rename CEDS9 1:1 to REMIND names (no aggregation)
    # map_CEDS9toREMIND  <- read.csv(toolMappingFile("sectoral", "mappingCEDS9toREMINDreporting.csv"), stringsAsFactors=FALSE)
    # ceds <-groupAggregate(ceds9,vectorfunction = "sum",dim=3.1,query = map_CEDS9toREMIND, from="CEDS9",to="REMIND")
    # 
    # # get variables names right
    # ceds[,,"N2O"] <- ceds[,,"N2O"] * 1000 # Mt -> kt
    # # change order, add "Emissions|": Waste.SO2.harm -> Emissions|SO2|Waste|harm
    # tmp <- gsub("^([^\\.]*)\\.(.*$)","Emi|\\2|\\1 (Mt \\2/yr)",getNames(ceds))
    # tmp <- gsub("Mt N2O","kt N2O",tmp)
    # tmp <- gsub("\\|SO2\\|","\\|Sulfur\\|",tmp)
    # # Add full scenario name
    # getNames(ceds) <- tmp
    # getSets(ceds) <- c("region","year","variable")
    # ceds <- add_dimension(ceds, dim=3.1, add="model",nm="CEDS")
    #
    # # Add some sectoral sums
    # # Emi|CO2|Energy and Industrial Processes, 
    # new_trans <- setNames(ceds[,,"Emi|CO2|Energy|Demand|Transportation|Aviation (Mt CO2/yr)"] +
    #                       ceds[,,"Emi|CO2|Energy|Demand|Transportation|International Shipping (Mt CO2/yr)"] +
    #                       ceds[,,"Emi|CO2|Energy|Demand|Transportation|Ground Transportation (Mt CO2/yr)"],
    #                       "Emi|CO2|Energy|Demand|Transportation (Mt CO2/yr)")
    # 
    # new_energy_ind <- setNames(ceds[,,"Emi|CO2|Energy|Demand|Transportation|Aviation (Mt CO2/yr)"] +
    #                            ceds[,,"Emi|CO2|Energy|Demand|Transportation|International Shipping (Mt CO2/yr)"] +
    #                            ceds[,,"Emi|CO2|Energy|Demand|Transportation|Ground Transportation (Mt CO2/yr)"],
    #                            "Emi|CO2|Energy|Demand|Transportation (Mt CO2/yr)")
    # 
    # tmp <- setNames(ceds[,,"Emi|CO2|Energy|Supply (Mt CO2/yr)"] + ceds[,,],"Emi|CO2|Energy (Mt CO2/yr)")
    # ceds <- mbind(ceds, tmp)
    
  # Historical emissions from CEDS data base (Steve Smith)
  ceds <- calcOutput("Emissions",datasource="CEDS2REMIND",aggregate=FALSE)
  getNames(ceds) <- gsub("Energy and Industrial Processes","Fossil Fuels and Industry",getNames(ceds))
  
  # Add GHG total 
  ceds <- add_columns(ceds,"Emi|GHGtot (Mt CO2-equiv/yr)",dim=3.1)
  ceds[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] <- ceds[,,"Emi|CO2 (Mt CO2/yr)"] + ceds[,,"Emi|CH4 (Mt CH4/yr)"]*28 + ceds[,,"Emi|N2O (kt N2O/yr)"]/1000*265
  ceds <- add_dimension(ceds, dim=3.1, add="model",nm="CEDS")

  # Historical emissions from EDGAR data base
  edgar <- calcOutput("Emissions",datasource="EDGAR",aggregate=FALSE)
  getNames(edgar) <- gsub("Emissions","Emi",getNames(edgar))
  edgar <- add_dimension(edgar, dim=3.1, add="model",nm="EDGAR")
  
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
  emiMktES <- setNames(readSource("Eurostat_EffortSharing",subtype="emissions"),"Emi|GHG|ES (Mt CO2-equiv/yr)") # Effort Sharing
  emiMktETS <- setNames(dimSums(readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2018),c("2_ Verified emissions.20-99 All stationary installations","3_ Estimate to reflect current ETS scope for allowances and emissions.20-99 All stationary installations")]),"Emi|GHG|ETS (Mt CO2-equiv/yr)") #ETS without aviation
  # national aviation is not included in REMIND ETS yet
  # aviation <- readSource("EEA_EuropeanEnvironmentAgency",subtype="ETS")[,seq(2005,2018),c("2_ Verified emissions.10 Aviation")]
  #set all non EU values to NA (by doing this we are excluding from the ETS the non EU28 countries - Norway, Liechtenstein and Iceland - because REMIND is not including them in the ETS)
  emiMktES[getRegions(emiMktES)[-which(getRegions(emiMktES) %in% EUcountries)],,] <- NA 
  emiMktES <- add_dimension(emiMktES, dim=3.1, add="model",nm="Eurostat")
  ETScountries <- c(EUcountries,"GRL","ISL","LIE","NOR","SJM","CHE")
  emiMktETS[getRegions(emiMktETS)[-which(getRegions(emiMktETS) %in% ETScountries)],,] <- NA 
  emiMktETS <- add_dimension(emiMktETS, dim=3.1, add="model",nm="Eurostat")
  # set remaining emissions to other market - it is missing lulucf (Land use, land-use change, and forestry)
  totalGHG <- dimSums(eurostat[,,c("Emi|GHGtot (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Aviation (Mt CO2-equiv/yr)","Emi|GHG|Bunkers|International Maritime Transport (Mt CO2-equiv/yr)")])
  years <- Reduce(intersect, list(getYears(totalGHG),getYears(emiMktES[,,"Emi|GHG|ES (Mt CO2-equiv/yr)"]),getYears(emiMktETS[,,"Emi|GHG|ETS (Mt CO2-equiv/yr)"])))
  emiMktESOthers <- setNames(collapseNames(totalGHG[,years,] - emiMktES[,years,"Emi|GHG|ES (Mt CO2-equiv/yr)"] - emiMktETS[,years,"Emi|GHG|ETS (Mt CO2-equiv/yr)"]),"Emi|GHG|other - Non ETS and ES (Mt CO2-equiv/yr)")
  emiMktESOthers <- add_dimension(emiMktESOthers, dim=3.1, add="model",nm="Eurostat")
  
  # EU Reference Scenario
  EU_ReferenceScenario <- readSource("EU_ReferenceScenario")
  EU_ReferenceScenarioEU <- EU_ReferenceScenario[EUcountries,,]
  EU_ReferenceScenarioEU[is.na(EU_ReferenceScenarioEU)] <- 0
  EU_ReferenceScenario[EUcountries,,] <- EU_ReferenceScenarioEU[EUcountries,,]
  EU_ReferenceScenario <- add_dimension(EU_ReferenceScenario, dim=3.1, add="model",nm="EU_ReferenceScenario")
  
  #Eurostat emissions
  eurostatEmi <- readSource(type="Eurostat",subtype="emissions")
  eurostatEmi[getRegions(eurostatEmi)[-which(getRegions(eurostatEmi) %in% EUcountries)],,] <- NA 
  emiEurostatEU <- eurostatEmi[EUcountries,,]
  emiEurostatEU[is.na(emiEurostatEU)] <- 0
  emiEurostat <- NULL
  emiEurostat <- mbind(
    setNames(eurostatEmi[,,"CH4.All sectors (excluding memo items)"],"Emi|CH4 (Mt CH4/yr)")/28,
    setNames(eurostatEmi[,,"N2O.All sectors (excluding memo items)"],"Emi|N2O (kt N2O/yr)")/(265 * 44 / 28)*1000 
  )
  emiEurostat <- add_dimension(emiEurostat, dim=3.1, add="model",nm="Eurostat")
  
  #====== start: blow up to union of years ===================
  # find all existing years (y) and variable names (n) 
  
  # varlist <- list( fe, fe_proj, pe, trade, pop, gdpp, ceds, edgar, cdiac, LU_EDGAR_LU, LU_CEDS, LU_FAO_EmisLUC, LU_FAO_EmisAg, LU_PRIMAPhist, LU_IPCC, LU_Nsurplus2)
  varlist <- list( fe_iea,fe_weo, fe_proj, pe_iea,pe_weo, trade, pop, gdpp_James, gdpp_WB, gdpp_IMF, ceds, edgar, primap, cdiac, LU_EDGAR_LU, LU_CEDS, LU_FAO_EmisLUC, LU_FAO_EmisAg, LU_PRIMAPhist, IRENAcap, emiMktES, emiMktETS, emiMktESOthers, EU_ReferenceScenario, emiEurostat)

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
