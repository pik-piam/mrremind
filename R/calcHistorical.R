#' @importFrom magclass setNames getNames getSets


calcHistorical <- function() {
  
  # Final Energy
  fe <- calcOutput("FE",aggregate=FALSE)
  fe <- add_dimension(fe, dim=3.1, add="model",nm="IEA")
  
  # Final Energy
  fe_proj_ssp1 <- calcOutput("FE", source = "EDGE_projections", scenario_proj = "SSP1",aggregate=FALSE)
  fe_proj_ssp1 <- add_dimension(fe_proj_ssp1, dim=3.1, add="model",nm="EDGE_SSP1")
  fe_proj_ssp2 <- calcOutput("FE", source = "EDGE_projections", scenario_proj = "SSP2",aggregate=FALSE)
  fe_proj_ssp2 <- add_dimension(fe_proj_ssp2, dim=3.1, add="model",nm="EDGE_SSP2")
  fe_proj = mbind(fe_proj_ssp1,fe_proj_ssp2)
  fe_proj <- fe_proj[,getYears(fe_proj,T)[which(getYears(fe_proj,T) <= 2100)],] # get rid of periods after 2100
  
  
  # Primary Energy
  pe <- calcOutput("PE",aggregate=FALSE)
  pe <- add_dimension(pe, dim=3.1, add="model",nm="IEA")
  
  # fossil trade
  trade <- calcOutput("Trade",aggregate=FALSE)
  trade <- add_dimension(trade, dim=3.1, add="model",nm="IEA")
 
  # Population
  pop <- calcOutput("PopulationPast",aggregate=FALSE)
  unit=strsplit(grep("unit",attributes(pop)$comment,value=TRUE),split = ": ")[[1]][[2]]
  getNames(pop) <- paste0("Population (",unit,")")
  pop <- add_dimension(pop, dim=3.1, add="model",nm="WDI")
  
  # GDP in ppp
  gdpp <- calcOutput("GDPpppPast",aggregate=FALSE) / 1000
  getNames(gdpp) <- paste0("GDP|PPP (billion US$2005/yr)")
  gdpp <- add_dimension(gdpp, dim=3.1, add="model",nm="IHME_USD05_PPP_pc")
  
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
  
  # Add GHG total 
  ceds <- add_columns(ceds,"Emi|GHGtot (Mt CO2-equiv/yr)",dim=3.1)
  ceds[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] <- ceds[,,"Emi|CO2 (Mt CO2/yr)"] + ceds[,,"Emi|CH4 (Mt CH4/yr)"]*28 + ceds[,,"Emi|N2O (kt N2O/yr)"]/1000*265
  ceds <- add_dimension(ceds, dim=3.1, add="model",nm="CEDS")

  # Historical emissions from EDGAR data base
  edgar <- calcOutput("Emissions",datasource="EDGAR",aggregate=FALSE)
  getNames(edgar) <- gsub("Emissions","Emi",getNames(edgar))
  edgar <- add_dimension(edgar, dim=3.1, add="model",nm="EDGAR")

  # Historical emissions from CDIAC data base
  cdiac <- calcOutput("Emissions",datasource="CDIAC",aggregate=FALSE)
  getNames(cdiac) <- gsub("Emissions","Emi",getNames(cdiac))
  #getNames(cdiac) <- gsub("Energy and Industrial Processes \\(Mt\\/yr\\)","Fossil Fuels and Industry w\\/o Bunkers \\(Mt CO2\\/yr\\)",getNames(cdiac))
  #cdiac <- mbind(cdiac, setNames(cdiac[,,"Emi|CO2|Fossil Fuels and Industry w/o Bunkers (Mt CO2/yr)"] 
  #                             + cdiac[,,"Emi|CO2|Energy|Bunkers (Mt/yr)"],"Emi|CO2|Fossil Fuels and Industry (Mt CO2/yr)"))
  cdiac <- add_dimension(cdiac, dim=3.1, add="model",nm="CDIAC")

  #====== start: blow up to union of years ===================
  # find all existing years (y) and variable names (n) 
  
  varlist <- list( fe, fe_proj, pe, trade, pop, gdpp, ceds, edgar, cdiac )
  y <- Reduce(union,lapply(varlist,getYears))
  n <- Reduce(c,lapply(varlist,getNames))
  y <- sort(y)
  
  # create empty object with full temporal, regional and data dimensionality
  data <- new.magpie(getRegions(fe),y,n,fill=NA)
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
  
  return(list(x=data,weight=NULL,unit="Various",description="Historical Data"))
}
