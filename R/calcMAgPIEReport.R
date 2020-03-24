

calcMAgPIEReport <- function(subtype){
  
  x <- readSource("MAgPIE", subtype = "MAgPIEReport_extensive")

  if (subtype == "CostTotal") {
    
    x <- x[,,"Costs|MainSolve w/o GHG Emissions (million US$05/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    d <- "Total Landuse Costs from MAgPIE excluding emission costs"
    u <- "T$2005/yr"
  
  } else if (subtype == "CostMAC") {
    
    x <- x[,,"Costs|MainSolve|MACCS (million US$05/yr)"]/1000/1000 # with transformation factor from 10E6 US$2005 to 10E12 US$2005
    d <- "MAC Costs for LU emissions from MAgPIE"
    u <- "T$2005/yr"
    
  } else if (subtype == "ProductionBiomass") {
  
    x <- x[,,"Demand|Bioenergy|++|2nd generation (EJ/yr)"]/31.536 # EJ to TWa
    d <- "Production of ligno-cellulosic purpose grown biomass in MAgPIE"
    u <- "TWa/yr"
    
  } else if (subtype == "ch4n2o") {
    
    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Animal waste management (Mt CH4/yr);ch4anmlwst",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Enteric fermentation (Mt CH4/yr);ch4animals",
      "Emissions before technical mitigation|CH4|Land|Agriculture|+|Rice (Mt CH4/yr);ch4rice",
      "Emissions before technical mitigation|N2O|Land|Agriculture|+|Animal Waste Management (Mt N2O/yr);n2oanwstm",               
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Decay of Crop Residues (Mt N2O/yr);n2ofertcr",     
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Inorganic Fertilizers (Mt N2O/yr);n2ofertin",      
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Manure applied to Croplands (Mt N2O/yr);n2oanwstc",
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Pasture (Mt N2O/yr);n2oanwstp",                    
      "Emissions before technical mitigation|N2O|Land|Agriculture|Agricultural Soils|+|Soil Organic Matter Loss (Mt N2O/yr);n2ofertsom")

    x <- x[,,mapping$oldnames]
    # rename
    getNames(x,dim=3) <- mapping$newnames
    d <- "CH4 and N2O land emissions"
    u <- "MtCH4/yr and Mt N2O/yr"

  } else if (subtype == "co2") {
    
    mapping <- inline.data.frame(
      "oldnames;newnames",
      "Emissions|CO2|Land (Mt CO2/yr);co2luc")
    
    x <- x[,,mapping$oldnames]
    # rename
    getNames(x,dim=3) <- mapping$newnames
    d <- "CO2 land emissions"
    u <- "Mt CO2/yr"
    
  } else {
    stop("Unknown subtype",subtype)
  }

  # remove model and variable name
  x <- collapseNames(x)

  # !!! ATTENTION !!! 
  # If you change the name of the baseline scenario from "none" to something else update "none" in calcMacBaseLandUse.R
  
  # rename the MAgPIE scenarios to RCP scenarios
  getNames(x) <- gsub("C_SDP-PkBudg900-mag-4",  "SDP-rcp20",getNames(x))
  getNames(x) <- gsub("C_SDP-PkBudg1300-mag-4", "SDP-rcp26",getNames(x))
  getNames(x) <- gsub("C_SDP-NDC-mag-4",        "SDP-rcp45",getNames(x))
  getNames(x) <- gsub("C_SDP-Base-mag-4",       "SDP-none", getNames(x))
  
  getNames(x) <- gsub("C_SSP1-PkBudg900-mag-4", "SSP1-rcp20",getNames(x))
  getNames(x) <- gsub("C_SSP1-PkBudg1300-mag-4","SSP1-rcp26",getNames(x))
  getNames(x) <- gsub("C_SSP1-NDC-mag-4",       "SSP1-rcp45",getNames(x))
  getNames(x) <- gsub("C_SSP1-Base-mag-4",      "SSP1-none", getNames(x))
  
  getNames(x) <- gsub("C_SSP2-PkBudg900-mag-4", "SSP2-rcp20",getNames(x))
  getNames(x) <- gsub("C_SSP2-PkBudg1300-mag-4","SSP2-rcp26",getNames(x))
  getNames(x) <- gsub("C_SSP2-NDC-mag-4",       "SSP2-rcp45",getNames(x))
  getNames(x) <- gsub("C_SSP2-Base-mag-4",      "SSP2-none", getNames(x))

  getNames(x) <- gsub("C_SSP5-PkBudg900-mag-4", "SSP5-rcp20",getNames(x))
  getNames(x) <- gsub("C_SSP5-PkBudg1300-mag-4","SSP5-rcp26",getNames(x))
  getNames(x) <- gsub("C_SSP5-NDC-mag-4",       "SSP5-rcp45",getNames(x))
  getNames(x) <- gsub("C_SSP5-Base-mag-4",      "SSP5-none", getNames(x))
  
  # Introduce new SSP/SDP dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9]|SDP)-","\\1.",getNames(x))
  
  return(list(x           = x,
              weight      = NULL,
              unit        = u,
              description = d))
}