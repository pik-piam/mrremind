#' @importFrom magclass setNames getNames<-
#' @importFrom utils read.csv2
#' @importFrom dplyr filter_ mutate_


calcEmissions <- function(datasource="CEDS16") {
  if (datasource == "CEDS16") {
      # read CEDS emissions data from sources
      bc    <- readSource("CEDS",subtype="BC")
      ch4   <- readSource("CEDS",subtype="CH4")
      co    <- readSource("CEDS",subtype="CO")
      co2   <- readSource("CEDS",subtype="CO2")#,convert = FALSE)
      n2o   <- readSource("CEDS",subtype="N2O")
      nh3   <- readSource("CEDS",subtype="NH3")
      nox   <- readSource("CEDS",subtype="NOx")
      nmvoc <- readSource("CEDS",subtype="NMVOC")
      oc    <- readSource("CEDS",subtype="OC")
      so2   <- readSource("CEDS",subtype="SO2")
      
      y <- Reduce(intersect,list(getYears(bc),
                                 getYears(ch4),
                                 getYears(co),
                                 getYears(co2),
                                 getYears(n2o),
                                 getYears(nh3),
                                 getYears(nox),
                                 getYears(nmvoc),
                                 getYears(oc),
                                 getYears(so2)))
                             
      emi <- mbind(bc[,y,],ch4[,y,],co[,y,],co2[,y,],n2o[,y,],nh3[,y,],nox[,y,],nmvoc[,y,],oc[,y,],so2[,y,]) / 1000 # kt -> Mt
      rm(bc,ch4,co,co2,n2o,nh3,nox,nmvoc,oc,so2)
      
      if (any(!emi[,,"6B_Other-not-in-total"]==0)) cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data! Please check CEDS source files.\n")
      
      emi <- emi[,,"6B_Other-not-in-total",invert=TRUE]
      
      # aggregate and rename CEDS59 sectors to CEDS16 sectors      
      map_CEDS59_to_CEDS16  <- read.csv(toolMappingFile("sectoral", "mappingCEDS59toCEDS16.csv"), stringsAsFactors=FALSE)
      tmp <- toolAggregate(x=emi,weight = NULL, dim=3.1, rel = map_CEDS59_to_CEDS16, from="CEDS59",to="CEDS16")

      # rename emissions according to map
      map <- c(BC="BC",CH4="CH4",CO="CO",CO2="CO2",N2O="N2O",NH3="NH3",NOx="NOx",NMVOC="VOC",OC="OC",SO2="SO2")
      getNames(tmp,dim=2) <- map[getNames(tmp,dim=2)]
      
      # remove third entry "kt" in data dimension
      tmp <- collapseNames(tmp,collapsedim = 3)
      
} else if (datasource == "CEDS2REMIND") {
    # read CEDS emissions data from sources
    bc    <- readSource("CEDS",subtype="BC")
    ch4   <- readSource("CEDS",subtype="CH4")
    co    <- readSource("CEDS",subtype="CO")
    co2   <- readSource("CEDS",subtype="CO2")#,convert = FALSE)
    n2o   <- readSource("CEDS",subtype="N2O")
    nh3   <- readSource("CEDS",subtype="NH3")
    nox   <- readSource("CEDS",subtype="NOx")
    nmvoc <- readSource("CEDS",subtype="NMVOC")
    oc    <- readSource("CEDS",subtype="OC")
    so2   <- readSource("CEDS",subtype="SO2")
    
    y <- Reduce(intersect,list(getYears(bc),
                               getYears(ch4),
                               getYears(co),
                               getYears(co2),
                               getYears(n2o),
                               getYears(nh3),
                               getYears(nox),
                               getYears(nmvoc),
                               getYears(oc),
                               getYears(so2)))
    
    emi <- mbind(bc[,y,],ch4[,y,],co[,y,],co2[,y,],n2o[,y,],nh3[,y,],nox[,y,],nmvoc[,y,],oc[,y,],so2[,y,]) / 1000 # kt -> Mt
    rm(bc,ch4,co,co2,n2o,nh3,nox,nmvoc,oc,so2)
    
    if (any(!emi[,,"6B_Other-not-in-total"]==0)) cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data! Please check CEDS source files.\n")
    
    emi <- emi[,,"6B_Other-not-in-total",invert=TRUE]
    
    # aggregate and rename CEDS59 sectors to CEDS16 sectors      
    map_CEDS59_to_CEDS16  <- read.csv(toolMappingFile("sectoral", "mappingCEDS59toREMINDreporting.csv"), stringsAsFactors=FALSE)
    emi <- toolAggregate(x=emi,weight = NULL, dim=3.1, rel = map_CEDS59_to_CEDS16, from="CEDS59",to="REMIND")
    
    # rename emissions according to map (currently only relevant for VOC)
    map <- c(BC="BC",CH4="CH4",CO="CO",CO2="CO2",N2O="N2O",NH3="NH3",NOx="NOx",NMVOC="VOC",OC="OC",SO2="SO2")
    getNames(emi,dim=2) <- map[getNames(emi,dim=2)]
    
    # remove third entry "kt" in data dimension
    emi <- collapseNames(emi,collapsedim = 3)
    
    # sectoral sums
    emi <- add_columns(emi,"Energy|Demand|Transportation",dim=3.1)
    emi[,,"Energy|Demand|Transportation"] <- emi[,, "Energy|Demand|Transportation|Aviation"] + 
                                             emi[,,"Energy|Demand|Transportation|Ground Transportation"] +
                                             emi[,,"Energy|Demand|Transportation|International Shipping"]
    
    emi <- add_columns(emi,"Energy|Supply",dim=3.1)
    emi[,,"Energy|Supply"] <- emi[,,"Energy|Supply|Electricity"] +
                              emi[,,"Energy|Supply|Heat"] +
                              emi[,,"Energy|Supply|Fuel Production"]
    
    emi <- add_columns(emi,"Energy|Demand",dim=3.1)
    emi[,,"Energy|Demand"] <- emi[,,"Energy|Demand|Transportation"] +
                              emi[,,"Energy|Demand|Residential and Commercial"] +
                              emi[,,"Energy|Demand|Industry"]
    
    emi <- add_columns(emi,"Energy",dim=3.1)
    emi[,,"Energy"] <- emi[,,"Energy|Demand"] +
                       emi[,,"Energy|Supply"]

    emi <- add_columns(emi,"Energy Supply and Demand",dim=3.1)
    emi[,,"Energy Supply and Demand"] <- emi[,,"Energy"]
    
    emi <- add_columns(emi,"Energy and Industrial Processes",dim=3.1)
    emi[,,"Energy and Industrial Processes"] <- emi[,,"Energy"] +
                                                emi[,,"Industrial Processes"]

    emi <- add_columns(emi,"Land Use",dim=3.1)
    emi[,,"Land Use"] <- emi[,,"Land Use|Agriculture and Biomass Burning"] +
                      emi[,,"Land Use|Forest Burning"] +
                      emi[,,"Land Use|Grassland Burning"]

    emi <- add_columns(emi,"Total",dim=3.1)
    emi[,,"Total"] <- emi[,,"Energy"] + 
                      emi[,,"Industrial Processes"] + 
                      emi[,,"Land Use"] +
                      emi[,,"Solvents"] +
                      emi[,,"Waste"]

    # get variables names right
    emi[,,"N2O"] <- emi[,,"N2O"] * 1000 # Mt -> kt
    # change order, add "Emissions|", and reduce to a single dimension by replacing dots: Waste.SO2.harm -> Emissions|SO2|Waste|harm
    tmp <- gsub("^([^\\.]*)\\.(.*$)","Emi|\\2|\\1 (Mt \\2/yr)",getNames(emi))
    # remove "Total" from variable name
    tmp <- gsub("\\|Total","",tmp)
    tmp <- gsub("Mt N2O","kt N2O",tmp)
    tmp <- gsub("\\|SO2\\|","\\|Sulfur\\|",tmp)
    # Add full scenario name
    getNames(emi) <- tmp
    getSets(emi) <- c("region","year","variable")
    tmp <- emi
    
#   } else if (datasource == "EDGAR") {
#     # Allocation to sectors the same for all pollutants
#     # read EDGAR emissions from sources
#     co    <- readSource("EDGAR",subtype="CO")
#     co <- add_dimension(co, dim=3.2, add="em",nm="CO")
#     voc   <- readSource("EDGAR",subtype="VOC")
#     voc <- add_dimension(voc, dim=3.2, add="em",nm="VOC")
#     nox   <- readSource("EDGAR",subtype="NOx")
#     nox <- add_dimension(nox, dim=3.2, add="em",nm="NOx")
#     nh3   <- readSource("EDGAR",subtype="NH3") 
#     nh3 <- add_dimension(nh3, dim=3.2, add="em",nm="NH3")
#     so2   <- readSource("EDGAR",subtype="SO2")
#     so2 <- add_dimension(so2, dim=3.2, add="em",nm="SO2")
#     pm10  <- readSource("EDGAR",subtype="PM10")
#     pm10 <- add_dimension(pm10, dim=3.2, add="em",nm="PM10")
# 
#     edgar <- mbind(co,nox,so2,voc,nh3,pm10) / 1000 # kt = Gg -> Mt
#     
#     map <- c(CO="CO",NOx="NOx",SO2="Sulfur",VOC="VOC",NH3="NH3",PM10="PM10")
#     tmp <- NULL
#     for (i in getNames(edgar,dim=2)) {
#       vars <- c("1A2","2A1","2A2","2A7","2B","2C","2D","2E","2F1","2F2","2F3","2F4","2F5","2F7","2F8","2F9","2G")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Industry (Mt/yr)")))
#       vars <- c("1A4")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Residential and Commercial (Mt/yr)")))
#       #vars <- c("1C1","1A3a", "1A3b", "1A3c", "1A3d", "1A3e","1C2")
#       #tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation (Mt/yr)")))
#       vars <- c("1C1")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|Aviation (Mt/yr)")))
#       vars <- c("1A3a", "1A3b", "1A3c", "1A3d", "1A3e")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|Ground Transportation (Mt/yr)")))
#       vars <- c("1C2")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|International Shipping (Mt/yr)")))
#       vars <- c("1A1a","1A1bc","1B1","1B2")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Supply (Mt/yr)")))
#       vars <- c("4F")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Agricultural Waste Burning (Mt/yr)")))
#       vars <- c("4A","4B","4C","4D1","4D2","4D3","4D4")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Agriculture (Mt/yr)")))
#       vars <- c("5A", "5D", "5F", "5F2")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Forest Burning (Mt/yr)")))
#       vars <- c("3A", "3B", "3C", "3D")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Solvents (Mt/yr)")))
#       vars <- c("6A","6B","6C","6D")
#       tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Waste (Mt/yr)")))
#     }
#     
  } else if (datasource == "EDGAR") {
    # read EDGAR emissions from sources
    co    <- readSource("EDGAR",subtype="CO")
    co <- add_dimension(co, dim=3.2, add="em",nm="CO")
    voc   <- readSource("EDGAR",subtype="VOC")
    voc <- add_dimension(voc, dim=3.2, add="em",nm="VOC")
    nox   <- readSource("EDGAR",subtype="NOx")
    nox <- add_dimension(nox, dim=3.2, add="em",nm="NOx")
    nh3   <- readSource("EDGAR",subtype="NH3") 
    nh3 <- add_dimension(nh3, dim=3.2, add="em",nm="NH3")
    so2   <- readSource("EDGAR",subtype="SO2")
    so2 <- add_dimension(so2, dim=3.2, add="em",nm="SO2")
    pm10  <- readSource("EDGAR",subtype="PM10")
    pm10 <- add_dimension(pm10, dim=3.2, add="em",nm="PM10")
    
    edgar <- mbind(co,nox,so2,voc,nh3,pm10) / 1000 # kt = Gg -> Mt
    
    # Allocation of pollutants to sectors: special treatment for solvents
    map <- c(CO="CO",NOx="NOx",SO2="Sulfur",VOC="VOC",NH3="NH3",PM10="PM10")
    tmp <- NULL
    for (i in names(map)) {
      vars <- c("1A4")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Residential and Commercial (Mt/yr)")))
      #vars <- c("1C1","1A3a", "1A3b", "1A3c", "1A3d", "1A3e","1C2")
      #tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation (Mt/yr)")))
      vars <- c("1C1")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|Aviation (Mt/yr)")))
      vars <- c("1A3a", "1A3b", "1A3c", "1A3d", "1A3e")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|Ground Transportation (Mt/yr)")))
      vars <- c("1C2")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Transportation|International Shipping (Mt/yr)")))
      vars <- c("1A1a","1A1bc","1B1","1B2")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Supply (Mt/yr)")))
      vars <- c("4F")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Agricultural Waste Burning (Mt/yr)")))
      vars <- c("4A","4B","4C","4D1","4D2","4D3","4D4")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Agriculture (Mt/yr)")))
      vars <- c("5A", "5D", "5F", "5F2")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Land Use|Forest Burning (Mt/yr)")))
      vars <- c("6A","6B","6C","6D")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Waste (Mt/yr)")))
    }
    # no extrawurst for voc
    map <- c(VOC="VOC")
    for (i in names(map)) {
      vars <- c("1A2","2A1","2A2","2A7","2B","2C","2D","2E","2F1","2F2","2F3","2F4","2F5","2F7","2F8","2F9","2G")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Industry (Mt/yr)")))
      vars <- c("3A", "3B", "3C", "3D")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Solvents (Mt/yr)")))
    }
    # extrawurst for everything but voc: industry = industry + solvents
    map <- c(CO="CO",NOx="NOx",SO2="Sulfur",NH3="NH3",PM10="PM10")
    for (i in names(map)) {
      vars <- c("1A2","2A1","2A2","2A7","2B","2C","2D","2E","2F1","2F2","2F3","2F4","2F5","2F7","2F8","2F9","2G","3A", "3B", "3C", "3D")
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1), paste0("Emissions|",map[i],"|Energy Demand|Industry (Mt/yr)")))
      # add zero dummy or Solvents
      tmp <- mbind(tmp, setNames(dimSums(edgar[,,i][,,vars],dim=3.1)*0, paste0("Emissions|",map[i],"|Solvents (Mt/yr)")))
    }
    
  } else if (datasource == "LIMITS") {
    # read LIMITS emissions from sources
    em_limits <- readSource("LIMITS", subtype="emissions")
    
    em_limits <- collapseNames(em_limits[,,"kt"][,,"CLE"][,,"Unattributed", invert=TRUE])
    
    getNames(em_limits, dim=2)[which(getNames(em_limits, dim=2) == "NOX")] <- "NOx" 
    
    map <- read.csv2(toolMappingFile("sectoral", "mappingLIMITSsectorstoREMINDsectors.csv"), stringsAsFactors=TRUE)
    
    em_limits[is.na(em_limits)] <- 0.0
    
    tmp <- NULL
    for (kap in getNames(em_limits, dim=2)) {
      tmp_limits <- collapseNames(em_limits[,,kap])
      
      tmp_map <- map %>% 
        filter_(~limits_sector %in% getNames(tmp_limits, dim=1)) %>% 
        mutate_(remind_sector = ~paste(sub("POLLUTANT", kap, remind_sector), "(Mt/yr)"))
      
      tmp <- mbind(tmp, toolAggregate(tmp_limits, tmp_map, dim=3.1))
      
    }
    
    tmp <- tmp / 1000 # kt = Gg -> Mt
    
    #tmp <- tmp[,2005,]

  } else if (datasource == "ECLIPSE") {
    # read ECLIPSE emissions from sources
    em_eclipse <- readSource("ECLIPSE", subtype="emissions.aggregated")
    
    em_eclipse <- collapseNames(em_eclipse[,,"CLE"])
    
    #map <- read.csv(toolMappingFile("sectoral", "mappingECLIPSEtoAggREMINDsectors.csv"), stringsAsFactors=TRUE)
    map <- read.csv2(toolMappingFile("sectoral", "mappingECLIPSEsectorstoREMINDsectors.csv"), stringsAsFactors=TRUE)
    
    em_eclipse[is.na(em_eclipse)] <- 0.0
    
    tmp <- NULL
    for (kap in getNames(em_eclipse, dim=2)) {
      tmp_eclipse <- collapseNames(em_eclipse[,,kap])
      
      tmp_map <- map %>% 
        filter_(~eclipse_sector %in% getNames(tmp_eclipse, dim=1)) %>% 
        mutate_(remind_sector = ~paste(sub("POLLUTANT", kap, remind_sector), "(Mt/yr)"))
      
      tmp <- mbind(tmp, toolAggregate(tmp_eclipse, tmp_map, dim=3.1))
      
    }
    
    tmp <- tmp / 1000 # kt = Gg -> Mt
    
    #tmp <- tmp[,2005,]
    
  } else if (datasource == "GFED") {  
    data <- readSource("GFED")
    # aggregate gfed sectors to sectors of all other data
    emi_subset <-  c("SO2","OC","NOx","NH3","CO","BC")
    tmp <- new.magpie(getRegions(data),getYears(data),c("agriwaste","forest","savannah"))
    tmp <- add_dimension(tmp,add="emi",dim=3.1,nm=emi_subset)
    tmp[,,"savannah"]  <- data[,,"savanna"][,,emi_subset]
    tmp[,,"agriwaste"] <- data[,,"agricultural_waste"][,,emi_subset]
    tmp[,,"forest"]    <- (data[,,"boreal"] + data[,,"temperate"] + data[,,"peat"] + data[,,"deforestation"])[,,emi_subset]
    tmp[,,"CO"] <- tmp[,,"CO"] / 10 # 1E11g -> Mt
    tmp[,,c("NH3","SO2","BC")] <- tmp[,,c("NH3","SO2","BC")] / 1000 # 1E9g -> Mt
    tmp[,,c("NOx","OC")] <- tmp[,,c("NOx","OC")] / 100 # 1E10g -> Mt
    
    getNames(tmp,dim=2)<- gsub("SO2","Sulfur",getNames(tmp,dim=2)) # rename SO2 -> Sulfur
  
  } else if (datasource == "CDIAC") {
    data <- readSource("CDIAC")
    
    # omitting "PerCap"
    map <- c(FFIC    ="Fossil Fuels and Industry", # <--- old name. New name wil be: "Energy and Industrial Processes",
             Solids  ="Energy|Solids",
             Liquids ="Energy|Liquids",
             Gases   ="Energy|Gases",
             Cement  ="Industrial Processes|Cement",
             Flaring ="Energy|Flaring",
             Bunker  ="Energy|Bunkers")
    
    tmp <- NULL
    for (i in names(map)) {
      tmp <- mbind(tmp, setNames(data[,,i],paste0("Emissions|CO2|",map[i]," (Mt/yr)")))
    }

    tmp <- tmp * 44/12 /1000 # from ktC -> MtCO2
  }

  return(list(x=tmp,
              weight=NULL,
              unit="Mt",
              description="historic emissions in 1970-2014"))
}
