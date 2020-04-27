#' @title calcLandEmissions
#' @description Land emission data. This function was originally called calcValidEmissions and located in 
#' mrvalidation.
#'
#' @param datasource The Emission Inventory that shall be used. For futher information, best see moinput function calcEmissionInventory. Options are e.g.  CEDS, combined_CEDS_IPCC (including own estimates where available), IPCC(own estimates), Nsurplus (own estimates)
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LandEmissions")
#' }

#' @importFrom  magpiesets reportingnames
#' @importFrom  luscale groupAggregate
#' 
calcLandEmissions <- function(datasource="CEDS") {
  
  if (datasource%in%c("CEDS","combined_CEDS_IPCC","combined_CEDS_Nsurplus2")){
    
    ceds<-calcOutput("EmissionInventory",
                     datasource=datasource,
                     mapping="mappingCEDS59toMAgPIE.csv",
                     from="CEDS59",
                     to="MAgPIE",
                     aggregate = FALSE)
    
    # add CO2-C, N2O-N, NH3-N and NO2-N (better standard units)
    ceds2<-ceds[,,c("nh3_n","no2_n","no3_n")]
    #ceds2[,,"n2o_n"]=ceds2[,,"n2o_n"]*44/28
    ceds2[,,"nh3_n"]=ceds2[,,"nh3_n"]*17/14
    ceds2[,,"no2_n"]=ceds2[,,"no2_n"]*46/14
    ceds2[,,"no3_n"]=ceds2[,,"no3_n"]*62/14
    #ceds2[,,"co2_c"]=ceds2[,,"co2_c"]*44/12
    map <- c(#co2_c="co2",
             #n2o_n="n2o",
             nh3_n="nh3",no2_n="no2",no3_n="no3")
    getNames(ceds2,dim=2) <- map[getNames(ceds2,dim=2)]
    ceds<-mbind(ceds,ceds2)
    
    
    # rename emissions according to map
    getNames(ceds,dim=2)<-reportingnames(getNames(ceds,dim=2))

    # add sum over sector for each species
    sector_sum <- dimSums(ceds,dim=3.1)
    agri_sum <- dimSums(ceds[,,"Agriculture",pmatch=TRUE],dim=3.1)
    burn_sum <- dimSums(ceds[,,"Biomass Burning",pmatch=TRUE],dim=3.1)
    # getNames(sector_sum) <- paste0(".",getNames(sector_sum))
    sector_sum <- add_dimension(sector_sum,dim=3.1,add="sector",nm="")
    getNames(agri_sum) <- paste0("+|Agriculture.",getNames(agri_sum))
    getNames(burn_sum) <- paste0("+|Biomass Burning.",getNames(burn_sum))
    ceds <- mbind(ceds,agri_sum,burn_sum,sector_sum)
    
    # change order, add "Emissions|": Waste.SO2.harm -> Emissions|SO2|Waste|harm
    tmp <- gsub("^([^\\.]*)\\.(.*$)","historical.ceds.Emissions|\\2|Land|\\1 (Mt \\2/yr)",getNames(ceds))
    
    # Add full scenario name
    getNames(ceds) <- tmp
    getSets(ceds) <- c("region","year","scenario","model","variable")
    ceds <- ceds[,getYears(ceds)>"y1960",]
    out<-ceds
    
  }  else if (datasource%in%c("Nsurplus","Nsurplus2")){
    
    out<-calcOutput("EmisNitrogenPast",method=datasource,aggregate = FALSE)
    out<-add_columns(out,addnm="n2o_n",dim = 3.2)
    out[,,"n2o_n"]<-dimSums(out[,,c("n2o_n_direct","n2o_n_indirect")],dim=3.2)
    out<-out[,,c("n2o_n_direct","n2o_n_indirect"),invert=TRUE]
    out<-out[,,c("accumulation","n2_n"),invert=TRUE]
    out<-add_columns(out,addnm = c("soils","agri"))
    out[,,"soils"]<-dimSums(out[,,c("cropland_soils","pasture_soils")],dim=3.1)
    out[,,"agri"]<-dimSums(out[,,c("soils","awms")],dim=3.1)
    map  <- toolGetMapping(type="sectoral",name="mappingIPCCtoMAgPIE.csv")
    out<-out[,,getNames(out,dim=1)[getNames(out,dim=1)%in%map[,1]]]
    out<-groupAggregate(data=out,dim=3.1,query = map, from="IPCC", to="MAgPIE")
    out<-out[,,sort(getNames(out,dim=1))]
    #adjust units
    # add CO2-C, N2O-N, NH3-N and NO2-N (better standard units)
    out<-out[,,c("n2o_n","nh3_n","no2_n","no3_n")]
    out[,,"n2o_n"]=out[,,"n2o_n"]*44/28
    out[,,"nh3_n"]=out[,,"nh3_n"]*17/14
    out[,,"no2_n"]=out[,,"no2_n"]*46/14
    out[,,"no3_n"]=out[,,"no3_n"]*62/14
    map <- c(n2o_n="n2o",nh3_n="nh3",no2_n="no2",no3_n="no3")
    getNames(out,dim=2) <- map[getNames(out,dim=2)]
    getNames(out,dim=2) <- reportingnames(getNames(out,dim=2))
    
    # change order, add "Emissions|": Waste.SO2.harm -> Emissions|SO2|Waste|harm
    tmp <- gsub("^([^\\.]*)\\.(.*$)","Emissions|\\2|Land|\\1 (Mt \\2/yr)",getNames(out))
    getNames(out) <- tmp
    out <- clean_magpie(out)
  
    # Add full scenario name
    names(dimnames(out))[3] <- "variable"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  }  else if (datasource%in%c("combined_CEDS_PRIMAPhist")){
    
    # WORK IN PROGRESS!!! 
    inventory<-calcOutput("EmissionInventory",
                     datasource=datasource,
                     mapping=NULL,
                     to="PRIMAPhist",
                     aggregate = FALSE)
    
    # add CO2-C, N2O-N, NH3-N and NO2-N (better standard units)
    inventory<-ceds[,,c("nh3_n","no2_n","no3_n")]
    #ceds2[,,"n2o_n"]=ceds2[,,"n2o_n"]*44/28
    ceds2[,,"nh3_n"]=ceds2[,,"nh3_n"]*17/14
    ceds2[,,"no2_n"]=ceds2[,,"no2_n"]*46/14
    ceds2[,,"no3_n"]=ceds2[,,"no3_n"]*62/14
    #ceds2[,,"co2_c"]=ceds2[,,"co2_c"]*44/12
    map <- c(#co2_c="co2",
      #n2o_n="n2o",
      nh3_n="nh3",no2_n="no2",no3_n="no3")
    getNames(ceds2,dim=2) <- map[getNames(ceds2,dim=2)]
    ceds<-mbind(ceds,ceds2)
    
    
    # rename emissions according to map
    getNames(ceds,dim=2)<-reportingnames(getNames(ceds,dim=2))
    
    # add sum over sector for each species
    sector_sum <- dimSums(ceds,dim=3.1)
    getNames(sector_sum) <- paste0("Agriculture.",getNames(sector_sum))
    ceds <- mbind(ceds,sector_sum)
    
    # change order, add "Emissions|": Waste.SO2.harm -> Emissions|SO2|Waste|harm
    tmp <- gsub("^([^\\.]*)\\.(.*$)","historical.ceds.Emissions|\\2|\\1 (Mt \\2/yr)",getNames(ceds))
    
    # Add full scenario name
    getNames(ceds) <- tmp
    getSets(ceds) <- c("region","year","scenario","model","variable")
    ceds <- ceds[,getYears(ceds)>"y1960",]
    out<-ceds
    
  } else if (datasource=="IPCC") {
    
    out <- calcOutput("EmisNitrogenPast",method=datasource,aggregate = FALSE)
    out <- add_columns(out,addnm="n2o_n",dim = 3.2)
    out[,,"n2o_n"] <- dimSums(out[,,c("n2o_n_direct","n2o_n_indirect")],dim=3.2)
    out <- add_columns(out,addnm = c("soils","agri"))
    out[,,"soils"] <- dimSums(out[,,c("inorg_fert","man_crop","resid","som","rice","pasture_soils")],dim=3.1)
    out[,,"agri"]  <- dimSums(out[,,c("soils","awms")],dim=3.1)
    map <- toolGetMapping(type="sectoral",name="mappingIPCCtoMAgPIE.csv")
    out <- out[,,getNames(out,dim=1)[getNames(out,dim=1)%in%map[,1]]]
    out <- groupAggregate(data=out,dim=3.1,query = map, from=datasource, to="MAgPIE")
    out <- out[,,sort(getNames(out,dim=1))]
    
    #adjust units
    # add CO2-C, N2O-N, NH3-N and NO2-N (better standard units)
    out <- out[,,c("n2o_n","nh3_n","no2_n","no3_n")]
    out[,,"n2o_n"]          <- out[,,"n2o_n"]*44/28
    out[,,"nh3_n"]          <- out[,,"nh3_n"]*17/14
    out[,,"no2_n"]          <- out[,,"no2_n"]*46/14
    out[,,"no3_n"]          <- out[,,"no3_n"]*62/14
    map <- c(n2o_n="n2o",nh3_n="nh3",no2_n="no2",no3_n="no3")
    getNames(out,dim=2) <- map[getNames(out,dim=2)]
    getNames(out,dim=2) <- reportingnames(getNames(out,dim=2))
    
    # change order, add "Emissions|": Waste.SO2.harm -> Emissions|SO2|Waste|harm
    tmp <- gsub("^([^\\.]*)\\.(.*$)","Emissions|\\2|Land|\\1 (Mt \\2/yr)",getNames(out))
    getNames(out) <- tmp
    out <- clean_magpie(out)
    
    # Add full scenario name
    names(dimnames(out))[3] <- "variable"
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    
  } else if (datasource=="EDGAR_LU") {
    
    co2 <- readSource("EDGAR_LU",subtype="CO2")*44/12 #convert from C to CO2
    getNames(co2)<-"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
    n2o <- readSource("EDGAR_LU",subtype="N2O")*44/28 # convert from N to N2O
    getNames(n2o)<-"Emissions|N2O|Land|+|Agriculture (Mt N2O/yr)"
    ch4 <- readSource("EDGAR_LU",subtype="CH4")
    getNames(ch4)<-"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"
    
    out <- mbind(co2,n2o,ch4)
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm="EDGAR_LU")
    names(dimnames(out))[3] <- "scenario.model.variable"
    
  } else if (datasource=="FAO_EmisLUC") {
    
    co2 <- readSource("FAO",subtype="EmisLuTotal")
    co2 <- co2[,,"1707|Land Use total + (Total).Net_emissions_removal_(CO2)_(Gigagrams)"]/1000 #convert from Gigagrams to Mt
    getNames(co2)<-"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
    out <- co2
    out <- add_dimension(out, dim=3.1, add="scenario", nm="historical")  
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    names(dimnames(out))[3] <- "scenario.model.variable"
    
  } else if (datasource=="FAO_EmisAg") {
    
    #The global warming potential (GWP) with a time horizon of 100 years based on the IPCC Fourth Assessment Report (AR4) (2007) is used to convert N2O and CH4 to CO2-eq terms. Consequently, GWP of 25 and 298 were used for CH4 and N2O, respectively.
    total <- readSource("FAO",subtype="EmisAgTotal")
    mapping=toolMappingFile(type = "sectoral",name = "FAOitems_emissions.csv",readcsv = T)
    #n2o
    n2o <- toolAggregate(total[,,"Emissions_(CO2eq)_from_N2O_(Gigagrams)"],rel = mapping,from = "fao",to = "magpie_n2o",dim = 3.1,partrel = TRUE)
    n2o<-n2o[,,"",invert=T]
    n2o<-n2o/1000/298
    n2o<-collapseNames(n2o)
    #ch4
    ch4 <- toolAggregate(total[,,"Emissions_(CO2eq)_from_CH4_(Gigagrams)"],rel = mapping,from = "fao",to = "magpie_ch4",dim = 3.1,partrel = TRUE)
    ch4<-ch4[,,"",invert=T]
    ch4<-ch4/1000/25
    ch4<-collapseNames(ch4)
    
    out<-mbind(
      n2o,ch4
    )
    
    out <- mbind(
      add_dimension(out, dim=3.1, add="scenario", nm="historical"),
      add_dimension(out, dim=3.1, add="scenario", nm="projection")
    )
    out[,,"projection"]<-NA
    out[,,"projection"][,c("y2030","y2050"),]<-collapseNames(out[,,"historical"][,c("y2030","y2050"),])
    out[,,"historical"][,c("y2030","y2050"),]<-NA
    
    out <- add_dimension(out, dim=3.2, add="model", nm=datasource)
    names(dimnames(out))[3] <- "scenario.model.variable"
    out[is.na(out)] <- 0
    
  } else if (datasource=="PRIMAPhist") {
    
    #The PRIMAP-hist national historical emissions time series (1850-2014)
    primap <- readSource("PRIMAPhist","hist")
    n2o <- primap[,,"n2o_n"][,,"CAT4"] / 28*44
    getNames(n2o)<-"Emissions|N2O|Land|+|Agriculture (Mt N2O/yr)"
    ch4 <- primap[,,"ch4"][,,"CAT4"]
    getNames(ch4)<-"Emissions|CH4|Land|+|Agriculture (Mt CH4/yr)"
    co2 <- primap[,,"co2_c"][,,"CAT4"] / 12*44
    getNames(co2)<-"Emissions|CO2|Land|+|Agriculture (Mt CO2/yr)"
    ag<- mbind(n2o,ch4,co2)
    
    n2o <- primap[,,"n2o_n"][,,"CAT5"] / 28*44
    getNames(n2o)<-"Emissions|N2O|Land|+|Land-use Change (Mt N2O/yr)"
    ch4 <- primap[,,"ch4"][,,"CAT5"]
    getNames(ch4)<-"Emissions|CH4|Land|+|Land-use Change (Mt CH4/yr)"
    co2 <- primap[,,"co2_c"][,,"CAT5"] / 12*44
    getNames(co2)<-"Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
    luluc <- mbind(n2o,ch4,co2)
    out<-mbind(ag,luluc)
    
    names_x <- getNames(out)
    names(names_x) <- NULL
    getNames(out) <- paste0("historical.PRIMAPhist.",names_x)
    getSets(out) <- c("region","year","scenario","model","variable")
    
  } else {stop("datasource unknown")}
  
  return(list(x=out,
              weight=NULL,
              unit="Mt",
              description="historic emissions in 1970-2015. NOx is in NO2 equivalents."))
}