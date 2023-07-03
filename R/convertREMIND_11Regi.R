#' Converts REMIND regional data
#' 
#' @param x MAgPIE object to be converted
#' @param subtype Name of the regional data, e.g. "p4", "biomass", "ch4waste", "tradecost", "pe2se", "xpres_tax", "deltacapoffset", capacityFactorRules", "taxConvergence", "maxFeSubsidy", "maxPeSubsidy", "propFeSubsidy", "fossilExtractionCoeff", "uraniumExtractionCoeff", "RLDCCoefficientsLoB", "RLDCCoefficientsPeak", "earlyRetirementAdjFactor"
#' @return A MAgPIE object containing country disaggregated data
#' @author original: not defined - capacity factor, tax, fossil and RLDC changes: Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertREMIND_11Regi(x,subtype="capacityFactorGlobal")
#' }
#' 

convertREMIND_11Regi <- function(x,subtype) {
  
  if(subtype == "p4" | subtype == "biomass" | subtype == "tradecost" |
     subtype == "pe2se" | subtype == "xpres_tax" | subtype == "storageFactor" | subtype == "transpEff" | subtype == "shareIndFE"  | 
     subtype == "residuesShare" | subtype == "vintage" | subtype=="ffPolyRent" | subtype == "earlyRetirementAdjFactor" ){
    # No weighting for spatial aggregation
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight=NULL)  
  } else if (subtype == "ch4waste" | subtype == "AP_starting_values") {
    pop <- calcOutput("Population",years=2005,aggregate=FALSE)[,,"pop_SSP2"]
    y <- toolAggregate(x,"regionmappingREMIND.csv",weight=pop)
  } else if (subtype == "deltacapoffset") {
    fe <- dimSums(calcOutput("IO",subtype="output",aggregate=FALSE)[,2010,c("feelb","feeli")],dim=3)
    y <- toolAggregate(x,"regionmappingREMIND.csv",weight=fe)
  } else if (subtype == "nashWeight") {
    gdp <- calcOutput("GDP",years=2005,aggregate=FALSE)[,,"gdp_SSP2"]
    y <- toolAggregate(x,"regionmappingREMIND.csv",weight=gdp)
  } else if (subtype=="capacityFactorRules" | subtype == "taxConvergence" | subtype == "maxFeSubsidy" | subtype == "maxPeSubsidy" | subtype == "propFeSubsidy") {
    # Loading REMIND old region mapping
    mapping <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", where = "mappingfolder")
    # Filtering REMIND old region mapping (selecting just regions available on data)
    mapping <- mapping[mapping[, 3] %in% getRegions(x),]
    # Replacing NA values with zero
    x[is.na(x)] <- 0
    # Converting old Region data to country data
    y <- toolAggregate(x, mapping, weight=NULL) 
    # Filling missing country data with zero values to avoid convert error check 
    y  <- toolCountryFill(y,fill=0)
  } else if (subtype == "fossilExtractionCoeff"){
    # weight
    oil <- readSource("BGR", subtype="oil")[,,"Remaining_Potential"]
    gas <- readSource("BGR", subtype="gas")[,,"Remaining_Potential"]
    coal <- readSource("BGR", subtype="coal")[,,"Remaining_Potential"]
    BGRData <- list(lowOil = oil, medOil = oil, highOil = oil, lowGas = gas, medGas = gas, highGas = gas, lowCoal = coal, medCoal = coal, highCoal = coal)
    # filling all countries weights and adding a very small weight to countries with no remaining potential to estimate a very step cost curve in this case  
    weight <- lapply(BGRData, function(df){
      df  <- toolCountryFill(df,fill=0) 
      df[df == 0] <- 1E-20 
      return(df)
    }) 
    #  mapping - original REMIND region mapping (11 regions)
    mapping <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", where = "mappingfolder")
    # maxExtraction (upper x limit for function estimation)
    upperBoundMaxExtractionPerCountry <- readSource("REMIND_11Regi", subtype = "ffPolyCumEx")[,,"max"]
    upperBoundMaxExtraction <- toolAggregate(upperBoundMaxExtractionPerCountry, mapping, weight=NULL)
    # return magpie list of country disaggregated coefficients
    countryCoeff <- suppressWarnings(toolCubicFunctionDisaggregate(x,weight,rel=mapping,xUpperBound=upperBoundMaxExtraction))
    # add dimension with the name of the pe on the magpie objects 
    output <- lapply(seq_along(countryCoeff), function(i) {
      out <- add_dimension(countryCoeff[[i]], dim = 3.1, nm = names(countryCoeff)[i])
    })
    names(output) <- names(countryCoeff) 
    # merge all magpie objects into a single one
    y <- mbind(output)
  } else if (subtype == "uraniumExtractionCoeff") {
    # changes hard coded in REMIND (Summarized from p31_costExPoly modification steps in file \modules\31_fossil\grades2poly\datainput.gms, Rev 7683)
    x[,,"peur.xi1"] <- 25/1000
    x[,,"peur.xi2"] <- 0
    x[,,"peur.xi3"] <- ( (300/1000)* 3 ** 1.8) / ((x[,,"peur.xi3"]* 14 /4.154) * 3) ** 2
    x[,,"peur.xi4"] <- 0
    # weight
    BGRuranium <- readSource("BGR", subtype="uranium")[,,"Remaining_Potential"] # Remaining extraction potential per country 
    BGRuranium  <- toolCountryFill(BGRuranium,fill=0) 
    BGRuranium[BGRuranium == 0] <- 1E-3 # assigning small uranium extraction potential for countries with none to help the curves estimation
    weight <- list(peur = BGRuranium)
    #  mapping - original REMIND region mapping (11 regions)
    mapping <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", where = "mappingfolder")
    # maxExtraction (upper x limit for function estimation)
    weightmaxExtraction <- toolAggregate(BGRuranium, mapping, weight=NULL) 
    totalWeightMaxExtraction <- as.numeric(colSums(weightmaxExtraction))
    upperBoundMaxExtraction <- 23* ( weightmaxExtraction/totalWeightMaxExtraction ) # 23 -> from s31_max_disp_peur -> max global "maximum amount of cumulative uranium production in Megatonnes of metal uranium (U3O8, the stuff that is traded at 40-60US$/lb)."
    upperBoundMaxExtraction <- add_dimension(upperBoundMaxExtraction, dim = 3.1, add = "pe", nm = "peur")
    upperBoundMaxExtraction <- upperBoundMaxExtraction * 1.5 # adding a 50% more to the maximum extraction because the countries shares do not take into consideration the different marginal costs of each country
    # return magpie list of country disaggregated coefficients
    countryCoeff <- suppressWarnings(toolCubicFunctionDisaggregate(x,weight,rel=mapping,xUpperBound=upperBoundMaxExtraction))
    y <- add_dimension(countryCoeff$peur, dim = 3.1, nm = "peur")
  } else if (subtype == "ffPolyCumEx") {
    # use remaining resources available per country as weight
    oil            <- readSource("BGR", subtype="oil")[,,"Remaining_Potential"]
    getNames(oil)  <- "peoil"
    gas            <- readSource("BGR", subtype="gas")[,,"Remaining_Potential"]
    getNames(gas)  <- "pegas"
    coal           <- readSource("BGR", subtype="coal")[,,"Remaining_Potential"]
    getNames(coal) <- "pecoal"
    # put the data for all fossils together
    bgr <- mbind(oil,coal,gas)
    bgr[is.na(bgr)] <- 0
    # make new magpie object with total dimensions
    w  <- new.magpie(getRegions(bgr),getYears(x),getNames(x))
    for(s in getNames(x,dim=1)) {
      w[,,s] <- bgr[,,s]
    }
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight=w)
    # SB & NB edit 2019/09/11: Increase SSP5 oil max cumulative extraction in USA and CAZ by 20% based on calibration with the SSP IAM project 2017
    y[c("USA","CAN","AUS","NZL","HMD","SPM"),,"peoil.max.highOil"] <- y[c("USA","CAN","AUS","NZL","HMD","SPM"),,"peoil.max.highOil"] * (1 + 0.2)
    
    #SB & NB edit 2019/11/18:
    # Increase SSP5 max cumulative coal extraction for USA and CAZ by 20%
    y[c("USA","CAN","AUS","NZL","HMD","SPM"),,"pecoal.max.highCoal"] <- 
      y[c("USA","CAN","AUS","NZL","HMD","SPM"),,"pecoal.max.highCoal"] * (1 + 0.2)
    
  } else if (subtype == "gridFactor") {
    y <- x
  } else if (subtype == "ccs") {
    # use total land area as weight
    area <- calcOutput("LanduseInitialisation",aggregate=FALSE)[,2005,] 
    # sum over 3.dimension
    area <- dimSums(area,dim=3)
    getYears(area) <- NULL
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight=area)
  } else if (subtype=="RLDCCoefficientsLoB") {
    # Converting old Region data to country data
    # setting country coefficient values equal to region that it belonged   
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight=NULL) 
    # setting country absciss value equal to original region values weighted by the PE use of the country (should be replaced by extraction quantities data)
    fe <- calcOutput("FE",aggregate=FALSE)
    z <- toolAggregate(x, "regionmappingREMIND.csv", weight=fe[,2005,"FE (EJ/yr)"])
    y[,,c("1.p00","2.p00","3.p00","4.p00")] <- z[,,c("1.p00","2.p00","3.p00","4.p00")]
  } else if (subtype=="RLDCCoefficientsPeak") {
    # Converting old Region data to country data
    # setting country coefficient values equal to region that it belonged   
    y <- toolAggregate(x, "regionmappingREMIND.csv", weight=NULL) 
    # setting country absciss value equal to original region values weighted by the PE use of the country (should be replaced by extraction quantities data)
    fe <- calcOutput("FE",aggregate=FALSE)
    z <- toolAggregate(x, "regionmappingREMIND.csv", weight=fe[,2005,"FE (EJ/yr)"])
    y[,,c("curt.p00","curtShVRE.p00","peak.p00","shtStor.p00","STScost.p00","STSRes2Cap.p00")] <- z[,,c("curt.p00","curtShVRE.p00","peak.p00","shtStor.p00","STScost.p00","STSRes2Cap.p00")]
  }
return(y)
}
