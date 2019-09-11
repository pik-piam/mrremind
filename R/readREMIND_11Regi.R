
#' Read REMIND region dependent data
#' 
#' Read-in an csv files that contains regional data
#' 
#' @param subtype Name of the regional data, e.g. "p4", "biomass", "ch4waste", "tradecost", "pe2se", "xpres_tax", "deltacapoffset", "capacityFactorGlobal", "capacityFactorRules", "residuesShare", "taxConvergence", "maxFeSubsidy", "maxPeSubsidy", "propFeSubsidy", "fossilExtractionCoeff", "uraniumExtractionCoeff", "RLDCCoefficientsLoB", "RLDCCoefficientsPeak", "earlyRetirementAdjFactor"
#' @return magpie object of region dependent data
#' @author original: not defined, capacity factor, tax, fossil and RLDC changes: Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="REMIND_11Regi",subtype="capacityFactorGlobal")
#' }

readREMIND_11Regi<- function(subtype) {
  
  if (subtype == "p4") {
    x <- read.csv("EconometricEmissionParameter_p4.csv",sep=";",row.names=1)
    x <- as.magpie(x)
  } else if (subtype == "biomass") {
    x <- read_excel("biomass.xlsx")
    x <- as.magpie(x,datacol=4)
  } else if (subtype == "ch4waste") {
    x <- read.csv("emimac0_ch4waste.csv",sep=";",row.names=1)
    x <- as.magpie(x)
  } else if (subtype== "tradecost") {
    x <- read.csv("LueckenDiss_TradeCost.csv", sep=";",row.names=1)
    x <- as.magpie(x)
  } else if (subtype=="pe2se") {
    x <- read.csv("tax_pe2se_sub.csv",sep=";")
    x <- as.magpie(x,datacol=2)
  } else if (subtype=="xpres_tax") {
    x <- read.csv("p21_tau_xpres_tax.csv",sep=";")
    x <- as.magpie(x,datacol=2)
    getYears(x) <- "y2005"
    getNames(x) <- "peoil"
  } else if (subtype=="deltacapoffset") {
    x <- read.csv("p_adj_deltacapoffset.csv",sep=";")
    x <- as.magpie(x,datacol=2)
  } else if (subtype=="capacityFactorGlobal") {
    x <- read.csv("f_cf-global.csv",sep=";")
    x <- as.magpie(x,datacol=2)
  } else if (subtype=="capacityFactorRules") {
    x <- read.csv("f_cf-rules.csv",sep=";")
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="storageFactor") {
    x <- read.csv("storageFactor.csv",sep=";")
    x <- as.magpie(x,datacol=2)
  } else if (subtype=="residuesShare") {
    x <- read.csv("residuesShare.csv", row.names = 1)
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="transpEff") {
    x <- read.csv2("transpEff.csv", stringsAsFactors = FALSE)
    x[,2:5] <- apply(x[,2:5], 2, as.numeric)
    x <- as.magpie(x, datacol=2)
  } else if (subtype=="shareIndFE") {
    x <- read.csv("shareIndustyFE.csv", sep=";",skip=3)
    x <- as.magpie(x,datacol=3)
  } else if (subtype=="taxConvergence") {
    x <- read.csv("tax_convergence.csv",sep=";")
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="maxFeSubsidy") {
    x <- read.csv("max_FE_subsidy.csv",sep=";")
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="maxPeSubsidy") {
    x <- read.csv("max_PE_subsidy.csv",sep=";")
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="propFeSubsidy") {
    x <- read.csv("prop_FE_subsidy.csv",sep=";")
    x <- as.magpie(x,datacol=4)
  } else if (subtype=="gridFactor") {
    x <- read.csv("homogenous_regions_for grids.csv", sep=";")
    x$X <- NULL
    x <- as.magpie(x,datacol=2) 
  } else if (subtype=="AP_starting_values") {
    x <- read.csv("f11_emiAPexsolve.cs4r", sep=",", skip = 1, header = F)
    x <- as.magpie(x,datacol=6)
  } else if (subtype=="vintage") {
    x <- read.csv("factorVintage.csv",sep=";")
    x <- as.magpie(x,spatial=1,datacol=3)
  } else if (subtype=="ccs") {
    x <- read.csv("p_dataccs.csv",sep=";")
    x <- as.magpie(x,spatial=1,datacol=2)  
	} else if (subtype=="ffPolyRent") {
    x <- read.csv("ffPolyRent.csv",sep=";")
    x <- as.magpie(x,spatial=1,datacol=5)
	} else if (subtype=="ffPolyCumEx") {
	  x <- read.csv("ffPolyCumEx.csv",sep=";")
	  x <- as.magpie(x,spatial=1,datacol=5)
  } else if (subtype=="fossilExtractionCoeff") {
    x <- read.csv("fossil_extraction_cost_eq_coefficients.csv",sep=";")
    #removing the X string added to the column names because how the read.table call inside the read.csv function converts column name numbers to valid variable strings (by using check.names)
    colnames(x) <- gsub("^X", "",  colnames(x))
    x <- as.magpie(x,spatial=1,temporal=0,datacol=3)
    #SB & NB edit 2019/09/11: Shifting SSP5 coal extraction cost curve down by 33% based on calibration with the SSP IAM project 2017
    x[,,c("highCoal.0","highCoal.1")] <- x[,,c("highCoal.0","highCoal.1")]*(1-0.33)
    # SB & NB edit 2019/09/11: Shifting SSP5 oil extraction cost curve down by 20% based on calibration with the SSP IAM project 2017
    x[c("USA","CAN","AUS","NZL","HMD","SPM"),,c("highOil.0","highOil.1")] <- x[c("USA","CAN","AUS","NZL","HMD","SPM"),,c("highOil.0","highOil.1")]*(1-0.2)
  } else if (subtype=="uraniumExtractionCoeff") {
    x <- read.csv("uranium_extraction_cost_eq_coefficients.csv",sep=";")
    x <- as.magpie(x,spatial=1,temporal=0,datacol=3)
  } else if (subtype=="RLDCCoefficientsLoB") {
    x <- read.csv("RLDC_Coefficients_LoB.csv",sep=";")
    x <- as.magpie(x,spatial=1,temporal=0,datacol=3)
  } else if (subtype=="RLDCCoefficientsPeak") {
    x <- read.csv("RLDC_Coefficients_Peak.csv",sep=";")
    x <- as.magpie(x,spatial=1,temporal=0,datacol=3)  
  } else if (subtype=="earlyRetirementAdjFactor") {
    y <- read.csv("earlyRetirementAdjFactor.csv",sep=";",skip=5)
    x <- as.magpie(y,spatial=1,temporal=0,datacol=2)  
    x <- setNames(x,colnames(y)[-1])
  } else if (subtype=="nashWeight") {
    x <- read.csv("nash_weights.csv",sep=";")
    x <- as.magpie(x,spatial=1,datacol=2)  
  } else {
    stop("Not a valid subtype!")
  }
  return(x)
}
