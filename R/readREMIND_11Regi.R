#' Read REMIND region dependent data
#'
#' Read-in an csv files that contains regional data
#'
#' @param subtype Name of the regional data, e.g. "p4", "biomass", "ch4waste",
#' "tradecost", "pe2se", "xpres_tax", "deltacapoffset", "capacityFactorGlobal",
#' "capacityFactorRules", "residuesShare", "taxConvergence", "maxFeSubsidy",
#' "maxPeSubsidy", "propFeSubsidy", "fossilExtractionCoeff", "uraniumExtractionCoeff",
#' "RLDCCoefficientsLoB", "RLDCCoefficientsPeak", "earlyRetirementAdjFactor"
#' @return magpie object of region dependent data
#' @author original: not defined, capacity factor, tax, fossil and RLDC changes: Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "REMIND_11Regi", subtype = "capacityFactorGlobal")
#' }
readREMIND_11Regi <- function(subtype) {
  switch(
    subtype,
    "p4"                  = read.csv("EconometricEmissionParameter_p4.csv", sep = ";", row.names = 1) %>% as.magpie(),
    "biomass"             = readxl::read_excel("biomass.xlsx")                  %>% as.magpie(x, datacol = 4),
    "ch4waste"            = read.csv("emimac0_ch4waste.csv", sep = ";", row.names = 1)      %>% as.magpie(),
    "tradecost"           = read.csv("LueckenDiss_TradeCost.csv", sep = ";", row.names = 1) %>% as.magpie(),
    "pe2se"               = read.csv("tax_pe2se_sub.csv", sep = ";")            %>% as.magpie(datacol = 2),
    "xpres_tax"           = {
      x <- read.csv("p21_tau_xpres_tax.csv", sep = ";")
      x <- as.magpie(x, datacol = 2)
      getYears(x) <- "y2005"
      getNames(x) <- "peoil"
      x
    },
    "deltacapoffset"       = read.csv("p_adj_deltacapoffset.csv", sep = ";")     %>% as.magpie(datacol = 2),
    "capacityFactorGlobal" = read.csv("f_cf-global_REMIND_3.3.5.csv", sep = ";") %>% as.magpie(datacol = 2),
    "capacityFactorRules"  = read.csv("f_cf-rules_v1.1.csv", sep = ";")          %>% as.magpie(datacol = 4),
    "storageFactor"        = read.csv("storageFactor.csv", sep = ";")            %>% as.magpie(datacol = 2),
    "residuesShare"        = read.csv("residuesShare.csv", row.names = 1)        %>% as.magpie(datacol = 4),
    "shareIndFE"           = read.csv("shareIndustyFE.csv", sep = ";", skip = 3) %>% as.magpie(datacol = 3),
    "taxConvergence"       = read.csv("tax_convergence.csv", sep = ";")          %>% as.magpie(datacol = 4),
    "maxFeSubsidy"         = read.csv("max_FE_subsidy.csv", sep = ";")           %>% as.magpie(datacol = 4),
    "maxPeSubsidy"         = read.csv("max_PE_subsidy.csv", sep = ";")           %>% as.magpie(datacol = 4),
    "propFeSubsidy"        = read.csv("prop_FE_subsidy.csv", sep = ";")          %>% as.magpie(datacol = 4),
    "gridFactor"           = {
      x <- read.csv("homogenous_regions_for grids.csv", sep = ";")
      x$X <- NULL
      as.magpie(x, datacol = 2)
    },
    "AP_starting_values"   = read.csv("f11_emiAPexsolve.cs4r", skip = 1, header = FALSE) %>% as.magpie(datacol = 6),
    "ccs"                  = read.csv("p_dataccs.csv", sep = ";")             %>% as.magpie(spatial = 1, datacol = 2),
    "ffPolyRent"           = read.csv("ffPolyRent.csv", sep = ";")            %>% as.magpie(spatial = 1, datacol = 5),
    "ffPolyCumEx"          = read.csv("ffPolyCumEx.csv", sep = ";")           %>% as.magpie(spatial = 1, datacol = 5),
    "fossilExtractionCoeff" = {
      x <- read.csv("fossil_extraction_cost_eq_coefficients.csv", sep = ";")
      # Removing the X string added to the column names because how the read.table call inside
      # the read.csv function converts column name numbers to valid variable strings (by using check.names)
      colnames(x) <- gsub("^X", "",  colnames(x))
      x <- as.magpie(x, spatial = 1, temporal = 0, datacol = 3)
      # JPN SSP5 gas extraction zero-order coeff was originally negative.
      x["JPN", , "highGas.0"] <- 0
      x
    },
    "uraniumExtractionCoeff" = read.csv("uranium_extraction_cost_eq_coefficients.csv", sep = ";") %>%
      as.magpie(spatial = 1, temporal = 0, datacol = 3),
    "RLDCCoefficientsLoB"    = read.csv("RLDC_Coefficients_LoB.csv", sep = ";") %>%
      as.magpie(spatial = 1, temporal = 0, datacol = 3),
    "RLDCCoefficientsPeak"   = read.csv("RLDC_Coefficients_Peak.csv", sep = ";") %>%
      as.magpie(spatial = 1, temporal = 0, datacol = 3),
    "earlyRetirementAdjFactor" = {
      y <- read.csv("earlyRetirementAdjFactor.csv", sep = ";", skip = 5)
      x <- as.magpie(y, spatial = 1, temporal = 0, datacol = 2)
      x <- setNames(x, colnames(y)[-1])
      x
    },
    "nashWeight"             =  read.csv("nash_weights.csv", sep = ";")        %>% as.magpie(spatial = 1, datacol = 2),
    stop("Not a valid subtype!")
  )
}
