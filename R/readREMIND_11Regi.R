#' Read REMIND region dependent data
#'
#' Read-in an csv files that contains regional data
#'
#' @param subtype Name of the regional data, e.g.
#' "tradecost", "deltacapoffset", "fossilExtractionCoeff", "uraniumExtractionCoeff"
#' @return magpie object of region dependent data
#' @author original: not defined, tax, fossil and RLDC changes: Renato Rodrigues
#' @examples
#' \dontrun{
#' a <- readSource(type = "REMIND_11Regi", subtype = "tradecost")
#' }
readREMIND_11Regi <- function(subtype) {
  switch(subtype,
    "tradecost" = read.csv("LueckenDiss_TradeCost.csv", sep = ";", row.names = 1) %>% as.magpie(),
    "deltacapoffset" = read.csv("p_adj_deltacapoffset_REMIND3.4.0.csv", sep = ";") %>% as.magpie(datacol = 2),
    "storageFactor" = read.csv("storageFactor_REMIND_3.4.0.csv", sep = ";") %>% as.magpie(datacol = 2),
    "ccs" = read.csv("p_dataccs.csv", sep = ";") %>% as.magpie(spatial = 1, datacol = 2),
    "ffPolyRent" = read.csv("ffPolyRent.csv", sep = ";") %>% as.magpie(spatial = 1, datacol = 5),
    "ffPolyCumEx" = read.csv("ffPolyCumEx.csv", sep = ";") %>% as.magpie(spatial = 1, datacol = 5),
    "fossilExtractionCoeff" = {
      x <- read.csv("fossil_extraction_cost_eq_coefficients.csv", sep = ";")
      # Removing the X string added to the column names because how the read.table call inside
      # the read.csv function converts column name numbers to valid variable strings (by using check.names)
      colnames(x) <- gsub("^X", "", colnames(x))
      x <- as.magpie(x, spatial = 1, temporal = 0, datacol = 3)
      # JPN SSP5 gas extraction zero-order coeff was originally negative.
      x["JPN", , "highGas.0"] <- 0
      x
    },
    "uraniumExtractionCoeff" = read.csv("uranium_extraction_cost_eq_coefficients.csv", sep = ";") %>%
      as.magpie(spatial = 1, temporal = 0, datacol = 3),
    stop("Not a valid subtype!")
  )
}
