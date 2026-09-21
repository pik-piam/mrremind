#' Read REMIND 11 Regi data
#'
#' @param subtype Name of the source, e.g. "fossilExtractionCoeff", "uraniumExtractionCoeff"
#' @author unknown
readREMIND_11Regi <- function(subtype) {
  switch(subtype,
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
