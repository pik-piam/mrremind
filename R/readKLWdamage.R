#' Reads country-specific damage coefficients for the damage function presented in 
#' Kotz et al. (2024). Data has been provided by the authors.
#' This contains data for all countries and for 1000 boostrapping realizations per country,
#' capturing uncertainty from climate and empirical modeling.
#' Subtypes are the temperature and temperature^2 coefficients and the maximum temperature
#' per country for which the function is defined.
#'
#' @return KLW damage coefficients
#' @author Franziska Piontek
#' @param subtype data subtype. Either "beta1", "beta2" or "maxGMT"

readKLWdamage <- function(subtype) {
  if (subtype == "beta1") {
    a <- read.csv(file = "PERC_scaling_coefs_ssp2_ssp585_lagdiff_lintren_fix_spec_NL_8_9_10_movfix_30_Nboot1000_beta1.csv")
  } else if (subtype == "beta2") {
    a <- read.csv(file = "PERC_scaling_coefs_ssp2_ssp585_lagdiff_lintren_fix_spec_NL_8_9_10_movfix_30_Nboot1000_beta2.csv")
  } else if (subtype == "maxGMT") {
    a <- read.csv(file = "PERC_scaling_coefs_ssp2_ssp585_lagdiff_lintren_fix_spec_NL_8_9_10_movfix_30_Nboot1000_maxGMT.csv")
  }
  iso <- colnames(a)[2:length(colnames(a))]

  for (i in iso) {
    col <- which(colnames(a) == i)
    if (col == 2) {
      out <- new.magpie(iso[1], NULL, seq(1:1000), a[, col], sets = c("CountryCode", "year", "sample"))
    } else {
      out <- mbind(out, new.magpie(i, NULL, seq(1:1000), a[, col], sets = c("CountryCode", "year", "sample")))
    }
  }
  return(out)
}
