#' Calculate baseline emissions trajectories 
#' for transport, adipic acid and nitric acid production
#' 
#' If source is PBL_2007, the source of the ultimately the baseline
#' scenario of Lucas et al 2007 (http://linkinghub.elsevier.com/retrieve/pii/S1462901106001316)
#' 
#' If source is PBL_2022, the source of the ultimately the baseline
#' scenario of Harmsen et al. 2023 (https://doi.org/10.1038/s41467-023-38577-4)
#' 
#' @return list of magclass with REMIND input data for
#' different sectors for timesteps 2000-2100, in Mt N per year
#' @author Lavinia Baumstark, Gabriel Abrhahao
# nolint start
calcMACCbaseN2O <- function(source = "PBL_2007") {
  if (source == "PBL_2007") {
    # readSource N2O and baseline Emissions
    baseline <- readSource("ImageMacc", "baseline_sources")
    baseline <- (44 / 12) * (1 / 298) * baseline[, , c("N2O Transport", "N2O Adipic acid production", "N2O Nitric acid production")]
    getNames(baseline) <- gsub("N2O Transport", "n2otrans", getNames(baseline))
    getNames(baseline) <- gsub("N2O Adipic acid production", "n2oadac", getNames(baseline))
    getNames(baseline) <- gsub("N2O Nitric acid production", "n2onitac", getNames(baseline))

    # overwritting european countries with eurostat data
    EUcountries <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
    baselineEurostat <- calcOutput("HistEmissions", subtype = "MAC", aggregate = F)
    for (y in getYears(baseline, as.integer = T)) {
      if (y <= 2010) {
        baseline[EUcountries, y, "n2otrans"] <- baselineEurostat[EUcountries, y, "n2otrans"]
        baseline[EUcountries, y, "n2oadac"] <- baselineEurostat[EUcountries, y, "n2oadac"]
        baseline[EUcountries, y, "n2onitac"] <- baselineEurostat[EUcountries, y, "n2onitac"]
      } else {
        baseline[EUcountries, y, "n2otrans"] <- baseline[EUcountries, y, "n2otrans"] * setYears((baselineEurostat[EUcountries, 2015, "n2otrans"]) / baseline[EUcountries, 2015, "n2otrans"])
        baseline[EUcountries, y, "n2oadac"] <- baseline[EUcountries, y, "n2oadac"] * setYears((baselineEurostat[EUcountries, 2015, "n2oadac"]) / baseline[EUcountries, 2015, "n2oadac"])
        baseline[EUcountries, y, "n2onitac"] <- baseline[EUcountries, y, "n2onitac"] * setYears((baselineEurostat[EUcountries, 2015, "n2onitac"]) / baseline[EUcountries, 2015, "n2onitac"])
      }
    }
    baseline[is.na(baseline)] <- 0
  } else if (source == "PBL_2022") {
    baseline = readSource("PBL_MACC_2022", subtype = "IMAGEBaselineEmissions")[,,c("n2otrans","n2oadac","n2onitac")]
  }


  return(list(x = baseline, weight = NULL, unit = "Mt N", description = "N2O Image baselines"))
}
# nolint end
