#' @importFrom magclass getNames

calcMACCbaseN2O <- function() {
  
  # readSource N2O and baseline Emissions
  baseline <- readSource("ImageMacc", "baseline_sources")
  baseline <- (44/12) * (1/298) * baseline[,,c("N2O Transport", "N2O Adipic acid production", "N2O Nitric acid production")]
  getNames(baseline) <- gsub("N2O Transport",             "n2otrans",getNames(baseline))
  getNames(baseline) <- gsub("N2O Adipic acid production","n2oadac",getNames(baseline))
  getNames(baseline) <- gsub("N2O Nitric acid production","n2onitac",getNames(baseline))
  
  pop <- calcOutput("Population",years=2005,aggregate=FALSE)[,,"pop_SSP2"]
  
  return(list(x=baseline,weight=pop,unit="Mt N",description="N2O Image baselines"))
}