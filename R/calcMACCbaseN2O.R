#' @importFrom magclass getNames

calcMACCbaseN2O <- function() {
  
  # readSource N2O and baseline Emissions
  baseline <- readSource("ImageMacc", "baseline_sources")
  baseline <- (44/12) * (1/298) * baseline[,,c("N2O Transport", "N2O Adipic acid production", "N2O Nitric acid production")]
  getNames(baseline) <- gsub("N2O Transport",             "n2otrans",getNames(baseline))
  getNames(baseline) <- gsub("N2O Adipic acid production","n2oadac",getNames(baseline))
  getNames(baseline) <- gsub("N2O Nitric acid production","n2onitac",getNames(baseline))
  
  # overwritting european countries with eurostat data
  EUcountries <- c("ALA","AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FRO","FIN","FRA","DEU","GIB","GRC","GGY","HUN","IRL","IMN","ITA","JEY","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR")
  baselineEurostat <- calcOutput("HistEmissions",subtype="MAC",aggregate=F)
  for(y in getYears(baseline,as.integer = T)){
    if (y <= 2010){
      baseline[EUcountries,y,"n2otrans"] <- baselineEurostat[EUcountries,y,"n2otrans"] 
      baseline[EUcountries,y,"n2oadac"] <- baselineEurostat[EUcountries,y,"n2oadac"] 
      baseline[EUcountries,y,"n2onitac"] <- baselineEurostat[EUcountries,y,"n2onitac"]  
    } else {
      baseline[EUcountries,y,"n2otrans"] <- baseline[EUcountries,y,"n2otrans"] * setYears( (baselineEurostat[EUcountries,2015,"n2otrans"]) / baseline[EUcountries,2015,"n2otrans"] )
      baseline[EUcountries,y,"n2oadac"] <- baseline[EUcountries,y,"n2oadac"] * setYears( (baselineEurostat[EUcountries,2015,"n2oadac"]) / baseline[EUcountries,2015,"n2oadac"] )
      baseline[EUcountries,y,"n2onitac"] <- baseline[EUcountries,y,"n2onitac"] * setYears( (baselineEurostat[EUcountries,2015,"n2onitac"]) / baseline[EUcountries,2015,"n2onitac"] )
    }
  }
  baseline[is.na(baseline)] <- 0
  
  return(list(x=baseline,weight=NULL,unit="Mt N",description="N2O Image baselines"))
}
