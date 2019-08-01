#' Calculation of ImageMacc N2O costcurve of Energy Industry and Landuse
#' 
#' Calculation of the N2O relative costcurves (subtypes: Energy Industry and
#' Landuse) weighted by the baseline emissions. Sources: N2O Transport, N2O
#' Adipic acid production, N2O Nitric acid production, N2O Fertilizer, N2O
#' Animal waste, N2O Domestic sewage divided in classes 1-201.
#' 
#' 
#' @param sector "all" or "landuse"; "all"" includes energy_industry and
#' landuse
#' @return MAgPIE object
#' @author Nele Steinmetz
#' @seealso \code{\link{calcOutput}}, \code{\link{readImageMacc}},
#' \code{\link{convertImageMacc}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("MACCsN2O")
#' 
#' }
#' @importFrom magclass getNames
calcMACCsN2O <- function(sector="all") {
  
  # readSource N2O and baseline Emissions
  energy_ind <- readSource("ImageMacc", "N2O_Energy_Industry")
  landuse <- readSource("ImageMacc", "N2O_Landuse")

  N2O <- mbind(energy_ind, landuse)
  
  rem_years <- sort(c(getYears(N2O,as.integer = TRUE),seq(2015,2055,10)))
  
  N2O <- time_interpolate(N2O, seq(2015,2095,10), integrate_interpolated_years=TRUE, extrapolation_type="linear")
  
  getNames(N2O) <- gsub("N2O Transport"             ,"n2otrans",getNames(N2O))
  getNames(N2O) <- gsub("N2O Adipic acid production","n2oadac",getNames(N2O))
  getNames(N2O) <- gsub("N2O Nitric acid production","n2onitac",getNames(N2O))
  getNames(N2O) <- gsub("N2O Fertilizer"            ,"n2ofert",getNames(N2O))
  getNames(N2O) <- gsub("N2O Animal waste"          ,"n2oanwst",getNames(N2O))
  getNames(N2O) <- gsub("N2O Domestic sewage"       ,"n2owaste",getNames(N2O))
  
  # weight for the aggregation
  baseline <- readSource("ImageMacc", "baseline_sources")
  w <- baseline[,getYears(N2O),c("N2O Transport", "N2O Adipic acid production", "N2O Nitric acid production",
                    "N2O Fertilizer", "N2O Animal waste", "N2O Domestic sewage")]
  
  getNames(w) <- gsub("N2O Transport"             ,"n2otrans",getNames(w))
  getNames(w) <- gsub("N2O Adipic acid production","n2oadac",getNames(w))
  getNames(w) <- gsub("N2O Nitric acid production","n2onitac",getNames(w))
  getNames(w) <- gsub("N2O Fertilizer"            ,"n2ofert",getNames(w))
  getNames(w) <- gsub("N2O Animal waste"          ,"n2oanwst",getNames(w))
  getNames(w) <- gsub("N2O Domestic sewage"       ,"n2owaste",getNames(w))
  
  if (sector == "all") {
    N2O <- N2O[,rem_years,]
    w <- w[,rem_years,]
  } else if(sector == "landuse") {
    N2O <- N2O[,,c("n2ofert","n2oanwst")]
    getNames(N2O) <- gsub("n2ofert","inorg_fert_n2o",getNames(N2O))
    getNames(N2O) <- gsub("n2oanwst","awms_manure_n2o",getNames(N2O))
    x <- new.magpie(getRegions(N2O),seq(2105,2150,5),getNames(N2O),0)
    x[,,] <- setYears(N2O[,2100,],NULL)
    N2O <- mbind2(N2O,x)

    w <- w[,,c("n2ofert","n2oanwst")]
    getNames(w) <- gsub("n2ofert","inorg_fert_n2o",getNames(w))
    getNames(w) <- gsub("n2oanwst","awms_manure_n2o",getNames(w))
    x <- new.magpie(getRegions(w),seq(2105,2150,5),getNames(w),0)
    x[,,] <- setYears(w[,2100,],NULL)
    w <- mbind2(w,x)
  }
  
  return(list(x=N2O,weight=w,unit="Tax level 200 steps each 5$/tC",description="N2O ImageMacc"))
}
