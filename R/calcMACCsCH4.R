#' Calculation of ImageMacc CH4 costcurve of Energy Industry and Landuse
#' 
#' Calculation of the CH4 relative costcurves (subtypes: Energy Industry and
#' Landuse) weighted by the baseline emissions. Sources: CH4 coal
#' losses/leakages, CH4 oil losses/leakages, CH4 natural gas losses/leakages,
#' CH4 Landfills, CH4 Domestic Sewage, CH4 Wetland rice, CH4 Animals, CH4
#' Animal waste divided in classes 1-201.
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
#' calcOutput("MACCsCH4")
#' 
#' }
#' @importFrom magclass getNames
calcMACCsCH4 <- function(sector="all") {
  
  # readSource CH4 and baseline Emissions
  energy_ind <- readSource("ImageMacc", "CH4_Energy_Industry")
  landuse <- readSource("ImageMacc", "CH4_Landuse")

  CH4 <- mbind(energy_ind, landuse)
  
  rem_years <- sort(c(getYears(CH4,as.integer = TRUE),seq(2015,2055,10)))
  
  CH4 <- time_interpolate(CH4, seq(2015,2095,10), integrate_interpolated_years=TRUE, extrapolation_type="linear")
  
  getNames(CH4) <- gsub("CH4 coal losses/leakages"       ,"ch4coal",getNames(CH4))
  getNames(CH4) <- gsub("CH4 oil losses/leakages"        ,"ch4oil",getNames(CH4))
  getNames(CH4) <- gsub("CH4 natural gas losses/leakages","ch4gas",getNames(CH4))
  getNames(CH4) <- gsub("CH4 Landfills"                  ,"ch4wstl",getNames(CH4))
  getNames(CH4) <- gsub("CH4 Domestic Sewage"            ,"ch4wsts",getNames(CH4))
  getNames(CH4) <- gsub("CH4 Wetland rice"               ,"ch4rice",getNames(CH4))
  getNames(CH4) <- gsub("CH4 Animals"                    ,"ch4animals",getNames(CH4))
  getNames(CH4) <- gsub("CH4 Animal waste"               ,"ch4anmlwst",getNames(CH4))
  
  # weight for the aggregation
  baseline <- readSource("ImageMacc", "baseline_sources")
  w <- baseline[,getYears(CH4),c("CH4 coal losses/leakages", "CH4 oil losses/leakages", 
                                 "CH4 natural gas losses/leakages", "CH4 Landfills", 
                                 "CH4 Domestic Sewage", "CH4 Wetland rice", "CH4 Animals", 
                                 "CH4 Animal waste")]
  
  getNames(w) <- gsub("CH4 coal losses/leakages"       ,"ch4coal",getNames(w))
  getNames(w) <- gsub("CH4 oil losses/leakages"        ,"ch4oil",getNames(w))
  getNames(w) <- gsub("CH4 natural gas losses/leakages","ch4gas",getNames(w))
  getNames(w) <- gsub("CH4 Landfills"                  ,"ch4wstl",getNames(w))
  getNames(w) <- gsub("CH4 Domestic Sewage"            ,"ch4wsts",getNames(w))
  getNames(w) <- gsub("CH4 Wetland rice"               ,"ch4rice",getNames(w))
  getNames(w) <- gsub("CH4 Animals"                    ,"ch4animals",getNames(w))
  getNames(w) <- gsub("CH4 Animal waste"               ,"ch4anmlwst",getNames(w))
  
  if (sector == "all") {
    CH4 <- CH4[,rem_years,]
    w <- w[,rem_years,]
  } else if(sector == "landuse") {
    CH4 <- CH4[,,c("ch4rice","ch4animals","ch4anmlwst")]
    getNames(CH4) <- gsub("ch4rice","rice_ch4",getNames(CH4))
    getNames(CH4) <- gsub("ch4animals","ent_ferm_ch4",getNames(CH4))
    getNames(CH4) <- gsub("ch4anmlwst","awms_ch4",getNames(CH4))
    x <- new.magpie(getRegions(CH4),seq(2105,2150,5),getNames(CH4),0)
    x[,,] <- setYears(CH4[,2100,],NULL)
    CH4 <- mbind2(CH4,x)
    
    w <- w[,,c("ch4rice","ch4animals","ch4anmlwst")]
    getNames(w) <- gsub("ch4rice","rice_ch4",getNames(w))
    getNames(w) <- gsub("ch4animals","ent_ferm_ch4",getNames(w))
    getNames(w) <- gsub("ch4anmlwst","awms_ch4",getNames(w))
    x <- new.magpie(getRegions(w),seq(2105,2150,5),getNames(w),0)
    x[,,] <- setYears(w[,2100,],NULL)
    w <- mbind2(w,x)
  }
  
  return(list(x=CH4,weight=w,unit="Tax level 200 steps each 5$/tC",description="CH4 ImageMacc"))
}
