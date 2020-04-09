#' @title calcYields
#' @description This function extracts yields from LPJ to MAgPIE
#'
#' @param version Switch between LPJmL4 and LPJmL4
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param time average, spline or raw (default)
#' @param averaging_range just specify for time=="average": number of time steps to average
#' @param dof             just specify for time=="spline": degrees of freedom
#' @param harmonize_baseline FALSE (default) nothing happens, if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year just specify for harmonize_baseline != FALSE : Reference year
#' @param calib_proxy calibrated proxy to FAO values ifsetto TRUE
#' 
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Yields", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset

calcYields <- function(version="LPJmL4", climatetype="CRU_4", time="raw", averaging_range=NULL, dof=NULL, 
                       harmonize_baseline=FALSE, ref_year="y2015", calib_proxy=TRUE){
  
  lpjml_years  <- findset("time")[as.numeric(substring(findset("time"),2))<2099]

  LPJ2MAG      <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
  lpjml_crops  <- unique(LPJ2MAG$LPJmL)
  irrig_types  <- c("irrigated","rainfed")
  
  lpjml_yields <- NULL
  
  for(crop in lpjml_crops){
    
    subdata <- as.vector(outer(crop, irrig_types, paste, sep="."))
    tmp     <-  calcOutput("LPJmL", version=version, climatetype=climatetype, subtype="harvest", subdata=subdata, time=time, averaging_range=averaging_range, dof=dof,
                           harmonize_baseline=harmonize_baseline, ref_year=ref_year, limited=TRUE, hard_cut=FALSE, aggregate=FALSE, years=lpjml_years)
    
    lpjml_yields  <- mbind(lpjml_yields, tmp)
  }
  
  # Aggregate to MAgPIE crops
  mag_yields   <- toolAggregate(lpjml_yields, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)

  # Check for NAs
  if(any(is.na(mag_yields))){
    stop("produced NA yields")
  }

  if(calib_proxy){
    
    FAOproduction     <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,,"dm"])
    MAGarea           <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate=FALSE)
    
    
    MAGcroptypes  <- findset("kcr")
    missing       <- c("betr","begr")
    MAGcroptypes  <- setdiff(MAGcroptypes, missing)
    FAOproduction <- add_columns(FAOproduction[,,MAGcroptypes],addnm = missing,dim = 3.1)
    FAOproduction[,,missing] <- 0
    
    FAOYields         <- dimSums(FAOproduction,dim=1)/dimSums(MAGarea, dim=1)
    
    Calib <- new.magpie("GLO", getYears(mag_yields),c(getNames(FAOYields), "pasture"), fill=1)
    Calib[,getYears(FAOYields),"oilpalm"]   <- FAOYields[,,"oilpalm"]/FAOYields[,,"groundnut"]      # LPJmL proxy for oil palm is groundnut
    Calib[,getYears(FAOYields),"cottn_pro"] <- FAOYields[,,"cottn_pro"]/FAOYields[,,"groundnut"]    # LPJmL proxy for cotton is groundnut
    Calib[,getYears(FAOYields),"foddr"]     <- FAOYields[,,"foddr"]/FAOYields[,,"maiz"]             # LPJmL proxy for fodder is maize
    Calib[,getYears(FAOYields),"others"]    <- FAOYields[,,"others"]/FAOYields[,,"maiz"]            # LPJmL proxy for others is maize
    Calib[,getYears(FAOYields),"potato"]    <- FAOYields[,,"potato"]/FAOYields[,,"sugr_beet"]       # LPJmL proxy for potato is sugar beet
    
    # interpolate between FAO years
    Calib <- toolFillYears(Calib, getYears(mag_yields))
    
    # recalibrate yields for proxys
    mag_yields           <- Calib[,,getNames(mag_yields, dim=1)] * mag_yields
  }
 
  return(list(
    x=mag_yields,
    weight=NULL,
    unit="t per ha",
    description="Yields in tons per hectar for different crop types.",
    isocountries=FALSE))
}
