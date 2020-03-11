#' @title calcYields
#' @description This function extracts yields from LPJ to MAgPIE
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

calcYields <- function(){

  ### Find a way to implement averaging or spline calculation

  # Load LPJmL harvested data as MAgPIe object and change unit gC/m?-> tDM/ha

  lpj_yields    <- readSource("LPJmL5", subtype = "harvest", convert="onlycorrect")

  # Load LPJmL to MAgPIE mapping to aggregate to MAgPIE crops
  LPJ2MAG      <- toolGetMapping( "MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")

  # Aggregate to MAgPIE crops
  mag_yields   <- toolAggregate(lpj_yields, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)

  # Remove negative yields
  mag_yields[mag_yields < 0] <- 0

  # Check for NAs
  if(any(is.na(mag_yields))){
    stop("produced NA yields")
  }

  years <- getYears(mag_yields)

  #remove later - start
  FAOproduction     <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,,"dm"])
  MAGarea           <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate=FALSE)
  #remove later - end
  #FAOproduction     <- collapseNames(magdrive::calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,,"dm"])
  #MAGarea           <- magdrive::calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate=FALSE)

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
  interpolate_years          <- getYears(FAOYields, as.integer = TRUE)
  interpolate_years          <- seq(interpolate_years[1], interpolate_years[length(interpolate_years)], 1)
  Calib[,interpolate_years,] <- time_interpolate(Calib[,getYears(FAOYields),], interpolate_years, integrate_interpolated_years=TRUE)

  # hold constant before and after FAO years
  missingyears <- setdiff(getYears(mag_yields, as.integer = TRUE),interpolate_years)
  beforeyears  <- missingyears[missingyears < interpolate_years[1]]
  afteryears   <- missingyears[missingyears > interpolate_years[1]]
  Calib[,beforeyears,] <- Calib[,interpolate_years[1],]
  Calib[,afteryears,]  <- Calib[,interpolate_years[length(interpolate_years)],]

  # recalibrate yields for proxys
  mag_yields           <- Calib * mag_yields

  return(list(
    x=mag_yields,
    weight=NULL,
    unit="t per ha",
    description="Yields in tons per hectar for different crop types.",
    isocountries=FALSE))
}
