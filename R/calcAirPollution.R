#' @importFrom dplyr group_by_ summarise_ ungroup mutate_ rename_ filter_ select_
#' @importFrom magclass as.magpie getCells getSets<- getNames<- getSets getRegions<- mselect<- setNames write.magpie
#' @importFrom tidyr gather_
#' @importFrom utils read.csv read.csv2
#' @importFrom madrat toolMappingFile



calcAirPollution <- function(subtype) {
  
  if (!(subtype %in% c("emission_factors", "emissions"))) stop('subtype must be in c("emission_factors", "emissions")')
  
  # user-defined parameters
  time     <- c(seq(2005,2055,5), seq(2060,2110,10), 2130, 2150)
  
  # read in data from calcECLIPSE_SSP
  ef            <- calcOutput("ECLIPSE_SSP",subtype="emission_factors", aggregate=FALSE)
  emi           <- calcOutput("ECLIPSE_SSP",subtype="emissions", aggregate=FALSE)
  
  # exogenous emissions
  map_sectors_ECLIPSE2REMIND <- read.csv(toolMappingFile("sectoral", "mappingECLIPSEtoREMINDsectors.csv"), stringsAsFactors=TRUE)
  mapsec = map_sectors_ECLIPSE2REMIND[map_sectors_ECLIPSE2REMIND$eclipse %in% getNames(emi, dim=1), c(1,3)]
  emi = toolAggregate(emi, mapsec, dim=3.1)
  
  # interpolate data (EFs and activities) over time. Remove y2000 from activities (this is the first time item hence -1)
  vcat(2,"  > Interpolate data over time... \n")
  ef  <- time_interpolate(ef, interpolated_year=time, integrate_interpolated_years=TRUE, extrapolation_type="constant")
  emi <- time_interpolate(emi, interpolated_year=time, integrate_interpolated_years=TRUE, extrapolation_type="constant")
  
  # change scenario names
  #getNames(emi) <- gsub("SSP","forcing_SSP",getNames(emi))
  #getNames(ef)  <- gsub("SSP","forcing_SSP",getNames(ef))
    
  if(subtype=="emissions") {
    x <- emi
    w <- NULL
  } else if (subtype=="emission_factors") {
    x <- ef
    activities.EF <- calcOutput("ECLIPSE_SSP",subtype="activities", aggregate=FALSE)
    w <- setYears(activities.EF[,2010,],NULL)
    mapsec = map_sectors_ECLIPSE2REMIND[map_sectors_ECLIPSE2REMIND$eclipse %in% getNames(w, dim=1), c(1,3)]
    w = toolAggregate(w, mapsec, dim=3.1)
  }  
   
  return(list(x           = x,
              weight      = w,
              unit        = "unit",
              description = "calcECLIPSE substitute"
              ))
}
