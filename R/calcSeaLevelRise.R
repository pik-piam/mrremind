#' @title calcSeaLevelRise
#' @description 
#' Calculates gridded percentage of land lost from 2015, relative to 2015.
#' Currently linearly interpolated (for lack of better SLR function) from 2100 back, see readGCF_SeaLevelRise
#' @return gridded magpie object, with percentage of land lost per grid
#' @author David Chen


calcSeaLevelRise <- function() {
  
slr <- readSource("GCF_SeaLevelRise", convert=F)
#make dummy 0 year to interpolate properly
slr2015 <- setYears(slr, 2015)
slr2015[] <- 0

slr <- mbind(slr2015,slr)

slr <- time_interpolate(slr, interpolated_year = c(seq(2015,2100,by=5)), integrate_interpolated_years = T)

x <- slr
return(list(
  x=slr,
  weight=NULL,
  unit="percentage land lost",
  isocountries=FALSE,
  description="Percentage of Land Lost to Sea Level Rise"))
}