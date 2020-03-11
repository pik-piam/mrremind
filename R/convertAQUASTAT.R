#' @title convertAQUASTAT
#' @description Convert data based on AQUASTAT database (http://www.fao.org/nr/water/aquastat/data/query/index.html?lang=en)
#'  
#' @param x MAgPIE object containing AQUASTAT data on country level
#' @return magpie objects with results on contury level
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#'  readSource("AQUASTAT", subtype="ConsAgri", convert=TRUE)
#' }

convertAQUASTAT <- function(x){
  
  for(iso3 in getCells(x)){
    
    na_years <- getYears(x[iso3,,], as.integer=TRUE)[which(is.na(x[iso3,,]))]
    years    <- setdiff(getYears(x[iso3,,], as.integer=TRUE), na_years)
    
    if(length(na_years) == 60){
      
      x[iso3,,] <- 0
      
    } else {
      
      if(length(na_years) == 59){
        
        null_year           <- years - 5
        x[iso3,null_year,]  <- 0
        years               <- c(null_year,years)
        na_years            <- setdiff(na_years, null_year)
      }
      
      # interpolate years to fill na_years (extrapolate with linear)
      x[iso3,na_years,]      <- time_interpolate(x[iso3,years,], interpolated_year = na_years, extrapolation_type = "linear")
      
      # removing values below zero (introduced by linear extrapolation)
      x[iso3,,][x[iso3,,]<0] <- 0
      
      # set all years after last reporting year to last reporting year
      after_years            <- na_years[na_years > tail(years,1)]
      x[iso3,after_years,]   <- setYears(x[iso3,tail(years,1),], NULL)
    }
  }
  
  out <- toolCountryFill(x, fill = 0)
  
  return(out)
}