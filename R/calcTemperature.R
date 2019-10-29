#' @title calcTemperature
#' @description calculates average monthly temperature on different landuse types
#'
#' @param landusetypes all or only one (to save computation memory)
#' @param months FALSE for yearly average, TRUE for monthly values
#' @param convert FALSE for raw values of temperature, TRUE add temperature of 15 degrees for countries without observations or land mass.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readLPJml_rev21}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Temperature")
#' }
#' @importFrom magpiesets findset


calcTemperature<-function(landusetypes="all", months=FALSE,convert=TRUE){
  
  temp<-readSource("LPJml_rev21",subtype = "temperature",convert = FALSE)
  temp<-toolCell2isoCell(temp)
  
  if (months==FALSE){
    temp<-dimSums(temp,dim=3.1)/12
  }
  
  landuse<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)
  landuse<-time_interpolate(landuse, interpolated_year = getYears(temp),extrapolation_type = "constant")

  # Aggregation
  CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
  landuse_sum<-dimSums(landuse,dim=3)
  out_sum <- toolAggregate(x = temp, rel = CountryToCell,weight = landuse_sum, from = "celliso", to = "iso", partrel = TRUE)
  
  #using 15 degrees for countries without values
  missing<-unique(which(out_sum == 0, arr.ind = TRUE)[,1])
  out_sum<-out_sum[-missing,,]
  
  if (landusetypes!="all"){
    # Aggregation by landuse type
    landuse<-landuse[,,landusetypes]
    out <- toolAggregate(x = temp, rel = CountryToCell,weight = landuse, from = "celliso", to = "iso", partrel = TRUE)
    out <- out[-missing,,]
    
    # put average temperature to landusetypes without measurements
    blank<-out*0
    blank[out==0] <- 1
    blank = blank * out_sum
    out<-out+blank
  } else {
    out<-out_sum
  }
  
  if(convert){
    out<-toolCountryFill(out,fill = 15)   
  }
  
  weight = calcOutput("LanduseInitialisation",cellular=FALSE,aggregate=FALSE)
  weight<-time_interpolate(weight, interpolated_year = getYears(out),extrapolation_type = "constant")
  if (landusetypes!="all"){weight=weight[,,landusetypes]}
   
  return(list(x=out,
              weight=weight,
              unit="Degree Celcius",
              min=-100,
              max=100,
              description="Countrylevel for Tmean",
              isocountries=convert
              )
  )                   
}
