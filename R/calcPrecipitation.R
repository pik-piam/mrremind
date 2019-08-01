#' @title calcTemperature
#' @description calculates average monthly or total yearly ppt (mm) on different landuse types
#' 
#' @param landusetypes all or only one (to save computation memory)
#' @param months FALSE for yearly total, TRUE for monthly values
#' @param convert FALSE for raw values, TRUE add temperature of 1000mm for countries without observations or land mass.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author David Chen
#' @seealso
#' \code{\link{readLPJml_rev21}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Precipitation")
#' }
#' @importFrom magpiesets findset


calcPrecipitation<-function(landusetypes="all", months=FALSE,convert=TRUE){
  
  ppt<-readSource("LPJml_rev21",subtype = "precipitation",convert = FALSE)
  ppt<-toolCell2isoCell(ppt)
  
  if (months==FALSE){
    ppt<-dimSums(ppt,dim=3.1)
  }
  
  landuse<-calcOutput("LanduseInitialisation",cellular=TRUE,aggregate=FALSE)
  landuse<-time_interpolate(landuse, interpolated_year = getYears(ppt),extrapolation_type = "constant")
  
  # Aggregation
  CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
  landuse_sum<-dimSums(landuse,dim=3)
  out_sum <- toolAggregate(x = ppt, rel = CountryToCell ,weight = landuse_sum, from = "celliso", to = "iso", partrel = TRUE)
  
  #using 1000mm  for countries without values, only HKG,BHR,MUS missing put in neighbouring
  missing<-unique(which(out_sum == 0, arr.ind = TRUE)[,1])
  out_sum<-out_sum[-missing,,]
  
  if (landusetypes!="all"){
    # Aggregation by landuse type
    landuse<-landuse[,,landusetypes]
    out <- toolAggregate(x = ppt, rel = CountryToCell,weight = landuse, from = "celliso", to = "iso", partrel = TRUE)
    out <- out[-missing,,]
    
    # put average precipitation to landusetypes without measurements
    blank<-out*0
    blank[out==0] <- 1
    blank = blank * out_sum
    out<-out+blank
  } else {
    out<-out_sum
  }
  
  #filling
  
  if(convert){
    out<-toolCountryFill(out,fill = 1000)   
  }
  
  weight = calcOutput("LanduseInitialisation",cellular=FALSE,aggregate=FALSE)
  weight<-time_interpolate(weight, interpolated_year = getYears(out),extrapolation_type = "constant")
  if (landusetypes!="all"){weight=weight[,,landusetypes]}
  
  return(list(x=out,
              weight=weight,
              unit="mm/year",
              min=0,
              max=10000,
              description="Countrylevel for ppt",
              isocountries=convert
  )
  )                   
}
