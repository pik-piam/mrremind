#' @title calcForestProtectionShare
#' @description calculates the share of protected forests. Data from FAO on protected forests is used to split the primary and secondary forest pool of the LU2v1 dataset.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data? For country-level vs regional data, remember to set "aggregate" to false.
#' @return List of magpie object with results on country or cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("ForestProtectionShare")
#' }
#' @importFrom magclass setNames

calcForestProtectionShare<-function(cellular=FALSE){
  past<-findset("past")
  countrydata<-calcOutput("LanduseInitialisation",aggregate = FALSE,years=past,cellular=FALSE)

  LUH2v2<-calcOutput("LUH2v2",landuse_types="LUH2v2",irrigation=FALSE,years=past,cellular=FALSE,aggregate = FALSE)
    
  protection_country   <- readSource("FAO_forestry","protection")
  protection_country  <-  time_interpolate(protection_country,interpolated_year = past,integrate_interpolated_years = TRUE,extrapolation_type = "constant")
  vcat(verbosity = 3,"Protected forest is interpolated for missing years and held constant for the period before FAO starts")
    
  # divide primary forest into protected and non protected forest
    
  protection_shr =protection_country / dimSums(countrydata[,,c("primforest","secdforest")],dim=3)
  protection_shr[is.na(protection_shr)]<-0
  protection_shr[protection_shr==Inf]<-0
  protection_shr[protection_shr>1]<-1
    
  tmp = dimSums(protection_shr * dimSums(countrydata[,,c("primforest","secdforest")],dim=3),dim=1)-dimSums(protection_country,dim=1)
  if (any(tmp<0)){
    vcat(verbosity = 2,paste("Mismatch of FAO protected areas and Hurtt forest:",paste(paste(getYears(tmp),round(tmp,0),"Mha, "),collapse = " "),". cut off."))
  }
  
  if (cellular==TRUE)  {
    mapping<-toolMappingFile(type = "cell",readcsv = TRUE,name = "CountryToCellMapping.csv")
    celldata <- calcOutput("LanduseInitialisation", cellular=TRUE, aggregate=FALSE)
    protection_shr <- toolAggregate(x = protection_shr[unique(mapping$iso),,],rel = mapping,from="iso",to="celliso",dim=1)
    weight <- dimSums(celldata[,,c("primforest","secdforest")],dim=3)
  } else {
    weight<-dimSums(countrydata[,,c("primforest","secdforest")],dim=3)
  }

  # scenarios  
  protection_shr<-setNames(toolHoldConstantBeyondEnd(protection_shr),"constant")
  weight<-setNames(toolHoldConstantBeyondEnd(weight),"constant")
  weight[is.na(weight)]<-0
  
  # for example how to create scenarios, look at calcAWMSconfShr()

  return(list(x=protection_shr,
        weight=weight,
        unit="share",
        description="Share of primary and secondary forest that is classified as protected",
        isocountries=!cellular
        )
  )
  

}







