#' @title calcNitrogenWithdrawalByCrop
#' @description calculates the crop-specific withdrawals of nutrients from soils
#'
#' @param indicator total: estimates the inputs per total crop production; by_area estimates the inputs per area harvested
#' @param cellular cellular disaggreagation or national values
#' @param irrigation FALSE for the sum of irrigated and rainfed, FALSE for seperated categories, 'rainfed' or 'irrigated for single categories
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenWithdrawalByCrop")
#' }
#' @importFrom magpiesets findset



calcNitrogenWithdrawalByCrop<-function(indicator="total",cellular=FALSE,irrigation=FALSE){
  
  if(irrigation%in%c("rainfed","irrigated")){  #again, for size reasons
    irrigation2=irrigation
    irrigation=TRUE
  } else {irrigation2=FALSE}
  
  harvest<-collapseNames(calcOutput("Production",products="kcr",cellular=cellular,attributes="nr",irrigation=irrigation,aggregate = FALSE))
  ag<- collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="ag",irrigation=irrigation,attributes="nr",aggregate=FALSE))
  bg<- collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="bg",irrigation=irrigation,attributes="nr",aggregate=FALSE))
  seed<-collapseNames(calcOutput("Seed",cellular=cellular,products="kcr",attributes="nr",irrigation=irrigation,aggregate=FALSE))
  fixation<-calcOutput("NitrogenFixationPast",cellular=cellular,irrigation=irrigation,fixation_types="fixation_crops",aggregate = FALSE)
  
  if(irrigation2!="FALSE"){ #again, for size reasons
    harvest<-harvest[,,irrigation2]
    ag<-ag[,,irrigation2]
    bg<-bg[,,irrigation2]
    seed<-seed[,,irrigation2]
    fixation<-fixation[,,irrigation2]
  }
  
  withdrawal=mbind(
    add_dimension(harvest,nm = "harvest",dim = 3.1),
    add_dimension(ag,nm = "ag",dim = 3.1),
    add_dimension(bg,nm = "bg",dim = 3.1),
    add_dimension(-fixation,nm = "fixation_crops",dim = 3.1),
    add_dimension(-seed,nm = "seed",dim = 3.1)
  )
 
  if (indicator=="by_physical_area"){
    area<-collapseNames(calcOutput("Croparea",aggregate = FALSE,physical=TRUE,cellular=cellular,irrigation=irrigation,sectoral="kcr"))
    if(irrigation2!="FALSE"){ #again, for size reasons
      area<-area[,,irrigation2]
    }
    out<-withdrawal[,,getNames(area)]/area
    weight<-out
    weight[,,]<-area
    data<-toolNAreplace(x=out,weight=weight)
    weight=data$weight
    out=data$x
    unit="t Nr per ha physical area"
  } else if (indicator=="by_area_harvested"){
    area<-collapseNames(calcOutput("Croparea",physical=FALSE,cellular=cellular,irrigation=irrigation,aggregate = FALSE,sectoral="kcr"))
    if(irrigation2!="FALSE"){ #again, for size reasons
      area<-area[,,irrigation2]
    }
    out<-withdrawal[,,getNames(area)]/area
    weight<-out
    weight[,,]<-area
    data<-toolNAreplace(x=out,weight=weight)
    weight=data$weight
    out=data$x
    unit="t Nr per ha area harvested"
  } else if (indicator=="total") {
    out<-withdrawal
    weight=NULL
    out[is.na(out)]<-0
    out[is.nan(out)]<-0
    unit="Mt Nr"
  } else {stop("unknown indicator")}
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description="Nitrogen inputs by crop type",
              isocountries =!cellular
              ))
}