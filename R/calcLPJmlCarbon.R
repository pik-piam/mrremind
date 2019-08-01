#' @title calcLPJmlCarbon
#' @description Handle LPJmL data for carbon module
#' 
#' @param landtype Switch between different land use types ("lu_maize_*", "lu_grass", "nat_veg")
#' @param subtype Switch between diffrent input ("soil_layer", "mrh_*", "lit*", "vegc") and also processed data (soilc_0-30, rh_0-30) 
#' @param climatetype Switch between different climate scenarios ("historical")
#' @param selectyears defaults to past
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readLPJmlCarbon}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LPJmlCarbon", landtype="nat_veg", subtype="soilc_layer", aggregate=FALSE)
#' }

calcLPJmlCarbon<-function(climatetype="historical", landtype="nat_veg", subtype="soilc_layer", selectyears="past"){
  
  
  years <- sort(findset(selectyears,noset = "original"))

  #################
  ### Processed ###
  #################

  if(subtype%in%c("soilc_0-30")){
      
    LPJml_input <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="soilc_layer", selectyears=selectyears, aggregate=FALSE)
      
    weight_layers <- as.magpie(c(layer1=1, 
                                   layer2=1/3, 
                                   layer3=0,
                                   layer4=0,
                                   layer5=0))
      
    LPJml_input   <- dimSums(LPJml_input * weight_layers, dim="layer")
    getNames(LPJml_input, dim="data") <- "soilc_0-30"
    
  } else if(subtype%in%c("soilc")){
    
    LPJml_input <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="soilc_layer", selectyears=selectyears, aggregate=FALSE)
    LPJml_input <- dimSums(LPJml_input, dim="layer")
    getNames(LPJml_input, dim="data") <- "soilc"
    
  } else if(subtype%in%c("rh_0-30")){
    
    rh_layer1 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer0", selectyears=selectyears, aggregate=FALSE)
    rh_layer2 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer1", selectyears=selectyears, aggregate=FALSE)
      
    weight_layers <- as.magpie(c(layer0=1, layer1=1/3))     # weighting to get 0-30cm
    LPJml_input   <- mbind(rh_layer1, rh_layer2)
    LPJml_input   <- dimSums(LPJml_input * weight_layers, dim="layer")
    getNames(LPJml_input, dim="data") <- "rh_0-30"  
  
  } else if(subtype%in%c("rh")){
    
    rh_layer1 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer0", selectyears=selectyears, aggregate=FALSE)
    rh_layer2 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer1", selectyears=selectyears, aggregate=FALSE)
    rh_layer3 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer2", selectyears=selectyears, aggregate=FALSE)
    rh_layer4 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer3", selectyears=selectyears, aggregate=FALSE)
    rh_layer5 <- calcOutput("LPJmlCarbon", climatetype=climatetype, landtype=landtype, subtype="mrh_layer4", selectyears=selectyears, aggregate=FALSE)
       
    LPJml_input   <- mbind(rh_layer1, rh_layer2, rh_layer3, rh_layer4, rh_layer5)
    LPJml_input   <- dimSums(LPJml_input, dim="layer")
    getNames(LPJml_input, dim="data") <- "rh"  
    
  } else {
      
  #################
  ### Raw LPJmL ###
  #################     
      
    if("y2010"%in%years){
      saveyears <- years
      if("y2009"%in%years){years <- years[1:(length(years)-1)]
      } else              {years[length(years)] <- "y2009"}
      expandyears <- TRUE
    } else {expandyears <- FALSE}
    
    readin_name <- paste0(climatetype,".",landtype,".",subtype)  
    LPJml_input <- readSource("LPJmlCarbon", subtype=readin_name, convert="onlycorrect")[,years,]
    
    if(expandyears){
      years       <- saveyears 
      TimeExpand  <- setYears(LPJml_input[,"y2009",], "y2010")
      LPJml_input <- mbind(LPJml_input,TimeExpand)[,years,]
    }
    
    if(grepl("^m", subtype)){
      
      LPJml_input   <- dimSums(LPJml_input, dim="month")                                     ### conversion to annual values for extensive quantities
      getNames(LPJml_input, dim=3)  <- substring(getNames(LPJml_input, dim=3), 2)            ### annual values as standard come wthout prefix 
    }  
  }

  ### weights as land surface area???
  #Landarea <- dimSums(calcOutput("LUH2v2", cellular=TRUE, selectyears=selectyears,aggregate= FALSE), dim=3)
  
  
  return(list(
    x=LPJml_input,
    weight=NULL,
    unit="tC/ha",
    description=paste0("Carbon output from LPJmL (",subtype,") for ", climatetype," climate and ", landtype," landuse type."),
    isocountries=FALSE))

}
  