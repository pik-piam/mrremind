#' @title calcCarbonMineralization
#' 
#' @description Calculates decompostion rates for land use types.
#'
#' @param landuse_types Sets: carbon or ini_new (magpie initialisation)
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonMineralization")
#' }
#' @importFrom magclass getCells
#' @importFrom magclass dimCode

calcCarbonMineralization<-function(landuse_types="carbon", cellular = TRUE){

  if(!cellular)(stop("For the moment just on cellular level"))
  
  years      <- findset("past")
  yearbefore <- (as.integer(substring(years,2)) - 1) 
  
  Landuse <- calcOutput("Landuse", landuse_types=landuse_types, cellular=cellular, aggregate=FALSE)[,years,]
  
  ##################
  ### single     ###
  ##################
  
  LPJml2MAgPIE  <- c(past = "lu_grass", natveg = "nat_veg", crop = "lu_maize_nres_rf") 

  Soilc_ha      <-  mbind(calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["crop"]), subtype="soilc", selectyears=yearbefore, aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["past"]), subtype="soilc", selectyears=yearbefore, aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["natveg"]), subtype="soilc", selectyears=yearbefore, aggregate=FALSE))
  Soilc_ha      <- setYears(Soilc_ha, nm = years)
  
  
  Rh_ha         <-  mbind(calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["crop"]), subtype="rh", aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["past"]), subtype="rh", aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["natveg"]), subtype="rh", aggregate=FALSE))

  # decomposition rate as rh per soilc per year in t/ha
  DecompRate <- collapseNames(Rh_ha / Soilc_ha, collapsedim ="data")
  getNames(DecompRate, dim="data") <- "decomp_rate"
  
  DecompRate[which(is.na(DecompRate))]       <- 0
  DecompRate[which(is.infinite(DecompRate))] <- 0

  ##################
  ### total set  ###
  ##################
  
  if(landuse_types=="carbon"){
    
    DecompRate <- add_columns(DecompRate, addnm="urban", dim = dimCode("landuse", DecompRate))
    DecompRate[,,"urban"] <- 0
    
  } else if(landuse_types=="ini_new"){
    
    DecompRate <- add_columns(DecompRate, addnm="urban", dim = dimCode("landuse", DecompRate))
    DecompRate[,,"urban"] <- 0
    
    natveg_disagg <- c("primforest","secdforest","forestry","other")
    DecompRate    <- add_columns(DecompRate, addnm=natveg_disagg, dim = dimCode("landuse", DecompRate))
    DecompRate[,,natveg_disagg] <- DecompRate[,,"natveg"]
    DecompRate    <- DecompRate[,,"natveg", invert=TRUE]
    
  } else {stop("Given landtypes (currently) not supported!")}
  
   
  #weights have to be determined
  
  return(list(x=DecompRate,
              weight=NULL,
              unit="MtC/MtC",
              description=paste0("Mineralization rates for ", landuse_types ," land use set"),
              min = 0,
              isocountries = !cellular)
         )  
}
