#' @title calcSOM
#' @description calculates Soil Organic Matter Pool, accounting for the management history.
#' We assume carbon Stocks from LPJml natural vegetation as a starting point.
#' Here we use the upper 30cm soil layer (0-20cm of + 1/3 of 30-50 cm).
#' We then correct carbon pools by lost c-share depending on the climate region, using default factors of IPCC Guidelines 2006 table 5.5.
#' We assume that this IPCC-corrected value is the target long-term equilibrium value for the soil stocks.
#' Because soil decline and build-up slowly, we assume that in every year, the carbon pools move 15% towards this new equilibrium.
#' This assumption is in line with IPCC saying that the process will take 20 years: with our assumption, 
#' after 5 years 44% of the carbon pool is gone, after 10 years 80% and after 20 years 96%.
#' We determine a carbon stock for cropland soils and non-cropland soils in every cell.
#' If the cropland area expands, the carbon stock of noncropland is proportionally assigned to the cropland pool and vice versa.
#' The outputs of the function are the soilc stocks for cropland and non-cropland.
#' Relevant for the release of N by SOM loss is also the change in carbon stocks per ha, as this relases or binds N. 
#' This is done in delta cropland soilc.
#' @param climatetype Switch between different climate scenarios (default on "historical")
#' @param subtype "stock" (default) for absoulte values, "density" for per hectar values
#'
#' @return List of magpie object with results on country or cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("SOM")
#' }
#' 

calcSOM <- function(climatetype = "historical", subtype = "stock"){
  
  startyear   <- 1951
  years       <- sort(findset("past_all"))
  spinup      <- paste0("y",(startyear:(as.integer(substring(years[1],2))-1)))
  years       <- c(spinup, years)
  lengthyears <- length(years)
  
  soilc     <-       calcOutput("LPJmlCarbon", climatetype=climatetype, landtype="nat_veg", subtype="soilc_0-30", selectyears="past_all", aggregate=FALSE)
  soilc     <- mbind(calcOutput("LPJmlCarbon", climatetype=climatetype, landtype="nat_veg", subtype="soilc_0-30", selectyears=spinup,     aggregate=FALSE), soilc)
  
  states    <- toolCell2isoCell(readSource("LUH2v2",subtype = "states",convert="onlycorrect")[,years,])
  
  #states  <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=FALSE, aggregate=FALSE) 
  
  
  cshare    <- toolCell2isoCell(readSource("LPJml_rev21",subtype="cshare",convert="onlycorrect"))
  attributes<- new.magpie("GLO",NULL,c("c","nr_available","nr_loss"))
  attributes[,,]<-c(1,1/15,0)
  
  # add rice to historical dataset
  # states<-add_columns(states,dim = 3.1,addnm = "c3rice")
  # a<-calcOutput("FAOCrop_aggr",aggregate = F)
  # calculate share of rice in c3 annual crops based on FAO physical area
  
  # in principle possible to add begr/betr area based on LUH2v2
  # crpbf_c3per: C3 perennial crops grown as biofuels
  # crpbf_c4per: C4 perennial crops grown as biofuels
  
  crops  <- c("c3ann","c4ann","c3per","c4per","c3nfx")
  cshare <- cshare*new.magpie("GLO",NULL,crops,fill = 1)
  
  # assumption that perrenial crops have constant c-content is dropped for now
  # until we classify the MAgPIE crops between annual and perrenials. 
  # They cover anyway only a small area.
  # moreover, they are often characterized by low input (e.g. cotton), which leads to low-input c stock losses.
  # cshare[,,c("c3per","c4per")]=0
  
  crop_area    <- dimSums(states[,,crops],dim=3)
  noncrop_area <- dimSums(states,dim=3)-crop_area
  
  target_c_crop    <- dimSums(soilc * (1-cshare[,,crops])*states[,,crops],dim=3)
  target_c_noncrop <- dimSums(soilc*states[,,crops,invert=TRUE],dim=3)
  
  transitions <- crop_area
  transitions[,years[2:length(years)],] <- crop_area[,years[2:length(years)],]-setYears(crop_area[,years[1:length(years)-1],],years[2:length(years)])
  
  abandonnedland <- newland <- transitions
  abandonnedland[abandonnedland>0] <- 0
  abandonnedland=abandonnedland*(-1)
  newland[newland<0] <- 0

  crop_c    = crop_c_ha    = delta_c_crop    = target_c_crop
  noncrop_c = noncrop_c_ha = delta_c_noncrop = target_c_noncrop
  
  crop_c[,2:lengthyears,]                <-NA
  noncrop_c[,2:lengthyears,]             <-NA

  crop_c_ha[,,]    <- delta_c_crop[,,]    <-NA
  noncrop_c_ha[,,] <- delta_c_noncrop[,,] <-NA
  
  crop_c_ha[,1,] = crop_c[,1,] / crop_area[,1,]
  crop_c_ha[is.nan(crop_c_ha)] =0 
  
  noncrop_c_ha[,1,] = noncrop_c[,1,] / noncrop_area[,1,]
  noncrop_c_ha[is.nan(noncrop_c_ha)]=0

  for (year_x in (2:(lengthyears))) {
    
    crop_c[,year_x,] = (setYears(crop_c[,year_x-1,],NULL)
                     + newland[,year_x,] * setYears(noncrop_c_ha[,year_x-1,] ,NULL)
                     - abandonnedland[,year_x,] * setYears(crop_c_ha[,year_x-1,],NULL))
    
    noncrop_c[,year_x,] = (setYears(noncrop_c[,year_x-1,],NULL)
                        - newland[,year_x,] * setYears(noncrop_c_ha[,year_x-1,],NULL) 
                        + abandonnedland[,year_x,] * setYears(crop_c_ha[,year_x-1,],NULL))
    
    
    # assumption on transition: 15% of the soil difference per year. 44% after 5 years, 20% after 10 years, 4% after 20 years
    
    delta_c_crop[,year_x,]=(target_c_crop[,year_x,]-crop_c[,year_x,])*0.15
    delta_c_noncrop[,year_x,]= (target_c_noncrop[,year_x,]-noncrop_c[,year_x,])*0.15
    
    # to avoid infs in division, a rounding is required
    
    crop_c[,year_x,] = round(crop_c[,year_x,] + delta_c_crop[,year_x,],10)
    noncrop_c[,year_x,] = round(noncrop_c[,year_x,] + delta_c_noncrop[,year_x,],10)
    
    crop_c_ha[,year_x,] = setYears(crop_c[,year_x,] / crop_area[,year_x,],NULL)
    crop_c_ha[is.nan(crop_c_ha)] = 0
    crop_c_ha[abs(crop_c_ha)==Inf] = 0
    
    noncrop_c_ha[,year_x,] = setYears(noncrop_c[,year_x,] / noncrop_area[,year_x,],NULL)
    noncrop_c_ha[is.nan(noncrop_c_ha)] = 0
    noncrop_c_ha[abs(noncrop_c_ha)==Inf] = 0
  }
  
  # delta_c is not equivalent to the difference in carbon_cropland_soils over time, as the area changes
  if(subtype=="stock"){
    out<-mbind(setNames(crop_c,"cropland.soilc"),
               setNames(noncrop_c,"noncropland.soilc"),
               setNames(delta_c_crop,"cropland.delta_soilc"),
               setNames(delta_c_noncrop,"noncropland.delta_soilc"),
               setNames(target_c_crop,"cropland.target_soilc"),
               setNames(target_c_noncrop,"noncropland.target_soilc"))
    
    unit    <- "Mt C"
    weight  <- NULL
    
  } else if(subtype=="density"){
    
    delta_c_crop_ha    <- toolNAreplace(delta_c_crop    / crop_area)$x    
    delta_c_noncrop_ha <- toolNAreplace(delta_c_noncrop / noncrop_area)$x 
    
    target_c_crop_ha    <- toolNAreplace(target_c_crop    / crop_area)$x    
    target_c_noncrop_ha <- toolNAreplace(target_c_noncrop / noncrop_area)$x 
    
    
    out<-mbind(setNames(crop_c_ha,"cropland.soilc"),
               setNames(noncrop_c_ha,"noncropland.soilc"),
               setNames(delta_c_crop_ha,"cropland.delta_soilc"),
               setNames(delta_c_noncrop_ha,"noncropland.delta_soilc"),
               setNames(target_c_crop_ha,"cropland.target_soilc"),
               setNames(target_c_noncrop_ha,"noncropland.target_soilc"))
    
    unit    <- "t C per ha"
    weight  <- mbind(setNames(   crop_area,    "cropland"),
                     setNames(noncrop_area, "noncropland"))[, -c(1:10) ,]
    
  } else {stop(paste("Subtype", subtype, "does not exist yet."))}
  
  #delete first 20 years of spin-up
  
  out<-out[,-c(1:10),]
  
  return(list(
    x            = out,
    weight       = weight,
    unit         = unit,
    description  = "Carbon in cropland and non-cropland soils, as well as change over time due to built-up or loss. Change is not equivalen to the difference in carbon_cropland_soils over time, as the area changes. ",
    isocountries = FALSE))
}
