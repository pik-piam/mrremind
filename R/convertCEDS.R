convertCEDS <- function(x,subtype) {
  
  # fill all missing countries with 0
  x[is.na(x)] <- 0

  x1 <- x["srb (kosovo)",,]
  getRegions(x1) <- c("srb")
  x["srb",,] <- x["srb",,] + x1
  x          <- x[c("srb (kosovo)"),,,invert=TRUE]
  
  # Steve Smith 11.3.2016 on CEDS_Review_3-10-16.zip: there is a huge bug for fugitive emissions in zmb, and eth 
  # I believe just past 2010. so do something to correct that (just keep those emissions constant from 2010 forward 
  # for that sector in those two countries).
  
  # checked: all zero expept for NMVOC. But for NMVOC emission after 2010 do not show significant deviations from before 2010
  
  # rename global to glo
  getRegions(x) <- gsub("global","glo", getRegions(x))
  getRegions(x) <- toupper(getRegions(x))
  
  # delete ANT and SCG from raw data because their successors are already included in the data
  #x <- x[c("ANT","SCG"),,invert=TRUE]

  # most shipping and aviation data is global only (except 1A3dii_Domestic-navigation regional). We want to distribute it evenly across all countries.
  # Therefore, save global data because it will be removed by toolCountryfill
  
  # 1A3dii_Domestic-navigation   regional (global value is zero )
  # 1A3di_International-shipping global   (no regional values exist)
  # 1A3ai_International-aviation global   (no regional values exist)
  # 1A3aii_Domestic-aviation     global   (no regional values exist)

  var_glob <- c("1A3di_International-shipping",
                "1A3ai_International-aviation",
                "1A3aii_Domestic-aviation")
  x_glo <- x["GLO",,var_glob] 
  
  # remove global values. Note: the sector 2A1_Cement-production has a global sum that is indentical to the sum over regions
  x <- x["GLO",,invert=TRUE]
  # fills missing ISO countires and remove unknown ISO countires
  x <- toolCountryFill(x,fill=0)

  # Create weight 1 for x_glo
  w <- new.magpie(getRegions(x),getYears(x),getNames(x_glo),fill=1)
  
  # Create mapping of each country to GLO
  mapping <- matrix(c(getRegions(x),rep("GLO",length(getRegions(x)))),length(getRegions(x))) 
  
  # Spread global shipping and aviation data evenly across countries and save it to regions of x
  x[,,var_glob] <- toolAggregate(x_glo,mapping,weight=w)
  
  return(x)
}  