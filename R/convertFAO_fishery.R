#' convertFAO_fishery
#' 
#' @description Converts readFAO_fishery output to complete MAgPIE object containing fishery data on country level (in tonnes)
#' @param subtype "capture" takes all fishdata into account that has been declared as capture fishery
#' "aquaculture" takes all fishdata into account that has been listed as aquaculture fishery
#' @return Fishery data as complete MAgPIE object on country level
#' @author Benjamin Leon Bodirsky, Jasmin Wehner
#' @seealso \code{\link{readSource}}
#' @importFrom madrat toolAggregate toolISOhistorical toolCountryFill readSource calcOutput 
#' @importFrom magclass mbind collapseNames add_columns getNames getRegions
#' @export
convertFAO_fishery <- function(subtype){
  if (subtype=="capture"){
    x_capture <-readFAO_fishery( subtype="capture")
    
    x_capture[is.na(x_capture)] <- 0
    
    #move Other nei countries to ATA
    getRegions(x_capture) <- gsub("XON","ATA",getRegions(x_capture))
    
    #rename fish category species to make them compartible with fish demand categories
    #unclear for freshwater fish. is diadromous fish now included or not in FBC
    #Demersal Marine Fish
    getNames(x_capture) <- gsub("Demersal Marine Fish","Demersal Fish",getNames(x_capture))
    #Pelagic Marine FIsh
    getNames(x_capture) <- gsub("Pelagic Marine Fish","Pelagic Fish",getNames(x_capture))
    #Marine Fish NEI 
    getNames(x_capture) <- gsub("Marine Fish NEI","Marine Fish, Other",getNames(x_capture))
    #"Molluscs excl Cephalopods" 
    getNames(x_capture) <- gsub("Molluscs excl Cephalopods","Molluscs, Other",getNames(x_capture))
    #Freshwater and Diadromous Fish
    getNames(x_capture) <- gsub("Freshwater and Diadromous Fish","Freshwater Fish",getNames(x_capture))
    #Aquatic Animal NEI unclear whether to Meat, Aquatic Mammals or Aquatic Animals, Others
    getNames(x_capture) <- gsub("Aquatic Animals NEI","Aquatic Animals, Others",getNames(x_capture))
    
    #move ABW to ANT to make it align with toolISOhistorical
    x_ABW <- x_capture["ABW",c("1950":"1984"),]
    getRegions(x_ABW) <- "GLO"
    x_capture["ANT",c("1950":"1984"),] <- x_capture["ANT",c("1950":"1984"),] + x_ABW
    x_capture["ABW",c("1950":"1984"),] <- 0 
    
    # Adding country Zanzibar to Tanzania 
    x_capture_Zanzibar <- x_capture["XXX",,]
    getRegions(x_capture_Zanzibar) <- "GLO"
    x_capture["TZA",,] <- x_capture["TZA",,] + x_capture_Zanzibar
    x_capture <- x_capture["XXX",,,invert = TRUE] # removing Zanzibar
    
    
    # split Channel Island (XIS) to GGY and JEY and Sudan (former) (XSF) to SSD and SDN 
    x_captureSUD <- x_capture[c("SSD","SDN")] 
    x_capture <- x_capture[c("SSD","SDN"),,invert=TRUE]
    m <- matrix(c(c("XIS","XIS","XSF","XSF"),c("GGY","JEY","SSD","SDN")),4)
    w <- calcOutput("Population",aggregate=FALSE)[c("GGY","JEY","SSD","SDN"),2010,"pop_SSP2"]
    x_split <- toolAggregate(x_capture[c("XIS","XSF"),,],m,weight=w)
    #delete XSF and XIS from x
    x_capture <- x_capture[c("XIS","XSF"),,invert=TRUE]
    x_capture <- mbind(x_capture,x_split)
    #combine it with data after 2011
    x_capture[c("SSD","SDN"),c(2012:2017),] <- x_captureSUD[c("SSD","SDN"),c(2012:2017),]
    
    
    #splitting up Former Soviet Area Inland waters
    #definitions
    formerSUAsiacountries <- c("AZE","ARM","GEO","KAZ","KGZ","TJK","TKM","UZB")  
    formerSUEuropecountries <- c("BLR","EST","LVA","LTU","MDA","RUS","UKR")
    f <- c("BLR","EST","LVA","LTU","MDA","RUS","UKR","AZE","ARM","GEO","KAZ","KGZ","TJK","TKM","UZB")  
    x_Former_USSR <- x_capture["SUN",,"Former USSR area Inland waters"] #100 percent
    getRegions(x_Former_USSR) <- "GLO"
    categories <- c("Freshwater Fish","Crustaceans","Demersal Fish")
    x_Europe_Inland_waters <- x_capture[formerSUEuropecountries,"y1988","Europe Inland waters"]
    getYears(x_Europe_Inland_waters) <- NULL
    x_Asia_Inland_waters <- x_capture[formerSUAsiacountries,"y1988","Asia Inland waters"]
    getYears(x_Asia_Inland_waters) <- NULL
    
    
    #Asia Inland waters
    share_Asia <- (dimSums(x_Asia_Inland_waters, dim=c(1,3))*100)/(dimSums(x_Asia_Inland_waters, dim=c(1,3))+dimSums(x_Europe_Inland_waters, dim=c(1,3)))
    tmp_Soviet_Asia <- x_Former_USSR * (share_Asia/100) #100 percent for all Europe countries
    x_Asia_country_shares <- (x_Asia_Inland_waters*100)/dimSums(x_Asia_Inland_waters, dim=1) 
    #select relevant categories
    x_Asia_country_shares <- x_Asia_country_shares[,,"Asia Inland waters"][,,categories]
    x_capture_final_shares_Asia <- collapseNames(tmp_Soviet_Asia) * (collapseNames(x_Asia_country_shares)/100)
    #insert data into original magpie object
    x_capture[formerSUAsiacountries,c("1950":"1987"),"Asia Inland waters"][,,categories] <- x_capture_final_shares_Asia[formerSUAsiacountries,c("1950":"1987"),categories]
    
    #Europe Inland waters
    share_Europe <- (dimSums(x_Europe_Inland_waters, dim=c(1,3))*100)/(dimSums(x_Asia_Inland_waters, dim=c(1,3))+dimSums(x_Europe_Inland_waters, dim=c(1,3)))
    tmp_Soviet_Europe <- x_Former_USSR * (share_Europe/100) #100 percent for all Europe countries
    x_Europe_country_shares <- (x_Europe_Inland_waters*100)/dimSums(x_Europe_Inland_waters, dim=1) 
    #select relevant categories
    x_Europe_country_shares <- x_Europe_country_shares[,,"Europe Inland waters"][,,categories]
    x_capture_final_shares_Europe <- collapseNames(tmp_Soviet_Europe) * (collapseNames(x_Europe_country_shares)/100)
    #insert data into original magpie object
    x_capture[formerSUEuropecountries,c("1950":"1987"),"Europe Inland waters"][,,categories] <- x_capture_final_shares_Europe[formerSUEuropecountries,c("1950":"1987"),][,,categories]
    #delete all data from "Former USSR area Inland waters" 
    x_capture <- x_capture[,,"Former USSR area Inland waters", invert =TRUE] 
    
    #execute toolISOhistorical with dynamic transition years because transition years 
    #vary from category to category
    SUN <- x_capture[c(f,"SUN"),,]
    x_incl_transition<-NULL
    for (name_x in getNames(SUN)) {
      firstyear= min(getYears(SUN,as.integer = T)[colSums(SUN[f,,name_x])>0])
      lastyear_sun = max(getYears(SUN,as.integer = T)[colSums(SUN["SUN",,name_x])>0])
      if(firstyear-lastyear_sun>1&lastyear_sun>0){firstyear=lastyear_sun+1}
      
      if(firstyear!=1950 & firstyear!=Inf) {
        add_mapp=data.frame(fromISO=rep("SUN",length(f)),toISO=f,lastYear=paste0("y",firstyear -1),stringsAsFactors = FALSE)
        single_indicator <- toolISOhistorical(SUN[,,name_x]+10^-10,mapping = add_mapp,overwrite = TRUE)
      } else {
        single_indicator <- SUN[f,,name_x]
      }
      x_incl_transition <- mbind(x_incl_transition, single_indicator)
    }
    
    #execute toolISOhistorical for the rest of historical countries
    x_capture <- toolISOhistorical(x_capture, overwrite = T)
    
    x_capture[f,,] <- x_incl_transition[f,,]
    
    #because of overwrite feature I have to add the data from Former Soviet Area Inland Waters ex post 
    x_capture[formerSUEuropecountries,c("1950":"1987"),"Europe Inland waters"][,,categories] <-  x_capture_final_shares_Europe[formerSUEuropecountries,c("1950":"1987"),][,,categories]
    x_capture[formerSUAsiacountries,c("1950":"1987"),"Asia Inland waters"][,,categories] <-  x_capture_final_shares_Asia[formerSUAsiacountries,c("1950":"1987"),categories]
    x_capture["ABW",c("2011":"2017"),] <- x_capture["ABW",c("2011":"2017"),]
    
    
    #for Cheung 2016
    #move Atlantic Antarctic and Indian Ocean Antarctic to Pacific Antarctic to synchronize conversion table with magpie object 
    #define common fish categories 
    #x_capture_extra <- x_capture[,,c("Atlantic Antarctic","Indian Ocean Antarctic")]
    #fish_category_common <- c("Aquatic Animals, Others","Cephalopods",            
                              #"Crustaceans" ,"Demersal Fish") 
    
    #x_capture_extra[,,"Indian Ocean Antarctic"][,,fish_category_common]<- x_capture_extra[,,"Indian Ocean Antarctic"][,,fish_category_common]+ x_capture_extra[,,"Atlantic Antarctic"][,,fish_category_common] 
    
    #x_capture_extra <- x_capture_extra[,,"Atlantic Antarctic", invert=T]
    #removing fishing_area dimension
    #x_capture_extra <- collapseNames(x_capture_extra, collapsedim=2)
    #x_capture_extra <- add_dimension(x_capture_extra, dim=3.2, add="fishing_area",nm="Pacific Antarctic")
    #extracting Molluscs,Other 
    #x_capture_extra_moll <- x_capture_extra[,,"Molluscs, Other"]
    #x_capture_extra <- x_capture_extra[,,"Molluscs, Other", invert=T]
    #x_capture[,,"Pacific Antarctic"] <- x_capture_extra[,,"Pacific Antarctic"]
    #x_capture <- mbind(x_capture, x_capture_extra_moll)
    #delete "Atlantic Antarctic","Indian Ocean Antarctic" entries 
    #x_capture <- x_capture[,,c("Atlantic Antarctic","Indian Ocean Antarctic"), invert=T]
    
    #for Cheung 2018:
    #remove Meditarrerranean Sea because they exlude semi enclosed fishing areas
    #move Pacific Antarctic to Indian Ocean  Antarctic (because fish categories fit better) 
    x_capture_PacAnt <- x_capture[,,"Pacific Antarctic"]
    x_capture_PacAnt <- dimReduce(x_capture_PacAnt, dim_exclude=c("iso","variable","fish_category"))
    i <- getNames(x_capture_PacAnt, dim=1)
    x_capture[,,"Indian Ocean Antarctic"][,,i] <- x_capture[,,"Indian Ocean Antarctic"][,,i] + x_capture_PacAnt[,,i]
    
    x_capture <- x_capture[,,c("Mediterranean and Black Sea","Pacific Antarctic"), invert=T]
    
    x_capture <- toolCountryFill(x_capture, fill=0)
    
     return(x_capture)
  }
  else if (subtype=="aquaculture"){
    x_aqua <-readFAO_fishery( subtype="aquaculture")
    
    #harmonization with x_capture
    x_aqua <- add_columns(x_aqua, addnm = c("Arctic Sea","Atlantic Antarctic","Indian Ocean Antarctic"), dim = 3.2)
    
    x_aqua[is.na(x_aqua)] <- 0
    #rename fish category species to make them compartible with fish demand categories
    #unclear for freshwater fish. is diadromous fish now included or not in FBC
    #Demersal Marine Fish
    getNames(x_aqua) <- gsub("Demersal Marine Fish","Demersal Fish",getNames(x_aqua))
    #Pelagic Marine FIsh
    getNames(x_aqua) <- gsub("Pelagic Marine Fish","Pelagic Fish",getNames(x_aqua))
    #Marine Fish NEI 
    getNames(x_aqua) <- gsub("Marine Fish NEI","Marine Fish, Other",getNames(x_aqua))
    #"Molluscs excl Cephalopods" 
    getNames(x_aqua) <- gsub("Molluscs excl Cephalopods","Molluscs, Other",getNames(x_aqua))
    #Freshwater and Diadromous Fish 
    getNames(x_aqua) <- gsub("Freshwater and Diadromous Fish","Freshwater Fish",getNames(x_aqua))
    #Aquatic Animal NEI unclear whether to Meat, Aquatic Mammals or Aquatic Animals, Others
    getNames(x_aqua) <- gsub("Aquatic Animals NEI","Aquatic Animals, Others",getNames(x_aqua))
    
    #split up Channel Islands into GGY and JEY
    x_aquaSUD <- x_aqua[c("SSD","SDN")] 
    x_aqua <- x_aqua[c("SSD","SDN"),,invert=TRUE]
    m <- matrix(c(c("XIS","XIS","XSF","XSF"),c("GGY","JEY","SSD","SDN")),4)
    w <- calcOutput("Population",aggregate=FALSE)[c("GGY","JEY","SSD","SDN"),2010,"pop_SSP2"]
    x_split <- toolAggregate(x_aqua[c("XIS","XSF"),,],m,weight=w)
    #delete XIS from x
    x_aqua <- x_aqua[c("XIS","XSF"),,invert=TRUE]
    x_aqua <- mbind(x_aqua,x_split)
    x_aqua[c("SSD","SDN"),c(2012:2017),] <- x_aquaSUD[c("SSD","SDN"),c(2012:2017),]
    
    #add countries SXM and CUW and YUG as blank magpie objects to make toolisohistorical work
    x_SXMCUWYUG <- new.magpie(cells_and_regions = c("SXM","CUW","YUG"), years = 1950:2017, getNames(x_aqua))
    x_SXMCUWYUG[is.na(x_SXMCUWYUG)] <- 0
    x_aqua <- mbind(x_aqua, x_SXMCUWYUG)
    
    #delete entries for Former USSR area - Inland waters
    formerSUAsiacountries <- c("AZE","ARM","GEO","KAZ","KGZ","TJK","TKM","UZB")  
    formerSUEuropecountries <- c("BLR","EST","LVA","LTU","MDA","RUS","UKR")
    x_Former_USSR <- x_aqua["SUN",,"Former USSR area Inland waters"] #100 percent
    f <- c("BLR","EST","LVA","LTU","MDA","RUS","UKR","AZE","ARM","GEO","KAZ","KGZ","TJK","TKM","UZB")  
    getRegions(x_Former_USSR) <- "GLO"
    
    x_Asia_Inland_waters <- x_aqua[formerSUAsiacountries,"y1988","Asia Inland waters"]
    getYears(x_Asia_Inland_waters) <- NULL
    x_Europe_Inland_waters <- x_aqua[formerSUEuropecountries,"y1988","Europe Inland waters"]
    getYears(x_Europe_Inland_waters) <- NULL
    
    
    #Asia Inland waters
    share_Asia <- (dimSums(x_Asia_Inland_waters, dim=c(1,3))*100)/(dimSums(x_Asia_Inland_waters, dim=c(1,3))+dimSums(x_Europe_Inland_waters, dim=c(1,3)))
    tmp_Soviet_Asia <- x_Former_USSR * (share_Asia/100) #100 percent for all Europe countries
    getRegions(tmp_Soviet_Asia) <- "GLO"
    x_Asia_country_shares <- (x_Asia_Inland_waters*100)/dimSums(x_Asia_Inland_waters, dim=1) 
    #select relevant category
    x_Asia_country_shares <- x_Asia_country_shares[,,"Freshwater Fish.Asia Inland waters.Freshwater"]
    x_aqua_final_shares_Asia <- collapseNames(tmp_Soviet_Asia) * collapseNames((x_Asia_country_shares/100))
    x_aqua[formerSUAsiacountries,c("1950":"1987"),"Freshwater Fish.Asia Inland waters.Freshwater"] <- x_aqua_final_shares_Asia[,c("1950":"1987"),]
    
    #Europe Inland waters
    share_Europe <- (dimSums(x_Europe_Inland_waters, dim=c(1,3))*100)/(dimSums(x_Asia_Inland_waters, dim=c(1,3))+dimSums(x_Europe_Inland_waters, dim=c(1,3)))
    tmp_Soviet_Europe <- x_Former_USSR * (share_Europe/100) #100 percent for all Europe countries
    getRegions(tmp_Soviet_Europe) <- "GLO"
    x_Europe_country_shares <- (x_Europe_Inland_waters*100)/dimSums(x_Europe_Inland_waters, dim=1) 
    x_Europe_country_shares <- x_Europe_country_shares[,,"Freshwater Fish.Europe Inland waters.Freshwater"]
    x_aqua_final_shares_Europe <- collapseNames(tmp_Soviet_Europe) * collapseNames((x_Europe_country_shares/100))
    
    x_aqua[formerSUEuropecountries,c("1950":"1987"),"Freshwater Fish.Europe Inland waters.Freshwater"] <- x_aqua_final_shares_Europe[,c("1950":"1987"),]
    
    x_aqua <- x_aqua[,,"Former USSR area Inland waters", invert =TRUE] 
    
    #execute toolISOhistorical with dynamic transition years
    SUN <- x_aqua[c(f,"SUN"),,]
    x_incl_transition<-NULL
    for (name_x in getNames(SUN)) {
      firstyear= min(getYears(SUN,as.integer = T)[colSums(SUN[f,,name_x])>0])
      lastyear_sun = max(getYears(SUN,as.integer = T)[colSums(SUN["SUN",,name_x])>0])
      if(firstyear-lastyear_sun>1&lastyear_sun>0){firstyear=lastyear_sun+1}
      
      if(firstyear!=1950 & firstyear!=Inf) {
        add_mapp=data.frame(fromISO=rep("SUN",length(f)),toISO=f,lastYear=paste0("y",firstyear -1),stringsAsFactors = FALSE)
        single_indicator <- toolISOhistorical(SUN[,,name_x]+10^-10,mapping = add_mapp,overwrite = TRUE)
      } else {
        single_indicator <- SUN[f,,name_x]
      }
      x_incl_transition <- mbind(x_incl_transition, single_indicator)
    }
    
    #execute toolISohistorical for countries besides SUN
    x_aqua <- toolISOhistorical(x_aqua, overwrite = T)
    
    x_aqua[f,,] <- x_incl_transition[f,,]
    
    #for Cheung 2018:
    #remove Mediterrerranean Sea because they exlude semi enclosed fishing areas
    x_aqua <- x_aqua[,,"Mediterranean and Black Sea", invert=T]
    #x_aqua <- dimReduce(x_aqua, dim_exclude = c("iso","variable","fish_category","fishing_area"))
    x_aqua <- toolCountryFill(x_aqua, fill=0)
    
   return(x_aqua)
  }
  
}

