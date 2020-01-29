#' @title historical emissions per sector or mac
#' @description provides historical emissions values per economic sector or per mac sector. For now it only includes European data.
#'
#' @param subtype Either "sector" or "MAC"
#' @return magpie object of historical emissions data
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("HistEmissions")
#' }
#'  


calcHistEmissions <- function(subtype="sector"){
  
  if (subtype == "sector") {
    ###european data from Eurostat
    emi <- readSource(type="Eurostat",subtype="sectorEmi")[,seq(1998,2017,1),c("CO2","CH4","N2O")] # "GHG","CO2","CH4","N2O","HFC","PFC","HFC_PFC_NSP","SF6","NF3"
    getNames(emi,dim=1) <- c("co2","ch4","n2o")
    description <- "Historical emissions per sector"
    gwp_ch4_eurostat <- 25 #values from AR4
    gwp_n2o_eurostat <- 298#values from AR4
    emi[,,"ch4"] <- emi[,,"ch4"]/gwp_ch4_eurostat
    emi[,,"n2o"] <- emi[,,"n2o"]/gwp_n2o_eurostat
    #list of countries with eurostat data
    c_eurostat <- where(emi>0)$true[]$regions
    c_noneu <- setdiff(getRegions(emi),c_eurostat)
    
    
    ###non-european data from CEDS
    ch4   <- readSource("CEDS",subtype="CH4")
    co2   <- readSource("CEDS",subtype="CO2")
    n2o   <- readSource("CEDS",subtype="N2O")
    y <- Reduce(intersect,list(getYears(ch4),
                               getYears(co2),
                               getYears(n2o)))
    emi_ot <- mbind(ch4[,y,],co2[,y,],n2o[,y,])/ 1000 # kt -> Mt
    if (any(!emi_ot[,,"6B_Other-not-in-total"]==0)) cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data! Please check CEDS source files.\n")
    emi_ot <- emi_ot[,,"6B_Other-not-in-total",invert=TRUE]
    map_CEDS59_to_Sec  <- read.csv2(toolMappingFile("sectoral", "mappingCEDS59toSECTOR17.csv"), stringsAsFactors=FALSE)
    emi_ot <- toolAggregate(x=emi_ot,weight = NULL, dim=3.1, rel = map_CEDS59_to_Sec, from="CEDS59",to="SECTOR")
    #add cdr-process and indirect-process
    tmp <- emi_ot[,,c("bunkers-energy","power-energy")] * 0
    getNames(tmp,dim=1) <- c("cdr-process","indirect-process")
    emi_ot <- mbind(emi_ot,tmp)
    # remove third entry "units" in data dimension
    emi_ot <- collapseNames(emi_ot,collapsedim = 3)
    # lower case
    map <- c(CH4="ch4",CO2="co2",N2O="n2o")
    getNames(emi_ot,dim=2) <- map[getNames(emi_ot,dim=2)]
    #switch order
    emi_ot <- dimOrder(emi_ot,perm=c(2,1))
    #split 2nd data column
    getNames(emi_ot,dim=2) <- gsub(x=getNames(emi_ot,dim=2),pattern="-",replacement=".")
    #add 2016 and 2017, assuming constant emissions
    emi_ot <- mbind(emi_ot,setYears(emi_ot[,2015,],2016),setYears(emi_ot[,2015,],2017))

    
    ###binding together european for european region(s) and non-european data for all others
    emi <- mbind(emi[c_eurostat,,],emi_ot[c_noneu,seq(1998,2017),])
    
  } else if (subtype == "MAC"){
    ###european data from Eurostat
    emi <- readSource(type="Eurostat",subtype="MACCemi")[,seq(1998,2017,1),]
    description <- "Historical emissions per MAC sector"
    gwp_ch4_eurostat <- 25 #values from AR4
    gwp_n2o_eurostat <- 298#values from AR4
    emi[,,grep("ch4",getNames(emi),value=T)] <- emi[,,grep("ch4",getNames(emi),value=T)]/gwp_ch4_eurostat
    emi[,,grep("n2o",getNames(emi),value=T)] <- emi[,,grep("n2o",getNames(emi),value=T)]/gwp_n2o_eurostat
    #list of countries with eurostat data
    c_eurostat <- where(emi>0)$true[]$regions
    c_noneu <- setdiff(getRegions(emi),c_eurostat)
    
    ch4   <- readSource("CEDS",subtype="CH4")
    co2   <- readSource("CEDS",subtype="CO2")
    n2o   <- readSource("CEDS",subtype="N2O")
    if (any(!ch4[,,"6B_Other-not-in-total"]==0) | any(!co2[,,"6B_Other-not-in-total"]==0) | any(!n2o[,,"6B_Other-not-in-total"]==0) ) cat("CEDS59 sector 6B_Other-not-in-total was removed although it contains data! Please check CEDS source files.\n")
    ch4 <- ch4[,,"6B_Other-not-in-total",invert=TRUE]
    co2 <- co2[,,"6B_Other-not-in-total",invert=TRUE]
    n2o <- n2o[,,"6B_Other-not-in-total",invert=TRUE]
    map_CEDS59_to_MAC  <- read.csv2(toolMappingFile("sectoral", "mappingCEDS59toMACperGas.csv"), stringsAsFactors=FALSE)
    ch4 <- toolAggregate(x=ch4,weight = NULL, dim=3.1, rel = map_CEDS59_to_MAC, from="CEDS59",to="MAC.ch4")
    co2 <- toolAggregate(x=co2,weight = NULL, dim=3.1, rel = map_CEDS59_to_MAC, from="CEDS59",to="MAC.co2")
    n2o <- toolAggregate(x=n2o,weight = NULL, dim=3.1, rel = map_CEDS59_to_MAC, from="CEDS59",to="MAC.n20")
    
    y <- Reduce(intersect,list(getYears(ch4),
                               getYears(co2),
                               getYears(n2o)))
    emi_ot <- mbind(ch4[,y,],co2[,y,],n2o[,y,])/ 1000 # kt -> Mt
    #get rid of other categories
    emi_ot <- emi_ot[,,c(".CO2.kt",".CH4.kt",".N2O.kt"),invert=T]
    # remove second and third entry "units" in data dimension
    emi_ot <- collapseNames(emi_ot,collapsedim = c(2,3))
    #add 2016 and 2017, assuming constant emissions
    emi_ot <- mbind(emi_ot,setYears(emi_ot[,2015,],2016),setYears(emi_ot[,2015,],2017))
  
    
    ###binding together european for european region(s) and non-european data for all others
    emi <- mbind(emi[c_eurostat,,],emi_ot[c_noneu,seq(1998,2017),])
    
  }
   
  out <- new.magpie(cells_and_regions = getRegions(emi), years = c(2000,2005,2010,2015), names = getNames(emi))
  out[,2000,] <- dimSums(emi[,c(1998,1999,2000,2001,2002),],dim = 2)/5
  out[,2005,] <- dimSums(emi[,c(2003,2004,2005,2006,2007),],dim = 2)/5
  out[,2010,] <- dimSums(emi[,c(2008,2009,2010,2011,2012),],dim = 2)/5
  out[,2015,] <- dimSums(emi[,c(2013,2014,2015,2016,2017),],dim = 2)/5
  
  #Returning emission values
  return(list(x=out, weight=NULL,
              unit="Mt CH4/N2O/CO2 respectively", 
              description=description             
  )) 
}

