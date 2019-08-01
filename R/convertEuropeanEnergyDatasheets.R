#' Convert European Energy Datasheets
#'
#' Converts European Energy Datasheets magpie object into appropriate form for the REMIND model
#' 
#' @param x European Energy Datasheets magpie object derived from readEuropeanEnergyDatasheets function
#' @param subtype as string. Possibilities are "Production","Mport","Xport","NetImports","Consumption","Transformation","EnergyBranchConsumption",
#' "DistributionLosses","PrimaryEnergy","AvailableFe","FinalEnergy","Electricity","Indicators","Heat", "CHP","Transport","Emissions","Population" and "GDP"
#' @return converted European Energy Datasheets magpie object
#' @author Atreya Shankar
#' @source European Energy Datasheets public database https://ec.europa.eu/energy/en/data-analysis/country 
#' @examples
#' \dontrun{a <- convertEuropeanEnergyDatasheets(x, "Production")}

convertEuropeanEnergyDatasheets <- function(x, subtype){
  
  s <- c("Production","Mport","Xport","NetImports","Consumption","Transformation","EnergyBranchConsumption",
         "DistributionLosses","PrimaryEnergy","AvailableFe","FinalEnergy","Electricity","Indicators","Heat",
         "CHP","Transport","Emissions","Population","GDP")
  
  if(length(subtype) == 1 & subtype %in% s){
    
    # map to remind and remove units column
    map2remind <- read.csv2("mapping2remind.csv", stringsAsFactors = FALSE, encoding="UTF-8")[,-c(1,9)]
    map2remind$value <- 1
    y <- as.magpie(map2remind)
    getNames(x) <- getNames(y)
    
    # convert European Energy Datasheets countries 2 char format to iso 3
    iso3 <- read.csv2("rawCountry2iso3Mapping.csv", stringsAsFactors = FALSE, encoding="UTF-8")
    getRegions(x) <- iso3[,2]
    
    # fill up remaining countries with 0s, replace NAs with 0s
    x <- toolCountryFill(x,fill=0,verbosity=2)
    x[is.na(x)] <- 0
    
    # return magpie with subtype and cleaner names
    check <- sapply(s, function(x) return(colSums(is.na(map2remind[which(map2remind[,1] == x),])) == nrow(map2remind[which(map2remind[,1] == x),])))
    
    x <- x[,,which(map2remind[,1] == subtype)]
    
    falselength <- 6 - as.numeric(colSums(check)[which(colnames(check) == subtype)])
    names(dimnames(x))[3] <- substr("dim1.dim2.dim3.dim4.dim5.dim6.dim7",1,(5*falselength)-1)
    
    truelength <- as.numeric(colSums(check[c(3:7),])[which(colnames(check) == subtype)])
    b <- getNames(x)
    b <- substr(b,1,nchar(b)-(3*truelength))
    
    check2 <- as.numeric(check[2,][which(colnames(check) == subtype)])
    
    # only remove first instance from left if no type present
    if(falselength == 0){
      b <- ""
    } else if(check2 == 1 & falselength > 0){
      b <- sub("NA.", "", b)
      b <- sub(paste(subtype, ".", sep=""), "", b)
    } else b <- sub(paste(subtype, ".", sep=""), "", b)
    
    getNames(x) <- b
    
    return(x)
  } else stop("invalid subtype")
}