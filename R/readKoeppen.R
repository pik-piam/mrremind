#' @title readKoeppen
#' @description Read Koeppen climate zones on iso-country and cellular level
#' @param subtype Switch between different levels
#' @return List of magpie objects with results on country or cellular level
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   readSource("Koeppen", subtype="iso")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom utils read.csv

readKoeppen<-function(subtype="iso"){
  
  if(subtype == "iso"){
  
    x         <- read.csv("kgzones.csv")
    x$country <- NULL
    mag       <- as.magpie(x,spatial=1,temporal=0,datacol=2)
    
  } else if(subtype == "cellular"){
    
    alltimes  <- c("1951-1975_ASCII.txt", "1976-2000_ASCII.txt", "2001-2025_A2_ASCII.txt")
    out       <- NULL
    
    for(i in 1:length(alltimes)){
    
      file      <- alltimes[i]
      x         <- read.table(file, skip=1, header=TRUE, stringsAsFactors=FALSE, allowEscapes=TRUE)
      
      grid           <- array(NA, dim=c(720,360))
      irowicol       <- cbind(2*(x[,"Lon"]+180+0.25),2*(x[,"Lat"]+90+0.25))
      grid[irowicol] <- x[,"Cls"]
      
      mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv", readcsv=TRUE)
      cellNames <- mapping$celliso
      years     <- as.numeric(unlist(regmatches(file, gregexpr("\\d{4}",file))))
      years     <- seq(years[1],years[2])
      mag       <- array(0, dim=c(59199,length(years),1),dimnames=list(cellNames,years,"Cls"))
      translate_grid2mag <- t(2*t(magpie_coord)+ c(360,180)+0.5)
      mag[]     <- rep(grid[translate_grid2mag], length(years))
      mag       <- as.magpie(mag)
      out       <- mbind(out, mag)
    }
    
    mag <- new.magpie(getCells(out), getYears(out), levels(factor(out)), fill=0)
    for(cls in getNames(mag)) mag[,,cls][which(out==cls)] <- rep(1,length(which(out==cls)))
    
    #mag         <- read.magpie("koeppen_geiger_0.5.mz") 
    
  } else stop("Invalid subtype! (Valid subtype: 'iso' and 'cellular'")
  
  return(mag)
}

