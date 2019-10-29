#' Convert Global plantation forest data
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object containing original values
#' @param subtype Available subtypes are Plantations, SNForest, PlantedSNForest, TotPlanted, GroParInit, AgeClassInit, Ownership and EndUse
#' @return Data as MAgPIE object
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}},
#' @examples
#' 
#' \dontrun{ a <- readSource("GPFADB_2005","Plantations",convert=TRUE)}

convertGPFADB_2005 <- function(x,subtype){
  if(any(c("Plantations", "SNForest", "PlantedSNForest", "TotPlanted", "GroParInit", "AgeClassInit", "Ownership", "EndUse")%in% subtype)){
    x <- toolCountryFill(x,fill = 0)
    }
  if(any(c("Plantations", "SNForest", "PlantedSNForest", "TotPlanted", "Ownership", "EndUse")%in% subtype)){
    x[,,] <- x[,,]/1000 # Change 000ha to mil ha
    return(x)
  } else if(any(c("GroParInit", "AgeClassInit")%in% subtype)){
    return(x)
  }
  else {stop("Invalid subtype ", subtype)}
}