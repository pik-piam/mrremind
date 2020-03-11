#' @title convertKoeppen
#' @description Convert Koeppen climate zones on iso-country level
#' @param x magpie object provided by the read function
#' @param subtype Switch between different levels
#' @return List of magpie objects with results on country level
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   readSource("Koeppen", subtype="iso", convert=TRUE)
#' }
#'
#' @import madrat
#' @import magclass

convertKoeppen<-function(x, subtype="iso") {
  
  if(subtype=="iso"){
    
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="ROM")]<-"ROU"
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="ZAR")]<-"COD"
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="MON")]<-"MCO"
    dimnames(x)[[1]][which(dimnames(x)[[1]]=="WSH")]<-"ESH"
    
    x <- toolCountryFill(x, fill = NA, BHR="QAT", HKG="CHN", MUS="MDG", PSE="ISR", SGP="MYS", TLS="IDN")
    
    return(x)
    
  } else stop("Cellular koeppen-geiger can not be converted to country level data. Use subtype='iso' for country level data.")

}