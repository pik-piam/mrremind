#' @title correctKoeppen
#' @description Correct Koeppen climate zones on cellular level
#' @param x magpie object provided by the read function
#' @param subtype Switch between different levels
#' @return List of magpie objects with results on cellular level
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   readSource("Koeppen", subtype="iso", convert="onlycorrect")
#' }
#' 
#' @import madrat
#' @import magclass

correctKoeppen <- function(x, subtype){

  if(subtype=="cellular"){
    
    x <- toolCell2isoCell(x)
    
    if(range(dimSums(x, dim=3))[1]==0){
      
      cat("Some cells to not have a value and fill be filled by proxies.")
      
      # Filling island states with the help of https://en.climate-data.org/
      x["MUS.8219",,"Am"]   <- 1
      x[8220:8224,,"ET"]    <- 1
      x[31185:31186,,"BWh"] <- 1
      x["REU.57134",,"Csb"] <- 1
      x["USA.42108",,"Cfb"] <- 1
      
      # Using magpie_coord to find the nearest neighbour to fill missing values
      x["SOM.7878",,]       <- x["SOM.7879",,]
      x["CHL.33450",,]      <- x["CHL.33451",,]
      x["USA.42166",,]      <- x["USA.42167",,]
      x["CAN.46648",,]      <- x["CAN.46647",,]
      x[c("ARG.33454","ARG.33455"),,][]              <- x["ARG.33456",,]
      x[c("FJI.57128","FJI.57132", "FJI.57133"),,][] <- x["FJI.57129",,]
    }  
  } 

  return(x)
}
