#' toolCell2isoCell
#' 
#' Sets cell names to [iso country code].[cell number]
#' @param x magpie object on cellular level
#' @return return changed input data
#' @author Kristine Karstens
#' 
#' @importFrom utils read.csv
#' @export


toolCell2isoCell <- function(x){

   CellToCellISO  <- toolMappingFile("cell","CountryToCellMapping.csv",readcsv = TRUE)[1:2]
   getCells(x)    <- CellToCellISO$celliso
   
   return(x)
}