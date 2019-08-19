## to do
# add functionality to combine Elements
# allow more than 2 datasets:
 # possibly via a loop over the lenth of ... make make a list containing all the information, then check for dublicate items. Or simply always take the first value found.
# if one dataset only contains NAs use the other

# x <- list(CropPrim, CropProc)
















#' Combine FAO datasets
#' 
#' Allows to combine two similar FAO datasets with dublicates being removed.
#' For instance combine Production:Crops and Production: Crops Processed to one
#' magpie object
#' 
#' 
#' @param ... two magpie objects with FAO data
#' @param combine "Item" to combine datasets that for instance both contain
#' palm oil data
#' @return MAgPIE object with data from both inputs but dublicates removed
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#'   \dontrun{ a <- toolFAOcombine(Crop,CropPro, combine="Item")
#'   }
#'   
#' @importFrom magclass getNames getYears dimSums mbind2
#' @export
#' 
toolFAOcombine <- function (..., combine="Item") {
  
  # the names of the ellipsis (...) arguments
  dotnames <- sapply(match.call()[-1], deparse)
  
  x <- list(...)
  if (length(x) < 2) stop("At least two files have to be provided")
  if (length(x) > 2) stop("Function currently only working for two files")

  # first programmed for only two files (change to infinte number later)
  
  if (combine == "Item") {
  
    x1 <- x[[1]]
    x1[is.na(x1)] <- 0
    x2 <- x[[2]]
    x2[is.na(x2)] <- 0
    
    ## match temporal dimension
    years <- intersect(getYears(x1),getYears(x2))
    if (length(setdiff(getYears(x1),getYears(x2))) > 0 ) vcat(1, "No data for year", gsub("y", "", setdiff(getYears(x1),getYears(x2))), "in", dotnames[2], "dataset. All data of this year removed. \n"  )
    if (length(setdiff(getYears(x2),getYears(x1))) > 0) vcat(1, "No data for year", gsub("y", "", setdiff(getYears(x2),getYears(x1))), "in", dotnames[1], "dataset. All data of this year removed. \n"  )
    x1 <- x1[,years,]
    x2 <- x2[,years,]


    # items that occur in both datasets
    items1 <- getNames(x1)
    items2 <- getNames(x2)
    inboth <- intersect(items1,items2)
    
    
    if (length(inboth)>0)  {
      vcat(2, "For the following items there were values in both datasets:", inboth, "Only values from", dotnames[1], "dataset were considered")
    
      x1_glo <- dimSums(x1[,,inboth], dim=1)
      x2_glo <- dimSums(x2[,,inboth], dim=1)
      
      # short check if global sums of the values are the same.
      for (item in inboth){
        avg <- mean((x1_glo[,,item]+10^-8)/(x2_glo[,,item]+10^-8))
        if (avg > 1.01 | avg < 0.99) cat(0, "For", item, "the values in the two datasets seem to differ. Manual check recommended.")
      }
    }
    
    # data2 without data already in data1 (CHECK!)
    x2_rm <- x2[,,items2[!items2 %in% items1]]
    
  }
    
  if (combine == "Element") {
  
    stop("This option is not available yet", "\n")

  }
  
    bothtogether <- mbind2(x1,x2_rm)
    
    return(bothtogether)
}
