#' Read in data on  food losses and waste from FAO for several commodity groups
#' 
#' Data from Annex 4 of the following FAO study:
#' FAO. 2011. Global food losses and food waste – Extent, causes and prevention. Rome 
#' (http://www.fao.org/3/a-i2697e.pdf)
#' 
#' @param subtype Steps of the food supply chain where food losses and waste occur. Available types are:
#' \itemize{ 
#' \item \code{Consumption}: consumption level
#' }
#' @return magpie object of food waste percentages for several commodity groups 
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="FAOLossesWaste",subtype="Consumption")
#' }
#' 
readFAOLossesWaste <- function(subtype) {
  
  if (subtype == "Consumption") {
      w <- read.csv("FAOLossesWaste_cons.csv", sep=";", header=TRUE)
      
      regions <- c("Food.items"="items",                                  
                   "Europe"="EUR",                                        
                   "USA...Canada...Oceania"="NAO",                        
                   "Industrialized.Asia"="IAS", 
                   "Sub.Saharan.Africa"="AFR",                            
                   "North.Africa...West.and.Central.Asia"="NAA",           
                   "South.and.Southeast.Asia"="SEA", 
                   "Latin.America"="LAC"                                 
                   )
      
      dimnames(w)[[2]] <- regions
      w <- as.magpie(w,spatial=2,temporal=NULL)
      getYears(w) <- "y2010"
 
  }else {
    stop("Not a valid subtype!")
  }            
      
      return(w)
}  
