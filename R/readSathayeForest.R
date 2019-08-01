#' Read Sathaye Forest
#' 
#' Read-in an Sathaye Forest data .csv file as magclass object
#' 
#' 
#' @return magpie object of the Sathaye Forest data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("SathayeForest")
#' }
#' 
readSathayeForest <- function() {
    
      forest <- read.csv("SathayeForest.csv", sep=";", header=TRUE, row.names=2)
      forest <- forest[,-1]
      forest <- as.magpie(forest)
      getYears(forest) <- "y2001"
      
      return(forest)
}  
