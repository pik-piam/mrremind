#' Read Krausmann
#' 
#' Read-in an Krausmann data .csv file as magclass object
#' 
#' 
#' @return magpie object of the Krausmann data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Krausmann")
#' }
#' 
readKrausmann <- function() {
      k <- read.csv("Krausmann.csv", sep=";", header=TRUE,row.names=1)
      regions <- c("S. & C. Asia"="SCA","E. Europe"="EER","N. Africa & W. Asia"="NAA","N. America & Oceania"="NAO","W. Europe" ="WER","Sub-Saharan Africa"="AFR","Latin America & Caribbean"="LAC","E. Asia"="EAS")
      row.names(k) <- regions
      k <- as.magpie(as.matrix(k))
      getYears(k) <- "y2000"
      return(k)
}  
