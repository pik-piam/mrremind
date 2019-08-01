#' Read Bodirsky
#' 
#' Read-in an Bodirsky data .csv file as magclass object
#' 
#' 
#' @return magpie object of the Bodirsky data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Bodirsky")
#' }
#' 
readBodirsky <- function() {
      file <- "f_res_use_min_shr.csv"
    
      bod <- read.csv(file, sep=";", header=T, row.names=1)
      bod <- as.magpie(bod)
      getYears(bod) <- "y2000"
      
      return(bod)
}  
