#' read-in power technology parameters for Australia from CEMO model database
#' Australian contact: Dylan McConnell, dylan.mcconnell(at)unimelb.edu.au
#' @return magpie object of the cemo database data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}




readCEMOdb <- function() {
  
  cemo.db.input <- read.csv("CEMOdb_data.csv") 
  cemo.db.input$country <- "AUS"
  cemo.db.input <- cemo.db.input[,c(9,2,3,4,6,8)]
  
  x = as.magpie(cemo.db.input, spatial=1, temporal=2, datacol=6)
  
  return(x)
}  
