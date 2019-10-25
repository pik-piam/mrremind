#' @title readSSPResults
#' @description Reads in a reporting mif file from the SSP scenario results
#'
#' 
#' @return MAgPIE object with regional aggregation of SSP regions, including all indicators
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("SSPresults",aggregate=FALSE)
#' }
#' @importFrom magclass read.report

readSSPResults <- function() {

  file <- "SSP_IAM_World_5Regs_2016-05-25.csv"
  
  data <- read.report(file, as.list = F)
  
  return(data)
}

