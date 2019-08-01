#' Read data of forest
#' 
#' Read in data of forest
#' 
#' 
#' @param subtype Type of FAO data that should be read. Available types are:
#' \itemize{ \item \code{protection}: forest within protected areas
#' (protection.csv) that has been downloaded from the CountrySTAT website. File
#' is aquired from: http://countrystat.org/home.aspx?c=FOR&tr=2 \item
#' \code{production}: forest designated for production (production.csv) that
#' has been downloaded from the CountrySTAT website. File is aquired from:
#' http://countrystat.org/home.aspx?c=FOR&tr=2 \item \code{extent}: forest
#' extent (forest_extent.csv) that has been downloaded from the CountrySTAT
#' website. File is aquired from: http://countrystat.org/home.aspx?c=FOR&tr=1 }
#' @return FAO data of forest as MAgPIE object
#' @author Nele Steinmetz
#' @seealso \code{\link{readSource}}
#' @importFrom magclass read.magpie
readFAO_forestry <- function(subtype)
{
  files <-  c(protection="protection.csv",
              production="production.csv",
              extent="forest_extent.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  data <- read.magpie(file,file_type = "cs3")
  return(data)
}




