#' Read IFA
#' 
#' Read-in IFA (International Fertilizer Association) data .xlsx file as
#' magclass object
#' 
#' 
#' @param subtype Type of IFA data that should be read. Available types are:
#' \itemize{ \item \code{consumption}: read in fertilizer_consumption.xlsx data
#' \item \code{production}: read in fertilizer_production.xlsx data }
#' @return magpie object of the IFA data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IFA",subtype="consumption")
#' }
#' 
readIFA <- function(subtype){
  files <- c(consumption="fertilizer_consumption.xlsx",
             production="fertilizer_production.xlsx")
  
  file <- toolSubtypeSelect(subtype,files)
  # read data         
  data <- as.data.frame(read_excel(file,skip=2))

  # remove lines in which the Product is NA (meaning that this is not a real data row)
  data <- data[!is.na(data$Product),]

  # convert country names into ISO 3166-1 alpha 3 country code
  data$Country <- toolCountry2isocode(data$Country)
  
  data <- as.magpie(data)
    
  return(data)
}



