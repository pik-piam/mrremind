  #' Read IIASA subsidies and taxes
#' 
#' Read-in country level data on final energy taxes and subsidies as provided
#' from IIASA from .csv file as magclass object
#' 
#' 
#' @param subtype Type of country level data as compiled by IIASA that should
#' be read in. Available types are: \itemize{ \item \code{tax_rate}: tax rate
#' pre final energy category \item \code{sub_rate}: subsidy rate per final
#' energy category \item \code{energy}: final energy quantities per category }
#' @return magpie object of the IIASA_subs_taxes data
#' @author Christoph Bertram
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IIASA_subs_taxes","tax_rate")
#' }
#' 
readIIASA_subs_taxes<- function(subtype){
  file <- "unlinked_countries_2017_03.xlsx"
  #subtype = tax_rate, sub_rate, energy
  data <- read_excel(file, sheet=subtype)
  data <- data[!is.na(data[[1]]),]
    
  data$CountryName <- NULL# remove country
  x <- as.data.frame(sapply(data[!names(data)=="CountryCode"],as.numeric))
  x$CountryCode <- data$CountryCode
  x <- as.magpie(x)
  return(x)
}
