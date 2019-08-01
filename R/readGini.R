#' Read Gini
#' 
#' Read Gini coefficients for SSP scenarios from Rao et al., Futures, 2018. Data has been provided by the authors, but will be made publicly available as well.
#' This contains data for 184 countries and from 2011 onwards.
#' 
#' Copied from the documentation provided by the authors:
#' This sheet contains the original Gini projections for 43 countries from the underlying empirical model (See reference to RSP 2016 in the main paper)	
#' and the extrapolations to all countries using the methodology described in the article.	The country codes are the World Bank codes.
#' 
#' @return magpie object of the Gini data
#' @author Bjoern Soergel
#' @seealso \code{\link{readSource}} \code{\link{convertGini}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="Gini")
#' }
#' 
#' @importFrom readxl read_excel

readGini <- function(){
  x <- read_excel("Gini_projections_SSPs.xlsx", sheet = 2)
  return(as.magpie(x))
}