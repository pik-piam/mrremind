#' Read ExpertGuess
#'
#' Read-in data that are based on expert guess
#'
#' @md
#' @param subtype Type of data that should be read.  One of
#'   - `Chinese_Steel_Production`: "Smooth" production estimates by Robert
#'     Pietzcker (2022).
#'   - Any of the others nobody cared to document.
#' @return magpie object of the data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#'
#' \dontrun{ a <- readSource(type="ExpertGuess",subtype="ies")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom quitte madrat_mule
#' @importFrom readr read_csv

readExpertGuess<-function(subtype){


  if (subtype == "ies"){
    a <- read.csv("ies.csv",sep = ";")
  } else if (subtype == "prtp"){
    a <- read.csv("prtp.csv",sep = ";")
  } else if (subtype == "CCSbounds"){
    a <- read.csv("CCSbounds.csv",sep = ";")
  } else if (subtype == "co2prices"){
    a <- read.csv("co2prices.csv",sep = ";")
  } else if (subtype == "costsTradePeFinancial"){
    a <- read.csv("pm_costsTradePeFinancial.csv",sep = ";",skip=2)
  }

  if (subtype == "ies" | subtype == "prtp" | subtype == "CCSbounds" | subtype == "co2prices") {
    a$RegionCode <- NULL
    a$Country    <- NULL
    out <- as.magpie(a)
  } else if (subtype == "costsTradePeFinancial"){
    out <- as.magpie(a,spatial=1,temporal=0,datacol=3)
    out <- collapseNames(out)
#    getSets(out) <- c("region","year","type","pe")
  }

  if (subtype == "ies" | subtype == "prtp"){
    getYears(out) <- "2005"
  }

  if ('Chinese_Steel_Production' == subtype) {
    out <- read_csv(file = 'Chinese_Steel_Production.csv',
                    comment = '#',
                    show_col_types = FALSE) %>%
      madrat_mule()
  } else if ('industry_specific_FE_limits' == subtype) {
    out <- read_csv(file = 'industry_specific_FE_limits.csv',
                    comment = '#',
                    show_col_types = FALSE) %>%
      madrat_mule()
  }

  return(out)
}
