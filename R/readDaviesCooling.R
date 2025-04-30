#' Read Davies Cooling
#'
#' Read in Davies (2013) data on shares of cooling types per electricity
#' technology and GCAM region
#'
#'
#' @param subtype Type of Davies data that should be read. Available types are:
#' \itemize{ \item \code{dataBase}: The Davies source data for the base year
#' \item \code{dataFuture}: The Daves source data for the future }
#' @return MAgPIE object of the Davies (2013) data
#' @author Lavinia Baumstark, Ioanna Mouratiadou
#' @examples
#' \dontrun{
#' a <- readSource(type = "DaviesCooling")
#' }
#'
readDaviesCooling <- function(subtype) {
  sheets <- c(dataBase = 1, dataFuture = 2)
  sheet <- toolSubtypeSelect(subtype, sheets)

  cool <- as.data.frame(read_excel("DaviesCooling.xlsx", sheet = sheet))
  cool <- cool[!is.na(cool$`Davies Source`), ]

  row.names(cool) <- paste(cool$"Davies Source", cool$"Davies Cooling", sep = ".")
  cool$"Davies Source" <- NULL
  cool$"Davies Cooling" <- NULL

  regions <- c("USA" = "USA", "Canada" = "CAN", "Western Europe" = "WEU",
               "Japan" = "JPN", "Australia & NZ" = "ANZ", "Former Soviet Union" = "FSU",
               "China" = "CHN", "Middle East" = "MEA", "Africa" = "AFR",
               "Latin America" = "LAM", "Southeast Asia" = "SEA",
               "Eastern Europe" = "EEU", "Korea" = "KOR", "India" = "IND")
  names(cool) <- regions # three-letter code"

  cool <- as.magpie(cool)
  getYears(cool) <- NULL

  return(cool)
}
