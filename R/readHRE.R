#' Read Heat Roadmap Europe data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @seealso [`readSource()`]
#'
#' @importFrom dplyr select
#' @importFrom magclass as.magpie
#' @importFrom tidyr separate
#' @importFrom readxl read_xlsx
#'
#' @export

readHRE <- function() {
  filename <- "HRE4-summary-tables-and-figures-web.xlsx"
  df <- read_excel(filename, sheet = "2. Consolidated Results", range = "A3:H9537")
  
  # only use data from these 4 scenarios
  df <- df[df$Scenario %in% c("BL 2015", "BL 2050", "CD 2050", "HRE 2050"), ]
  
  df <- separate(df, "Scenario", c("model", "year"), sep=" ")
  
  # combine category columns to one variable column
  df$variable <- paste(df$Category, df$SourceType, df$Source, sep = "|")
  df <- select(df, c("Country", "year", "model", "DataType", "variable", "Unit", "Value"))

  # convert to magpie object
  x <- as.magpie(df, spatial="Country", temporal="year", tidy = TRUE)
  
  # change country code to iso3
  getItems(x, dim=1) <- countrycode(getItems(x, dim=1), "eurostat", "iso3c")
  getSets(x)[1] <- "region"

  return(x)
}