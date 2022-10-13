#' Read Ember Yearly Electricity Data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @seealso [`readSource()`]
#' @source https://ember-climate.org/data-catalogue/yearly-electricity-data/
#'
#' @importFrom dplyr select
#' @importFrom magclass as.magpie
#'
#' @export

readEmber <- function() {
  filename <- "yearly_full_release_long_format.csv"
  df <- read.csv(filename)

  # filter out aggregated regions by only choosing rows that don't have a blank country.code
  df <- df[df$Country.code != "", ]

  # combine category columns to one variable column
  df$variable <- paste(df$Category, df$Subcategory, df$Variable, sep = "|")
  df <- select(df, c("Country.code", "Year", "variable", "Unit", "Value"))

  # convert to magpie object
  x <- as.magpie(df, spatial="Country.code", temporal="Year", tidy = TRUE)
  getSets(x) <- c("region", "year", "variable", "unit")

  return(x)
}
