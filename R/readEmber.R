#' Read Ember Yearly Electricity Data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @source https://ember-climate.org/data-catalogue/yearly-electricity-data/
#'
#' @importFrom dplyr select
#'
readEmber <- function() {
  filename <- "yearly_full_release_long_format.csv"
  df <- read.csv(file.path("2025", filename))

  # filter out aggregated regions by only choosing rows that don't have a blank country.code
  df <- df[df$ISO.3.code != "", ]

  # combine category columns to one variable column and rename according to madrat standard
  df$variable <- paste(df$Category, df$Subcategory, df$Variable, sep = "|")
  df <- select(df, c(region = "ISO.3.code",
                     year = "Year",
                     variable = "variable",
                     unit = "Unit",
                     value = "Value"))

  # convert to magpie object
  x <- as.magpie(df, tidy = TRUE)

  return(x)
}
