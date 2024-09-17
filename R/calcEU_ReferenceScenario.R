calcEU_ReferenceScenario <- function() {
  EU_ReferenceScenario_2016 <- readSource("EU_ReferenceScenario", subtype = "2016")
  EU_ReferenceScenario_2016 <- add_dimension(EU_ReferenceScenario_2016, dim = 3.1, add = "model",
                                             nm = "EU_ReferenceScenario_2016")

  EU_ReferenceScenario_2020 <- readSource("EU_ReferenceScenario", subtype = "2020")
  EU_ReferenceScenario_2020 <- add_columns(EU_ReferenceScenario_2020, "y2000", dim = 2)
  EU_ReferenceScenario_2020 <- add_dimension(EU_ReferenceScenario_2020, dim = 3.1, add = "model",
                                             nm = "EU_ReferenceScenario_2020")

  x <- mbind(EU_ReferenceScenario_2016, EU_ReferenceScenario_2020)

  weights <- x
  weights[, , ] <- NA
  weights[, , "Price|Secondary Energy|Electricity (EUR2013/GJ)"] <- 1

  return(list(
    x = x, weight = weights, mixed_aggregation = TRUE,
    unit = "Various", description = "Historical Data"
  ))
}
