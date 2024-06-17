calcINNOPATHS <- function() {

  x <- readSource("INNOPATHS")

  # merge variable and unit to one dimension
  getNames(x, dim = 1) <- paste0(gsub("\\.", " (", getNames(x)), ")")
  x <- collapseDim(x, dim = 3.2)

  map <- toolGetMapping("Mapping_INNOPATHS.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND))

  for (var in intersect(getNames(x, dim = 1), unique(map$Variable))) {
    x[, , var] <- x[, , var] * map[map$Variable == var, "factor"]
  }

  x <- toolAggregate(x,
    dim = 3.1, rel = map, from = "Variable",
    to = "REMIND", partrel = TRUE, verbosity = 2
  )

  weights <- x
  weights[, , ] <- NA
  weights[, , "US$2005", pmatch = TRUE] <- 1
  weights[, , "GDP|MER (billion US$2005/yr)"] <- NA

  return(list(
    x = x,
    weight = weights,
    mixed_aggregation = TRUE,
    unit = "Various",
    description = "INNOPATHS projections as REMIND variables"
  ))
}
