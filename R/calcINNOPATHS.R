calcINNOPATHS <- function() {

  x <- readSource("INNOPATHS")

  # merge variable and unit to one dimension
  getNames(x, dim = 1) <- paste0(gsub("\\.", " (", getNames(x)), ")")
  x <- collapseDim(x, dim = 3.2)

  map <- toolGetMapping("Mapping_INNOPATHS.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND))

  for (var in intersect(getNames(x, dim = 1), unique(map$Variable))) {

    conv <- map[map$Variable == var, "factor"]

    # there should be a distinct conversion factor in the mapping
    # if there is more than one conversion factor, it means that one source variable
    # is converted two more than one target variable using a different conversion
    # this case is not covered by the logic
    if (length(unique(conv)) > 1) {
      stop(paste0("Cannot apply conversion factor for variable ", var))
    }

    x[, , var] <- x[, , var] * unique(conv)
  }

  x <- toolAggregate(x,
    dim = 3.1, rel = map, from = "Variable",
    to = "REMIND", partrel = TRUE, verbosity = 2
  )

  # convert currency units from EUR 2015 to $2017
  tmp <-  x[, , "EUR2015", pmatch = TRUE]
  x <- x[, , getNames(tmp), invert = TRUE]
  getNames(tmp) <- gsub("EUR2015", "US$2017", getNames(tmp))

  tmp <- GDPuc::toolConvertGDP(
    gdp = tmp,
    unit_in = "constant 2015 EUR",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  x <- mbind(x, tmp)

  weights <- x
  weights[, , ] <- NA
  weights[, , "US$2017", pmatch = TRUE] <- 1
  weights[, , "GDP|MER (billion US$2017/yr)"] <- NA

  return(list(
    x = x,
    weight = weights,
    mixed_aggregation = TRUE,
    unit = "Various",
    description = "INNOPATHS projections as REMIND variables"
  ))
}
