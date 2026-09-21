#' Calculate REMIND variables from IEA Global EV Outlook data
#'
#' @author Falk Benke
#'
calcIEA_EVOutlook <- function() {
  x <- readSource("IEA_EVOutlook", convert = FALSE)[c("World", "Europe", "EU27"), , ]
  getItems(x, dim = 1) <- c("GLO", "EUR", "EU27")

  x <- mbind(x, readSource("IEA_EVOutlook", convert = TRUE))

  # merge variable and unit to one dimension
  getNames(x, dim = 2) <- unlist(
    lapply(
      strsplit(getNames(x), "\\."),
      function(y) {
        paste0(y[2], " (", y[3], ")")
      }
    )
  )
  x <- collapseDim(x, dim = 3.3)

  # rename scenario
  getNames(x, dim = 1) <- paste0("IEA GEVO ", gsub("Projection-", "", getNames(x, dim = 1)))

  map <- toolGetMapping("Mapping_IEA_EV_Outlook.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND_Variable), .data$REMIND_Variable != "") %>%
    mutate(
      "REMIND" = paste0(.data$REMIND_Variable, " (", .data$REMIND_Unit, ")"),
      "Variable" = paste0(.data$Variable, " (", .data$Unit, ")")
    ) %>%
    select("Variable", "REMIND", "Factor")

  for (var in intersect(getNames(x, dim = 2), unique(map$Variable))) {
    conv <- map[map$Variable == var, "Factor"]

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
                     dim = 3.2, rel = map, from = "Variable",
                     to = "REMIND", partrel = TRUE, verbosity = 2
  )

  getSets(x)[3.1] <- "model"

  dataReg <- x[c("GLO", "EUR", "EU27"), , ]
  x <- x[c("GLO", "EUR", "EU27"), , , invert = TRUE]

  return(list(
    x = x,
    weight = NULL,
    unit = c("EJ/yr", "Million vehicles"),
    aggregationFunction = toolAggregateCustomRegs,
    aggregationArguments = list(agg = dataReg, regs = c("GLO", "EUR", "EU27")),
    description = "IEA EV Outlook data in REMIND variables"
  ))
}
