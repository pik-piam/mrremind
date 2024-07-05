#' Calculate REMIND variables from IEA EV Outlook data
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @export

calcIEA_EVOutlook <- function() {
  x <- readSource("IEA_EVOutlook")

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

  # set 0s in other CHA countries than China to approximate CHA as China
  x[c("HKG", "MAC", "TWN"), , ] <- 0

  return(list(
    x = x,
    weight = NULL,
    unit = c("EJ/yr", "Million vehicles"),
    description = "IEA EV Outlook data in REMIND variables"
  ))
}
