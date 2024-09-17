#' Calculate REMIND variables from IEA World Energy Outlook data.
#'
#' @author Falk Benke
#' @export

calcIEA_WorldEnergyOutlook <- function() { # nolint

  dataGlo <- readSource("IEA_WorldEnergyOutlook", convert = FALSE)["World", , ]
  magclass::getItems(dataGlo, dim = 1) <- "GLO"
  dataReg <- readSource("IEA_WorldEnergyOutlook", convert = TRUE)

  .mapToRemind <- function(data) {

    # copy over Stated Policies Scenario for 2010 - 2022 to other scenarios
    for (s in magclass::getNames(data, dim = 1)) {
      data[, c("y2010", "y2015", "y2021", "y2022"), s] <-
        data[, c("y2010", "y2015", "y2021", "y2022"), "Stated Policies Scenario"][, , magclass::getNames(data[, , s], dim = 2)]
    }

    # rename scenarios
    scens <- c(
      "Stated Policies Scenario" = "SPS",
      "Announced pledges scenario" = "APS",
      "Announced Pledges Scenario" = "APS",
      "Net Zero Emissions by 2050 Scenario" = "Net2050"
    )

    magclass::getNames(data, dim = 1) <- paste0("IEA WEO 2023 ", scens[magclass::getNames(data, dim = 1)])
    magclass::getSets(data)[3] <- "model"

    map <- toolGetMapping("Mapping_IEA_WEO_complete.csv", type = "reportingVariables", where = "mrremind") %>%
      dplyr::filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
      dplyr::mutate("conversion" = as.numeric(.data$Conversion)) %>%
      dplyr::select("from" = "Variable", "to" = "REMIND", "conversion")

    for (var in intersect(getNames(data, dim = 2), unique(map$from))) {
      conv <- map[map$from == var, "conversion"]

      # if there is more than one conversion factor, it means that one source variable
      # is converted two more than one target variable using a different conversion factor
      if (length(unique(conv)) > 1) {

        # create unique "from" variables for each mapping entry by appending numbers
        map[map$from == var, "from"] <- paste0(map[map$from == var, "from"], " ", seq(1, nrow(map[map$from == var, ])))

        # duplicate "from" data with for each mapping entry
        for (i in seq(1, length(unique(conv)))) {
          dup <- data[, , var]
          getNames(dup, dim = 2) <- paste0(getNames(dup, dim = 2), " ", i)
          data <- mbind(data, dup)
        }
        data <- data[, , var, invert = TRUE]
      } else {
        data[, , var] <- data[, , var] * unique(conv)
      }
    }

    x <- toolAggregate(data,
      dim = 3.2, rel = map, from = "from",
      to = "to", partrel = TRUE, verbosity = 2
    )

    return(x)
  }

  dataGlo <- .mapToRemind(dataGlo)
  dataReg <- .mapToRemind(dataReg)

  .calcAdditionalVars <- function(x) {
    # correct PE|Nuclear and PE
    # PE Nuclear is usually reported in direct equivalents, values from IEA are
    # roughly 3 times higher than the REMIND ones
    x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] - x[, , "PE|Nuclear (EJ/yr)"]
    x[, , "PE|Nuclear (EJ/yr)"] <- x[, , "PE|Nuclear (EJ/yr)"] / 3
    x[, , "PE (EJ/yr)"] <- x[, , "PE (EJ/yr)"] + x[, , "PE|Nuclear (EJ/yr)"]

    return(x)
  }

  dataGlo <- .calcAdditionalVars(dataGlo)
  dataReg <- .calcAdditionalVars(dataReg)

  dataGlo <- magclass::add_columns(dataGlo, "Cap|Electricity|Biomass|w/o CC (GW)", dim = 3.2)
  dataGlo[, , "Cap|Electricity|Biomass|w/o CC (GW)"] <-
    dataGlo[, , "Cap|Electricity|Biomass (GW)"] - dataGlo[, , "Cap|Electricity|Biomass|w/ CC (GW)"]

  dataGlo <- magclass::add_columns(dataGlo, "Cap|Electricity|Coal (GW)", dim = 3.2)
  dataGlo[, , "Cap|Electricity|Coal (GW)"] <-
    dataGlo[, , "Cap|Electricity|Coal|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Coal|w/ CC (GW)"]

  dataGlo <- magclass::add_columns(dataGlo, "Cap|Electricity|Solar (GW)", dim = 3.2)
  dataGlo[, , "Cap|Electricity|Solar (GW)"] <-
    dataGlo[, , "Cap|Electricity|Solar|CSP (GW)"] + dataGlo[, , "Cap|Electricity|Solar|PV (GW)"]

  dataGlo <- magclass::add_columns(dataGlo, "Cap|Electricity|Fossil (GW)", dim = 3.2)
  dataGlo[, , "Cap|Electricity|Fossil (GW)"] <-
    dataGlo[, , "Cap|Electricity|Fossil|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Fossil|w/ CC (GW)"]

  dataGlo <- magclass::add_columns(dataGlo, "Cap|Electricity|Gas (GW)", dim = 3.2)
  dataGlo[, , "Cap|Electricity|Gas (GW)"] <-
    dataGlo[, , "Cap|Electricity|Gas|w/o CC (GW)"] + dataGlo[, , "Cap|Electricity|Gas|w/ CC (GW)"]

  dataGlo <- magclass::add_columns(dataGlo, "SE|Electricity|Solar (EJ/yr)", dim = 3.2)
  dataGlo[, , "SE|Electricity|Solar (EJ/yr)"] <-
    dataGlo[, , "SE|Electricity|Solar|PV (EJ/yr)"] + dataGlo[, , "SE|Electricity|Solar|CSP (EJ/yr)"]

  # includes values from the original source for global region instead of calculating
  # it as the sum of all countries (as countries are incomplete)
  .customAggregate <- function(x, rel, to = NULL, glo) {
    x <- toolAggregate(x, rel = rel, to = to)

    if ("GLO" %in% magclass::getItems(x, dim = 1)) {
      x <- x["GLO", , , invert = TRUE]

      out <- magclass::new.magpie(
        cells_and_regions = union(magclass::getItems(x, dim = 1), "GLO"),
        years = union(magclass::getYears(x), magclass::getYears(glo)),
        names = union(magclass::getNames(x), magclass::getNames(glo)),
        fill = NA,
        sets = names(dimnames(x))
      )

      out[magclass::getItems(x, dim = 1), magclass::getYears(x), magclass::getNames(x)] <- x
      out["GLO", magclass::getYears(glo), magclass::getNames(glo)] <- glo

      return(out)
    } else {
      return(x)
    }
  }

  return(list(
    x = dataReg,
    weight = NULL,
    unit = c("GW", "EJ/yr", "Mt CO2/yr"),
    aggregationFunction = .customAggregate,
    aggregationArguments = list(glo = dataGlo),
    description = "IEA World Energy Outlook 2023 values as REMIND variables"
  ))
}
