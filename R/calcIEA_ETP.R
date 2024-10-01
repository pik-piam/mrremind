#' Calculate REMIND emission variables from IEA ETP values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @export

calcIEA_ETP <- function() {

  .map <- function(x, mapping) {

    data <- as.data.frame(x) %>%
      as_tibble() %>%
      select(
        "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
        "year" = "Year", "value" = "Value"
      ) %>%
      filter(!is.na(.data$value))

    data <- full_join(data, mapping, by = "variable", relationship = "many-to-many") %>%
      filter(.data$REMIND != "")

    # composite REMIND variables that must be removed from the data,
    # because we do not have all the components in the IEA data
    remove <- data %>% filter(is.na(value))

    data <- data %>%
      filter(!is.na(.data$value),
             !(.data$variable %in% unique(remove$REMIND))) %>%
      mutate(
        "value" = .data$value * .data$Conversion,
        "REMIND" = paste0(.data$REMIND, " (", .data$Unit_REMIND, ")"),
        "model" = paste0("IEA ETP ", .data$scenario),
        "year" = as.numeric(as.character(.data$year))
      ) %>%
      select("region", "year", "model", "variable" = "REMIND", "value")

    x <- aggregate(value ~ region + year + model + variable, data, sum) %>%
      as.magpie()

    return(x)
  }

  mapping <- toolGetMapping("Mapping_IEA_ETP.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
    mutate(
      "Conversion" = as.numeric(.data$Conversion),
      "variable" = trimws(.data$IEA_ETP),
      "REMIND" = trimws(.data$REMIND)
      ) %>%
    select("variable", "REMIND", "Conversion", "Unit_REMIND")


  xReg <- mbind(
    readSource("IEA_ETP", subtype = "industry"),
    readSource("IEA_ETP", subtype = "transport"),
    readSource("IEA_ETP", subtype = "buildings"),
    readSource("IEA_ETP", subtype = "summary")
  )

  dataReg <- .map(xReg, mapping) %>%
    toolCountryFill(fill = NA, verbosity = 2)

  xGlo <- mbind(
    readSource("IEA_ETP", subtype = "industry", convert = FALSE)["WORLD", , ],
    readSource("IEA_ETP", subtype = "transport", convert = FALSE)["WORLD", , ],
    readSource("IEA_ETP", subtype = "buildings", convert = FALSE)["WORLD", , ],
    readSource("IEA_ETP", subtype = "summary", convert = FALSE)["WORLD", , ]
  )

  getItems(xGlo, dim = 1) <- "GLO"
  dataGlo <- .map(xGlo, mapping)

  # includes global values from the original source instead of calculating
  # them as the sum of all countries (as countries are incomplete)
  .customAggregate <- function(x, rel, to = NULL, glo) {
    x <- toolAggregate(x, rel = rel, to = to)

    if ("GLO" %in% getItems(x, dim = 1)) {
      out <- new.magpie(
        cells_and_regions = getItems(x, dim = 1),
        years = union(getYears(x), getYears(glo)),
        names = union(getNames(x), getNames(glo)),
        fill = NA,
        sets = names(dimnames(x))
      )

      x <- x["GLO", , , invert = TRUE]

      out[getItems(x, dim = 1), getYears(x), getNames(x)] <- x
      out["GLO", getYears(glo), getNames(glo)] <- glo

      return(out)
    } else {
      return(x)
    }
  }

  return(list(
    x = dataReg,
    weight = NULL,
    aggregationFunction = .customAggregate,
    aggregationArguments = list(glo = dataGlo),
    unit = c("EJ/yr", "Mt CO2/yr", "Mt/yr", "bn pkm/yr", "bn tkm/yr"),
    description = "IEA ETP projections as REMIND variables"
  ))
}
