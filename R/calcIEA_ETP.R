#' Calculate REMIND emission variables from IEA ETP values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom rlang sym
#' @importFrom stats aggregate na.pass
#' @export

calcIEA_ETP <- function() {
  mapping <- toolGetMapping("Mapping_IEA_ETP.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
    mutate("Conversion" = as.numeric(.data$Conversion)) %>%
    select("variable" = "IEA_ETP", "REMIND", "Conversion", "Unit_REMIND")

  mapping$variable <- trimws(mapping$variable)
  mapping$REMIND <- trimws(mapping$REMIND)

  x1 <- readSource("IEA_ETP", subtype = "industry")
  x2 <- readSource("IEA_ETP", subtype = "transport")
  x3 <- readSource("IEA_ETP", subtype = "buildings")
  x4 <- readSource("IEA_ETP", subtype = "summary")

  x5 <- mbind(
    readSource("IEA_ETP", subtype = "industry", convert = F)["WORLD", , ],
    readSource("IEA_ETP", subtype = "transport", convert = F)["WORLD", , ],
    readSource("IEA_ETP", subtype = "buildings", convert = F)["WORLD", , ],
    readSource("IEA_ETP", subtype = "summary", convert = F)["WORLD", , ]
  )


  data <- as.data.frame(mbind(x1, x2, x3, x4)) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "year" = "Year", "value" = "Value"
    ) %>%
    filter(!is.na(.data$value))

  dataGlo <- as.data.frame(x5) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "year" = "Year", "value" = "Value"
    ) %>%
    filter(!is.na(.data$value)) %>%
    mutate(region = "GLO")



  x <- left_join(
    rbind(data, dataGlo),
    mapping,
    by = "variable",
    relationship = "many-to-many"
  ) %>%
    filter(.data$REMIND != "") %>%
    mutate(
      "value" = .data$value * .data$Conversion,
      "REMIND" = paste0(.data$REMIND, " (", .data$Unit_REMIND, ")"),
      "model" = paste0("IEA ETP ", .data$scenario),
      "year" = as.numeric(as.character(.data$year))
    ) %>%
    select("region", "year", "model", "variable" = "REMIND", "value")

  x <- aggregate(value ~ region + year + model + variable, x, sum) %>%
    as.magpie()

  dataGlo <- x["GLO", , ]
  x <- x["GLO", , invert = T] %>%
    toolCountryFill(fill = NA, verbosity = 2)


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
    x = x,
    weight = NULL,
    aggregationFunction = .customAggregate,
    aggregationArguments = list(glo = dataGlo),
    unit = c("EJ/yr", "Mt CO2/yr", "Mt/yr", "bn pkm/yr", "bn tkm/yr"),
    description = "IEA ETP projections as REMIND variables"
  ))
}
