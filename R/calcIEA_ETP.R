#' Calculate REMIND emission variables from IEA ETP values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr select mutate left_join %>% if_any filter all_of
#' @importFrom madrat toolGetMapping
#' @importFrom utils read.csv2
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
    remove <- data %>% filter(is.na(.data$value))

    data <- data %>%
      filter(!is.na(.data$value),
             !(.data$REMIND %in% unique(remove$REMIND))) %>%
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

  # keep only countries that were reported individually or are part of given regions
  .removeRegions <- function(x, keepRegions) {
    map <- toolGetMapping("regionmappingIEA_ETP.csv",
                          where = "mrremind", type = "regional",
                          returnPathOnly = TRUE) %>%
      read.csv2(check.names = FALSE)
    keepCountries <- map %>%
      filter(if_any(all_of(c("individual", keepRegions)))) %>%
      getElement("CountryCode")
    x[keepCountries, , ]
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

  xReg <- .removeRegions(xReg, keepRegions = c("European Union", "ASEAN"))

  dataReg <- .map(xReg, mapping) %>%
    toolCountryFill(fill = NA, verbosity = 2)

  # set 0s in other CHA countries than China to approximate CHA as China
  dataReg[c("HKG", "MAC", "TWN"), , ] <- 0

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
