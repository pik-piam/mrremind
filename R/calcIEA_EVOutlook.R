#' Calculate REMIND variables from IEA EV Outlook data
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @importFrom dplyr select mutate left_join
#' @importFrom madrat toolGetMapping toolCountryFill
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel
#' @importFrom rlang sym
#' @importFrom stats aggregate
#' @export

calcIEA_EVOutlook <- function() {
  data <- readSource("IEA_EVOutlook")

  data <- as.data.frame(data) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "unit" = "Data3", "year" = "Year", "value" = "Value"
    ) %>%
    mutate(!!sym("model") := case_when(
      !!sym("scenario") %in% c("Historical_MoMo", "Historical") ~ "IEA EV Historical",
      !!sym("scenario") == "Projection-SDS" ~ "IEA EV Outlook SDS",
      !!sym("scenario") == "Projection-STEPS" ~ "IEA EV Outlook STEPS"
    ))

  mapping <- toolGetMapping("Mapping_IEA_EV_Outlook.csv", type = "reportingVariables", where = "mappingfolder") %>%
    filter(!is.na(!!sym("REMIND_Variable")), !!sym("REMIND_Variable") != "") %>%
    mutate(!!sym("REMIND") := paste0(!!sym("REMIND_Variable"), " (", !!sym("REMIND_Unit"), ")")) %>%
    select("variable" = "Variable", "REMIND", "Factor")

  mapping$variable <- trimws(mapping$variable)
  mapping$REMIND <- trimws(mapping$REMIND)

  x <- left_join(
    data,
    mapping,
    by = "variable"
  ) %>%
    filter(!!sym("REMIND") != "") %>%
    mutate(!!sym("value") := !!sym("value") * !!sym("Factor")) %>%
    select("region", "year", "model", "variable" = "REMIND", "value")

  x <- aggregate(value ~ region + year + model + variable, x, sum, na.action = na.exclude) %>%
    as.magpie() %>%
    toolCountryFill(fill = NA, verbosity = 2)

  # set 0s in other CHA countries than China to approximate CHA as China
  x[c("HKG", "MAC", "TWN"),,] <- 0

  return(list(
    x = x,
    weight = NULL,
    unit = c("EJ/yr", "Million vehicles"),
    description = "IEA EV Outlook data in REMIND variables"
  ))
}
