calcIEA_WEO_2021 <- function(x) {
  
  mapping <- toolGetMapping("Mapping_IEA_WEO_2021.csv", type = "reportingVariables") %>%
    filter(!is.na(!!sym("REMIND")), !!sym("REMIND") != "") %>%
    mutate(WEO := paste0(!!sym("WEO"), " (", !!sym("Unit_WEO"), ")")) %>%
    select("variable" = "WEO", "REMIND", "Conversion", "unit" = "Unit_WEO", "Unit_REMIND")
  
  mapping$variable <- trimws(mapping$variable)
  
  data <- readSource("IEA_WEO_2021", convert = F)["World",,] %>%
    as.data.frame(data) %>%
    as_tibble() %>%
    select(
      "region" = "Region", "scenario" = "Data1", "variable" = "Data2",
      "year" = "Year", "value" = "Value"
    ) %>%
    mutate(region := "GLO")
  
  x <- left_join(
    data,
    mapping,
    by = "variable"
  ) %>%
    filter(!!sym("REMIND") != "") %>%
    mutate(
      !!sym("value") := ifelse(
        is.na(!!sym("value")), 0, !!sym("value") * !!sym("Conversion")
      ),
      !!sym("REMIND") := paste0(!!sym("REMIND"), " (", !!sym("Unit_REMIND"), ")")
    ) %>%
    select("scenario", "region", "year", "variable" = "REMIND", "value")
  
  x <- as.magpie(x)
  
  x <- add_columns(x, "Cap|Electricity|Biomass|w/o CCS (GW)", dim = 3.2)
  x[,, "Cap|Electricity|Biomass|w/o CCS (GW)"] <- x[,, "Cap|Electricity|Biomass (GW)"] - x[,, "Cap|Electricity|Biomass|w/ CCS (GW)"]
  
  x <- add_columns(x, "Cap|Electricity|Coal (GW)", dim = 3.2)
  x[,, "Cap|Electricity|Coal (GW)"] <- x[,, "Cap|Electricity|Coal|w/o CCS (GW)"] + x[,, "Cap|Electricity|Coal|w/ CCS (GW)"]
  
  x <- add_columns(x, "Cap|Electricity|Solar (GW)", dim = 3.2)
  x[,, "Cap|Electricity|Solar (GW)"] <- x[,, "Cap|Electricity|Solar|CSP (GW)"] + x[,, "Cap|Electricity|Solar|PV (GW)"]

  x <- add_columns(x, "Cap|Electricity|Fossil (GW)", dim = 3.2)
  x[,, "Cap|Electricity|Fossil (GW)"] <- x[,, "Cap|Electricity|Fossil|w/o CCS (GW)"] + x[,, "Cap|Electricity|Fossil|w/ CCS (GW)"]

  x <- add_columns(x, "Cap|Electricity|Gas (GW)", dim = 3.2)
  x[,, "Cap|Electricity|Gas (GW)"] <- x[,, "Cap|Electricity|Gas|w/o CCS (GW)"] + x[,, "Cap|Electricity|Gas|w/ CCS (GW)"]
  
  return(list(
    x = x, 
    unit = c("GW", "EJ/yr", "Mt CO2/yr"),
    description = "IEA WEO 2021 values as REMIND variables"
  ))

}