#' Read IEA ETP projections
#'
#' @author Falk Benke
#' @param subtype data subtype. Either "industry", "buildings", "summary", or "transport"
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows bind_cols select mutate
#' @importFrom readxl read_xlsx
#' @importFrom tidyr drop_na pivot_longer extract
#' @importFrom rlang sym
#' @importFrom magclass as.magpie

readIEA_ETP <- function(subtype) {
  
  region <- NULL
  
  subtypes <- list(
    industry = {
      list(
        file = "ETP2017_industry_summary.xlsx",
        prefix = "Industry",
        sheets = list(
          "OECD", "Non-OECD",
          "ASEAN", "Brazil", "China", "European Union",
          "India", "Mexico", "Russia", "South Africa", "United States"
        ),
        scenarios = list(
          "RTS" = "B4:K110",
          "2DS" = "O4:X110",
          "B2DS" = "AB4:AK110"
        ),
        rows = tibble(
          name = {
            c(
              NA,
              "Total industry final energy consumption|Coal (PJ)",
              "Total industry final energy consumption|Oil (PJ)",
              "Total industry final energy consumption|Natural gas (PJ)",
              "Total industry final energy consumption|Electricity (PJ)",
              "Total industry final energy consumption|Heat (PJ)",
              "Total industry final energy consumption|Biomass (PJ)",
              "Total industry final energy consumption|Waste (PJ)",
              "Total industry final energy consumption|Other renewables (PJ)",
              "Total industry final energy consumption|Total (PJ)",
              NA,
              NA,
              "CO2 emissions|Total CO2 emissions (MtCO2)",
              "CO2 emissions|CO2 captured (MtCO2)",
              NA,
              NA,
              "Cement - final energy consumption|Coal (PJ)",
              "Cement - final energy consumption|Oil (PJ)",
              "Cement - final energy consumption|Natural gas (PJ)",
              "Cement - final energy consumption|Electricity (PJ)",
              "Cement - final energy consumption|Heat (PJ)",
              "Cement - final energy consumption|Biomass (PJ)",
              "Cement - final energy consumption|Waste (PJ)",
              "Cement - final energy consumption|Other renewables (PJ)",
              "Cement - final energy consumption|Total (PJ)",
              NA,
              NA,
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Coal (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Oil (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Natural gas (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Electricity (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Heat (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Biomass (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Waste (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Other renewables (PJ)",
              "Chemicals and petrochemicals - final energy consumption and chemical feedstock|Total (PJ)",
              NA,
              NA,
              "Chemicals and petrochemicals - feedstocks|Coal (PJ)",
              "Chemicals and petrochemicals - feedstocks|Oil (PJ)",
              "Chemicals and petrochemicals - feedstocks|Natural gas (PJ)",
              "Chemicals and petrochemicals - feedstocks|Electricity (PJ)",
              "Chemicals and petrochemicals - feedstocks|Heat (PJ)",
              "Chemicals and petrochemicals - feedstocks|Biomass (PJ)",
              "Chemicals and petrochemicals - feedstocks|Waste (PJ)",
              "Chemicals and petrochemicals - feedstocks|Other renewables (PJ)",
              "Chemicals and petrochemicals - feedstocks|Total (PJ)",
              NA,
              NA,
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Coal (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Oil (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Natural gas (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Electricity (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Heat (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Biomass (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Waste (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Other renewables (PJ)",
              "Iron and steel - final energy consumption incl. blast furnaces and coke ovens|Total (PJ)",
              NA,
              NA,
              "Iron and steel - blast furnaces and coke ovens|Coal (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Oil (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Natural gas (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Electricity (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Heat (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Biomass (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Waste (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Other renewables (PJ)",
              "Iron and steel - blast furnaces and coke ovens|Total (PJ)",
              NA,
              NA,
              "Pulp and paper - final energy consumption|Coal (PJ)",
              "Pulp and paper - final energy consumption|Oil (PJ)",
              "Pulp and paper - final energy consumption|Natural gas (PJ)",
              "Pulp and paper - final energy consumption|Electricity (PJ)",
              "Pulp and paper - final energy consumption|Heat (PJ)",
              "Pulp and paper - final energy consumption|Biomass (PJ)",
              "Pulp and paper - final energy consumption|Waste (PJ)",
              "Pulp and paper - final energy consumption|Other renewables (PJ)",
              "Pulp and paper - final energy consumption|Total (PJ)",
              NA,
              NA,
              "Aluminium - final energy consumption|Coal (PJ)",
              "Aluminium - final energy consumption|Oil (PJ)",
              "Aluminium - final energy consumption|Natural gas (PJ)",
              "Aluminium - final energy consumption|Electricity (PJ)",
              "Aluminium - final energy consumption|Heat (PJ)",
              "Aluminium - final energy consumption|Biomass (PJ)",
              "Aluminium - final energy consumption|Waste (PJ)",
              "Aluminium - final energy consumption|Other renewables (PJ)",
              "Aluminium - final energy consumption|Total (PJ)",
              NA,
              NA,
              "Sub-sector CO2 emissions|Cement (MtCO2)",
              "Sub-sector CO2 emissions|Chemicals and petrochemicals (MtCO2)",
              "Sub-sector CO2 emissions|Iron and steel (MtCO2)",
              "Sub-sector CO2 emissions|Pulp and paper (MtCO2)",
              "Sub-sector CO2 emissions|Aluminium (MtCO2)",
              NA,
              NA,
              "Materials production|Cement (Mt)",
              "Materials production|High value chemicals (Mt)",
              "Materials production|Ammonia (Mt)",
              "Materials production|Methanol (Mt)",
              "Materials production|Crude steel (Mt)",
              "Materials production|Paper and paperboard (excl. recovered paper) (Mt)",
              "Materials production|Total aluminium (primary and secondary) (Mt)"
            )
          }
        ) %>%
          extract("name", c("variable", "unit"), "^(.*) \\((.*)\\)$")
      )
    },
    buildings = {
      list(
        file = "ETP2017_buildings_summary.xlsx",
        prefix = "Buildings",
        sheets = list(
          "OECD", "NonOECD",
          "ASEAN", "Brazil", "China", "European Union",
          "India", "Mexico", "Russia", "South Africa", "United States"
        ),
        scenarios = list(
          "RTS" = "B4:K109",
          "2DS" = "O4:X109",
          "B2DS" = "AB4:AK109"
        ),
        rows = tibble(
          name = {
            c(
              NA,
              "Buildings - Total final energy consumption|Coal (PJ)",
              "Buildings - Total final energy consumption|Oil products (PJ)",
              "Buildings - Total final energy consumption|Natural gas (PJ)",
              "Buildings - Total final energy consumption|Electricity (PJ)",
              "Buildings - Total final energy consumption|Biomass, waste and other renewables (PJ)",
              "Buildings - Total final energy consumption|Commercial heat (PJ)",
              "Buildings - Total final energy consumption|Total (PJ)",
              NA,
              NA,
              "Buildings - Total final energy consumption by end-use|Space heating (PJ)",
              "Buildings - Total final energy consumption by end-use|Water heating (PJ)",
              "Buildings - Total final energy consumption by end-use|Space cooling (PJ)",
              "Buildings - Total final energy consumption by end-use|Lighting (PJ)",
              "Buildings - Total final energy consumption by end-use|Appliances and miscellaneous equipments (PJ)",
              "Buildings - Total final energy consumption by end-use|Cooking (PJ)",
              "Buildings - Total final energy consumption by end-use|Total (PJ)",
              NA,
              NA,
              "Buildings - Total emissions by fuel|Coal (MtCO2)",
              "Buildings - Total emissions by fuel|Oil products (MtCO2)",
              "Buildings - Total emissions by fuel|Natural gas (MtCO2)",
              "Buildings - Total emissions by fuel|Electricity (MtCO2)",
              "Buildings - Total emissions by fuel|Biomass, waste and other renewables (MtCO2)",
              "Buildings - Total emissions by fuel|Commercial heat (MtCO2)",
              "Buildings - Total emissions by fuel|Total (MtCO2)",
              NA,
              NA,
              "Buildings - Total emissions by end-use|Space heating (MtCO2)",
              "Buildings - Total emissions by end-use|Water heating (MtCO2)",
              "Buildings - Total emissions by end-use|Space cooling (MtCO2)",
              "Buildings - Total emissions by end-use|Lighting (MtCO2)",
              "Buildings - Total emissions by end-use|Appliances and miscellaneous equipments (MtCO2)",
              "Buildings - Total emissions by end-use|Cooking (MtCO2)",
              "Buildings - Total emissions by end-use|Total (MtCO2)",
              NA,
              NA,
              "Residential - Total final energy consumption|Coal (PJ)",
              "Residential - Total final energy consumption|Oil products (PJ)",
              "Residential - Total final energy consumption|Natural gas (PJ)",
              "Residential - Total final energy consumption|Electricity (PJ)",
              "Residential - Total final energy consumption|Biomass, waste and other renewables (PJ)",
              "Residential - Total final energy consumption|Commercial heat (PJ)",
              "Residential - Total final energy consumption|Total (PJ)",
              NA,
              NA,
              "Residential - Total final energy consumption by end-use|Space heating (PJ)",
              "Residential - Total final energy consumption by end-use|Water heating (PJ)",
              "Residential - Total final energy consumption by end-use|Space cooling (PJ)",
              "Residential - Total final energy consumption by end-use|Lighting (PJ)",
              "Residential - Total final energy consumption by end-use|Appliances and miscellaneous equipments (PJ)",
              "Residential - Total final energy consumption by end-use|Cooking (PJ)",
              "Residential - Total final energy consumption by end-use|Total (PJ)",
              NA,
              NA,
              "Residential - Total emissions by fuel|Coal (MtCO2)",
              "Residential - Total emissions by fuel|Oil products (MtCO2)",
              "Residential - Total emissions by fuel|Natural gas (MtCO2)",
              "Residential - Total emissions by fuel|Electricity (MtCO2)",
              "Residential - Total emissions by fuel|Biomass, waste and other renewables (MtCO2)",
              "Residential - Total emissions by fuel|Commercial heat (MtCO2)",
              "Residential - Total emissions by fuel|Total (MtCO2)",
              NA,
              NA,
              "Residential - Total emissions by end-use|Space heating (MtCO2)",
              "Residential - Total emissions by end-use|Water heating (MtCO2)",
              "Residential - Total emissions by end-use|Space cooling (MtCO2)",
              "Residential - Total emissions by end-use|Lighting (MtCO2)",
              "Residential - Total emissions by end-use|Appliances and miscellaneous equipments (MtCO2)",
              "Residential - Total emissions by end-use|Cooking (MtCO2)",
              "Residential - Total emissions by end-use|Total (MtCO2)",
              NA,
              NA,
              "Services - Total final energy consumption|Coal (PJ)",
              "Services - Total final energy consumption|Oil products (PJ)",
              "Services - Total final energy consumption|Natural gas (PJ)",
              "Services - Total final energy consumption|Electricity (PJ)",
              "Services - Total final energy consumption|Biomass, waste and other renewables (PJ)",
              "Services - Total final energy consumption|Commercial heat (PJ)",
              "Services - Total final energy consumption|Total (PJ)",
              NA,
              NA,
              "Services - Total final energy consumption by end-use|Space heating (PJ)",
              "Services - Total final energy consumption by end-use|Water heating (PJ)",
              "Services - Total final energy consumption by end-use|Space cooling (PJ)",
              "Services - Total final energy consumption by end-use|Lighting (PJ)",
              "Services - Total final energy consumption by end-use|Appliances and miscellaneous equipments (PJ)",
              "Services - Total final energy consumption by end-use|Cooking (PJ)",
              "Services - Total final energy consumption by end-use|Total (PJ)",
              NA,
              NA,
              "Services - Total final energy consumption by end-use|Coal (MtCO2)",
              "Services - Total final energy consumption by end-use|Oil products (MtCO2)",
              "Services - Total final energy consumption by end-use|Natural gas (MtCO2)",
              "Services - Total final energy consumption by end-use|Electricity (MtCO2)",
              "Services - Total final energy consumption by end-use|Biomass, waste and other renewables (MtCO2)",
              "Services - Total final energy consumption by end-use|Commercial heat (MtCO2)",
              "Services - Total final energy consumption by end-use|Total (MtCO2)",
              NA,
              NA,
              "Services - Total emissions by end-use|Space heating (MtCO2)",
              "Services - Total emissions by end-use|Water heating (MtCO2)",
              "Services - Total emissions by end-use|Space cooling (MtCO2)",
              "Services - Total emissions by end-use|Lighting (MtCO2)",
              "Services - Total emissions by end-use|Appliances and miscellaneous equipments (MtCO2)",
              "Services - Total emissions by end-use|Total (MtCO2)"
            )
          }
        ) %>%
          extract("name", c("variable", "unit"), "^(.*) \\((.*)\\)$")
      )
    },
    summary = {
      list(
        file = "ETP2017_scenario_summary.xlsx",
        prefix = "Summary",
        sheets = list(
          "OECD", "NonOECD",
          "ASEAN", "Brazil", "China", "European Union",
          "India", "Mexico", "Russia", "South Africa", "United States"
        ),
        scenarios = list(
          "RTS" = "B4:K154",
          "2DS" = "O4:X154",
          "B2DS" = "AB4:AK154"
        ),
        rows = tibble(
          name = {
            c(
              NA,
              "Total primary energy demand|Oil (PJ)",
              "Total primary energy demand|Coal (PJ)",
              "Total primary energy demand|Natural gas (PJ)",
              "Total primary energy demand|Nuclear (PJ)",
              "Total primary energy demand|Biomass and waste (PJ)",
              "Total primary energy demand|Hydro (PJ)",
              "Total primary energy demand|Other (PJ)",
              "Total primary energy demand|Total (PJ)",
              "Total primary energy demand|Memo: Biofuels and waste (excl. conversion losses of liquid and gaseous biofuels production) (PJ)",
              NA,
              NA,
              "Fuel input electricity and heat generation|Oil (PJ)",
              "Fuel input electricity and heat generation|Coal (PJ)",
              "Fuel input electricity and heat generation|Natural gas (PJ)",
              "Fuel input electricity and heat generation|Nuclear (PJ)",
              "Fuel input electricity and heat generation|Biomass and waste (PJ)",
              "Fuel input electricity and heat generation|Hydro (PJ)",
              "Fuel input electricity and heat generation|Geothermal (PJ)",
              "Fuel input electricity and heat generation|Wind (PJ)",
              "Fuel input electricity and heat generation|Solar PV (PJ)",
              "Fuel input electricity and heat generation|Solar CSP (PJ)",
              "Fuel input electricity and heat generation|Ocean (PJ)",
              "Fuel input electricity and heat generation|Hydrogen (PJ)",
              "Fuel input electricity and heat generation|Other (PJ)",
              "Fuel input electricity and heat generation|Total (PJ)",
              NA,
              NA,
              "Final energy demand|Oil (PJ)",
              "Final energy demand|Coal (PJ)",
              "Final energy demand|Natural gas (PJ)",
              "Final energy demand|Electricity (PJ)",
              "Final energy demand|Heat (PJ)",
              "Final energy demand|Biomass and waste (PJ)",
              "Final energy demand|Hydrogen (PJ)",
              "Final energy demand|Other (PJ)",
              "Final energy demand|Total (PJ)",
              NA,
              NA,
              "Final energy demand industry sector|Oil (PJ)",
              "Final energy demand industry sector|Coal (PJ)",
              "Final energy demand industry sector|Natural gas (PJ)",
              "Final energy demand industry sector|Electricity (PJ)",
              "Final energy demand industry sector|Heat (PJ)",
              "Final energy demand industry sector|Biomass and waste (PJ)",
              "Final energy demand industry sector|Hydrogen (PJ)",
              "Final energy demand industry sector|Other (PJ)",
              "Final energy demand industry sector|Total (PJ)",
              NA,
              NA,
              "Final energy demand non-energy use|Total (PJ)",
              NA,
              NA,
              "Final energy demand transport sector|Oil (PJ)",
              "Final energy demand transport sector|Coal (PJ)",
              "Final energy demand transport sector|Natural gas (PJ)",
              "Final energy demand transport sector|Electricity (PJ)",
              "Final energy demand transport sector|Biomass (PJ)",
              "Final energy demand transport sector|Hydrogen (PJ)",
              "Final energy demand transport sector|Total (PJ)",
              NA,
              NA,
              "Final energy demand residential sector|Oil (PJ)",
              "Final energy demand residential sector|Coal (PJ)",
              "Final energy demand residential sector|Natural gas (PJ)",
              "Final energy demand residential sector|Electricity (PJ)",
              "Final energy demand residential sector|Heat (PJ)",
              "Final energy demand residential sector|Biomass and waste (PJ)",
              "Final energy demand residential sector|Hydrogen (PJ)",
              "Final energy demand residential sector|Other (PJ)",
              "Final energy demand residential sector|Total (PJ)",
              NA,
              NA,
              "Final energy demand services sector|Oil (PJ)",
              "Final energy demand services sector|Coal (PJ)",
              "Final energy demand services sector|Natural gas (PJ)",
              "Final energy demand services sector|Electricity (PJ)",
              "Final energy demand services sector|Heat (PJ)",
              "Final energy demand services sector|Biomass and waste (PJ)",
              "Final energy demand services sector|Hydrogen (PJ)",
              "Final energy demand services sector|Other (PJ)",
              "Final energy demand services sector|Total (PJ)",
              NA,
              NA,
              "Final energy demand agriculture, fisheries and forestry sector|Oil (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Coal (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Natural gas (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Electricity (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Heat (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Biomass and waste (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Hydrogen (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Other (PJ)",
              "Final energy demand agriculture, fisheries and forestry sector|Total (PJ)",
              NA,
              NA,
              "Gross electricity generation|Oil (TWh)",
              "Gross electricity generation|Coal (TWh)",
              "Gross electricity generation|Coal with CCS (TWh)",
              "Gross electricity generation|Natural gas (TWh)",
              "Gross electricity generation|Natural gas with CCS (TWh)",
              "Gross electricity generation|Nuclear (TWh)",
              "Gross electricity generation|Biomass and waste (TWh)",
              "Gross electricity generation|Biomass with CCS (TWh)",
              "Gross electricity generation|Hydro (excl. pumped storage) (TWh)",
              "Gross electricity generation|Geothermal (TWh)",
              "Gross electricity generation|Wind onshore (TWh)",
              "Gross electricity generation|Wind offshore (TWh)",
              "Gross electricity generation|Solar PV (TWh)",
              "Gross electricity generation|Solar CSP (TWh)",
              "Gross electricity generation|Ocean (TWh)",
              "Gross electricity generation|Other (TWh)",
              "Gross electricity generation|Total (TWh)",
              NA,
              NA,
              "Gross electricity capacity|Oil (GW)",
              "Gross electricity capacity|Coal (GW)",
              "Gross electricity capacity|Coal with CCS (GW)",
              "Gross electricity capacity|Natural gas (GW)",
              "Gross electricity capacity|Natural gas with CCS (GW)",
              "Gross electricity capacity|Nuclear (GW)",
              "Gross electricity capacity|Biomass and waste (GW)",
              "Gross electricity capacity|Biomass with CCS (GW)",
              "Gross electricity capacity|Hydro (excl. pumped storage) (GW)",
              "Gross electricity capacity|Geothermal (GW)",
              "Gross electricity capacity|Wind onshore (GW)",
              "Gross electricity capacity|Wind offshore (GW)",
              "Gross electricity capacity|Solar PV (GW)",
              "Gross electricity capacity|Solar CSP (GW)",
              "Gross electricity capacity|Ocean (GW)",
              "Gross electricity capacity|Other (GW)",
              "Gross electricity capacity|Total (GW)",
              NA,
              NA,
              "Direct CO2 emissions|Industry (Mt CO2)",
              "Direct CO2 emissions|Buildings, agriculture, fishing, non-specified other (Mt CO2)",
              "Direct CO2 emissions|Transport (Mt CO2)",
              "Direct CO2 emissions|Power (Mt CO2)",
              "Direct CO2 emissions|Other transformation (Mt CO2)",
              "Direct CO2 emissions|Total (Mt CO2)",
              NA,
              NA,
              "CO2 captured|Industry (Mt CO2)",
              "CO2 captured|Power (Mt CO2)",
              "CO2 captured|Other transformation (Mt CO2)",
              "CO2 captured|Total (Mt CO2)",
              NA,
              NA,
              "BECCS, CO2 captured|Industry (Mt CO2)",
              "BECCS, CO2 captured|Power (Mt CO2)",
              "BECCS, CO2 captured|Other transformation (Mt CO2)",
              "BECCS, CO2 captured|Total (Mt CO2)"
            )
          }
        ) %>%
          extract("name", c("variable", "unit"), "^(.*) \\((.*)\\)$")
      )
    },
    transport = {
      list(
        file = "ETP2017_transport_summary.xlsx",
        prefix = "Transport",
        sheets = list(
          "OECD", "NonOECD",
          "ASEAN", "Brazil", "China", "European Union",
          "India", "Mexico", "Russia", "South Africa", "United States"
        ),
        scenarios = list(
          "RTS" = "B4:K49",
          "2DS" = "O4:X49",
          "B2DS" = "AB4:AK49"
        ),
        rows = tibble(
          name = {
            c(
              NA,
              "Total final energy consumption|Conventional gasoline (PJ)",
              "Total final energy consumption|Conventional diesel (PJ)",
              "Total final energy consumption|Jet fuel (PJ)",
              "Total final energy consumption|Residual Fuel (PJ)",
              "Total final energy consumption|Compressed natural gas and liquid petroleum gas (PJ)",
              "Total final energy consumption|Electricity (PJ)",
              "Total final energy consumption|Biofuels (PJ)",
              "Total final energy consumption|Hydrogen (PJ)",
              "Total final energy consumption|Total (PJ)",
              NA,
              NA,
              "Passenger transport final energy consumption|Air (PJ)",
              "Passenger transport final energy consumption|Light road (PJ)",
              "Passenger transport final energy consumption|Heavy road (PJ)",
              "Passenger transport final energy consumption|Rail (PJ)",
              "Passenger transport final energy consumption|Shipping (PJ)",
              "Passenger transport final energy consumption|Total (PJ)",
              NA,
              NA,
              "Freight transport final energy consumption|Air (PJ)",
              "Freight transport final energy consumption|Light road (PJ)",
              "Freight transport final energy consumption|Heavy road (PJ)",
              "Freight transport final energy consumption|Rail (PJ)",
              "Freight transport final energy consumption|Shipping (PJ)",
              "Freight transport final energy consumption|Total (PJ)",
              NA,
              NA,
              "Well-to-wheel emissions by mode|Air (MtCO2)",
              "Well-to-wheel emissions by mode|Light road (MtCO2)",
              "Well-to-wheel emissions by mode|Heavy road (MtCO2)",
              "Well-to-wheel emissions by mode|Rail (MtCO2)",
              "Well-to-wheel emissions by mode|Shipping (MtCO2)",
              "Well-to-wheel emissions by mode|Total (MtCO2)",
              NA,
              NA,
              "Passenger kilometres|Air (billion)",
              "Passenger kilometres|Light road (billion)",
              "Passenger kilometres|Heavy road (billion)",
              "Passenger kilometres|Rail (billion)",
              "Passenger kilometres|Total (billion)",
              NA,
              NA,
              "Freight tonne kilometres|Light road (billion)",
              "Freight tonne kilometres|Heavy road (billion)",
              "Freight tonne kilometres|Rail (billion)"
            )
          }
        ) %>%
          extract("name", c("variable", "unit"), "^(.*) \\((.*)\\)$")
      )
    }
  )

  # ---- guardians ----
  if (!subtype %in% names(subtypes)) {
    stop(
      "Invalid subtype -- supported subtypes are: ",
      paste(names(subtypes), collapse = ", ")
    )
  }

  file <- subtypes[[subtype]]$file

  col_names <- c("rownames", "2014", "2025", "2030", "2035", "2040", "2045", "2050", "2055", "2060")
  col_types <- c("text", rep("numeric", length(col_names) - 1))
  tmp <- tibble()
  for (sheet in subtypes[[subtype]]$sheets) {
    for (scenario in names(subtypes[[subtype]]$scenarios)) {
      tmp <- bind_rows(
        tmp,
        read_xlsx(
          path = file, sheet = sheet, col_names = col_names,
          col_types = col_types, range = subtypes[[subtype]]$scenarios[[scenario]]
        ) %>%
          # add variable and unit columns
          bind_cols(subtypes[[subtype]]$rows) %>%
          # drop unneeded rows
          drop_na("variable", "unit") %>%
          # drop rownames from worksheet
          select(-1) %>%
          # add variable prefix
          mutate(variable = paste0(subtypes[[subtype]]$prefix, "|", !!sym("variable"))) %>%
          pivot_longer(
            cols = c(-"variable", -"unit"), names_to = "year",
            names_transform = list("year" = as.integer)
          ) %>%
          mutate(region := sheet, scenario := scenario)
      )
    }
  }

  tmp <- tmp %>%
    select("region", "year", "scenario", "variable", "unit", "value") %>%
    as.magpie(spatial = 1, temporal = 2, tidy = TRUE)
  
  # set all 2055 data (for RTS/OECD/Chemicals with feedstocks) to NA due to faulty data in source
  if (subtype == "industry") {
    tmp[, 2055, "RTS.Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|",
      pmatch = T
    ]["OECD",,] <- NA
  }
  
  return(tmp)
}
