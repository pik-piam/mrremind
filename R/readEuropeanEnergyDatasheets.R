#' Read European Energy Datasheets
#'
#' Read European Energy Datasheets .xlsx file as magpie object.
#'
#' @return magpie object of European Energy Datasheets
#' @author Renato Rodrigues, Atreya Shankar, Falk Benke
#' @source European Energy Datasheets public database
#' https://energy.ec.europa.eu/data-and-analysis/eu-energy-statistical-pocketbook-and-country-datasheets_en
#' @examples
#' \dontrun{
#' test <- readSource("EuropeanEnergyDatasheet", subtype = "EU27", convert = FALSE)
#' }
#' @importFrom readxl excel_sheets read_excel
#' @param subtype data subtype. Either "EU28" (data from June 20 including GBR)
#' or "EU27" (latest data from August 23 without GBR)
#' @importFrom reshape2 melt
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na extract
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats aggregate
#'
readEuropeanEnergyDatasheets <- function(subtype) {
  if (!subtype %in% c("EU27", "EU28")) {
    stop("Invalid subtype. Must be either EU27 or EU28")
  }

  if (subtype == "EU28") {
    # load mapping and find mappable parameters
    mapping <- read_excel("eurostat2REMIND.xlsx")
    mapping <- mapping[-which(is.na(mapping$Original)), ]
    # creating vector with mapping
    indices_1 <- which(!(is.na(mapping$REMIND)))
    hold <- mapping[, c("REMIND", "factor")]
    hold <- hold[which(!is.na(hold$REMIND)), ]
    indices_2 <- which(!(is.na(mapping$REMIND_2)))
    hold_2 <- mapping[, c("REMIND_2", "factor")]
    hold_2 <- hold_2[which(!is.na(hold_2$REMIND_2)), ]
    indices_3 <- which(!(is.na(mapping$REMIND_3)))
    hold_3 <- mapping[, c("REMIND_3", "factor")]
    hold_3 <- hold_3[which(!is.na(hold_3$REMIND_3)), ]
    # extract data from excel sheet
    sheets <- excel_sheets("energy_statistical_countrydatasheets_jun20.xlsx")
    sheets <- sheets[which(nchar(sheets) == 2)]
    data <- lapply(sheets, function(region) {
      countrySheet <- suppressMessages(read_excel("energy_statistical_countrydatasheets_jun20.xlsx", sheet = region))
      countrySheet <- countrySheet[, colSums(is.na(countrySheet)) != nrow(countrySheet)]
      # find correct column to search for names, most likely should be column 3
      nameColumn <- grep("Energy Balance", countrySheet)
      # remove NAs, remove "Energy statistics for", then get exact matches by row numbers
      countrySheet <- countrySheet[-which(is.na(countrySheet[, nameColumn])), ]
      countrySheet <- countrySheet[-grep("Energy Statistics for:", countrySheet[, nameColumn]), ]
      # replace with remind_1 mapping
      countrySheet_1 <- countrySheet[indices_1, nameColumn:ncol(countrySheet)]
      countrySheet_1 <- cbind(hold, countrySheet_1)
      # making sure the data is numeric
      countrySheet_1[, -c(1, 2, 3)] <- sapply(countrySheet_1[, -c(1, 2, 3)], as.numeric)
      # converting unit to REMIND unit
      countrySheet_1[, -c(1, 2, 3)] <- countrySheet_1[, -c(1, 2, 3)] * countrySheet_1[, c("factor")]
      countrySheet_1 <- countrySheet_1[, -c(2, 3)] # removing extra columns
      countrySheet_1 <- cbind(region, countrySheet_1) # adding region column
      countrySheet_1 <- aggregate(. ~ REMIND + region, data = countrySheet_1, FUN = sum, na.action = na.pass)
      colnames(countrySheet_1) <- c("variable", "region", 1990:(1990 + ncol(countrySheet_1) - 3))
      # replace with remind_2 mapping
      countrySheet_2 <- countrySheet[indices_2, nameColumn:ncol(countrySheet)]
      countrySheet_2 <- cbind(hold_2, countrySheet_2)
      # making sure the data is numeric
      countrySheet_2[, -c(1, 2, 3)] <- sapply(countrySheet_2[, -c(1, 2, 3)], as.numeric)
      # converting unit to REMIND unit
      countrySheet_2[, -c(1, 2, 3)] <- countrySheet_2[, -c(1, 2, 3)] * countrySheet_2[, c("factor")]
      countrySheet_2 <- countrySheet_2[, -c(2, 3)] # removing extra columns
      countrySheet_2 <- cbind(region, countrySheet_2) # adding region column
      # merge repeated items
      countrySheet_2 <- aggregate(. ~ REMIND_2 + region, data = countrySheet_2, FUN = sum, na.action = na.pass)
      colnames(countrySheet_2) <- c("variable", "region", 1990:(1990 + ncol(countrySheet_2) - 3))
      # replace with remind_3 mapping
      countrySheet_3 <- countrySheet[indices_3, nameColumn:ncol(countrySheet)]
      countrySheet_3 <- cbind(hold_3, countrySheet_3)
      # making sure the data is numeric
      countrySheet_3[, -c(1, 2, 3)] <- sapply(countrySheet_3[, -c(1, 2, 3)], as.numeric)
      # converting unit to REMIND unit
      countrySheet_3[, -c(1, 2, 3)] <- countrySheet_3[, -c(1, 2, 3)] * countrySheet_3[, c("factor")]
      countrySheet_3 <- countrySheet_3[, -c(2, 3)] # removing extra columns
      countrySheet_3 <- cbind(region, countrySheet_3) # adding region column
      # merge repeated items
      countrySheet_3 <- aggregate(. ~ REMIND_3 + region, data = countrySheet_3, FUN = sum, na.action = na.pass)
      colnames(countrySheet_3) <- c("variable", "region", 1990:(1990 + ncol(countrySheet_3) - 3))
      # merge both REMIND mappings
      countrySheet <- rbind(countrySheet_1, countrySheet_2, countrySheet_3)
      return(countrySheet)
    })
    # merge into single dataframe
    data <- do.call("rbind", data)
    # long format
    data <- melt(data, id.vars = 1:2)
    colnames(data) <- c("variable", "region", "period", "value")
    # dump contents into magpie
    x <- as.magpie(data, spatial = 2, datacol = 4, temporal = 3)
    return(x)
  } else {
    # nolint start
    rows <- tibble(
      name = {
        c(
          NA,
          NA,
          NA,
          "Energy Balance|Production (Mtoe)",
          "Energy Balance|Production|Solid fossil fuels (Mtoe)",
          "Energy Balance|Production|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Production|Solid fossil fuels|of which brown coal (Mtoe)",
          "Energy Balance|Production|Peat and peat products (Mtoe)",
          "Energy Balance|Production|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Production|Oil and petroleum products (Mtoe)",
          "Energy Balance|Production|Oil and petroleum products|of which crude oil (Mtoe)",
          "Energy Balance|Production|Natural gas (Mtoe)",
          "Energy Balance|Production|Nuclear (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Hydro (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Wind (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Solar photovoltaic (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Solar thermal (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Solid biofuels (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Biogases (Mtoe)",
          "Energy Balance|Production|Renewables and biofuels|Liquid biofuels (Mtoe)",
          "Energy Balance|Production|Wastes, Non-Renewable (Mtoe)",
          NA,
          "Energy Balance|Imports (Mtoe)",
          "Energy Balance|Imports|Solid fossil fuels (Mtoe)",
          "Energy Balance|Imports|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Imports|Oil and petroleum products (Mtoe)",
          "Energy Balance|Imports|Oil and petroleum products|of which crude oil and NGL (Mtoe)",
          "Energy Balance|Imports|Natural gas (Mtoe)",
          "Energy Balance|Imports|Renewables and biofuels (Mtoe)",
          "Energy Balance|Imports|Electricity (Mtoe)",
          "Energy Balance|Imports|Heat (Mtoe)",
          "Energy Balance|Imports|Waste, Non-Renewable (Mtoe)",
          NA,
          "Energy Balance|Exports (Mtoe)",
          "Energy Balance|Exports|Solid fossil fuels (Mtoe)",
          "Energy Balance|Exports|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Exports|Oil and petroleum products (Mtoe)",
          "Energy Balance|Exports|Oil and petroleum products|of which crude oil and NGL (Mtoe)",
          "Energy Balance|Exports|Natural gas (Mtoe)",
          "Energy Balance|Exports|Renewables and biofuels (Mtoe)",
          "Energy Balance|Exports|Electricity (Mtoe)",
          "Energy Balance|Exports|Heat (Mtoe)",
          "Energy Balance|Exports|Waste, Non-Renewable (Mtoe)",
          NA,
          "Energy Balance|Net Imports (Mtoe)",
          "Energy Balance|Net Imports|Solid fossil fuels (Mtoe)",
          "Energy Balance|Net Imports|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Net Imports|Oil and petroleum products (Mtoe)",
          "Energy Balance|Net Imports|Oil and petroleum products|of which crude oil and NGL (Mtoe)",
          "Energy Balance|Net Imports|Natural gas (Mtoe)",
          "Energy Balance|Net Imports|Renewables and biofuels (Mtoe)",
          "Energy Balance|Net Imports|Electricity (Mtoe)",
          "Energy Balance|Net Imports|Heat (Mtoe)",
          "Energy Balance|Net Imports|Waste, Non-Renewable (Mtoe)",
          NA,
          "Energy Balance|Gross available energy (Mtoe)",
          NA,
          "Energy Balance|International maritime bunkers (Mtoe)",
          "Energy Balance|International maritime bunkers|of which petroleum products (Mtoe)",
          NA,
          "Energy Balance|Gross inland consumption (Mtoe)",
          "Energy Balance|Gross inland consumption|Solid fossil fuels (Mtoe)",
          "Energy Balance|Gross inland consumption|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Gross inland consumption|Solid fossil fuels|of which brown coal (Mtoe)",
          "Energy Balance|Gross inland consumption|Manufactured gases (Mtoe)",
          "Energy Balance|Gross inland consumption|Peat and peat products (Mtoe)",
          "Energy Balance|Gross inland consumption|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Gross inland consumption|Oil and petroleum products (Mtoe)",
          "Energy Balance|Gross inland consumption|Oil and petroleum products|of which crude and NGL (Mtoe)",
          "Energy Balance|Gross inland consumption|Natural gas (Mtoe)",
          "Energy Balance|Gross inland consumption|Nuclear (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Hydro (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Wind (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Solar photovoltaic (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Solar thermal (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Tide, Wave and Ocean (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Biofuels and renewable waste (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Geothermal (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Ambient heat, heat pumps (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Electricity (Mtoe)",
          "Energy Balance|Gross inland consumption|Renewables and biofuels|Heat (Mtoe)",
          "Energy Balance|Gross inland consumption|Waste, non-renewable (Mtoe)",
          NA,
          "Energy Balance|International aviation (Mtoe)",
          "Energy Balance|International aviation|of which petroleum products (Mtoe)",
          NA,
          "Energy Balance|Total energy supply (Mtoe)",
          "Energy Balance|Total energy supply|Solid fossil fuels (Mtoe)",
          "Energy Balance|Total energy supply|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Total energy supply|Solid fossil fuels|of which brown coal (Mtoe)",
          "Energy Balance|Total energy supply|Manufactured gases (Mtoe)",
          "Energy Balance|Total energy supply|Peat and peat products (Mtoe)",
          "Energy Balance|Total energy supply|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Total energy supply|Oil and petroleum products (Mtoe)",
          "Energy Balance|Total energy supply|Oil and petroleum products|of which crude and NGL (Mtoe)",
          "Energy Balance|Total energy supply|Natural gas (Mtoe)",
          "Energy Balance|Total energy supply|Nuclear (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Hydro (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Wind (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Solar photovoltaic (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Solar thermal (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Tide, Wave and Ocean (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Solid biofuels and renewable waste (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Liquid biofuels (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Biogases (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Geothermal (Mtoe)",
          "Energy Balance|Total energy supply|Renewables and biofuels|Ambient heat, heat pumps (Mtoe)",
          "Energy Balance|Total energy supply|Electricity (Mtoe)",
          "Energy Balance|Total energy supply|Heat (Mtoe)",
          "Energy Balance|Total energy supply|Waste, non-renewable (Mtoe)",
          NA,
          "Energy Balance|Transformation Input (Mtoe)",
          NA,
          "Energy Balance|Transformation Input|Solid fossil fuels (Mtoe)",
          "Energy Balance|Transformation Input|Manufactured gases (Mtoe)",
          "Energy Balance|Transformation Input|Peat and peat products (Mtoe)",
          "Energy Balance|Transformation Input|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Transformation Input|Oil and petroleum products (Mtoe)",
          "Energy Balance|Transformation Input|Natural gas (Mtoe)",
          "Energy Balance|Transformation Input|Nuclear (Mtoe)",
          "Energy Balance|Transformation Input|Renewables and biofuels (Mtoe)",
          "Energy Balance|Transformation Input|Waste, non-renewable (Mtoe)",
          NA,
          "Energy Balance|Transformation Input|Electricity producers (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Solid fossil fuels (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Manufactured gases (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Peat and peat products (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Oil and petroleum products (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Nuclear (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Natural gas (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Renewables and biofuels (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Waste, non-renewable (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Derived heat for electricity production (Mtoe)",
          "Energy Balance|Transformation Input|Electricity producers|Electricity from pumped storage (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Solid fossil fuels (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Manufactured gases (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Peat and peat products (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Oil and petroleum products (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Nuclear (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Natural gas (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Renewables and biofuels (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Waste, non-renewable (Mtoe)",
          "Energy Balance|Transformation Input|Heat producers|Electric boilers and electrically driven heat pumps (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Solid fossil fuels (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Manufactured gases (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Peat and peat products (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Oil and petroleum products (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Nuclear (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Natural gas (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Renewables and biofuels (Mtoe)",
          "Energy Balance|Transformation Input|CHP producers|Waste, non-renewable (Mtoe)",
          "Energy Balance|Transformation Input|Refineries, Petroleum and sub-products (Mtoe)",
          "Energy Balance|Transformation Input|Other transformation input (Mtoe)",
          NA,
          "Energy Balance|Transformation Output (Mtoe)",
          NA,
          "Energy Balance|Transformation Output|Solid fossil fuels (Mtoe)",
          "Energy Balance|Transformation Output|Manufactured gases (Mtoe)",
          "Energy Balance|Transformation Output|Peat and peat products (Mtoe)",
          "Energy Balance|Transformation Output|Oil and petroleum products (Mtoe)",
          "Energy Balance|Transformation Output|Natural gas (Mtoe)",
          "Energy Balance|Transformation Output|Renewables and biofuels (Mtoe)",
          "Energy Balance|Transformation Output|Waste, non-renewable (Mtoe)",
          "Energy Balance|Transformation Output|Electricity (Mtoe)",
          "Energy Balance|Transformation Output|Heat (Mtoe)",
          NA,
          "Energy Balance|Transformation Output|Electricity producers (Mtoe)",
          "Energy Balance|Transformation Output|Electricity from pumped storage (Mtoe)",
          "Energy Balance|Transformation Output|Heat producers (Mtoe)",
          "Energy Balance|Transformation Output|CHP producers (Mtoe)",
          "Energy Balance|Transformation Output|CHP producers|Electricity (Mtoe)",
          "Energy Balance|Transformation Output|CHP producers|Heat (Mtoe)",
          "Energy Balance|Transformation Output|Refineries, Petroleum and sub-products (Mtoe)",
          "Energy Balance|Transformation Output|Other transformation output (Mtoe)",
          NA,
          "Energy Balance|Transformation Losses (Mtoe)",
          NA,
          "Energy Balance|Energy Sector (Mtoe)",
          "Energy Balance|Energy Sector|Solid fossil fuels (Mtoe)",
          "Energy Balance|Energy Sector|Peat and products, oil shale and sands (Mtoe)",
          "Energy Balance|Energy Sector|Oil and petroleum products (Mtoe)",
          "Energy Balance|Energy Sector|Natural gas and manufactured gases (Mtoe)",
          "Energy Balance|Energy Sector|Renewables and biofuels (Mtoe)",
          "Energy Balance|Energy Sector|Waste, non-renewable (Mtoe)",
          "Energy Balance|Energy Sector|Electricity (Mtoe)",
          "Energy Balance|Energy Sector|Heat (Mtoe)",
          NA,
          "Energy Balance|Distribution losses (Mtoe)",
          NA,
          "Energy Balance|Available for final consumption (Mtoe)",
          NA,
          "Energy Balance|Final non-energy consumption (Mtoe)",
          NA,
          "Energy Balance|Final energy consumption (Mtoe)",
          NA,
          "Energy Balance|Final energy consumption|Solid fossil fuels (Mtoe)",
          "Energy Balance|Final energy consumption|Solid fossil fuels|of which hard coal (Mtoe)",
          "Energy Balance|Final energy consumption|Solid fossil fuels|of which brown coal (Mtoe)",
          "Energy Balance|Final energy consumption|Manufactured gases (Mtoe)",
          "Energy Balance|Final energy consumption|Peat and peat products (Mtoe)",
          "Energy Balance|Final energy consumption|Oil shale and oil sands (Mtoe)",
          "Energy Balance|Final energy consumption|Oil and petroleum products (Mtoe)",
          "Energy Balance|Final energy consumption|Natural gas (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Solar thermal (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Geothermal (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Solid biofuels and renewable waste (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Biogases (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Liquid biofuels (Mtoe)",
          "Energy Balance|Final energy consumption|Renewables and biofuels|Ambient heat, heat pumps (Mtoe)",
          "Energy Balance|Final energy consumption|Waste, non-renewable (Mtoe)",
          "Energy Balance|Final energy consumption|Electricity (Mtoe)",
          "Energy Balance|Final energy consumption|Heat (Mtoe)",
          NA,
          "Energy Balance|Final energy consumption|Industry (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Iron and steel (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Chemical and petrochemical (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Non-ferrous metals (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Non-metallic minerals (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Mining and quarrying (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Food, beverages and tobacco (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Textile and leather (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Paper, pulp and print (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Transport equipment (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Machinery (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Wood and wood products (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Construction (Mtoe)",
          "Energy Balance|Final energy consumption|Industry|Other industry (Mtoe)",
          "Energy Balance|Final energy consumption|Transport (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Rail (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Road (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Domestic aviation (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Domestic navigation (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Pipeline transport (Mtoe)",
          "Energy Balance|Final energy consumption|Transport|Other transport (Mtoe)",
          "Energy Balance|Final energy consumption|Residential (Mtoe)",
          "Energy Balance|Final energy consumption|Services (Mtoe)",
          "Energy Balance|Final energy consumption|Agriculture and Fishing (Mtoe)",
          "Energy Balance|Final energy consumption|Others (Mtoe)",
          NA,
          NA,
          NA,
          "Electricity Production|Gross Electricity Generation (TWh)",
          NA,
          "Electricity Production|Solid fossil fuels, peat and products, oil shale and oil sands (TWh)",
          "Electricity Production|Solid fossil fuels, peat and products, oil shale and oil sands|of which hard coal (TWh)",
          "Electricity Production|Solid fossil fuels, peat and products, oil shale and oil sands|of which brown coal (TWh)",
          "Electricity Production|Oil and petroleum products (TWh)",
          "Electricity Production|Natural gas and manufactured gas (TWh)",
          "Electricity Production|Natural gas and manufactured gas|of which natural gas (TWh)",
          "Electricity Production|Nuclear (TWh)",
          "Electricity Production|Renewables and biofuels (TWh)",
          "Electricity Production|Renewables and biofuels|Hydro (TWh)",
          "Electricity Production|Renewables and biofuels|Wind (TWh)",
          "Electricity Production|Renewables and biofuels|Solid biofuels and renewable wastes (TWh)",
          "Electricity Production|Renewables and biofuels|Biogases (TWh)",
          "Electricity Production|Renewables and biofuels|Liquid biofuels (TWh)",
          "Electricity Production|Renewables and biofuels|Solar (TWh)",
          "Electricity Production|Renewables and biofuels|Geothermal (TWh)",
          "Electricity Production|Renewables and biofuels|Tide, Wave and Ocean (TWh)",
          "Electricity Production|Wastes non-RES (TWh)",
          "Electricity Production|Other (TWh)",
          NA,
          "Electricity Production|Main Activity Electricity Only (TWh)",
          "Electricity Production|Main Activity CHP Plants (TWh)",
          "Electricity Production|Autoproducer Electricity Only (TWh)",
          "Electricity Production|Autoproducer CHP Plants (TWh)",
          NA,
          "Electricity Production|Installed Electricity Capacity (MW)",
          "Electricity Production|Installed Electricity Capacity|Combustible Fuels (MW)",
          "Electricity Production|Installed Electricity Capacity|Nuclear (MW)",
          "Electricity Production|Installed Electricity Capacity|Hydro (MW)",
          "Electricity Production|Installed Electricity Capacity|Wind (MW)",
          "Electricity Production|Installed Electricity Capacity|Solar PV (MW)",
          "Electricity Production|Installed Electricity Capacity|Solar Thermal (MW)",
          "Electricity Production|Installed Electricity Capacity|Geothermal (MW)",
          "Electricity Production|Installed Electricity Capacity|Tide, Wave and Ocean (MW)",
          "Electricity Production|Installed Electricity Capacity|Other Sources (MW)",
          NA,
          NA,
          "Electricity Production|Solar and Wind Energy|Cumulative Solar and Wind Capacity (MW)",
          "Electricity Production|Solar and Wind Energy|Cumulative Solar and Wind Capacity (%)",
          NA,
          NA,
          "Electricity Production|Solar and Wind Energy|Wind|Wind Cumulative Installed Capacity (MW)",
          "Electricity Production|Solar and Wind Energy|Wind|Wind Cumulative Installed Capacity (%)",
          "Electricity Production|Solar and Wind Energy|Wind|Annual Installed Wind Capacity (MW)",
          "Electricity Production|Solar and Wind Energy|Wind|Wind Cumulative Capacity Growth Rate (% YoY)",
          "Electricity Production|Solar and Wind Energy|Wind|Wind Gross Electricity Production (TWh)",
          "Electricity Production|Solar and Wind Energy|Wind|Wind Gross Electricity Penetration Level (%)",
          "Electricity Production|Solar and Wind Energy|Wind|Wind Annual Average Capacity Factor (%)",
          NA,
          NA,
          "Electricity Production|Solar and Wind Energy|Solar|Solar Thermal Collector's Surface (1000 m2)",
          "Electricity Production|Solar and Wind Energy|Solar|Solar Total Installed Capacity (MW)",
          "Electricity Production|Solar and Wind Energy|Solar|Solar Gross Electricity Production (TWh)",
          "Electricity Production|Solar and Wind Energy|Solar|Solar Gross Electricity Penetration Level (%)",
          NA,
          NA,
          NA,
          NA,
          "Market Indicators|Natural Gas|Entities bringing gas into the country (Nr)",
          "Market Indicators|Natural Gas|Main Entities bringing gas in the country, Deal >5% Total (Nr)",
          "Market Indicators|Natural Gas|Cumulative Market Share, Main Entities (%)",
          "Market Indicators|Natural Gas|Market Share - Largest Production & Import Company (%)",
          "Market Indicators|Natural Gas|Retailers to Final Consumers (Nr)",
          "Market Indicators|Natural Gas|Main Retailers, Sales >5% Total (Nr)",
          "Market Indicators|Natural Gas|Cumulative Market Share, Main Retailers (%)",
          "Market Indicators|Natural Gas|Market Share, Largest Retailer (%)",
          NA,
          "Market Indicators|Electricity|Producers, Representing 95% Total (Nr)",
          "Market Indicators|Electricity|Main Producers, >5% Total (Nr)",
          "Market Indicators|Electricity|Cumulative Market Share Generation, Main Entities (%)",
          "Market Indicators|Electricity|Cumulative Market Share Capacity, Main Entities (%)",
          "Market Indicators|Electricity|Market Share Largest Producer (%)",
          "Market Indicators|Electricity|New Capacity in the Year (MW)",
          "Market Indicators|Electricity|Retailers to Final Consumers (Nr)",
          "Market Indicators|Electricity|Main Retailers, Sales >5% Total (Nr)",
          "Market Indicators|Electricity|Cumulative Market Share, Main Retailers (%)",
          NA,
          NA,
          NA,
          "Heat Production (PJ)",
          "Heat Production|Solid fossil fuels, peat and products, oil shale and oil sands (PJ)",
          "Heat Production|Solid fossil fuels, peat and products, oil shale and oil sands|of which hard coal (PJ)",
          "Heat Production|Solid fossil fuels, peat and products, oil shale and oil sands|of which brown coal (PJ)",
          "Heat Production|Oil and petroleum products (PJ)",
          "Heat Production|Natural gas and manufactured gas (PJ)",
          "Heat Production|Natural gas and manufactured gas|of which natural gas (PJ)",
          "Heat Production|Nuclear (PJ)",
          "Heat Production|Renewables and biofuels (PJ)",
          "Heat Production|Renewables and biofuels|Solid biofuels and renewable wastes (PJ)",
          "Heat Production|Renewables and biofuels|Biogases (PJ)",
          "Heat Production|Renewables and biofuels|Liquid biofuels (PJ)",
          "Heat Production|Renewables and biofuels|Solar thermal (PJ)",
          "Heat Production|Renewables and biofuels|Geothermal (PJ)",
          "Heat Production|Renewables and biofuels|Ambient heat (PJ)",
          "Heat Production|Renewables and biofuels|Wastes non-RES (PJ)",
          "Heat Production|Renewables and biofuels|Other (PJ)",
          NA,
          NA,
          NA,
          "Cogeneration Heat and Power|CHP Electricity Generation (TWh)",
          "Cogeneration Heat and Power|CHP Electricity Generation (PJ)",
          "Cogeneration Heat and Power|CHP Electrical Capacity (GW)",
          "Cogeneration Heat and Power|CHP in Total Electricity Generation (%)",
          "Cogeneration Heat and Power|CHP Heat Production (PJ)",
          "Cogeneration Heat and Power|CHP Heat Capacity (GW)",
          "Cogeneration Heat and Power|Fuel Input Into CHP (PJ)",
          "Cogeneration Heat and Power|Share Solid fossil fuels (%)",
          "Cogeneration Heat and Power|Share Oil (%)",
          "Cogeneration Heat and Power|Share Natural Gas (%)",
          "Cogeneration Heat and Power|Share RES and Waste (%)",
          "Cogeneration Heat and Power|Share Other Fuels (%)",
          NA,
          NA,
          NA,
          "Transport Fuels|Production biofuels (ktoe)",
          "Transport Fuels|Production biofuels|Pure biogasoline (ktoe)",
          "Transport Fuels|Production biofuels|Pure biodiesel (ktoe)",
          "Transport Fuels|Production biofuels|Other liquid biofuels (ktoe)",
          NA,
          "Transport Fuels|Final consumption biofuels (ktoe)",
          "Transport Fuels|Final consumption biofuels|Pure biogasoline (ktoe)",
          "Transport Fuels|Final consumption biofuels|Blended biogasoline (ktoe)",
          "Transport Fuels|Final consumption biofuels|Pure biodiesel (ktoe)",
          "Transport Fuels|Final consumption biofuels|Blended biodiesel (ktoe)",
          "Transport Fuels|Final consumption biofuels|Other liquid biofuels (ktoe)",
          NA,
          "Transport Fuels|Final consumption petroleum products (ktoe)",
          "Transport Fuels|Final consumption petroleum products|of which LPG (ktoe)",
          "Transport Fuels|Final consumption petroleum products|of which motor gasoline (ktoe)",
          "Transport Fuels|Final consumption petroleum products|of which Gas/Diesel oil (ktoe)",
          NA,
          "Transport Fuels|Final consumption natural gas (ktoe)",
          NA,
          "Transport Fuels|Biofuels Production Capacity (kton/year)",
          "Transport Fuels|Biofuels Production Capacity|Biogasoline (kton/year)",
          "Transport Fuels|Biofuels Production Capacity|Biodiesel (kton/year)",
          "Transport Fuels|Biofuels Production Capacity|Other liquid biofuels (kton/year)",
          NA,
          "Transport Fuels|Share of Biofuels in Transport Fuels (%)",
          "Transport Fuels|Share of Biofuels in Transport Fuels|of Biogasoline in Motor Gasoline (%)",
          "Transport Fuels|Share of Biofuels in Transport Fuels|of Biodiesel in Gas/Diesel Oil (%)",
          NA,
          NA,
          NA,
          NA,
          "Main Energy Indicators|Gross inland consumption 2020-2030 (Mtoe)",
          "Main Energy Indicators|Primary energy consumption 2020-2030 (Mtoe)",
          "Main Energy Indicators|Final energy consumption 2020-2030 (Mtoe)",
          NA,
          "Main Energy Indicators|Final energy intensity 2020-2030 (toe/MEUR15)",
          NA,
          "Main Energy Indicators|Primary Energy Intensity 2020-2030 (toe/MEUR15)",
          NA,
          NA,
          "Main Energy Indicators|Overall Renewable share [with aviation cap] (%)",
          "Main Energy Indicators|RE-T - Renewable energy in Transport (%)",
          "Main Energy Indicators|RES-E - Renewable Electricity Generation (%)",
          "Main Energy Indicators|RES-H&C - Renewable Heating and Cooling (%)",
          NA,
          NA,
          NA,
          "Main Energy Indicators|Energy Intensity [GAE/GDP2015] (toe/MEUR15)",
          NA,
          "Main Energy Indicators|Energy per Capita [GIC/pop] (kgoe/cap)",
          NA,
          "Main Energy Indicators|Final Electricity per Capita (KWh per Capita)",
          NA,
          "Main Energy Indicators|Import Dependency (%)",
          "Main Energy Indicators|Import Dependency|of Solid fossil fuels (%)",
          "Main Energy Indicators|Import Dependency|of Hard Coal (%)",
          "Main Energy Indicators|Import Dependency|of Oil and petroleum products (%)",
          "Main Energy Indicators|Import Dependency|of Crude and NGL (%)",
          "Main Energy Indicators|Import Dependency|of Natural Gas (%)",
          NA,
          NA,
          "Main Energy Indicators|Energy Mix|Solid fossil fuels (%)",
          "Main Energy Indicators|Energy Mix|Oil and petroleum products (%)",
          "Main Energy Indicators|Energy Mix|Natural gas (%)",
          "Main Energy Indicators|Energy Mix|Nuclear (%)",
          "Main Energy Indicators|Energy Mix|Renewables and biofuels (%)",
          "Main Energy Indicators|Energy Mix|Others [non-res waste, peat, oil shale and sands, manufactured gases] (%)",
          NA,
          NA,
          "Main Energy Indicators|Electricity Mix|Solid fossil fuels, oil shale and sands, peat (%)",
          "Main Energy Indicators|Electricity Mix|Oil and petroleum products (%)",
          "Main Energy Indicators|Electricity Mix|Natural gas and manufactured gases (%)",
          "Main Energy Indicators|Electricity Mix|Nuclear (%)",
          "Main Energy Indicators|Electricity Mix|Renewables and biofuels (%)",
          "Main Energy Indicators|Electricity Mix|Others (%)",
          NA,
          NA,
          "Main Energy Indicators|Final Energy by sector|Industry (%)",
          "Main Energy Indicators|Final Energy by sector|Transport (%)",
          "Main Energy Indicators|Final Energy by sector|Households (%)",
          "Main Energy Indicators|Final Energy by sector|Services (%)",
          "Main Energy Indicators|Final Energy by sector|Agriculture and Fishing (%)",
          "Main Energy Indicators|Final Energy by sector|Other (%)",
          NA,
          NA,
          NA,
          "CO2 emissions - National total [incl. int. aviation, without LULUCF] (Mt CO2)",
          "CO2 emissions|Energy (Mt CO2)",
          "CO2 emissions|Energy|Energy Industries (Mt CO2)",
          "CO2 emissions|Energy|Energy Industries|Public Electricity and Heat Production (Mt CO2)",
          "CO2 emissions|Energy|Energy Industries|Petroleum Refining (Mt CO2)",
          "CO2 emissions|Energy|Energy Industries|Manufacture of Solid Fuels and Other Energy Industries (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Iron and Steel (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Non-Ferrous Metals (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Chemicals (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Pulp, Paper and Print (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Food Processing, Beverages and Tobacco (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Non-metallic minerals (Mt CO2)",
          "CO2 emissions|Energy|Manufacturing Industries and Construction|Other Manufacturing Industries and Constructions (Mt CO2)",
          "CO2 emissions|Energy|Transport (Mt CO2)",
          "CO2 emissions|Energy|Transport|Domestic Aviation (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation|Cars (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation|Light duty trucks (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation|Heavy duty trucks and buses (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation|Motorcycles (Mt CO2)",
          "CO2 emissions|Energy|Transport|Road Transportation|Other road transportation (Mt CO2)",
          "CO2 emissions|Energy|Transport|Railways (Mt CO2)",
          "CO2 emissions|Energy|Transport|Domestic Navigation (Mt CO2)",
          "CO2 emissions|Energy|Transport|Other Transportation (Mt CO2)",
          "CO2 emissions|Energy|Commercial/Institutional (Mt CO2)",
          "CO2 emissions|Energy|Residential (Mt CO2)",
          "CO2 emissions|Energy|Agriculture/Forestry/Fisheries (Mt CO2)",
          "CO2 emissions|Energy|Other Sectors (Mt CO2)",
          "CO2 emissions|Energy|Other Combustion and Fugitive Emissions (Mt CO2)",
          "CO2 emissions|Industrial Processes and Product Use (Mt CO2)",
          "CO2 emissions|Agriculture (Mt CO2)",
          "CO2 emissions|Waste and Others (Mt CO2)",
          "CO2 emissions|Indirect CO2 (Mt CO2)",
          "CO2 emissions|International aviation (Mt CO2)",
          "CO2 emissions|International navigation (Mt CO2)",
          NA,
          "GHG emissions - National total [incl. int. aviation, without LULUCF] (Mt CO2eq)",
          "GHG emissions|Energy (Mt CO2eq)",
          "GHG emissions|Energy|Energy Industries (Mt CO2eq)",
          "GHG emissions|Energy|Energy Industries|Public Electricity and Heat Production (Mt CO2eq)",
          "GHG emissions|Energy|Energy Industries|Petroleum Refining (Mt CO2eq)",
          "GHG emissions|Energy|Energy Industries|Manufacture of Solid Fuels and Other Energy Industries (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Iron and Steel (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Non-Ferrous Metals (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Chemicals (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Pulp, Paper and Print (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Food Processing, Beverages and Tobacco (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Non-metallic minerals (Mt CO2eq)",
          "GHG emissions|Energy|Manufacturing Industries and Construction|Other Manufacturing Industries and Constructions (Mt CO2eq)",
          "GHG emissions|Energy|Transport (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Domestic Aviation (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Cars (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Light duty trucks (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Heavy duty trucks and buses (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Motorcycles (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Other road transportation (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Railways (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Domestic Navigation (Mt CO2eq)",
          "GHG emissions|Energy|Transport|Road Transportation|Other Transportation (Mt CO2eq)",
          "GHG emissions|Energy|Commercial/Institutional (Mt CO2eq)",
          "GHG emissions|Energy|Residential (Mt CO2eq)",
          "GHG emissions|Energy|Agriculture/Forestry/Fisheries (Mt CO2eq)",
          "GHG emissions|Energy|Other Sectors (Mt CO2eq)",
          "GHG emissions|Energy|Other Combustion and Fugitive Emissions (Mt CO2eq)",
          "GHG emissions|Industrial Processes and Product Use (Mt CO2eq)",
          "GHG emissions|Agriculture (Mt CO2eq)",
          "GHG emissions|Waste and Others (Mt CO2eq)",
          "GHG emissions|Indirect CO2 (Mt CO2eq)",
          "GHG emissions|International aviation (Mt CO2eq)",
          "GHG emissions|International navigation (Mt CO2eq)",
          NA,
          NA,
          "Main Emissions Indicators|GHG national total emissions (index 1990=100)",
          "Main Emissions Indicators|Total GHG per capita (t CO2 eq./capita)",
          "Main Emissions Indicators|GHG Intensity of Energy (kg CO2 eq./toe)",
          "Main Emissions Indicators|Total GHG - GDP Intensity (ton CO2 eq./MEUR15)",
          NA,
          NA,
          "Total Population (thousands of people)",
          "GDP2015 (Mrd EUR at 2015 exchange rates)",
          "GDP-market prices (Mrd EUR at current prices)"
        )
      }
    ) %>%
      extract("name", c("variable", "unit"), "^(.*) \\((.*)\\)$")
    # nolint end

    file <- "energy_statistical_countrydatasheets_aug23.xlsx"
    sheets <- excel_sheets(file)
    sheets <- sheets[which(nchar(sheets) == 2)]

    tmp <- tibble()
    for (sheet in sheets) {
      tmp <- rbind(
        tmp,
        suppressMessages(read_xlsx(path = file, sheet = sheet, range = "C8:AI543", )) %>%
          bind_cols(rows) %>%
          drop_na("variable", "unit") %>%
          select(-1) %>%
          melt(id.vars = c("variable", "unit"), variable.name = "year") %>%
          mutate(
            "year" := as.numeric(as.character(.data$year)),
            "region" := sheet,
            "value" := suppressWarnings(as.numeric(.data$value))
          )
      )
    }

    tmp %>%
      select("region", "year", "variable", "unit", "value") %>%
      as.magpie(spatial = 1, tidy = TRUE) %>%
      return()
  }
}
