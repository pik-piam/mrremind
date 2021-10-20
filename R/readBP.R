#' BP Capacity and Generation Data
#' @description  BP data. See README in input file for more details.
#' @details Data contains historical electricity renewable capacities (in MW for Wind, Solar, and Geothermal), Generation (in TWh for Nuclear, Hydro, Wind, Solar, Other Renewables, and Geo Biomass)
#' and Production (Oil, Gas, and Oil in Tonnes/EJ)
#'
#' @param subtype Either "Capacity", "Generation", "Production", "Consumption", "Trade", "Price"
#' @return A magpie object
#' @author Aman Malik
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' @importFrom readxl read_excel
#' @importFrom reshape merge_recurse
#' @importFrom reshape2 melt dcast
#' @importFrom rlang sym

readBP <- function(subtype) {
  value <- NULL
  Country <- NULL
  Year <- NULL
  filename <- c("bp-stats-review-2021-all-data.xlsx")

  tidy_data <- function(df, variable) {
    years <- as.character(c(1900:2020))
    rows2remove <- c("Total|OECD|European|Rest of World")
    colnames(df)[1] <- "Country"
    df$Country <- gsub("\\.", "", df$Country)
    df <- df %>%
      gather(colnames(df[1, -1]), key = "Year", value = value) %>%
      filter(!grepl(rows2remove, Country), !is.na(value), !value == "n/a", Year %in% years) %>%
      mutate(Year = as.integer(Year), value = as.numeric(value)) %>%
      mutate(Country = gsub(pattern = " and ", replacement = " & ", x = Country)) %>%
      mutate(Country = gsub(pattern = "[0-9]", replacement = "", x = Country))

    colnames(df)[3] <- variable

    return(df)
  }
  
  tidy_data_vertical <- function(df) {
    years <- as.character(c(1900:2020))
    rows2remove <- c("Total|OECD|European")
    df$Country <- gsub("\\.", "", df$Country)
    df <- df %>%
      filter(!grepl(rows2remove, Country), Year %in% years) %>%
      mutate(Year = as.integer(Year)) %>%
      mutate(Country = gsub(pattern = " and ", replacement = " & ", x = Country)) %>%
      mutate(Country = gsub(pattern = "[0-9]", replacement = "", x = Country))
    return(df)
  }

  # Capacity Data for Wind, Solar, and Geobiomass
  if (subtype == "Capacity") 
  {
    data_solar <- read_excel(filename, sheet = "Solar Capacity", range = "A4:Z72")
    data_solar <- tidy_data(data_solar, "Capacity|Solar (MW)")

    data_wind <- read_excel(filename, sheet = "Wind Capacity", range = "A4:AA70")
    data_wind <- tidy_data(data_wind, "Capacity|Wind (MW)")

    data_geothermal <- read_excel(filename, sheet = "Geothermal Capacity", range = "A4:AA43")
    data_geothermal <- tidy_data(data_geothermal, "Capacity|Geothermal (MW)")

    data <- merge_recurse(list(data_solar, data_wind, data_geothermal))
  }
  # Generation data for Nuclear, Hydro, Solar, Wind, Geobiomass, Other Renewables
  else if (subtype == "Generation") 
  {
    data_nuclear <- read_excel(filename, sheet = "Nuclear Generation - TWh", range = "A3:BE114")
    data_nuclear <- tidy_data(data_nuclear, "Generation|Nuclear (TWh)")

    data_hydro <- read_excel(filename, sheet = "Hydro Generation - TWh", range = "A3:BE114")
    data_hydro <- tidy_data(data_hydro, "Generation|Hydro (TWh)")

    data_solar <- read_excel(filename, sheet = "Solar Generation - TWh", range = "A3:BE114")
    data_solar <- tidy_data(data_solar, "Generation|Solar (TWh)")

    data_wind <- read_excel(filename, sheet = "Wind Generation - TWh", range = "A3:BE114")
    data_wind <- tidy_data(data_wind, "Generation|Wind (TWh)")

    data_elec <- read_excel(filename, sheet = "Electricity Generation ", range = "A3:AK113")
    data_elec <- tidy_data(data_elec, "Generation|Electricity (TWh)")

    data_elec_gas <- read_excel(filename, sheet = "Elec Gen from Gas", range = "A3:AK58")
    data_elec_gas <- tidy_data(data_elec_gas, "Generation|Electricity|Gas (TWh)")

    data_elec_oil <- read_excel(filename, sheet = "Elec Gen from Oil", range = "A3:AK58")
    data_elec_oil <- tidy_data(data_elec_oil, "Generation|Electricity|Oil (TWh)")

    data_elec_coal <- read_excel(filename, sheet = "Elec Gen from Coal", range = "A3:AK58")
    data_elec_coal <- tidy_data(data_elec_coal, "Generation|Electricity|Coal (TWh)")

    data_geo_biomass <- read_excel(filename, sheet = "Geo Biomass Other - TWh", range = "A3:BE114")
    data_geo_biomass <- tidy_data(data_geo_biomass, "Generation|Geo_biomass (TWh)")

    data <- merge_recurse(list(
      data_wind, data_solar, data_hydro, data_geo_biomass, data_nuclear,
      data_elec, data_elec_gas, data_elec_oil, data_elec_coal
    ))

    data <- filter(data, !grepl("\\.", data$Year))
  } 
  else if (subtype == "Production") 
  {
    data_oil <- read_excel(filename, sheet = "Oil Production - Tonnes", range = "A3:BE80")
    data_oil <- tidy_data(data_oil, "Oil Production (million t)")

    data_coal_ej <- read_excel(filename, sheet = "Coal Production - EJ", range = "A3:AO62")
    data_coal_ej <- tidy_data(data_coal_ej, "Coal Production (EJ)")

    data_coal_ton <- read_excel(filename, sheet = "Coal Production - Tonnes", range = "A3:AO62")
    data_coal_ton <- tidy_data(data_coal_ton, "Coal Production (t)")

    data_gas <- read_excel(filename, sheet = "Gas Production - EJ", range = "A3:AZ78")
    data_gas <- tidy_data(data_gas, "Gas Production (EJ)")

    # Includes crude oil, shale oil, oil sands, condensates (lease condensate or gas condensates that require
    # further refining) and NGLs (natural gas liquids - ethane, LPG and naphtha separated from the production of natural gas).
    data <- merge_recurse(list(data_oil, data_coal_ej, data_coal_ton, data_gas)) # merging all datasets into one
    data <- filter(data, !grepl("\\.", data$Year))
  } 
  else if (subtype == "Consumption")
  {
    data_pe_consumption <- read_excel(filename, sheet = "Primary Energy Consumption", range = "A3:BE114")
    data_pe_consumption <- tidy_data(data_pe_consumption, "Primary Energy Consumption (EJ)")

    data_liq_consumption <- read_excel(filename, sheet = "Total Liquids - Consumption", range = "A3:BE114")
    data_liq_consumption <- tidy_data(data_liq_consumption, "Liquids Consumption (kb/d)")

    data_oil_consumption <- read_excel(filename, sheet = "Oil Consumption - EJ", range = "A3:BE114")
    data_oil_consumption <- tidy_data(data_oil_consumption, "Oil Consumption (EJ)")

    data_gas_consumption <- read_excel(filename, sheet = "Gas Consumption - EJ", range = "A3:BE114")
    data_gas_consumption <- tidy_data(data_gas_consumption, "Gas Consumption (EJ)")

    data_coal_consumption <- read_excel(filename, sheet = "Coal Consumption - EJ", range = "A3:BE114")
    data_coal_consumption <- tidy_data(data_coal_consumption, "Coal Consumption (EJ)")

    data_solar_consumption <- read_excel(filename, sheet = "Solar Consumption - EJ", range = "A3:BE114")
    data_solar_consumption <- tidy_data(data_solar_consumption, "Solar Consumption (EJ)")

    data_wind_consumption <- read_excel(filename, sheet = "Wind Consumption - EJ", range = "A3:BE114")
    data_wind_consumption <- tidy_data(data_wind_consumption, "Wind Consumption (EJ)")

    data_nuclear_consumption <- read_excel(filename, sheet = "Nuclear Consumption - EJ", range = "A3:BE114")
    data_nuclear_consumption <- tidy_data(data_nuclear_consumption, "Nuclear Consumption (EJ)")

    data_hydro_consumption <- read_excel(filename, sheet = "Hydro Consumption - EJ", range = "A3:BE114")
    data_hydro_consumption <- tidy_data(data_hydro_consumption, "Hydro Consumption (EJ)")

    data <- merge_recurse(list(
      data_pe_consumption, data_liq_consumption, data_oil_consumption, data_gas_consumption,
      data_coal_consumption, data_solar_consumption, data_wind_consumption, data_nuclear_consumption,
      data_hydro_consumption
    ))
  } else if (subtype == "Trade") {
    
    # Oil Trade
    data_oil_trade <- read_excel(filename, sheet = "Oil - Trade movements", range = "A3:AP27")
    data_oil_trade_import <- tidy_data(data_oil_trade[seq(1, 8), ], "Trade|Import|Oil (kb/d)")
    data_oil_trade_export <- tidy_data(data_oil_trade[seq(9, 24), ], "Trade|Export|Oil (kb/d))")

    data_oil_trade_detail <- read_excel(filename, sheet = "Oil - Trade 2019 - 2020", range = "A28:I50")
    colnames(data_oil_trade_detail) <- c("Country", rep(c("Trade|Import|Oil|Crude (kb/d)", "Trade|Import|Oil|Product (kb/d)", 
                                                          "Trade|Export|Oil|Crude (kb/d)", "Trade|Export|Oil|Product (kb/d)"), times = 2))
    data_oil_trade_detail <- rbind(data_oil_trade_detail[, seq(1, 5)] %>% mutate(Year := 2019), data_oil_trade_detail[, c(1, seq(6, 9))] %>% mutate(Year := 2020)) %>% 
                                                  tidy_data_vertical()

    # Coal Trade
    data_coal_trade <- read_excel(filename, sheet = "Coal - Trade movements", range = "A3:V34")
    data_coal_trade_import <- tidy_data(data_coal_trade[seq(1, 15), ], "Trade|Import|Coal (EJ)")
    data_coal_trade_export <- tidy_data(data_coal_trade[seq(17, 31), ], "Trade|Export|Coal (EJ)")

    # Gas Trade
    data_gas_trade <- read_excel(filename, sheet = "Gas - Inter-regional trade", range = "A3:V105")
    colnames(data_gas_trade)[1] <- "Variable"
    variable_mapping <- {
      c(
        NA,
        NA,
        NA,
        "US|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "US|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        "Other North America|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "Other North America|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        "Brazil|Trade|Import|Gas (bcm)",
        NA,
        "Brazil|Trade|Export|Gas (bcm)",
        NA,
        NA,
        "Other S&C America|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "Other S&C America|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        "Europe|Trade|Import|Gas (bcm)",
        NA,
        "Europe|Trade|Export|Gas (bcm)",
        NA,
        NA,
        "Russia|Trade|Import|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        "Russia|Trade|Export|Gas (bcm)",
        NA,
        NA,
        "Other CIS|Trade|Import|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        NA,
        "Other CIS|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        NA,
        "Middle East|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "Middle East|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        "Africa|Trade|Import|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        "Africa|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        NA,
        "China|Trade|Import|Gas (bcm)",
        NA,
        "China|Trade|Export|Gas (bcm)",
        NA,
        NA,
        "India|Trade|Import|Gas (bcm)",
        NA,
        "India|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        "OECD Asia|Trade|Import|Gas (bcm)",
        NA,
        "OECD Asia|Trade|Export|Gas (bcm)",
        NA,
        NA,
        "Other Asia|Trade|Import|Gas (bcm)",
        NA,
        NA,
        NA,
        NA,
        "Other Asia|Trade|Export|Gas (bcm)",
        NA,
        NA,
        NA,
        NA
      )
    }
    data_gas_trade$Variable <- variable_mapping
    data_gas_trade <- filter(data_gas_trade, !is.na(!!sym("Variable"))) %>%
      mutate(
        !!sym("Country") := sub("\\|.*", "", !!sym("Variable")),
        !!sym("Variable") := sub(".*\\|Trade", "\\Trade", !!sym("Variable")),
      ) %>%
      reshape2::melt(id.vars = c("Variable", "Country")) %>%
      reshape2::dcast(Country + variable ~ Variable, value.var = "value")
    colnames(data_gas_trade)[2] <- "Year"
    data_gas_trade <- tidy_data_vertical(data_gas_trade)

    data <- merge_recurse(list(
      data_oil_trade_import, data_oil_trade_export, data_oil_trade_detail,
      data_coal_trade_import, data_coal_trade_export, data_gas_trade
    ))
  } else if (subtype == "Price") {
    data_oil_spot_crude_price <- read_excel(filename, sheet = "Oil - Spot crude prices", range = "A4:E54")
    colnames(data_oil_spot_crude_price) <- c("Year", "Price|Oil|Dubai ($/bbl)", "Price|Oil|Brent ($/bbl)", "Price|Oil|Nigerian Forcados ($/bbl)", "Price|Oil|Western Texas Intermediate ($/bbl)")
    data_oil_spot_crude_price <- filter(data_oil_spot_crude_price, !is.na(Year))

    data_oil_crude_price <- read_excel(filename, sheet = "Oil - Crude prices since 1861", range = "A4:C164")
    colnames(data_oil_crude_price) <- c("Year", "Price|Oil ($ money of the day)", "Price ($ 2020)")

    data_gas_price <- read_excel(filename, sheet = "Gas - Prices ", range = "A5:H42")
    colnames(data_gas_price) <- c(
      "Year",
      "Price|LNG|Japan|CIF ($/mbtu)",
      "Price|LNG|Japan|Korea Marker ($/mbtu)",
      "Price|Natural Gas|Avg German Import Price ($/mbtu)",
      "Price|Natural Gas|UK Heren NBP Index ($/mbtu)",
      "Price|Natural Gas|Netherlands TTF DA Heren Index ($/mbtu)",
      "Price|Natural Gas|US Henry Hub ($/mbtu)",
      "Price|Natural Gas|Alberta ($/mbtu)"
    )

    data_coal_price <- read_excel(filename, sheet = "Coal - Prices", range = "A2:H37")
    colnames(data_coal_price) <- c(
      "Year",
      "Price|Coal|Northwest Europe marker price ($/t)",
      "Price|Coal|US Central Appalachian coal spot price index ($/t)",
      "Price|Coal|Japan steam spot CIF price ($/t)",
      "Price|Coal|China Qinhuangdao spot price ($/t)",
      "Price|Coal|Japan coking coal import CIF price (t/$)",
      "Price|Coal|Japan steam coal import CIF price (t/$)",
      "Price|Coal|Asian marker price (t/$)"
    )
    data_coal_price <- filter(data_coal_price, !is.na(Year))

    data <- merge_recurse(list(data_oil_spot_crude_price, data_oil_crude_price, data_gas_price, data_coal_price))
  
  } else {
    stop("Not a valid subtype!")
  }

  x <- as.magpie(data, temporal = 2, spatial = 1, datacol = 3)
  x <- magpiesort(x)

  return(x)
}
