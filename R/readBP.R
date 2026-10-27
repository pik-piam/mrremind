#' BP Capacity and Generation Data
#'
#' @param subtype Either "Emission", Capacity", "Generation", "Production",
#' "Consumption", "Trade Oil", "Trade Gas", "Trade Coal" or "Price"
#' @return A [`magpie`][magclass::magclass] object.
#' @author Aman Malik, Falk Benke, Pascal Weigmann
readBP <- function(subtype) {

  filename <- file.path("2026", "EI-Stats-Review-ALL-data.xlsx")

  tidyData <- function(df, variable, rows2remove = c("Total|OECD|European")) {
    colnames(df)[1] <- "Country"
    df$Country <- trimws(gsub("\\.", "", df$Country))
    df <- df %>%
      # columns containing "n/a" are read as character, so they need to be converted
      # before they can be combined with the numeric ones
      tidyr::pivot_longer(colnames(df[1, -1]), names_to = "Year", values_to = "value",
                          values_transform = list(value = function(x) suppressWarnings(as.numeric(x)))) %>%
      dplyr::mutate("Year" = as.integer(.data$Year)) %>%
      dplyr::filter(!grepl(rows2remove, .data$Country), !is.na(.data$value),
                    .data$value != "n/a", .data$Year >= 1900) %>%
      dplyr::mutate("Country" = gsub(pattern = " and ", replacement = " & ", x = .data$Country)) %>%
      dplyr::mutate("Country" = gsub(pattern = "[0-9]", replacement = "", x = .data$Country))

    colnames(df)[3] <- variable

    return(df)
  }

  tidyDataVertical <- function(df, rows2remove = c("Total|OECD|European")) {
    df$Country <- trimws(gsub("\\.", "", df$Country))
    if (!is.null(rows2remove)) {
      df <- dplyr::filter(df, !grepl(rows2remove, .data$Country))
    }
    df <- df %>%
      dplyr::mutate("Year" = suppressWarnings(as.integer(as.character(.data$Year)))) %>%
      dplyr::filter(.data$Year > 1990) %>%
      dplyr::mutate("Country" = gsub(pattern = " and ", replacement = " & ", x = .data$Country)) %>%
      dplyr::mutate("Country" = gsub(pattern = "[0-9]", replacement = "", x = .data$Country))
    return(df)
  }

  if (subtype == "Emission") {

    dataEmi <- readxl::read_excel(filename, sheet = "CO2 from Energy", range = "A3:BJ114")
    data <- tidyData(dataEmi, "Emi|CO2 (Mt CO2)")

  } else if (subtype == "Capacity") {
    # Capacity Data for Wind, Solar, and Geobiomass
    dataSolar <- readxl::read_excel(filename, sheet = "Solar Installed Capacity", range = "A4:AA78")
    dataSolar <- tidyData(dataSolar, "Capacity|Solar (MW)")

    dataWind <- readxl::read_excel(filename, sheet = "Wind Installed Capacity", range = "A4:AA77")
    dataWind <- tidyData(dataWind, "Capacity|Wind (MW)")

    data <- list(dataSolar, dataWind) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Generation") {
    # Generation data for Nuclear, Hydro, Solar, Wind, Geobiomass, Other Renewables

    dataNuclear <- readxl::read_excel(filename, sheet = "Nuclear Generation - TWh", range = "A3:BJ114")
    dataNuclear <- tidyData(dataNuclear, "Generation|Nuclear (TWh)")

    dataHydro <- readxl::read_excel(filename, sheet = "Hydro Generation - TWh", range = "A3:BJ114")
    dataHydro <- tidyData(dataHydro, "Generation|Hydro (TWh)")

    dataSolar <- readxl::read_excel(filename, sheet = "Solar Generation - TWh", range = "A3:BJ114")
    dataSolar <- tidyData(dataSolar, "Generation|Solar (TWh)")

    dataWind <- readxl::read_excel(filename, sheet = "Wind Generation - TWh", range = "A3:BJ114")
    dataWind <- tidyData(dataWind, "Generation|Wind (TWh)")

    dataElec <- readxl::read_excel(filename, sheet = "Electricity Generation - TWh", range = "A3:AP113")
    dataElec <- tidyData(dataElec, "Generation|Electricity (TWh)")

    dataElectRenewable <- readxl::read_excel(filename, sheet = "Renewable Power (inc hydro) -EJ", range = "A3:BJ114")
    dataElectRenewable <- tidyData(dataElectRenewable, "Generation|Electricity|Renewable (EJ)")

    dataElecGas <- readxl::read_excel(filename, sheet = "Gas inputs - Elec generation", range = "A3:AP59")
    dataElecGas <- tidyData(dataElecGas, "Generation|Electricity|Gas (TWh)")

    dataElecOil <- readxl::read_excel(filename, sheet = "Oil inputs - Elec generation ", range = "A3:AP59")
    dataElecOil <- tidyData(dataElecOil, "Generation|Electricity|Oil (TWh)")

    dataElecCoal <- readxl::read_excel(filename, sheet = "Coal inputs - Elec generation ", range = "A3:AP59")
    dataElecCoal <- tidyData(dataElecCoal, "Generation|Electricity|Coal (TWh)")

    dataGeoBiomass <- readxl::read_excel(filename, sheet = "Geo Biomass Other - TWh", range = "A3:BJ114")
    dataGeoBiomass <- tidyData(dataGeoBiomass, "Generation|Geo_biomass (TWh)")

    data <- list(
      dataWind, dataSolar, dataHydro, dataGeoBiomass, dataNuclear,
      dataElec, dataElectRenewable, dataElecGas, dataElecOil, dataElecCoal
    ) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Production") {

    # This part is currently not used in any other madrat function

    dataOil <- readxl::read_excel(filename, sheet = "Oil Production - tonnes", range = "A3:BJ81")
    dataOil <- tidyData(dataOil, "Oil Production (million t)")

    dataCoalEj <- readxl::read_excel(filename, sheet = "Coal Production - EJ", range = "A3:AT62")
    dataCoalEj <- tidyData(dataCoalEj, "Coal Production (EJ)")

    dataCoalTon <- readxl::read_excel(filename, sheet = "Coal Production - mt", range = "A3:AT62")
    dataCoalTon <- tidyData(dataCoalTon, "Coal Production (million t)")

    dataGas <- readxl::read_excel(filename, sheet = "Gas Production - EJ", range = "A3:BE78")
    dataGas <- tidyData(dataGas, "Gas Production (EJ)")

    # Includes crude oil, shale oil, oil sands, condensates (lease condensate or gas condensates that require
    # further refining) and NGLs (natural gas liquids - ethane, LPG and naphtha separated from the production of
    # natural gas).
    data <- list(dataOil, dataCoalEj, dataCoalTon, dataGas) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Consumption") {

    dataPeConsumption <- readxl::read_excel(filename, sheet = "Total Energy Supply (TES) -EJ", range = "A3:BJ114")
    dataPeConsumption <- tidyData(dataPeConsumption, "Primary Energy Consumption (EJ)")

    dataLiqConsumption <- readxl::read_excel(filename, sheet = "Liquids Consumption - barrels", range = "A3:BJ114")
    dataLiqConsumption <- tidyData(dataLiqConsumption, "Liquids Consumption (kb/d)")

    dataOilConsumption <- readxl::read_excel(filename, sheet = "Oil Consumption - EJ", range = "A3:BJ114")
    dataOilConsumption <- tidyData(dataOilConsumption, "Oil Consumption (EJ)")

    dataGasConsumption <- readxl::read_excel(filename, sheet = "Gas Consumption - EJ", range = "A3:BJ111")
    dataGasConsumption <- tidyData(dataGasConsumption, "Gas Consumption (EJ)")

    dataCoalConsumption <- readxl::read_excel(filename, sheet = "Coal Consumption - EJ", range = "A3:BJ114")
    dataCoalConsumption <- tidyData(dataCoalConsumption, "Coal Consumption (EJ)")

    dataSolarConsumption <- readxl::read_excel(filename, sheet = "Solar Consumption - EJ", range = "A3:BJ114")
    dataSolarConsumption <- tidyData(dataSolarConsumption, "Solar Consumption (EJ)")

    dataWindConsumption <- readxl::read_excel(filename, sheet = "Wind Consumption - EJ", range = "A3:BJ114")
    dataWindConsumption <- tidyData(dataWindConsumption, "Wind Consumption (EJ)")

    dataNuclearConsumption <- readxl::read_excel(filename, sheet = "Nuclear Consumption - EJ", range = "A3:BJ114")
    dataNuclearConsumption <- tidyData(dataNuclearConsumption, "Nuclear Consumption (EJ)")

    dataHydroConsumption <- readxl::read_excel(filename, sheet = "Hydro Consumption - EJ", range = "A3:BJ114")
    dataHydroConsumption <- tidyData(dataHydroConsumption, "Hydro Consumption (EJ)")

    data <- list(
      dataPeConsumption, dataLiqConsumption, dataOilConsumption, dataGasConsumption,
      dataCoalConsumption, dataSolarConsumption, dataWindConsumption, dataNuclearConsumption,
      dataHydroConsumption
    ) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Trade Oil") {

    dataOilTrade <- readxl::read_excel(filename, sheet = "Oil trade movements", range = "A3:AU26")

    dataOilTradeImport <- tidyData(dataOilTrade[seq(1, 8), ], "Trade|Import|Oil (kb/d)")
    dataOilTradeExport <- tidyData(dataOilTrade[seq(9, 23), ], "Trade|Export|Oil (kb/d)")

    dataOilTradeDetail <- readxl::read_excel(filename, sheet = "Oil - Trade movements in 24-25",
                                             range = "A28:I50", .name_repair = "unique_quiet")

    colnames(dataOilTradeDetail) <- c("Country", rep(c(
      "Trade|Import|Oil|Crude (kb/d)", "Trade|Import|Oil|Product (kb/d)",
      "Trade|Export|Oil|Crude (kb/d)", "Trade|Export|Oil|Product (kb/d)"
    ), times = 2))

    dataOilTradeDetail <- rbind(
      dataOilTradeDetail[, seq(1, 5)] %>% dplyr::mutate("Year" = 2024),
      dataOilTradeDetail[, c(1, seq(6, 9))] %>% dplyr::mutate("Year" = 2025)
    ) %>%
      tidyDataVertical()

    data <- list(dataOilTradeImport, dataOilTradeExport, dataOilTradeDetail) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Trade Coal") {

    dataCoalTrade <- readxl::read_excel(filename, sheet = "Coal - Trade movements", range = "A3:AA34")

    dataCoalTradeImport <- tidyData(dataCoalTrade[seq(1, 15), ], "Trade|Import|Coal (EJ)",
      rows2remove = c("Total|OECD|European|Rest")
    )

    dataCoalTradeExport <- tidyData(dataCoalTrade[seq(17, 31), ], "Trade|Export|Coal (EJ)",
      rows2remove = c("Total|OECD|European|Rest")
    )

    data <- list(dataCoalTradeImport, dataCoalTradeExport) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = c("Country", "Year")))

  } else if (subtype == "Trade Gas") {

    dataGasTrade <- readxl::read_excel(filename, sheet = "Gas - Trade movements", range = "A3:AA107")
    colnames(dataGasTrade)[1] <- "Variable"
    variableMapping <- {
      c(
        NA,
        NA,
        NA,
        "USA|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "USA|Trade|Export|Gas (bcm)",
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
        "Other S & C America|Trade|Import|Gas (bcm)",
        NA,
        NA,
        "Other S & C America|Trade|Export|Gas (bcm)",
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
    dataGasTrade$Variable <- variableMapping
    dataGasTrade <- dplyr::filter(dataGasTrade, !is.na(.data$Variable)) %>%
      dplyr::mutate(
        Country = sub("\\|.*", "", .data$Variable),
        Variable = sub(".*\\|Trade", "\\Trade", .data$Variable)
      ) %>%
      reshape2::melt(id.vars = c("Variable", "Country"), variable.name = "Year") %>%
      reshape2::dcast(Country + Year ~ Variable, value.var = "value")

    data <- tidyDataVertical(dataGasTrade, rows2remove = NULL)

  } else if (subtype == "Price") {

    dataOilSpotCrudePrice <- readxl::read_excel(filename, sheet = "Spot crude prices", range = "A4:E58")

    colnames(dataOilSpotCrudePrice) <- c(
      "Year",
      "Price|Oil|Dubai ($/bbl)",
      "Price|Oil|Brent ($/bbl)",
      "Price|Oil|Nigerian Forcados ($/bbl)",
      "Price|Oil|Western Texas Intermediate ($/bbl)"
    )

    dataOilSpotCrudePrice <- dplyr::filter(dataOilSpotCrudePrice, !is.na(.data$Year))

    dataOilCrudePrice <- readxl::read_excel(filename, sheet = "Oil crude prices since 1861", range = "A4:C169")

    colnames(dataOilCrudePrice) <- c(
      "Year",
      "Price|Crude Oil ($money of the day/bbl)",
      "Price|Crude Oil ($2025/bbl)"
    )

    # the sheet was restructured: the series "Avg German Import Price" and
    # "Alberta" are no longer reported, additional markers were added
    dataGasPrice <- readxl::read_excel(filename, sheet = "Gas Prices ", range = "A4:K48",
                                         .name_repair = "unique_quiet")
    colnames(dataGasPrice) <- c(
      "Year",
      "Price|LNG|Japan|CIF ($/mbtu)",
      "Price|LNG|South Korea ($/mbtu)",
      "Price|LNG|Japan|Korea Marker ($/mbtu)",
      "Price|LNG|China ($/mbtu)",
      "Price|LNG|West India Marker ($/mbtu)",
      "Price|LNG|Northwest Europe Marker ($/mbtu)",
      "Price|Natural Gas|Zeebrugge ($/mbtu)",
      "Price|Natural Gas|UK Heren NBP Index ($/mbtu)",
      "Price|Natural Gas|Netherlands TTF DA Heren Index ($/mbtu)",
      "Price|Natural Gas|US Henry Hub ($/mbtu)"
    )

    dataGasPrice <- dplyr::filter(dataGasPrice, !is.na(.data$Year))

    dataCoalPrice <- readxl::read_excel(filename, sheet = "Coal & Uranium - Prices", range = "A4:I43",
                                        .name_repair = "unique_quiet")

    colnames(dataCoalPrice) <- c(
      "Year",
      "Price|Coal|United States ($/t)",
      "Price|Coal|Colombia ($/t)",
      "Price|Coal|Northwest Europe ($/t)",
      "Price|Coal|South Africa ($/t)",
      "Price|Coal|Indonesia ($/t)",
      "Price|Coal|South China ($/t)",
      "Price|Coal|Japan ($/t)",
      "Price|Coal|Australia ($/t)"
    )

    dataCoalPrice <- dplyr::filter(dataCoalPrice, !is.na(.data$Year))

    data <- list(dataOilSpotCrudePrice, dataOilCrudePrice, dataGasPrice, dataCoalPrice) %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = "Year"))

    data[-1] <- lapply(data[-1], function(x) {
      suppressWarnings(as.numeric(x))
    })
    data <- cbind(Country = "GLO", data)
  } else {
    stop("Not a valid subtype!")
  }

  x <- as.magpie(data, temporal = 2, spatial = 1, datacol = 3)
  x <- magpiesort(x)

  return(x)
}
