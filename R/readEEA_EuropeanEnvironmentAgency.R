#' Read European Environment Agency (EEA) data
#'
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#'
#'
#' @param subtype data subtype. Either "ETS", "ESR", "total", "sectoral", or "projections"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2)
#' @author Renato Rodrigues, Falk Benke
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "EEA_EuropeanEnvironmentAgency", subtype = "ETS")
#' }
#'
#' @importFrom dplyr left_join select filter mutate
#' @importFrom magclass as.magpie
#' @importFrom quitte calc_addVariable
#' @importFrom readxl read_excel excel_sheets
#' @importFrom reshape2 melt
#' @importFrom madrat toolCountry2isocode

readEEA_EuropeanEnvironmentAgency <- function(subtype) {
  if (subtype == "ETS") {
    #2020 data
    data <- read.csv("ETS_Database_v44.csv", sep = "\t")
    # 2019 data
    #data <- read.csv("ETS_Database_v38.csv", sep = "\t")
    data <- data[, -c(2, 5)]
    data$year <- as.numeric(as.character(data$year))
    data <- data[(!(is.na(data$year))), ]
    colnames(data) <- c("region", "ETS_info", "sector", "value", "period")
    data$region <- toolCountry2isocode(data$region, warn = F)
    data <- data[(!(is.na(data$region))), ]
    data$ETS_info <- gsub(pattern = "\\.", replacement = "_", data$ETS_info)
    data$sector <- gsub(pattern = "\\.", replacement = "", data$sector)
    data$value <- as.numeric(gsub(" ", "", data$value)) / 1000000
    data <- data[, c(1, 5, 2, 3, 4)]
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 5)
  }
  else if (subtype == "ESR") {
    # 2020 data
    data <- read_excel(path = "EEA_GHG_ESD_Dec 2020.xlsx", trim_ws = T)
    data <- data[, c(1:3)]
    # 2019 data
    # data <- read_excel(path = "ESD-GHG_2019_revised.xlsx", trim_ws = T, skip = 11)
    # data <- data[, c(1, 17:30)]
    # data <- melt(data, id.vars = 1)
    colnames(data) <- c("region", "period", "value")
    data$region <- toolCountry2isocode(data$region, warn = F)
    data <- data[(!(is.na(data$region))), ]
    data$variable <- "Emi|GHG|ESR (Mt CO2-equiv/yr)"
    data <- data[, c(1, 2, 4, 3)]
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 4)
  }
  else if (subtype == "total") {
    data <- read_excel(path = "GHG_Total_historical.xlsx", trim_ws = T)
    data$...1 <- NULL
    eur <- c(
      "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL",
      "ITA", "LVA", "LIE", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE",
      "CHE", "TUR", "GBR"
    )
    colnames(data) <- c("year", eur)
    data <- melt(data, id.vars = 1)
    colnames(data) <- c("year", "region", "value")
    data <- cbind(data, variable = c("Emi|GHGtot"))
    data$variable <- paste0(data$variable, sep = " ", "(Mt CO2-equiv/yr)")
    data <- data[, c(1, 2, 4, 3)]
    x <- as.magpie(data, spatial = 2, temporal = 1, datacol = 4)
  }
  else if (subtype == "sectoral") {
    sheets <- excel_sheets("GHG_ETS_ES_Projections_by_sector.xlsx")
    historical <- NULL
    timeframe <- seq(2005, 2017) # excluding WEM projections

    for (s in sheets) {
      tmp <- suppressMessages(read_excel(path = "GHG_ETS_ES_Projections_by_sector.xlsx", sheet = s, skip = 1, trim_ws = T))
      tmp <- melt(tmp, id.vars = 1)
      tmp <- mutate(tmp, !!sym("value") := ifelse(is.na(!!sym("value")), 0, !!sym("value"))) # set 0s for NAs
      colnames(tmp) <- c("label", "period", "value")
      tmp <- cbind(tmp[!is.na(tmp$value) & tmp$period %in% timeframe, ], region = s)
      historical <- rbind(historical, tmp)
    }

    mapping.variable <- as.data.frame(
      cbind(
        variable=c(
          "Emi|GHG|ETS",
          "Emi|GHG|Energy|ETS",
          "Emi|GHG|Industry|ETS",
          "Emi|GHG|ESR",
          "Emi|GHG|Transport|ESR",
          "Emi|GHG|Buildings|ESR",
          "Emi|GHG|Industry|ESR",
          "Emi|GHG|Agriculture|ESR",
          "Emi|GHG|Waste|ESR"
        ),
        label=c(
          "Emissions Trading System (stationary installations)",
          "Energy Industries",
          "Other stationary installations",
          "Effort Sharing Decision and Regulation",
          "Transport",
          "Buildings",
          "Industry and other",
          "Agriculture",
          "Waste"
        )
      )
    )

    historical <- left_join(mapping.variable, historical, by = c("label"))
    historical$variable <- paste0(historical$variable, sep = " ", "(Mt CO2-equiv/yr)")
    historical$value <- as.double(historical$value)
    historical$label <- NULL
    historical <- historical[, c(1, 4, 2, 3)]
    x <- as.magpie(historical, spatial = 2, datacol = 4, temporal = 3)
  }
  else if (subtype == "projections_old") {

    mapping <- toolGetMapping(type = "sectoral", name = "mappingEEAGHGProjections2019.csv")

    projections <- read.csv(file = "GHG_projections/GHG_projections_2019_EEA.csv", stringsAsFactors = FALSE, strip.white = TRUE) %>%
      filter(!!sym("CountryCode") != "", !!sym("CountryCode") != "EU", !!sym("Gapfilled") != as.double(0), !is.na(!!sym("Gapfilled"))) %>%
      select("CountryCode","Year","Category_name","Scenario","Gas","Gapfilled")

    projections <- left_join(mapping, projections, by = c("Category_name", "Gas")) %>%
      select("scenario" = "Scenario", "variable" = "Variable", "region" = "CountryCode", "period" = "Year", "value" = "Gapfilled") %>%
      calc_addVariable(
        "`Emi|GHG|Industry|ETS`" = "`Emi|GHG|Industrial Processes|ETS` + `Emi|GHG|Demand|Industry|Energy|ETS`",
        "`Emi|GHG|Industry|ESR`" = "`Emi|GHG|Industrial Processes|ESR` + `Emi|GHG|Demand|Industry|Energy|ESR`",
        "`Emi|GHG|Industry`" = "`Emi|GHG|Industry|ETS` + `Emi|GHG|Industry|ESR`",
        completeMissing = F
      ) %>%
      filter(!is.na(!!sym("scenario")), !is.na(!!sym("value"))) %>%
      mutate(
        !!sym("value") := !!sym("value") / 1000,
        !!sym("scenario") := paste0("EEA_", !!sym("scenario"), "_2019"),
        !!sym("variable") :=  paste0(!!sym("variable"), " (Mt CO2-equiv/yr)")
      )

    projections <- projections[(!(is.na(projections$region))), ]
    projections <- projections[(!(is.na(projections$period))), ]

    x <- as.magpie(projections, spatial = 3, temporal = 4, datacol = 5)

    return(x)
  }
  else if (subtype == "projections") {

    mapping <- toolGetMapping(type = "sectoral", name = "mappingEEAGHGProjections2021.csv")

    projections <- read.csv(file = "GHG_projections/GHG_projections_2021_EEA.csv", stringsAsFactors = FALSE, strip.white = TRUE) %>%
      filter(!!sym("CountryCode") != "", !!sym("CountryCode") != "EU", !!sym("Final.Gap.filled") != as.double(0), !is.na(!!sym("Final.Gap.filled"))) %>%
      select("CountryCode", "Year", "Category", "Scenario", "Gas", "Gapfilled" = "Final.Gap.filled")

    projections <- left_join(mapping, projections, by = c("Category", "Gas")) %>%
      select("scenario" = "Scenario", "variable" = "Variable", "region" = "CountryCode", "period" = "Year", "value" = "Gapfilled") %>%
      calc_addVariable(
        "`Emi|GHG|Industry|ETS`" = "`Emi|GHG|Industrial Processes|ETS` + `Emi|GHG|Demand|Industry|Energy|ETS`",
        "`Emi|GHG|Industry|ESR`" = "`Emi|GHG|Industrial Processes|ESR` + `Emi|GHG|Demand|Industry|Energy|ESR`",
        "`Emi|GHG|Industry`" = "`Emi|GHG|Industry|ETS` + `Emi|GHG|Industry|ESR`",
        "`Emi|GHG|Intl aviation in ETS|ETS`" = "`Emi|GHGtot|w/ Intl aviation` - `Emi|GHGtot`",
        completeMissing = F
      ) %>%
      filter(!is.na(!!sym("scenario")), !is.na(!!sym("value"))) %>%
      mutate(
        !!sym("value") := !!sym("value") / 1000,
        !!sym("scenario") := paste0("EEA_", !!sym("scenario"), "_2021"),
        !!sym("variable") := paste0(!!sym("variable"), " (Mt CO2-equiv/yr)")
      )
    x <- as.magpie(projections, spatial = 3, temporal = 4, datacol = 5)

    return(x)
  }
  else {
    stop("Not a valid subtype!")
  }

  return(x)
}
