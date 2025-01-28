#' Read IEA World Energy Outlook data
#'
#' @description Read-in IEA WEO 2016 data for investment costs and O&M costs
#' of different technologies, and WEO 2017 data for historical
#' electricity capacities (GW), generation (TWh) or emissions (Mt CO2).
#' WEO 2019 data for PE and FE (Mtoe).
#' @param subtype data subtype. Either "Capacity", "Generation", "Emissions",
#' "Investment Costs", or "O&M Costs"
#' @return magpie object of the WEO data on generation (TWh), capacities (GW),
#' emissions (Mt CO2) or disaggregated investment cost as magpie object
#' @author Renato Rodrigues, Aman Malik, and Jerome Hilaire
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "WEO", subtype = "Capacity")
#' }
#'
#' @importFrom tidyr gather
#' @importFrom readxl read_excel

readIEA_WEO <- function(subtype) {

  period <- NULL
  value <- NULL
  variable <- NULL
  model <- NULL
  scenario <- NULL
  region <- NULL
  unit <- NULL

  if (subtype == "Invest_Costs" || subtype == "O&M_Costs") {
    # read WEO 2016 input files- values are from the  New Policy  scenario,
    # except for CCS costs which are from 450 scenario.
    # Coal
    input_coal <- read.csv(file = "WEO_2016-coal.csv", na.strings = "n.a.", stringsAsFactors = FALSE)
    input_coal$maintech <- "Coal"
    # Gas
    input_gas <-  read.csv(file = "WEO_2016-gas.csv", na.strings = "n.a.", stringsAsFactors = FALSE)
    input_gas$maintech <- "Gas"
    # Tech with CCS
    input_ccs <- read.csv(file = "WEO_2016-ccs.csv", na.strings = "n.a.", stringsAsFactors = FALSE)
    input_ccs$maintech <- "CCS"
    # Tech with NUC
    input_nuc <- read.csv(file = "WEO_2016-nuclear.csv", na.strings = "n.a.", stringsAsFactors = FALSE)
    input_nuc$maintech <- "Nuclear"
    # Tech with REN
    input_ren <-  read.csv(file = "WEO_2016-ren.csv", na.strings = "n.a.", stringsAsFactors = FALSE)
    input_ren$maintech <- "Renewables"
    # Special case for hydro, values are taken not from the IEA database.
    input_hydro <- read.csv(file = "hydro.csv", stringsAsFactors = FALSE)
    # removing 2025 values
    input_hydro <- input_hydro %>% select(-5)

    # 2040 values same as 2030 values
    input_hydro$X2040 <- input_hydro$X2030
    input_hydro <- cbind(input_hydro, input_ren[1:12, 7:14])
    input_hydro[, 7:14] <- 0
    input_hydro$maintech <- "Hydro_2"
    input_all <- bind_rows(input_coal, input_gas, input_ccs, input_nuc, input_ren, input_hydro)

    if (subtype == "Invest_Costs") {
      input <- input_all[, c(15, 1:6)]
      colnames(input)[4:7] <- c(2015, 2020, 2030, 2040)
      input <- input %>%
        gather(4:7, key = "Year", value = "costs")

      x <- as.magpie(input, spatial = 3, temporal = 4, datacol = 5)
    } else if (subtype == "O&M_Costs") {
      input <- input_all[, c(15, 1:2, 7:10)]
      colnames(input)[4:7] <- c(2015, 2020, 2030, 2040)
      input <- input %>%
        gather(4:7, key = "Year", value = "costs")

      x <- as.magpie(input, spatial = 3, temporal = 4, datacol = 5)
    }

  } else if ((subtype == "Capacity") || (subtype == "Generation") || (subtype == "Emissions")) {

    if (subtype == "Capacity") {
      data <- read.csv("WEO_2017/WEO-capacity.csv", sep = ";")[, c(2, 3, 4, 5)]
    } else if (subtype == "Generation") {
      data <- read.csv("WEO_2017/WEO-generation.csv", sep = ";")[, c(2, 3, 4, 5)]
    } else if (subtype == "Emissions") {
      data <- read.csv("WEO_2017/WEO-EmiCO2.csv", sep = ";")[, c(2, 3, 4, 5)]
    }

    # creating capacity, generation or emissions magpie object
    x <- as.magpie(data, temporal = 2, spatial = 1, datacol = 4)

  } else if ((subtype == "PE") || (subtype == "FE")) {
    # read-in sheet names from the excel file
    data_weo2019_sheets <- readxl::excel_sheets("WEO2019_AnnexA.xlsx")

    sheets_balance <- grep("Balance", data_weo2019_sheets, value = TRUE)

    tmp_all <- list()
    for (ksheet in sheets_balance) {
      kreg <- substr(ksheet, 1, nchar(ksheet) - nchar("_Balance"))
      tmp_sps <- readxl::read_excel("WEO2019_AnnexA.xlsx", sheet = ksheet, range = "A5:H56", .name_repair = "unique_quiet")
      tmp_cps <- readxl::read_excel("WEO2019_AnnexA.xlsx", sheet = ksheet, range = "M5:Q56", .name_repair = "unique_quiet")
      tmp_sds <- readxl::read_excel("WEO2019_AnnexA.xlsx", sheet = ksheet, range = "U5:X56", .name_repair = "unique_quiet")

      names(tmp_sps)[1] <- "variable"
      names(tmp_cps)[1] <- "variable"

      tmp_sds <- cbind(
        tmp_cps[, 1],
        tmp_sds
      )

      # Primary energy demand
      tmp1 <- tmp_sps[which(tmp_sps[, 1] == "Total primary demand"):(which(tmp_sps[, 1] == "Power sector") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
        mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
        mutate(variable = paste0("Primary Energy|", variable)) %>%
        mutate(variable = ifelse(variable == "Primary Energy|Total primary demand", "Primary Energy", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select("model", "scenario", "region", "variable", "unit", "period", "value")

      tmp1 <- rbind(
        tmp1,
        rbind(
          tmp1 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Total primary demand"):(which(tmp_sps[, 1] == "Power sector") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Primary Energy|", variable)) %>%
            mutate(variable = ifelse(variable == "Primary Energy|Total primary demand", "Primary Energy", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp1 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Total primary demand"):(which(tmp_sps[, 1] == "Power sector") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Primary Energy|", variable)) %>%
            mutate(variable = ifelse(variable == "Primary Energy|Total primary demand", "Primary Energy", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))

      # Power sector
      tmp2 <- tmp_sps[which(tmp_sps[, 1] == "Power sector"):(which(tmp_sps[, 1] == "Other energy sector") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
        # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
        mutate(variable = paste0("Primary Energy|Electricity|", variable)) %>%
        mutate(variable = ifelse(variable == "Primary Energy|Electricity|Power Sector", "Primary Energy|Electricity", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)
      tmp2 <- rbind(
        tmp2,
        rbind(
          tmp2 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Power sector"):(which(tmp_sps[, 1] == "Other energy sector") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Primary Energy|Electricity|", variable)) %>%
            mutate(variable = ifelse(variable == "Primary Energy|Electricity|Power Sector", "Primary Energy|Electricity", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp2 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Power sector"):(which(tmp_sps[, 1] == "Other energy sector") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Primary Energy|Electricity|", variable)) %>%
            mutate(variable = ifelse(variable == "Primary Energy|Electricity|Power Sector", "Primary Energy|Electricity", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))


      # Final energy consumption
      tmp3 <- tmp_sps[which(tmp_sps[, 1] == "Total final consumption"):(which(tmp_sps[, 1] == "Industry") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
        # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
        mutate(variable = paste0("Final Energy|", variable)) %>%
        mutate(variable = ifelse(variable == "Final Energy|Total final consumption", "Final Energy", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)
      tmp3 <- rbind(
        tmp3,
        rbind(
          tmp3 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Total final consumption"):(which(tmp_sps[, 1] == "Industry") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Total final consumption", "Final Energy", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp3 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Total final consumption"):(which(tmp_sps[, 1] == "Industry") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Total final consumption", "Final Energy", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))

      # Final energy consumption (Industry)
      tmp4 <- tmp_sps[which(tmp_sps[, 1] == "Industry"):(which(tmp_sps[, 1] == "Transport") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
        mutate(variable = paste0("Final Energy|Industry|", variable)) %>%
        mutate(variable = ifelse(variable == "Final Energy|Industry|Industry", "Final Energy|Industry", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)

      tmp4 <- rbind(
        tmp4,
        rbind(
          tmp4 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Industry"):(which(tmp_sps[, 1] == "Transport") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|Industry|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Industry|Industry", "Final Energy|Industry", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp4 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Industry"):(which(tmp_sps[, 1] == "Transport") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Natural gas", "Gas", variable)) %>%
            # mutate(variable = ifelse(variable == "Bioenergy", "Biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|Industry|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Industry|Industry", "Final Energy|Industry", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))

      # Final energy consumption (Transport)
      tmp5 <- tmp_sps[which(tmp_sps[, 1] == "Transport"):(which(tmp_sps[, 1] == "Buildings") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "International bunkers", "Oil|International bunkers", variable)) %>%
        mutate(variable = paste0("Final Energy|Transportation|", variable)) %>%
        mutate(variable = ifelse(variable == "Final Energy|Transportation|Transport", "Final Energy|Transportation", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)

      tmp5 <- rbind(
        tmp5,
        rbind(
          tmp5 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Transport"):(which(tmp_sps[, 1] == "Buildings") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "International bunkers", "Oil|International bunkers", variable)) %>%
            mutate(variable = paste0("Final Energy|Transportation|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Transportation|Transport", "Final Energy|Transportation", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp5 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Transport"):(which(tmp_sps[, 1] == "Buildings") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "International bunkers", "Oil|International bunkers", variable)) %>%
            mutate(variable = paste0("Final Energy|Transportation|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Transportation|Transport", "Final Energy|Transportation", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))


      # Final energy consumption (Buildings)
      tmp6 <- tmp_sps[which(tmp_sps[, 1] == "Buildings"):(which(tmp_sps[, 1] == "Other") - 1), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = ifelse(variable == "Traditional biomass", "Bioenergy|Traditional biomass", variable)) %>%
        mutate(variable = paste0("Final Energy|Buildings|", variable)) %>%
        mutate(variable = ifelse(variable == "Final Energy|Buildings|Buildings", "Final Energy|Buildings", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)

      tmp6 <- rbind(
        tmp6,
        rbind(
          tmp6 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Buildings"):(which(tmp_sps[, 1] == "Other") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Traditional biomass", "Bioenergy|Traditional biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|Buildings|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Buildings|Buildings", "Final Energy|Buildings", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
          tmp6 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Sustainable Development Scenario"),
          tmp_sds[which(tmp_sps[, 1] == "Buildings"):(which(tmp_sps[, 1] == "Other") - 1), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Sustainable Development Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = ifelse(variable == "Traditional biomass", "Bioenergy|Traditional biomass", variable)) %>%
            mutate(variable = paste0("Final Energy|Buildings|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Buildings|Buildings", "Final Energy|Buildings", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)))

      # Final energy consumption (Other)
      tmp7 <- tmp_sps[which(tmp_sps[, 1] == "Other"):which(tmp_sps[, 1] == "Petrochem. feedstock"), ] %>%
        gather(period, value, -variable) %>%
        mutate(period = as.numeric(period)) %>%
        mutate(model = "IEA-WEM2019") %>%
        mutate(scenario = "Stated Policies Scenario") %>%
        mutate(region = kreg)  %>%
        mutate(variable = paste0("Final Energy|Other|", variable)) %>%
        mutate(variable = ifelse(variable == "Final Energy|Other|Other", "Final Energy|Other", variable)) %>%
        mutate(unit = "Mtoe") %>%
        select(model, scenario, region, variable, unit, period, value)
      tmp7 <- rbind(
        tmp7,
        rbind(
          tmp7 %>%
            filter(period < 2025) %>%
            mutate(scenario = "Current Policies Scenario"),
          tmp_cps[which(tmp_sps[, 1] == "Other"):which(tmp_sps[, 1] == "Petrochem. feedstock"), ] %>%
            gather(period, value, -variable) %>%
            mutate(period = as.numeric(period)) %>%
            mutate(model = "IEA-WEM2019") %>%
            mutate(scenario = "Current Policies Scenario") %>%
            mutate(region = kreg)  %>%
            mutate(variable = paste0("Final Energy|Other|", variable)) %>%
            mutate(variable = ifelse(variable == "Final Energy|Other|Other", "Final Energy|Other", variable)) %>%
            mutate(unit = "Mtoe") %>%
            select(model, scenario, region, variable, unit, period, value)),
        rbind(
            tmp7 %>%
              filter(period < 2025) %>%
              mutate(scenario = "Sustainable Development Scenario"),
            tmp_sds[which(tmp_sps[, 1] == "Other"):which(tmp_sps[, 1] == "Petrochem. feedstock"), ] %>%
              gather(period, value, -variable) %>%
              mutate(period = as.numeric(period)) %>%
              mutate(model = "IEA-WEM2019") %>%
              mutate(scenario = "Sustainable Development Scenario") %>%
              mutate(region = kreg)  %>%
              mutate(variable = paste0("Final Energy|Other|", variable)) %>%
              mutate(variable = ifelse(variable == "Final Energy|Other|Other", "Final Energy|Other", variable)) %>%
              mutate(unit = "Mtoe") %>%
              select(model, scenario, region, variable, unit, period, value))
        )

      tmp_all[[kreg]] <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7)

    }

    data_weo2019 <- do.call("rbind", tmp_all)
    data_weo2019 <- data_weo2019[, c(3, 6, 1, 2, 5, 4, 7)]
    data_weo2019$period <- as.numeric(data_weo2019$period)
    data_weo2019$variable <- gsub(pattern = "\\.", replacement = "_", x = data_weo2019$variable)

    if (subtype == "PE") {
      data_weo2019 <- data_weo2019 %>% filter(variable %in%
                      grep("Primary Energy", unique(data_weo2019$variable), value = TRUE))
    } else if (subtype == "FE") {
      data_weo2019 <- data_weo2019 %>% filter(variable %in%
                      grep("Final Energy", unique(data_weo2019$variable), value = TRUE))
    }

    x <- as.magpie(data_weo2019, spatial = 1, temporal = 2, datacol = 7)
  } else {
    stop("Not a valid subtype!")
  }

  return(x)
}
