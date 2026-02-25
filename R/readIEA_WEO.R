#' Read IEA World Energy Outlook data
#'
#' @description Read-in IEA WEO 2016 data for investment costs of different technologies,
#' and WEO 2017 data for historical electricity capacities (GW)
#' @param subtype data subtype. Either "Capacity" or "Invest_Costs"
#' @author Renato Rodrigues, Aman Malik, and Jerome Hilaire
#'
readIEA_WEO <- function(subtype) {

  if (subtype == "Invest_Costs") {
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

    input <- input_all[, c(15, 1:6)]
    colnames(input)[4:7] <- c(2015, 2020, 2030, 2040)
    input <- input %>%
      tidyr::gather(4:7, key = "Year", value = "costs")

    x <- as.magpie(input, spatial = 3, temporal = 4, datacol = 5)


  } else if (subtype == "Capacity") {

    data <- read.csv("WEO_2017/WEO-capacity.csv", sep = ";")[, c(2, 3, 4, 5)]

    # creating capacity, generation or emissions magpie object
    x <- as.magpie(data, temporal = 2, spatial = 1, datacol = 4)

  } else {
    stop("Not a valid subtype!")
  }

  return(x)
}
