#' Read UNFCCC data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @seealso [`readSource()`]
#'
#' @importFrom dplyr %>% bind_rows bind_cols mutate select
#' @importFrom magclass as.magpie
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom reshape2 melt
#' @importFrom readxl read_xlsx
#' @importFrom rlang sym
#'
#' @export
readUNFCCC <- function() {

  sheets <- list(
    "Table1s1" = list(
      range = "A7:H26",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O", "NOX", "CO", "NMVOC", "SO2")),
      rows = tibble(
        name = {
          c(
            "Total Energy",
            "Total Energy|Fuel combustion activities",
            "Total Energy|Fuel combustion activities|Energy industries",
            "Total Energy|Fuel combustion activities|Energy industries|Public electricity and heat production",
            "Total Energy|Fuel combustion activities|Energy industries|Petroleum refining",
            "Total Energy|Fuel combustion activities|Energy industries|Manufacture of solid fuels and other energy industries",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Iron and steel",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Non-ferrous metals",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Chemicals",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Pulp, paper and print",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Food processing, beverages and tobacco",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Non-metallic minerals",
            "Total Energy|Fuel combustion activities|Manufacturing industries and construction|Other",
            "Total Energy|Fuel combustion activities|Transport",
            "Total Energy|Fuel combustion activities|Transport|Domestic aviation",
            "Total Energy|Fuel combustion activities|Transport|Road transportation",
            "Total Energy|Fuel combustion activities|Transport|Railways",
            "Total Energy|Fuel combustion activities|Transport|Domestic navigation",
            "Total Energy|Fuel combustion activities|Transport|Other transportation"
          )
        }
      )
    ),
    "Table1s2" = list(
      range = "A7:H36",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O", "NOX", "CO", "NMVOC", "SO2")),
      rows = tibble(
        name = {
          c(
            "Total Energy|Fuel combustion activities|Other sectors",
            "Total Energy|Fuel combustion activities|Other sectors|Commercial/institutional",
            "Total Energy|Fuel combustion activities|Other sectors|Residential",
            "Total Energy|Fuel combustion activities|Other sectors|Agriculture/forestry/fishing",
            "Total Energy|Fuel combustion activities|Other",
            "Total Energy|Fuel combustion activities|Other|Stationary",
            "Total Energy|Fuel combustion activities|Other|Mobile",
            "Total Energy|Fugitive emissions from fuels",
            "Total Energy|Fugitive emissions from fuels|Solid fuels",
            "Total Energy|Fugitive emissions from fuels|Solid fuels|Coal mining and handling",
            "Total Energy|Fugitive emissions from fuels|Solid fuels|Solid fuel transformation",
            "Total Energy|Fugitive emissions from fuels|Solid fuels|Other (as specified in table 1.B.1)",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Oil",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Natural gas",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Venting and flaring",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Other (as specified in table 1.B.2)",
            "Total Energy|Fugitive emissions from fuels|CO2 Transport and storage",
            "Total Energy|Fugitive emissions from fuels|CO2 Transport and storage|Transport of CO2",
            "Total Energy|Fugitive emissions from fuels|CO2 Transport and storage|Injection and storage",
            "Total Energy|Fugitive emissions from fuels|CO2 Transport and storage|Other",
            NA,
            "International bunkers",
            "International bunkers|Aviation",
            "International bunkers|Navigation",
            "Multilateral operations",
            "CO2 emissions from biomass",
            "CO2 captured",
            "CO2 captured|For domestic storage",
            "CO2 captured|For storage in other countries"
          )
        }
      )
    ),
    "Table1.A(a)s1" = list(
      range = "A10:I60",
      colnames = c(NA, NA, NA, NA, NA, paste0("kt ", c("CO2", "CH4", "N2O"))),
      rows = tibble(
        name = {
          c(
            "Fuel combustion",
            "Fuel combustion|Liquid fuels",
            "Fuel combustion|Solid fuels",
            "Fuel combustion|Gaseous fuels",
            "Fuel combustion|Other fossil fuels",
            "Fuel combustion|Peat",
            "Fuel combustion|Biomass",
            "Fuel combustion|Energy industries",
            "Fuel combustion|Energy industries|Liquid fuels",
            "Fuel combustion|Energy industries|Solid fuels",
            "Fuel combustion|Energy industries|Gaseous fuels",
            "Fuel combustion|Energy industries|Other fossil fuels",
            "Fuel combustion|Energy industries|Peat",
            "Fuel combustion|Energy industries|Biomass",
            "Fuel combustion|Energy industries|Public electricity and heat production",
            "Fuel combustion|Energy industries|Public electricity and heat production|Liquid fuels",
            "Fuel combustion|Energy industries|Public electricity and heat production|Solid fuels",
            "Fuel combustion|Energy industries|Public electricity and heat production|Gaseous fuels",
            "Fuel combustion|Energy industries|Public electricity and heat production|Other fossil fuels",
            "Fuel combustion|Energy industries|Public electricity and heat production|Peat",
            "Fuel combustion|Energy industries|Public electricity and heat production|Biomass",
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            "Fuel combustion|Energy industries|Petroleum refining",
            "Fuel combustion|Energy industries|Petroleum refining|Liquid fuels",
            "Fuel combustion|Energy industries|Petroleum refining|Solid fuels",
            "Fuel combustion|Energy industries|Petroleum refining|Gaseous fuels",
            "Fuel combustion|Energy industries|Petroleum refining|Other fossil fuels",
            "Fuel combustion|Energy industries|Petroleum refining|Peat",
            "Fuel combustion|Energy industries|Petroleum refining|Biomass",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Liquid fuels",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Solid fuels",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Gaseous fuels",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Other fossil fuels",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Peat",
            "Fuel combustion|Energy industries|Manufacture of solid fuels and other energy industries|Biomass",
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA,
            NA
          )
        }
      )
    ),
    "Table2(I)s1" = list(
      range = "A7:D31",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total industrial processes",
            "Total industrial processes|Mineral industry",
            "Total industrial processes|Mineral industry|Cement production",
            "Total industrial processes|Mineral industry|Lime production",
            "Total industrial processes|Mineral industry|Glass production",
            "Total industrial processes|Mineral industry|Other process uses of carbonates",
            "Total industrial processes|Chemical industry ",
            "Total industrial processes|Chemical industry|Ammonia production",
            "Total industrial processes|Chemical industry|Nitric acid production ",
            "Total industrial processes|Chemical industry|Adipic acid production",
            "Total industrial processes|Chemical industry|Caprolactam, glyoxal and glyoxylic acid production",
            "Total industrial processes|Chemical industry|Carbide production",
            "Total industrial processes|Chemical industry|Titanium dioxide production",
            "Total industrial processes|Chemical industry|Soda ash production",
            "Total industrial processes|Chemical industry|Petrochemical and carbon black production",
            "Total industrial processes|Chemical industry|Fluorochemical production",
            "Total industrial processes|Chemical industry|Other",
            "Total industrial processes|Metal industry",
            "Total industrial processes|Metal industry|Iron and steel production",
            "Total industrial processes|Metal industry|Ferroalloys production",
            "Total industrial processes|Metal industry|Aluminium production",
            "Total industrial processes|Metal industry|Magnesium production",
            "Total industrial processes|Metal industry|Lead production",
            "Total industrial processes|Metal industry|Zinc production",
            "Total industrial processes|Metal industry|Other"
          )
        }
      )
    )
  )

  dirs <- list.files(path = ".")

  tmp <- NULL
  for (dir in dirs) {
    files <- list.files(path = paste0("./", dir))
    region <- toupper(sub("\\-.*", "\\1", dir))
    for (file in files) {
      year <- as.integer(sub(".{3}_[0-9]{4}_([0-9]{4})_.*", "\\1", file))
      if (is.na(year)) {
        next
      }
      for (i in names(sheets)) {
        tmp <- bind_rows(
          tmp,
          suppressMessages(
            suppressWarnings(
              read_xlsx(path = paste0(dir, "/", file), sheet = i,
                range = sheets[[i]][["range"]],
                col_names = c("variable", sheets[[i]][["colnames"]])) %>%
                bind_cols(sheets[[i]]$rows, year = year, region = region) %>%
                select(-1) %>%
                select(-which(is.na(sheets[[i]][["colnames"]]))) %>%
                filter(!is.na(!!sym("name"))) %>%
                melt(id.vars = c("name", "region", "year")) %>%
                mutate(
                  !!sym("value") := as.double(!!sym("value")),
                  !!sym("name") := paste0(!!sym("name"), "|", sub(".+ ", "", !!sym("variable")))
                ) %>%
                select(-"name", "unit" = "variable", "variable" = "name") %>%
                filter(!is.na(!!sym("value")))
            )
          )
        )
      }
    }
  }

  tmp %>%
    select("region", "year", "variable", "unit", "value") %>%
    as.magpie(tidy = TRUE) %>%
    return()
}
