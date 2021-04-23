#' Read UNFCC data
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
readUNFCC <- function() {
  
  sheets <- list(
    "Table1s1" = list(
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
            "CO2 captured|For storage in other countries",
            NA,
            NA,
            NA,
            NA
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
      for (sheet in names(sheets)) {
        tmp <- bind_rows(
          tmp,
          suppressWarnings(
            read_xlsx(path = paste0(dir, "/", file), sheet = sheet, skip = 4) %>%
              drop_na("GREENHOUSE GAS SOURCE AND SINK CATEGORIES") %>%
              bind_cols(sheets[[sheet]]$rows, year = year, region = region) %>%
              select(-1) %>%
              drop_na("name") %>%
              melt(id.vars = c("name", "region", "year")) %>%
              mutate(
                !!sym("value") := as.double(!!sym("value")),
                !!sym("name") := paste0(!!sym("name"), "|", !!sym("variable")),
                !!sym("variable") := paste0("kt ", !!sym("variable")),
              ) %>%
              select(-"name", "unit" = "variable", "variable" = "name")
          )
        )
      }
    }
  }
  
  tmp %>% 
    select('region', 'year', 'variable', 'unit', 'value') %>% 
    as.magpie(tidy = TRUE) %>% 
    return()
}
