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
            "Total industrial processes|Chemical industry",
            "Total industrial processes|Chemical industry|Ammonia production",
            "Total industrial processes|Chemical industry|Nitric acid production",
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
    ),
    "Table2(I)s2" = list(
      range = "A7:D29",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total industrial processes|Non-energy products from fuels and solvent use",
            "Total industrial processes|Non-energy products from fuels and solvent use|Lubricant use",
            "Total industrial processes|Non-energy products from fuels and solvent use|Paraffin wax use",
            "Total industrial processes|Non-energy products from fuels and solvent use|Other",
            "Total industrial processes|Electronics industry",
            "Total industrial processes|Electronics industry|Integrated circuit or semiconductor",
            "Total industrial processes|Electronics industry|TFT flat panel display",
            "Total industrial processes|Electronics industry|Photovoltaics",
            "Total industrial processes|Electronics industry|Heat transfer fluid",
            "Total industrial processes|Electronics industry|Other (as specified in table 2(II))",
            "Total industrial processes|Product uses as substitutes for ODS(2)",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Refrigeration and air conditioning",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Foam blowing agents",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Fire protection",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Aerosols",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Solvents",
            "Total industrial processes|Product uses as substitutes for ODS(2)|Other applications",
            "Total industrial processes|Other product manufacture and use",
            "Total industrial processes|Other product manufacture and use|Electrical equipment",
            "Total industrial processes|Other product manufacture and use|SF6 and PFCs from other product use",
            "Total industrial processes|Other product manufacture and use|N2O from product uses",
            "Total industrial processes|Other product manufacture and use|Other",
            "Total industrial processes|Other"
          )
        }
      )
    ),
    "Table3s1" = list(
      range = "A7:D57",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total agriculture",
            "Total agriculture|Livestock",
            "Total agriculture|Enteric fermentation",
            "Total agriculture|Enteric fermentation|Cattle",
            NA,
            "Total agriculture|Enteric fermentation|Cattle|Dairy cattle",
            "Total agriculture|Enteric fermentation|Cattle|Non-dairy cattle",
            NA,
            "Total agriculture|Enteric fermentation|Cattle|Mature dairy cattle",
            "Total agriculture|Enteric fermentation|Cattle|Other mature cattle",
            "Total agriculture|Enteric fermentation|Cattle|Growing cattle",
            NA,
            "Total agriculture|Enteric fermentation|Cattle|Other",
            "Total agriculture|Enteric fermentation|Sheep",
            "Total agriculture|Enteric fermentation|Swine",
            "Total agriculture|Enteric fermentation|Other livestock",
            "Total agriculture|Enteric fermentation|Other livestock|Buffalo",
            "Total agriculture|Enteric fermentation|Other livestock|Deer",
            "Total agriculture|Enteric fermentation|Other livestock|Goats",
            "Total agriculture|Enteric fermentation|Other livestock|Horses",
            "Total agriculture|Enteric fermentation|Other livestock|Mules and Asses",
            "Total agriculture|Enteric fermentation|Other livestock|Poultry",
            "Total agriculture|Enteric fermentation|Other livestock|Other",
            "Total agriculture|Enteric fermentation|Other livestock|Rabbit",
            "Total agriculture|Enteric fermentation|Other livestock|Ostrich",
            "Total agriculture|Enteric fermentation|Other livestock|Fur-bearing Animals",
            "Total agriculture|Manure management",
            "Total agriculture|Manure management|Cattle",
            NA,
            "Total agriculture|Manure management|Cattle|Dairy cattle",
            "Total agriculture|Manure management|Cattle|Non-dairy cattle",
            NA,
            "Total agriculture|Manure management|Cattle|Mature dairy cattle",
            "Total agriculture|Manure management|Cattle|Other mature cattle",
            "Total agriculture|Manure management|Cattle|Growing cattle",
            NA,
            "Total agriculture|Manure management|Cattle|Other",
            "Total agriculture|Manure management|Sheep",
            "Total agriculture|Manure management|Swine",
            "Total agriculture|Manure management|Other livestock",
            "Total agriculture|Manure management|Other livestock|Buffalo",
            "Total agriculture|Manure management|Other livestock|Deer",
            "Total agriculture|Manure management|Other livestock|Goats",
            "Total agriculture|Manure management|Other livestock|Horses",
            "Total agriculture|Manure management|Other livestock|Mules and Asses",
            "Total agriculture|Manure management|Other livestock|Poultry",
            "Total agriculture|Manure management|Other livestock|Other",
            "Total agriculture|Manure management|Other livestock|Rabbit",
            "Total agriculture|Manure management|Other livestock|Ostrich",
            "Total agriculture|Manure management|Other livestock|Fur-bearing Animals",
            "Total agriculture|Manure management|Indirect N2O emissions"
          )
        }
      )
    ),
    "Table3s2" = list(
      range = "A7:D18",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total agriculture|Rice cultivation",
            "Total agriculture|Agricultural soils",
            "Total agriculture|Prescribed burning of savannas",
            "Total agriculture|Field burning of agricultural residues",
            "Total agriculture|Liming",
            "Total agriculture|Urea application",
            "Total agriculture|Other carbon-containing fertilizers",
            "Total agriculture|Other",
            "Total agriculture|Other|3B NOx Emissions",
            "Total agriculture|Other|Digestate renewable raw material atmospheric deposition",
            "Total agriculture|Other|Digestate renewable raw material storage of dry matter",
            "Total agriculture|Other|Digestate renewable raw material"
          )
        }
      )
    ),
    "Table4" = list(
      range = "A7:D29",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total LULUCF",
            "Total LULUCF|Forest land",
            "Total LULUCF|Forest land|Forest land remaining forest land",
            "Total LULUCF|Forest land|Land converted to forest land",
            "Total LULUCF|Cropland",
            "Total LULUCF|Cropland|Cropland remaining cropland",
            "Total LULUCF|Cropland|Land converted to cropland",
            "Total LULUCF|Grassland",
            "Total LULUCF|Grassland|Grassland remaining grassland",
            "Total LULUCF|Grassland|Land converted to grassland",
            "Total LULUCF|Wetlands",
            "Total LULUCF|Wetlands|Wetlands remaining wetlands",
            "Total LULUCF|Wetlands|Land converted to wetlands",
            "Total LULUCF|Settlements",
            "Total LULUCF|Settlements|Settlements remaining settlements",
            "Total LULUCF|Settlements|Land converted to settlements",
            "Total LULUCF|Other land",
            "Total LULUCF|Other land|Other land remaining other land",
            "Total LULUCF|Other land|Land converted to other land",
            "Total LULUCF|Harvested wood products",
            "Total LULUCF|Other",
            "Total LULUCF|Other|Settlements",
            "Total LULUCF|Other|Other"
          )
        }
      )
    ),
    "Table5" = list(
      range = "A7:D24",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
          "Total waste",
          "Total waste|Solid waste disposal",
          "Total waste|Solid waste disposal|Managed waste disposal sites",
          "Total waste|Solid waste disposal|Unmanaged waste disposal sites",
          "Total waste|Solid waste disposal|Uncategorized waste disposal sites",
          "Total waste|Biological treatment of solid waste",
          "Total waste|Biological treatment of solid waste|Composting",
          "Total waste|Biological treatment of solid waste|Anaerobic digestion at biogas facilities",
          "Total waste|Incineration and open burning of waste",
          "Total waste|Incineration and open burning of waste|Waste incineration",
          "Total waste|Incineration and open burning of waste|Open burning of waste",
          "Total waste|Wastewater treatment and discharge",
          "Total waste|Wastewater treatment and discharge|Domestic wastewater",
          "Total waste|Wastewater treatment and discharge|Industrial wastewater",
          "Total waste|Wastewater treatment and discharge|Other",
          "Total waste|Other",
          "Total waste|Other|Mechanical-Biological Treatment MBT",
          "Total waste|Other|Accidental fires"
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
