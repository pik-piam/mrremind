#' Read UNFCCC data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @seealso [`readSource()`]
#'
#' @importFrom dplyr %>% bind_rows bind_cols mutate select
#'
#' @importFrom magclass as.magpie
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom reshape2 melt
#' @importFrom readxl read_xlsx
#' @importFrom rlang sym
#'
#'
#'
#' @export
#'
#'
#'
readUNFCCC <- function() {

  # structural definition of the source ----
  # nolint start
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
            "Total Energy|Fugitive emissions from fuels|Solid fuels|Other",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Oil",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Natural gas",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Venting and flaring",
            "Total Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production|Other",
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
      range = "A10:I23",
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
            "Fuel combustion|Energy industries|Biomass"
          )
        }
      )
    ),
    "Table1.A(a)s2" = list(
      range = "A9:I58",
      colnames = c(NA, NA, NA, NA, NA, paste0("kt ", c("CO2", "CH4", "N2O"))),
      rows = tibble(
        name = {
          c(
            "Manufacturing industries and construction",
            "Manufacturing industries and construction|Liquid fuels",
            "Manufacturing industries and construction|Solid fuels",
            "Manufacturing industries and construction|Gaseous fuels",
            "Manufacturing industries and construction|Other fossil fuels",
            "Manufacturing industries and construction|Peat",
            "Manufacturing industries and construction|Biomass",
            "Manufacturing industries and construction|Iron and steel",
            "Manufacturing industries and construction|Iron and steel|Liquid fuels",
            "Manufacturing industries and construction|Iron and steel|Solid fuels",
            "Manufacturing industries and construction|Iron and steel|Gaseous fuels",
            "Manufacturing industries and construction|Iron and steel|Other fossil fuels",
            "Manufacturing industries and construction|Iron and steel|Peat",
            "Manufacturing industries and construction|Iron and steel|Biomass",
            "Manufacturing industries and construction|Non-ferrous metals",
            "Manufacturing industries and construction|Non-ferrous metals|Liquid fuels",
            "Manufacturing industries and construction|Non-ferrous metals|Solid fuels",
            "Manufacturing industries and construction|Non-ferrous metals|Gaseous fuels",
            "Manufacturing industries and construction|Non-ferrous metals|Other fossil fuels",
            "Manufacturing industries and construction|Non-ferrous metals|Peat",
            "Manufacturing industries and construction|Non-ferrous metals|Biomass",
            "Manufacturing industries and construction|Chemicals",
            "Manufacturing industries and construction|Chemicals|Liquid fuels",
            "Manufacturing industries and construction|Chemicals|Solid fuels",
            "Manufacturing industries and construction|Chemicals|Gaseous fuels",
            "Manufacturing industries and construction|Chemicals|Other fossil fuels",
            "Manufacturing industries and construction|Chemicals|Peat",
            "Manufacturing industries and construction|Chemicals|Biomass",
            "Manufacturing industries and construction|Pulp, paper and print",
            "Manufacturing industries and construction|Pulp, paper and print|Liquid fuels",
            "Manufacturing industries and construction|Pulp, paper and print|Solid fuels",
            "Manufacturing industries and construction|Pulp, paper and print|Gaseous fuels",
            "Manufacturing industries and construction|Pulp, paper and print|Other fossil fuels",
            "Manufacturing industries and construction|Pulp, paper and print|Peat",
            "Manufacturing industries and construction|Pulp, paper and print|Biomass",
            "Manufacturing industries and construction|Food processing, beverages and tobacco",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Liquid fuels",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Solid fuels",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Gaseous fuels",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Other fossil fuels",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Peat",
            "Manufacturing industries and construction|Food processing, beverages and tobacco|Biomass",
            "Manufacturing industries and construction|Non-metallic minerals",
            "Manufacturing industries and construction|Non-metallic minerals|Liquid fuels",
            "Manufacturing industries and construction|Non-metallic minerals|Solid fuels",
            "Manufacturing industries and construction|Non-metallic minerals|Gaseous fuels",
            "Manufacturing industries and construction|Non-metallic minerals|Other fossil fuels",
            "Manufacturing industries and construction|Non-metallic minerals|Peat",
            "Manufacturing industries and construction|Non-metallic minerals|Biomass",
            "Manufacturing industries and construction|Other"
          )
        }
      )
    ),
    "Table1.A(b)" = list(
      range = "C8:S25",
      colnames = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "kt CO2"),
      rows = tibble(
        name = {
          c(
            "Fuel Types|Liquid fossil|Primary fuels|Crude oil",
            "Fuel Types|Liquid fossil|Primary fuels|Orimulsion",
            "Fuel Types|Liquid fossil|Primary fuels|Natural gas liquids",
            "Fuel Types|Liquid fossil|Secondary fuels|Gasoline",
            "Fuel Types|Liquid fossil|Secondary fuels|Jet kerosene",
            "Fuel Types|Liquid fossil|Secondary fuels|Other kerosene",
            "Fuel Types|Liquid fossil|Secondary fuels|Shale oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Gas/diesel oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Residual fuel oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Liquefied petroleum gases (LPG)",
            "Fuel Types|Liquid fossil|Secondary fuels|Ethane",
            "Fuel Types|Liquid fossil|Secondary fuels|Naphtha",
            "Fuel Types|Liquid fossil|Secondary fuels|Bitumen",
            "Fuel Types|Liquid fossil|Secondary fuels|Lubricants",
            "Fuel Types|Liquid fossil|Secondary fuels|Petroleum coke",
            "Fuel Types|Liquid fossil|Secondary fuels|Refinery feedstocks",
            "Fuel Types|Liquid fossil|Secondary fuels|Other oil",
            "Fuel Types|Other liquid fossil"
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
            "Total industrial processes|Electronics industry|Other",
            "Total industrial processes|Product uses as substitutes for ODS",
            "Total industrial processes|Product uses as substitutes for ODS|Refrigeration and air conditioning",
            "Total industrial processes|Product uses as substitutes for ODS|Foam blowing agents",
            "Total industrial processes|Product uses as substitutes for ODS|Fire protection",
            "Total industrial processes|Product uses as substitutes for ODS|Aerosols",
            "Total industrial processes|Product uses as substitutes for ODS|Solvents",
            "Total industrial processes|Product uses as substitutes for ODS|Other applications",
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
      range = "A7:D10",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Total agriculture",
            "Total agriculture|Livestock",
            "Total agriculture|Enteric fermentation",
            "Total agriculture|Enteric fermentation|Cattle",
            # extra variables
            "Total agriculture|Manure management"
          )
        }
      ),
      extraVariables = "B.  Manure management"
    ),
    "Table3s2" = list(
      range = "A7:D14",
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
            "Total agriculture|Other"
          )
        }
      )
    ),
    "Table4" = list(
      range = "A7:D27",
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
            "Total LULUCF|Other"
          )
        }
      )
    ),
    "Table5" = list(
      range = "A7:D22",
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
            "Total waste|Other"
          )
        }
      )
    ),
    "Summary1.As1" = list(
      range = "A7:D28",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Net emissions",
            "Net Emissions|Energy",
            "Net Emissions|Energy|Fuel combustion|Reference approach",
            "Net Emissions|Energy|Fuel combustion|Sectoral approach",
            "Net Emissions|Energy|Fuel combustion|Energy industries",
            "Net Emissions|Energy|Fuel combustion|Manufacturing industries and construction ",
            "Net Emissions|Energy|Fuel combustion|Transport",
            "Net Emissions|Energy|Fuel combustion|Other sectors",
            "Net Emissions|Energy|Fuel combustion|Other",
            "Net Emissions|Energy|Fugitive emissions from fuels",
            "Net Emissions|Energy|Fugitive emissions from fuels|Solid fuels",
            "Net Emissions|Energy|Fugitive emissions from fuels|Oil and natural gas and other emissions from energy production",
            "Net Emissions|Energy|CO2 Transport and storage",
            "Net Emissions|Industrial processes and product use",
            "Net Emissions|Industrial processes and product use|Mineral industry",
            "Net Emissions|Industrial processes and product use|Chemical industry",
            "Net Emissions|Industrial processes and product use|Metal industry",
            "Net Emissions|Industrial processes and product use|Non-energy products from fuels and solvent use ",
            "Net Emissions|Industrial processes and product use|Electronic industry ",
            "Net Emissions|Industrial processes and product use|Product uses as substitutes for ODS",
            "Net Emissions|Industrial processes and product use|Other product manufacture and use ",
            "Net Emissions|Industrial processes and product use|Other"
          )
        }
      )
    ),
    "Summary1.As2" = list(
      range = "A8:D34",
      colnames = paste0("kt ", c("CO2", "CH4", "N2O")),
      rows = tibble(
        name = {
          c(
            "Net Emissions|Agriculture",
            "Net Emissions|Agriculture|Enteric fermentation",
            "Net Emissions|Agriculture|Manure management",
            "Net Emissions|Agriculture|Rice cultivation",
            "Net Emissions|Agriculture|Agricultural soils",
            "Net Emissions|Agriculture|Prescribed burning of savannas",
            "Net Emissions|Agriculture|Field burning of agricultural residues",
            "Net Emissions|Agriculture|Liming",
            "Net Emissions|Agriculture|Urea application",
            "Net Emissions|Agriculture|Other carbon-contining fertilizers",
            "Net Emissions|Agriculture|Other ",
            "Net Emissions|Land use, land-use change and forestry ",
            "Net Emissions|Land use, land-use change and forestry|Forest land",
            "Net Emissions|Land use, land-use change and forestry|Cropland",
            "Net Emissions|Land use, land-use change and forestry|Grassland",
            "Net Emissions|Land use, land-use change and forestry|Wetlands",
            "Net Emissions|Land use, land-use change and forestry|Settlements",
            "Net Emissions|Land use, land-use change and forestry|Other land",
            "Net Emissions|Land use, land-use change and forestry|Harvested wood products",
            "Net Emissions|Land use, land-use change and forestry|Other ",
            "Net Emissions|Waste",
            "Net Emissions|Waste|Solid waste disposal",
            "Net Emissions|Waste|Biological treatment of solid waste",
            "Net Emissions|Waste|Incineration and open burning of waste",
            "Net Emissions|Waste|Wastewater treatment and discharge",
            "Net Emissions|Waste|Other",
            "Net Emissions|Other"
          )
        }
      )
    )
  )
  # nolint end

  # parse directories ----

  dirs <- list.files(path = "./2023")

  tmp <- NULL
  for (dir in dirs) {
    files <- list.files(path = file.path(".", "2023", dir))
    region <- toupper(sub("\\-.*", "\\1", dir))
    for (file in files) {
      year <- as.integer(sub(".{3}_[0-9]{4}_([0-9]{4})_.*", "\\1", file))
      if (is.na(year)) {
        next
      }

      availableSheets <- excel_sheets(file.path(".", "2023", dir, file))

      for (i in intersect(names(sheets), availableSheets)) {

        s <- suppressMessages(
          read_xlsx(
            path = file.path("2023", dir, file), sheet = i,
            range = sheets[[i]][["range"]],
            col_names = c("variable", sheets[[i]][["colnames"]])
          )
        )

        if (!is.null(sheets[[i]][["extraVariables"]])) {
          extra <- suppressMessages(
            read_xlsx(path = file.path("2023", dir, file), sheet = i) %>%
              select(seq(1:4))
          )
          colnames(extra) <- c("variable", sheets[[i]][["colnames"]])
          extra <- extra %>%
            filter(!!sym("variable") %in% sheets[[i]][["extraVariables"]])
          s <- rbind(s, extra)
        }

        tmp <- bind_rows(
          tmp,
          suppressMessages(
            suppressWarnings(
              s %>%
                bind_cols(sheets[[i]]$rows, year = year, region = region) %>%
                select(-1) %>%
                select(-which(is.na(sheets[[i]][["colnames"]]))) %>%
                filter(!is.na(!!sym("name"))) %>%
                melt(id.vars = c("name", "region", "year")) %>%
                mutate(
                  !!sym("value") := suppressWarnings(as.double(!!sym("value"))),
                  !!sym("name") := paste0(sub("\\.", "_", i), "|", !!sym("name"), "|", sub(".+ ", "", !!sym("variable")))
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
