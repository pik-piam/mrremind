#' Reads NDC policy database with capacity and emission targets, originally based on Rogelj et al. 2017
#'
#' @description Reads excel sheet with NDC (Nationally Determined Contributions)
#'  data on different policy targets (capacity, production, emissions) with different variations.
#'
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux, Falk Benke
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year,
#'   determines the database version to be read in
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned (only used in convert).
#'

#remove later
# setwd("C:/Users/rahelma/Documents/NDC_update/February2026/")
# devtools::load_all()
# library(mrcommons)
# setConfig("cachefolder" = "C:/Users/rahelma/remind/cache/clean-cache")
# setConfig(forcecache = c("calcEmissions", "calcGDP" , "calcUNFCCC"))
# subtype <- "Emissions_2026_cond"
# subset <- "SSP2"

readUNFCCC_NDC <- function(subtype, subset) {
  NDCfile <- dplyr::case_when(
    grepl("2018", subtype, fixed = TRUE) ~ "NDC_2018.xlsx",
    grepl("2021", subtype, fixed = TRUE) ~ "NDC_2021.xlsx",
    grepl("2022", subtype, fixed = TRUE) ~ "NDC_2022-12-31.xlsx",
    grepl("2023", subtype, fixed = TRUE) ~ "NDC_2023-11-29.xlsx",
    grepl("2024", subtype, fixed = TRUE) ~ "NDC_2024-12-31.xlsx",
    .default = "NDC_2024-12-31.xlsx"
  )
  NDC2035 <- dplyr::case_when(
    grepl("2026", subtype, fixed = TRUE) ~ "ELEVATE Task 6.3 Scenario Protocol NDC and LTS v1.xlsx", 
    .default = "ELEVATE Task 6.3 Scenario Protocol NDC and LTS v1.xlsx"
  )
  if (grepl("Capacity", subtype, fixed = TRUE)) {
    data <- readxl::read_excel(
      NDCfile,
      sheet = "Capacity_target",
      col_types = c(
        "text", "skip", "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"
      )
    )
    targetTypes <- c("AC-Absolute", "Production-Absolute", "TIC-Absolute", "FE-Production-Share")
    if (!all(unique(data$`Type of target`) %in% targetTypes)) {
      stop(
        subtype, ": Table read from UNFCCC_NDC contains unknown target types: ",
        unique(data$`Type of target`)[which(!(unique(data$`Type of target`) %in% targetTypes))]
      )
    }
    conditional <- ifelse(length(grep("unconditional", subtype)) == 0, "conditional", "unconditional")
    d <- data %>%
      filter(.data$`Type of target` == "FE-Production-Share", .data$Conditionality == conditional)
    if (nrow(d > 0)) {
      message(
        subtype, ": Found targets of type 'FE-Production-Share' for ", paste(unique(d$ISO), collapse = ", "),
        ". These will be dropped, as this type is currently not supported."
      )
    }
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 3)
    return(x)
  } else if (grepl("Emissions", subtype, fixed = TRUE)) {
    input <- readxl::read_excel(
      NDCfile,
      sheet = "Emissions", skip = 3, na = c("?", ""), progress = FALSE
    ) %>%
      suppressMessages()
    if ("LULUCF" %in% names(input)) {
      input <- input %>%
        select(
          "ISO_Code" = 2, "Reference_Year" = 7,
          "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
          "Type" = 10, "LULUCF" = 11, "Unconditional Relative" = 12,
          "Conditional Relative" = 13, "Unconditional Absolute" = 14,
          "Conditional Absolute" = 15
        )
    } else {
      input <- input %>%
        select(
          "ISO_Code" = 2, "Reference_Year" = 7,
          "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
          "Type" = 10, "Unconditional Relative" = 11, "Conditional Relative" = 12,
          "Unconditional Absolute" = 13, "Conditional Absolute" = 14
        )
    }
    if (any(grepl("2026", subtype, fixed = TRUE))) {
      # read raw data from NDC emissions targets collection
      majorE_raw <- readxl::read_excel(
        NDC2035,
        sheet = "NDC details major emitters", progress = FALSE
      ) %>%
        suppressMessages() 
      
      ######################################
      ##quick fix needs to be removed latter
      ######################################
      majorE_raw <- majorE_raw %>%
        # Keep non-KOR rows
        filter(`ISO-3` != "KOR") %>%
        # Add back KOR, keeping only the last row per Target_Year
        bind_rows(
          majorE_raw %>%
            filter(`ISO-3` == "KOR") %>%
            group_by(`Target Year`) %>%
            slice_tail(n = 1) %>%
            ungroup()
        )
      
      
      
      # clean data for missing gas definition and rename columns
      emissions <- c("Emissions|Kyoto Gases", 
                     "Emissions|Kyoto Gases (excl LULUCF)",
                     "GHG intensity target (tCO2/GDP)",
                     "GHG intensity (tCO2e/GDP)")
      
      majorE_prepared <- majorE_raw %>%
        dplyr::filter(`Model Target Indicator` %in% emissions) %>%
        dplyr::rename(
          ISO_Code = `ISO-3`,
          Target_Year = `Target Year`,
          Reference_Year = Reference,
          BAU_or_Reference_emissions_in_MtCO2e = `Reference level`
        ) %>%
        dplyr::mutate(
          LULUCF = dplyr::case_when(
            grepl("excl LULUCF", `Model Target Indicator`) ~ "Excluding",
            `Model Target Indicator` == "Emissions|Kyoto Gases" ~ "Including",
            TRUE ~ NA_character_),
          target_value = dplyr::case_when(
            Conditionality == "Unconditional" ~ `Target Value Min`, 
            Conditionality == "Conditional"   ~ `Target Value Max`, 
            TRUE ~ NA_real_
          ) *
            dplyr::if_else(`Target Unit` == "Gt CO2e", 1000, 1) *
            dplyr::if_else(`Target Unit` == "%", 1 / 100, 1),
          target_type = paste(
            Conditionality,
            dplyr::if_else(`Target Unit` == "%", "Relative", "Absolute")
          ),
          Type = dplyr::case_when(
            `Target Unit` == "%" &
              grepl("Emissions\\|Kyoto Gases", `Model Target Indicator`) ~ "GHG",
            
            `Target Unit` == "%" &
              grepl("GHG intensity", `Model Target Indicator`, ignore.case = TRUE) ~ "GHG/GDP",
            
            `Target Unit` %in% c("MtCO2e", "Gt CO2e") &
              target_value > 0 ~ "GHG-fixed-total",
            
            `Target Unit` %in% c("MtCO2e", "Gt CO2e") &
              target_value < 0 ~ "GHG-Absolute",
            
            TRUE ~ NA_character_
          )
        )
      
      
      
      
      majorE <-   majorE_prepared %>%
        tidyr::pivot_wider(
          id_cols = c("ISO_Code", "Reference_Year", "BAU_or_Reference_emissions_in_MtCO2e", 
                      "Target_Year", "Type", "LULUCF"),
          names_from  = target_type,
          values_from = target_value) 
      
      ###################################################################
      #manual corrections due to poor data quality should be remove later
      ###################################################################
      majorE[majorE$ISO_Code == "ARG", c("Unconditional Absolute", "Conditional Absolute")] <-
        list(majorE$`Conditional Absolute`[majorE$ISO_Code == "ARG"], NA)
      #China 2030
      majorE[majorE$ISO_Code == "CHN" & majorE$Target_Year == 2030,
             c("BAU_or_Reference_emissions_in_MtCO2e", "LULUCF")] <-
        list(1.069, "Excluding")
      #China 2035
      majorE[majorE$ISO_Code == "CHN" & majorE$Target_Year == 2035,
             c("Reference_Year","BAU_or_Reference_emissions_in_MtCO2e")] <-
        list("2025", 15500)
      #India ref year
      majorE[majorE$ISO_Code == "IND" & majorE$Type == "GHG/GDP",
             "Reference_Year"] <- "2005"
      #remove sink target, cannot be capture
      majorE <- majorE[!(majorE$ISO_Code == "IND" & majorE$Type == "GHG-Absolute"), ]
      #Saudi Arabia 2019
      majorE[majorE$ISO_Code == "SAU",
             "Reference_Year"] <- "2019"
      
      PBL_majorE <- majorE
      
      majorISO <- unique( PBL_majorE$ISO_Code)
      
      
      EUR_NDC_countries <- c(
        "POL", "CZE", "ROU", "BGR", "HUN", "SVK", "LTU", "EST", "SVN",
        "LVA", "DEU", "FRA", "ITA", "ESP", "NLD", "BEL", "GRC", "AUT",
        "PRT", "FIN", "SWE", "IRL", "DNK", "LUX", "CYP", "MLT", "JEY",
        "FRO", "GIB", "GGY", "IMN", "HRV", "GBR")
      
      PBL_NDCs <- readxl::read_excel(
        NDC2035,
        sheet = "NDC emission levels",skip = 2, progress = FALSE
      ) %>%select(ISO_Code = ...2, target_2030 = `excl LULUCF...5`, target_2035 =`excl LULUCF...7`) %>%
        filter(!ISO_Code %in% majorISO, !ISO_Code %in% EUR_NDC_countries) %>%
        # pivot longer to get Target_Year and Conditional Absolute
        pivot_longer(
          cols = c(target_2030, target_2035),
          names_to = "Target_Year",
          values_to = "Conditional Absolute"
        ) %>%
        # convert Target_Year names into numeric
        mutate(
          Target_Year = dplyr::case_when(
            Target_Year == "target_2030" ~ 2030,
            Target_Year == "target_2035" ~ 2035
          ),
          Type = "GHG-fixed-total",
          LULUCF = "Excluding"
        )%>%  suppressMessages() 
      
      
      
      input <-   dplyr::bind_rows(PBL_majorE, PBL_NDCs)  %>%
        quitte::revalue.levels(ISO_Code = c("EU" = "EUR")) %>%
        filter(!is.na(ISO_Code))
     
         
    }
    # Continue processing
    input <- toolProcessClimateTargetDatabase(
      input,
      database = "UNFCCC_NDC", subtype = subtype
    )
    x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")
    return(x)
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
}
