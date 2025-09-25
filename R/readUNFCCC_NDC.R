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
readUNFCCC_NDC <- function(subtype, subset) {
  
  NDCfile <- dplyr::case_when(
    grepl("2018", subtype, fixed = TRUE) ~ "NDC_2018.xlsx",
    grepl("2021", subtype, fixed = TRUE) ~ "NDC_2021.xlsx",
    grepl("2022", subtype, fixed = TRUE) ~ "NDC_2022-12-31.xlsx",
    grepl("2023", subtype, fixed = TRUE) ~ "NDC_2023-11-29.xlsx",
    grepl("2024", subtype, fixed = TRUE) ~ "NDC_2024-08-31_corrected.xlsx",
    .default = "NDC_2024-08-31_corrected.xlsx"
  )
  
  NDC2035 <- dplyr::case_when(
    grepl("2025", subtype, fixed = TRUE) ~ "2025 NDC status per country_sept2025.xlsx",
    .default = "2025 NDC status per country_sept2025.xlsx"
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
      message(subtype, ": Found targets of type 'FE-Production-Share' for ", paste(unique(d$ISO), collapse = ", "),
              ". These will be dropped, as this type is currently not supported.")
    }
    
    x <- as.magpie(data, spatial = 1, temporal = 2, datacol = 3)
    return(x)
    
  } else if (grepl("Emissions", subtype, fixed = TRUE)) {
    
    input <- readxl::read_excel(
      NDCfile, sheet = "Emissions", skip = 3, na = c("?", ""), progress = FALSE) %>%
      suppressMessages()
    
    if ("LULUCF" %in% names(input)) {
      input <- input %>%
        select(
          "ISO_Code" = 2, "Reference_Year" = 7,
          "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
          "Type" = 10, "LULUCF" = 11, "Unconditional Absolute" = 12, 
          "Conditional Absolute" = 13, "Unconditional Relative" = 14, 
          "Conditional Relative" = 15
        )
    } else {
      input <- input %>%
        select(
          "ISO_Code" = 2, "Reference_Year" = 7,
          "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
          "Type" = 10, "Unconditional Absolute" = 11, "Conditional Absolute" = 12,
          "Unconditional Relative" = 13, "Conditional Relative" = 14
        )
    }
    
    input2035 <- readxl::read_excel(
      NDC2035, sheet = "NDC overview", skip = 1, na = c("?", ""), progress = FALSE
    ) %>%
      suppressMessages() %>%
      filter(!is.na(`Gas coverage`)) %>%
      rename(
        ISO_Code = ISO3,
        Target_Year = `Target year`,
        Reference_Year = `base year`,
        BAU_or_Reference_emissions_in_MtCO2e = `BAU emission level (Mt CO2eq)`
       # Type = `Type of NDC`
      ) %>%
      
      # --- ensure all target columns exist ---
      mutate(
        `Unconditional Relative` = NA_character_,
        `Conditional Relative`   = NA_character_,
        `Unconditional Absolute` = NA_character_,
        `Conditional Absolute`   = NA_character_,
        `Type`   = NA_character_
      ) %>%
      
      # --- fill values by different types ---
      mutate(
        # base year unconditional relative
        `Unconditional Relative` =dplyr::if_else(
          `Type of NDC` == "Base year" & Conditionality == "Unconditional",
          as.character(`reduction min (%)...21`),
          `Unconditional Relative`
        ),
        # base year conditional relative
        `Conditional Relative` =dplyr::if_else(
          `Type of NDC` == "Base year" & Conditionality == "Conditional",
          as.character(`reduction max (%)...22`),
          `Conditional Relative`
        ),
        # unconditional absolute emission targets
        `Unconditional Absolute` =dplyr::if_else(
          `Type of NDC` == "Specific" & Conditionality == "Unconditional",
          as.character(`emission level min (Mt CO2eq)...16`),
          `Unconditional Absolute`
        ),
        # conditional absolute emission targets
        `Conditional Absolute` =dplyr::if_else(
          `Type of NDC` == "Specific" & Conditionality == "Conditional",
          as.character(`emission level max (Mt CO2eq)...17`),
          `Conditional Absolute`
        ),
        
        # BAU unconditional
        `Unconditional Relative` =dplyr::case_when(
          `Type of NDC` == "BAU" & Conditionality == "Unconditional" & !is.na(`reduction min (%)...18`) ~ 
            as.character(`reduction min (%)...18`),
          TRUE ~ `Unconditional Relative`
        ),
        `Unconditional Absolute` =dplyr::case_when(
          `Type of NDC` == "BAU" & Conditionality == "Unconditional" & is.na(`reduction min (%)...18`) ~ 
            as.character(`emission level min (Mt CO2eq)...16`),
          TRUE ~ `Unconditional Absolute`
        ),
        
        # BAU conditional
        `Conditional Relative` =dplyr::case_when(
          `Type of NDC` == "BAU" & Conditionality == "Conditional" & !is.na(`reduction min (%)...18`) ~ 
            as.character(`reduction min (%)...18`),
          TRUE ~ `Conditional Relative`
        ),
        `Conditional Absolute` =dplyr::case_when(
          `Type of NDC` == "BAU" & Conditionality == "Conditional" & is.na(`reduction min (%)...18`) ~ 
            as.character(`emission level min (Mt CO2eq)...16`),
          TRUE ~ `Conditional Absolute`
        ),
        
        # correct the GHG type 
        Type =dplyr::case_when(
          `Gas coverage` == "GHG" & 
            (!is.na(`Unconditional Relative`) | !is.na(`Conditional Relative`)) ~ "GHG",
          
          `Gas coverage` == "GHG" & `Type of NDC` != "Specific" &
            (!is.na(`Unconditional Absolute`) | !is.na(`Conditional Absolute`)) ~ "GHG-Absolute",
          
          `Gas coverage` == "GHG" & `Type of NDC` == "Specific" &
            (!is.na(`Unconditional Absolute`) | !is.na(`Conditional Absolute`)) ~ "GHG-fixed-total",
          
          TRUE ~ "type missing"
        )
      ) %>%
  select(
    ISO_Code,
    Reference_Year,
    BAU_or_Reference_emissions_in_MtCO2e,
    Target_Year,
    Type,
    LULUCF,
    `Unconditional Absolute`,
    `Conditional Absolute`,
    `Unconditional Relative`,
    `Conditional Relative`
  )
    
input <- dplyr::case_when(
      grepl("2025", subtype, fixed = TRUE) ~ rbind(input,input2035)
    )    


# Continue processing
input <- toolProcessClimateTargetDatabase(
  input, database = "UNFCCC_NDC", subtype = subtype
)

x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")

return(x)
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
}
