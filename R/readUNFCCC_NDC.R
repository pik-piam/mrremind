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
    grepl("2024", subtype, fixed = TRUE) ~ "NDC_2024-12-31.xlsx",
    .default = "NDC_2024-12-31.xlsx"
  )
  NDC2035 <- dplyr::case_when(
    grepl("2025", subtype, fixed = TRUE) ~ "2025 NDC status per country_sept2025_PIK.xlsx",
    .default = "2025 NDC status per country_sept2025_PIK.xlsx"
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
    if (any(grepl("2025", subtype, fixed = TRUE))) {
      # read raw data from NDC emissions targets collection
      input2035_raw <- readxl::read_excel(
        NDC2035,
        sheet = "NDC overview", skip = 1, na = c("?", ""), progress = FALSE
      ) %>%
        suppressMessages()

      # clean data for missing gas definition and rename columns
      input2035 <- input2035_raw %>%
        filter(!is.na(.data$`Gas coverage`)) %>%
        dplyr::rename(
          "ISO_Code" = "ISO3",
          "Target_Year" = .data$`Target year`,
          "Reference_Year" = .data$`base year`,
          "BAU_or_Reference_emissions_in_MtCO2e" = .data$`BAU emission level (Mt CO2eq)`
        )


      # add target value column and match with respective columns of target values depending on target type (e.g. absolute or relative emissions targets)
      input2035 <- input2035 %>%
        # add distintion between absolute and relative targets
        # target type = "Specific" refers to absolute targets, others to relative
        mutate(RelOrAbsTarget = ifelse(.data$`Type of NDC` == "Specific",
          "Absolute",
          "Relative"
        )) %>%
        # add target column and fill with values of respective columns depending on types of targets
        mutate(target = NA) %>%
        mutate(target = dplyr::case_when(
          # target values for base year targets, unconditional -> minimum relative reduction target
          (.data$`Type of NDC` == "Base year" &
            .data$Conditionality == "Unconditional") ~ .data$`reduction min (%)...21`,
          # target values for base year targets, conditional -> maximum relative reduction target
          (.data$`Type of NDC` == "Base year" &
            .data$Conditionality == "Conditional") ~ .data$`reduction max (%)...22`,
          # target values for specific (absolute) targets, unconditional -> maximum emissions level targets
          (.data$`Type of NDC` == "Specific" &
            .data$Conditionality == "Unconditional") ~ .data$`emission level max (Mt CO2eq)...17`,
          # target values for specific (absolute) targets, conditional -> minimum emissions level targets
          (.data$`Type of NDC` == "Specific" &
            .data$Conditionality == "Conditional") ~ .data$`emission level min (Mt CO2eq)...16`,
          # target values for BAU targets, unconditional, without minimum reduction target -> maximum emissions level targets
          (.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Unconditional" &
            is.na(.data$`reduction min (%)...18`)) ~ .data$`emission level max (Mt CO2eq)...17`,
          # target values for BAU targets, conditional, without maximum reduction target -> minimum emissions level targets
          (.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Conditional" &
            is.na(.data$`reduction max (%)...19`)) ~ .data$`emission level min (Mt CO2eq)...16`,
          # target values for BAU targets, unconditional, with minimum reduction target -> minimum reduction target
          (.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Unconditional" &
            !is.na(.data$`reduction min (%)...18`)) ~ .data$`reduction min (%)...18`,
          # target values for BAU targets, conditional, with maximum reduction target -> maximum reduction target
          (.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Conditional" &
            !is.na(.data$`reduction max (%)...19`)) ~ .data$`reduction max (%)...19`
        )) %>%
        # if target values for BAU targets but maximum/minimum relative reduction targets not present -> convert target type to absolute target as
        # absolute target emissions values have been chosen in the previous lines
        mutate(
          RelOrAbsTarget = dplyr::if_else((.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Unconditional" &
            is.na(.data$`reduction min (%)...18`)) |
            (.data$`Type of NDC` == "BAU" &
              .data$`Conditionality` == "Conditional" &
              is.na(.data$`reduction max (%)...19`)), "Absolute", .data$RelOrAbsTarget),
          `Type of NDC` = dplyr::if_else((.data$`Type of NDC` == "BAU" &
            .data$`Conditionality` == "Unconditional" &
            is.na(.data$`reduction min (%)...18`)) |
            (.data$`Type of NDC` == "BAU" &
              .data$`Conditionality` == "Conditional" &
              is.na(.data$`reduction max (%)...19`)), "Specific", .data$`Type of NDC`)
        )


      # only select columns which are needed for further calculations to apply pivot_wider as desired below
      # also rename EUU region to EUR (REMIND name)
      input2035 <- input2035 %>%
        select(
          "ISO_Code",
          "Reference_Year",
          "BAU_or_Reference_emissions_in_MtCO2e",
          "Target_Year",
          "Gas coverage",
          "Type of NDC",
          "LULUCF",
          "Conditionality",
          "RelOrAbsTarget",
          "target"
        ) %>%
        quitte::revalue.levels(ISO_Code = c("EUU" = "EUR"))

      # for countries that have two different reference emissions for unconditional and conditional targets
      # make assumptions that the reference emissions of the unconditional target are also used for the conditional targets
      # this only affects very few coutries so far (Ecudador) and is a simplification which makes the data processing easier at the moment
      # in the future this can in theory be account for
      input2035 <- input2035 %>%
        group_by(.data$`ISO_Code`) %>%
        mutate(
          # paste the reference emissions of unconditional target to another new column
          RefEmiUnCond = dplyr::first(.data$`BAU_or_Reference_emissions_in_MtCO2e`[.data$`Conditionality` == "Unconditional"]),
          # Replace the reference only for rows where category == "B"
          BAU_or_Reference_emissions_in_MtCO2e = dplyr::if_else(.data$`Conditionality` == "Conditional" & !is.na(.data$`RefEmiUnCond`),
            .data$`RefEmiUnCond`,
            .data$`BAU_or_Reference_emissions_in_MtCO2e`
          )
        ) %>%
        select(-.data$RefEmiUnCond) %>%
        ungroup()

      # create wide format of target values with columns for
      # "unconditional absolute", "unconditional relative", "conditional absolute", "conditional relative"
      input2035 <- input2035 %>%
        tidyr::pivot_wider(
          names_from = c(.data$Conditionality, .data$RelOrAbsTarget),
          values_from = .data$target,
          names_sep = " "
        )

      # add target type ("GHG" = relative emissions target (to base year or BAU), "GHG-fixed-total" = absolute emissions target)
      input2035 <- input2035 %>%
        mutate(
          # correct the GHG type
          Type = dplyr::case_when(
            .data$`Gas coverage` == "GHG" &
              (!is.na(.data$`Unconditional Relative`) | !is.na(.data$`Conditional Relative`)) ~ "GHG",
            # attention! In the current PBL file absolute emission targets always
            # refer to emission level targeted in the target year, no reduction levels
            .data$`Gas coverage` == "GHG" & .data$`Type of NDC` != "Specific" &
              (!is.na(.data$`Unconditional Absolute`) | !is.na(.data$`Conditional Absolute`)) ~ "GHG-fixed-total",
            .data$`Gas coverage` == "GHG" & .data$`Type of NDC` == "Specific" &
              (!is.na(.data$`Unconditional Absolute`) | !is.na(.data$`Conditional Absolute`)) ~ "GHG-fixed-total",
            TRUE ~ "type missing"
          )
        )

      # only select columns needed for further calculation
      input2035 <- input2035 %>%
        select(
          "ISO_Code",
          "Reference_Year",
          "BAU_or_Reference_emissions_in_MtCO2e",
          "Target_Year",
          "Type",
          "LULUCF",
          .data$`Unconditional Absolute`,
          .data$`Conditional Absolute`,
          .data$`Unconditional Relative`,
          .data$`Conditional Relative`
        )
      # set BAU where needed
      input2035$Reference_Year[!is.na(input2035$BAU_or_Reference_emissions_in_MtCO2e) &
        input2035$Type != "GHG-fixed-total"] <- "BAU"
      # set as negative % targets (reduction)
      input2035$`Unconditional Relative` <- as.numeric(input2035$`Unconditional Relative`) / -100
      input2035$`Conditional Relative` <- as.numeric(input2035$`Conditional Relative`) / -100
      input <- rbind(input, input2035)
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
