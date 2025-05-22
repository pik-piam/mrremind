#' Reads NDC policy database with capacity, emission, and share targets, originally based on Rogelj et al. 2017
#'
#' @description Reads excel sheet with NDC (Nationally Determined Contributions)
#'  data on different policy targets (capacity, emission, and share targets) with different variations.
#'
#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW.
#' Generation/Production targets are in GWh.
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#'   Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year
#' @param subset A string (or vector of strings) designating the scenario(s) to be returned.
#'
readUNFCCC_NDC <- function(subtype, subset) {

  NDCfile <- dplyr::case_when(
    grepl("2018", subtype, fixed = TRUE) ~ "NDC_2018.xlsx",
    grepl("2021", subtype, fixed = TRUE) ~ "NDC_2021.xlsx",
    grepl("2022", subtype, fixed = TRUE) ~ "NDC_2022-12-31.xlsx",
    grepl("2023", subtype, fixed = TRUE) ~ "NDC_2023-11-29.xlsx",
    grepl("2024", subtype, fixed = TRUE) ~ "NDC_2024-08-31.xlsx",
    .default = "NDC_2024-08-31.xlsx"
  )

  if (grepl("Capacity", subtype, fixed = TRUE)) {
    NDC <- readxl::read_excel(
      NDCfile,
      sheet = "Capacity_target",
      col_types = c(
        "text", "skip", "numeric", "text", "text", "numeric",
        "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"
      )
    )
    x <- as.magpie(NDC, spatial = 1, temporal = 2, datacol = 3)
    return(x)
  } else if (grepl("Emissions", subtype, fixed = TRUE)) {

    input <- readxl::read_excel(
      NDCfile, sheet = "Emissions", skip = 3, na = c("?", ""), progress = FALSE) %>%
      suppressMessages() %>%
      select(
        "ISO_Code" = 2, "Reference_Year" = 7,
        "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
        "Type" = 10, "Unconditional Absolute" = 11, "Conditional Absolute" = 12,
        "Unconditional Relative" = 13, "Conditional Relative" = 14
      )

    # Check that no row has both absolute and relative conditional/unconditional
    bothcolumns <- input$ISO_Code[(!is.na(input$`Unconditional Absolute`) & !is.na(input$`Unconditional Relative`)) |
                                    (!is.na(input$`Conditional Absolute`) & !is.na(input$`Conditional Relative`))]
    if (length(bothcolumns) > 0) {
      stop("readUNFCCC_NDC with subtype=", subtype, " has both absolute and relative emission targets in: ",
           paste(bothcolumns, collapse = ", "))
    }

    # Check for valid types. Note: this has to be identical to the definition in convert function
    allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")

    if (any(!input$Type %in% allowedType)) {
      stop(
        "Unknown data type used in ", NDCfile, ": ",
        paste(unique(input$Type)[!unique(input$Type) %in% allowedType], collapse = ", "),
        ". Please use: ", paste(allowedType, collapse = " or "), "."
      )
    }

    # Check that EUR only has "GHG" or "GHG-fixed-total" targets
    if (any(!input[input$ISO_Code == "EUR", ]$Type %in% c("GHG", "GHG-fixed-total"))) {
      stop("EU targets may only be 'GHG' or 'GHG-fixed-total'")
    }

    # Check that type matches values in absolute/relative conditional/unconditional columns
    if (!grepl("Emissions_20(18|21)", subtype)) {
      colRelative <- is.na(input$`Conditional Relative`) & is.na(input$`Unconditional Relative`) &
        input$Type %in% c("GHG", "GHG/GDP", "CO2/GDP", "GHG/CAP")
      colAbsolute <- is.na(input$`Conditional Absolute`) & is.na(input$`Unconditional Absolute`) &
        input$Type %in% c("GHG-Absolute", "GHG-fixed-total")
      colInconsistent <- !(colRelative | colAbsolute)
      if (any(colInconsistent)) {
        stop(
          "readUNFCCC_NDC with subtype=", subtype, " noticed that the target column used does not ",
          "correspond to the type used for: ", paste(input$ISO_Code[colInconsistent], collapse = ", ")
        )
      }
    }

    input <- input %>%
      mutate(
        # merge absolute and relative columns
        "Unconditional" = ifelse(is.na(.data$`Unconditional Absolute`), .data$`Unconditional Relative`,
                                 .data$`Unconditional Absolute`),
        "Conditional" = ifelse(is.na(.data$`Conditional Absolute`), .data$`Conditional Relative`,
                               .data$`Conditional Absolute`),
        # if conditional is empty, fill with unconditional
        "Conditional" = ifelse(is.na(.data$Conditional), .data$Unconditional, .data$Conditional)
      ) %>%
      select(-"Conditional Absolute", -"Unconditional Absolute",
             -"Conditional Relative", -"Unconditional Relative")

    # In case a country has two or more types of targets for same year, use GHG-Absolute targets
    # note: the only remaining country in 2021 is MGD Madagascar based on its 2016 submission
    input <- input[!(input$ISO_Code %in% input[duplicated(input[c(1, 4)]), ]$ISO_Code &
                       input$Target_Year %in% input[duplicated(input[c(1, 4)]), ]$Target_Year &
                       input$Type != "GHG-Absolute"), ]

    # Check whether conditional is more stringent than unconditional
    condTrumpsUncond <- (input$Conditional <= input$Unconditional) | is.na(input$Unconditional)
    if (any(!condTrumpsUncond)) {
      stop(
        "readUNFCCC_NDC with subtype=", subtype, ": unconditional target more stringent than conditional in: ",
        paste(input$ISO_Code[!condTrumpsUncond], collapse = ", ")
      )
    }

    # Check whether Types describing emission changes relative to a reference year
    # have a reference year or BAU reference
    ref4change <- input$ISO_Code[input$Reference_Year %in% c("no", NA) &
                                   input$Type %in% c("GHG", "CO2/GDP", "GHG/GDP", "GHG-Absolute")]
    if (length(ref4change) > 0) {
      stop(
        "readUNFCCC_NDC with subtype=", subtype, " has no entry in column reference year in:",
        paste(ref4change, collapse = ", ")
      )
    }

    # as magclass can only cover numerical values well, transform:
    # - Reference_Year: BAU to -1 and 'no'  to -2
    # - Type into number based on allowedType
    input <- input %>%
      dplyr::mutate(
        Reference_Year = dplyr::case_match(
          .data$Reference_Year,
          "BAU" ~ -1,
          "no" ~ -2,
          .default = as.numeric(.data$Reference_Year)
        ),
        Type = match(.data$Type, allowedType)
      ) %>%
      suppressWarnings()

    x <- as.magpie(input, spatial = "ISO_Code", temporal = "Target_Year")

    return(x)
  }
}
