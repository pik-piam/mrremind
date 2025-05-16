#' Reads NDC policy database with capacity, emission, and share targets, originally based on Rogelj et al. 2017
#'
#' @description Reads excel sheet with NDC (Nationally Determined Contributions)
#'  data on different policy targets (capacity, emission, and share targets) with different variations
#'
#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW. Generation/Production targets
#'  are in GWh.
#' @return  magpie object
#' @author  Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux
#' @param subtype Capacity_2023_cond (or 2018/2021/2022 or uncond) for capacity target,
#'  Emissions_2023_cond (or 2018/2021/2022 or uncond) for Emissions targets
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
        "ISO_Code" = 2, "Reference_Year" = 7, "BAU_or_Reference_emissions_in_MtCO2e" = 8, "Target_Year" = 9,
        "Type" = 10, "Unconditional" = 11, "Conditional" = 12, "Uncond2" = 13, "Cond2" = 14
      )

    # Look for entries with entries in both unconditional and/or conditional targets
    bothcolumns <- input$ISO_Code[(!is.na(input$Unconditional) & !is.na(input$Uncond2)) |
                                    (!is.na(input$Conditional) & !is.na(input$Cond2))]
    if (length(bothcolumns) > 0) {
      stop("readUNFCCC_NDC with subtype=", subtype, " has values in more than one Uncond/Cond column in: ",
           paste(bothcolumns, collapse = ", "))
    }

    # Check consistency of Type. Note: this has to be identical to the definition in calcEmiTarget.R
    allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")

    if (any(!input$Type %in% allowedType)) {
      stop(
        "Unknown data type used in ", NDCfile, ": ",
        paste(unique(input$Type)[!unique(input$Type) %in% allowedType], collapse = ", "),
        ". Please use: ", paste(allowedType, collapse = " or "), "."
      )
    }

    # Check that correct columns are used for relative and Mt stuff
    if (!grepl("Emissions_20(18|21)", subtype)) {
      colRelative <- is.na(input$Uncond2) & is.na(input$Cond2) &
        input$Type %in% c("GHG", "GHG/GDP", "CO2/GDP", "GHG/CAP")
      colAbsolute <- is.na(input$Unconditional) & is.na(input$Conditional) &
        input$Type %in% c("GHG-Absolute", "GHG-fixed-total")
      colInconsistent <- !(colRelative | colAbsolute)
      if (any(colInconsistent)) {
        stop(
          "readUNFCCC_NDC with subtype=", subtype, " noticed that the target column used does not ",
          "correspond to the type used for: ", paste(input$ISO_Code[colInconsistent], collapse = ", ")
        )
      }
    }

    # Drop extra columns
    input[is.na(input$Unconditional), ]$Unconditional <- input[is.na(input$Unconditional), ]$Uncond2
    input[is.na(input$Conditional), ]$Conditional <- input[is.na(input$Conditional), ]$Cond2
    input$Uncond2 <- NULL
    input$Cond2 <- NULL

    # If conditional is empty, fill with unconditional
    input[is.na(input$Conditional), ]$Conditional <- input[is.na(input$Conditional), ]$Unconditional

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

    # Check if emission changes have no reference year or BAU reference
    ref4change <- input$ISO_Code[input$Reference_Year %in% c("no", NA) &
                                   input$Type %in% c("GHG", "CO2/GDP", "GHG/GDP", "GHG-Absolute")]
    if (length(ref4change) > 0) {
      stop(
        "readUNFCCC_NDC with subtype=", subtype, " has no entry in column reference year in:",
        paste(ref4change, collapse = ", ")
      )
    }

    # as magclass can only cover numerical values well, transform: in column 2 BAU into -1, and
    # Type into number based on allowedType
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
