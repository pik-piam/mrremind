#' Helper to validate data read in from UNFCCC NDC and New Climate databases and
#' apply some pre-processing
#'
#' @seealso [readUNFCCC_NDC()], [readNewClimate()]
#'
#' @param input data frame representing the data from climate target database
#' @param database database to be read in, used for logging info
#' @param subtype database version to be read in, used for logging info
toolProcessClimateTargetDatabase <- function(input, database, subtype) {
  
  # Check that no row has both absolute and relative conditional/unconditional
  bothcolumns <- input$ISO_Code[(!is.na(input$`Unconditional Absolute`) & !is.na(input$`Unconditional Relative`)) |
                                  (!is.na(input$`Conditional Absolute`) & !is.na(input$`Conditional Relative`))]
  if (length(bothcolumns) > 0) {
    stop(database, " with subtype=", subtype, " has both absolute and relative emission targets in: ",
         paste(bothcolumns, collapse = ", "))
  }
  
  # Check for valid types. Note: this has to be identical to the definition in convert function
  allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")
  
  if (any(!input$Type %in% allowedType)) {
    stop(
      "Unknown data type used in ", database, " with subtype=", subtype, ": ",
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
    #no values should be in the wrong types
    colRelative <- is.na(input$`Conditional Relative`) & is.na(input$`Unconditional Relative`) &
      input$Type %in% c("GHG-Absolute", "GHG-fixed-total")
    colAbsolute <- is.na(input$`Conditional Absolute`) & is.na(input$`Unconditional Absolute`) &
      input$Type %in% c("GHG", "GHG/GDP", "CO2/GDP", "GHG/CAP")
    colInconsistent <- !(colRelative | colAbsolute)
    if (any(colInconsistent)) {
      stop(
        database, " with subtype=", subtype, " noticed that the target column used does not ",
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
  if (any(grepl("2026", subtype, fixed = TRUE))) {
    input <- input[!(input$ISO_Code %in% input[duplicated(input[c(1, 4,7)]), ]$ISO_Code &
                     input$Target_Year %in% input[duplicated(input[c(1, 4,7)]), ]$Target_Year &
                     input$Type != "GHG-Absolute"), ]
  } else { 
    input <- input[!(input$ISO_Code %in% input[duplicated(input[c(1, 4)]), ]$ISO_Code &
                       input$Target_Year %in% input[duplicated(input[c(1, 4)]), ]$Target_Year &
                       input$Type != "GHG-Absolute"), ]
  }
  # Check whether conditional is more stringent than unconditional
  input$Conditional <- as.numeric(input$Conditional)
  input$Unconditional <- as.numeric(input$Unconditional)
  
  condTrumpsUncond <- (input$Conditional <= input$Unconditional) | is.na(input$Unconditional)
  if (any(!condTrumpsUncond)) {
    stop(
      database, " with subtype=", subtype, ": unconditional target more stringent than conditional in: ",
      paste(input$ISO_Code[!condTrumpsUncond], collapse = ", ")
    )
  }
  
  # Check whether Types describing emission changes relative to a reference year
  # have a reference year or BAU reference
  ref4change <- input$ISO_Code[input$Reference_Year %in% c("no", NA) &
                                 input$Type %in% c("GHG", "CO2/GDP", "GHG/GDP", "GHG-Absolute")]
  if (length(ref4change) > 0) {
    stop(
      database, " with subtype=", subtype, " has no entry in column reference year in:",
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
        "no"  ~ -2,
        .default = as.numeric(.data$Reference_Year)
      ),
      Type = match(.data$Type, allowedType)
    ) %>%
    suppressWarnings()
  
  if ("LULUCF" %in% names(input)) {
    input <- input %>%
      dplyr::mutate(
        LULUCF = dplyr::case_match(
          .data$LULUCF,
          "unknown"   ~ 0,
          "Including" ~ 1,
          "Excluding" ~ -1,
          .default    = as.numeric(.data$LULUCF)
        )
      ) %>%
      suppressWarnings()
  }
  
  return(input)
}
