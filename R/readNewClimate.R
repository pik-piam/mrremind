#' Reads NPI policy database with technology capacity target from the Policy data base (v4 August 2024)
#' by PBL that translate the high impact policies of https://climatepolicydatabase.org/.

#' @description Reads excel sheet with NPi (National Policies Implemented)
#' data on different policy targets (capacity) with different variations:
#' unconditional for minimal and conditional for maximum targets.
#' NPI targets only include targets that are based on implemented policy instruments.

#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW.

#' @return  magpie object
#' @author  Rahel Mandaroux, Léa Hayez, Falk Benke
#' @param subtype Capacity_YYYY_cond or Capacity_YYYY_uncond for Capacity Targets, Emissions_YYYY_cond or
#' Emissions_YYYY_uncond for Emissions targets, with YYYY NDC version year

readNewClimate <- function(subtype) {

  # keep structure to compare when new versions (NPi target updates)
  if (grepl("2025", subtype, fixed = TRUE)) {
    NPIfile <- "NPi_2025-02-03.xlsx"
  } else {
    NPIfile <- "NPi_2025-02-03.xlsx"
    if (!grepl("2025", subtype, fixed = TRUE)) {
      warning("\nNo data for year in ", subtype, " available. Choose default: ", NPIfile)
    }
  }

  if (grepl("Capacity", subtype, fixed = TRUE)) {
    # Capacity/Additional Capacity targets are in GW.
    NPI <- read_excel(NPIfile, sheet = "Capacity_target_PBL_2025",
                      col_types = c("text", "skip", "numeric", "text", "text", "numeric",
                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "skip", "skip", "skip", "skip"))
    x <- as.magpie(NPI, spatial = 1, temporal = 2, datacol = 3)
  } else if (grepl("Emissions", subtype, fixed = TRUE)) {
    input <- suppressMessages(readxl::read_xlsx(
      NPIfile, sheet = "EmissionTargets", skip = 3, na = c("?", "")))

    # select the relevant columns to work upon
    input2 <- dplyr::select(input, 2, 7:14)

    # rename columns
    colnames(input2) <- c("ISO_Code", "Reference_Year", "BAU_or_Reference_emissions_in_MtCO2e", "Target_Year",
                          "Type", "Unconditional", "Conditional", "Uncond2", "Cond2")

    # if no entry in first two quantitative columns, use data from 3rd and 4th
    bothcolumns <- input2$ISO_Code[(!is.na(input2$Unconditional) & !is.na(input2$Uncond2)) |
                                     (!is.na(input2$Conditional) & !is.na(input2$Cond2))]
    if (length(bothcolumns) > 0) {
      warning("readNewClimate with subtype=", subtype, " has values in more than one Uncond/Cond column in: ",
              paste(bothcolumns, collapse = ", "))
    }

    # check consistency of Type. Note: this has to be identical to the definition in 'calcEmiTarget.R'
    allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")

    if (!all(input2$Type %in% allowedType)) {
      warning("Unknown data type used in ", NPIfile, ": ",
              paste(unique(input2$Type)[!unique(input2$Type) %in% allowedType], collapse = ", "),
              ". Please use: ", paste(allowedType, collapse = " or "), ".")
    }

    # drop extra columns
    input2[is.na(input2$Unconditional), ]$Unconditional <- input2[is.na(input2$Unconditional), ]$Uncond2
    input2[is.na(input2$Conditional), ]$Conditional <- input2[is.na(input2$Conditional), ]$Cond2
    input2$Uncond2 <- NULL
    input2$Cond2 <- NULL

    # if conditional is empty, fill with unconditional
    input2[is.na(input2$Conditional), ]$Conditional <- input2[is.na(input2$Conditional), ]$Unconditional

    # in case a country has two or more types of targets for same year, use GHG-Absolute targets
    # note: the only remaining country in 2021 is MGD Madagascar based on its 2016 submission
    input2 <- input2[!(input2$ISO_Code %in% input2[duplicated(input2[c(1, 4)]), ]$ISO_Code &
                         input2$Target_Year %in% input2[duplicated(input2[c(1, 4)]), ]$Target_Year &
                         input2$Type != "GHG-Absolute"), ]

    # check whether conditional is more stringent than unconditional
    condTrumpsUncond <- (input2$Conditional <= input2$Unconditional) | is.na(input2$Unconditional)
    if (any(!condTrumpsUncond)) {
      warning("readNewClimate with subtype=", subtype, ": unconditional target more stringent than conditional in: ",
              paste(input2$ISO_Code[!condTrumpsUncond], collapse = ", "))
    }

    # warning if emission changes have no reference year or BAU reference
    ref4change <- input2$ISO_Code[input2$Reference_Year %in% c("no", NA) &
                                    input2$Type %in% c("GHG", "CO2/GDP", "GHG/GDP", "GHG-Absolute")]
    if (length(ref4change) > 0) {
      warning("readNewClimate with subtype=", subtype, " has no entry in column reference year in:",
              paste(ref4change, collapse = ", "))
    }
    # as magclass can only cover numerical values well, transform: in column 2 BAU into -1, and Type into number based on allowedType
    input2[2] <- as.numeric(unlist(rapply(input2[2], function(x) {
      ifelse(x == "BAU", -1, ifelse(x == "no", -2, x))
    }, how = "replace")))
    input2[5] <- as.numeric(unlist(rapply(input2[5], function(x) match(x, allowedType), how = "replace")))
    # sort with c(1,4,2,3,5,6,7) to get region into first and years into second column
    x <- as.magpie(input2[c(1, 4, 2, 3, 5, 6, 7)], spatial = 1, temporal = 2, datacol = 3)
  } else {
    stop("Incorrect subtype, please use Capacity_YYYY_cond or Emissions_YYYY_cond (or uncond).")
  }
  return(x)
}
