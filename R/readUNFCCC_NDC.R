#' Reads NDC policy database with capacity, emission, and share targets, originally based on Rogelj et al. 2017
#' @description Reads excel sheet with NDC (Nationally Determined Contributions)
#' data on different policy targets (capacity, emission, and share targets) with different variations
#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW. Generation/Production targets are in GWh.
#' @return  magpie object
#' @author  Aman Malik, Christoph Bertram, Oliver Richters, Sophie Fuchs, Rahel Mandaroux
#' @param subtype Capacity_2023_cond (or 2018/2021/2022 or uncond) for capacity target,
#' Emissions_2023_cond (or 2018/2021/2022 or uncond) for Emissions targets
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select

readUNFCCC_NDC <- function(subtype) {

  if (grepl("2018", subtype, fixed = TRUE)) {
    NDCfile <- "NDC_2018.xlsx"
  } else if (grepl("2021", subtype, fixed = TRUE)) {
    NDCfile <- "NDC_2021.xlsx"
  } else if (grepl("2022", subtype, fixed = TRUE)) {
    NDCfile <- "NDC_2022-12-31.xlsx"
  } else if (grepl("2023", subtype, fixed = TRUE)) {
    NDCfile <- "NDC_2023-11-29.xlsx"
  } else {
    NDCfile <- "NDC_2024-08-31.xlsx"
    if (!grepl("2024", subtype, fixed = TRUE)) {
      warning("\nNo data for year in ", subtype, " available. Choose default: ", NDCfile)
    }
  }

  if (grepl("Capacity", subtype, fixed = TRUE)) {
    # Capacity/Additional Capacity targets are in GW. Generation/Production targets are in GWh.
    NDC <- read_excel(NDCfile, sheet = "Capacity_target",
                      col_types = c("text", "skip", "numeric", "text", "text", "numeric",
                        "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip"))
    x <- as.magpie(NDC, spatial = 1, temporal = 2, datacol = 3)
  }
  if (grepl("Emissions", subtype, fixed = TRUE)) {
    input <- read_xlsx(NDCfile, sheet = "Emissions", skip = 3, na = c("?", ""))
    # select the relevant columns to work upon
    input2 <- select(input, 2, 7:14)
    # rename columns
    colnames(input2) <- c("ISO_Code", "Reference_Year", "BAU_or_Reference_emissions_in_MtCO2e", "Target_Year",
                          "Type", "Unconditional", "Conditional", "Uncond2", "Cond2")
    # if no entry in first two quantitative columns, use data from 3rd and 4th
    bothcolumns <- input2$ISO_Code[(! is.na(input2$Unconditional) & ! is.na(input2$Uncond2)) |
                                   (! is.na(input2$Conditional) & ! is.na(input2$Cond2))]
    if (length(bothcolumns) > 0) {
        warning("readUNFCCC_NDC with subtype=", subtype, " has values in more than one Uncond/Cond column in: ",
                paste(bothcolumns, collapse = ", "))
    }
    # check consistency of Type. Note: this has to be identical to the definition in calcEmiTarget.R
    allowedType <- c("GHG-Absolute", "GHG", "GHG/GDP", "CO2/GDP", "GHG-fixed-total", "GHG/CAP")
    if (FALSE %in% (input2$Type %in% allowedType)) {
      warning("Unknown data type used in ", NDCfile, ": ",
              paste(unique(input2$Type)[!unique(input2$Type) %in% allowedType], collapse = ", "),
              ". Please use: ", paste(allowedType, collapse = " or "), ".")
    }
    # check that correct columns are used for relative and Mt stuff
    if (! grepl("Emissions_20(18|21)", subtype)) {
        colRelative <- is.na(input2$Uncond2) & is.na(input2$Cond2) &
                       input2$Type %in% c("GHG", "GHG/GDP", "CO2/GDP", "GHG/CAP")
        colAbsolute <- is.na(input2$Unconditional) & is.na(input2$Conditional) &
                       input2$Type %in% c("GHG-Absolute", "GHG-fixed-total")
        colInconsistent <- ! (colRelative | colAbsolute)
        if (any(colInconsistent)) {
            warning("readUNFCCC_NDC with subtype=", subtype, " noticed that the target column used does not ",
                    "correspond to the type used for: ", paste(input2$ISO_Code[colInconsistent], collapse = ", "))
        }
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
    input2 <- input2[!(input2$ISO_Code %in% input2[duplicated(input2[c(1, 4)]), ]$ISO_Code
                     & input2$Target_Year %in% input2[duplicated(input2[c(1, 4)]), ]$Target_Year
                     & input2$Type != "GHG-Absolute"), ]

    # check whether conditional is more stringent than unconditional
    condTrumpsUncond <- (input2$Conditional <= input2$Unconditional) | is.na(input2$Unconditional)
    if (any(! condTrumpsUncond)) {
      warning("readUNFCCC_NDC with subtype=", subtype, ": unconditional target more stringent than conditional in: ",
              paste(input2$ISO_Code[! condTrumpsUncond], collapse = ", "))
    }

    # warning if emission changes have no reference year or BAU reference
    ref4change <- input2$ISO_Code[input2$Reference_Year %in% c("no", NA) &
                                  input2$Type %in% c("GHG", "CO2/GDP", "GHG/GDP", "GHG-Absolute")]
    if (length(ref4change) > 0) {
      warning("readUNFCCC_NDC with subtype=", subtype, " has no entry in column reference year in:",
              paste(ref4change, collapse = ", "))
    }
    # as magclass can only cover numerical values well, transform: in column 2 BAU into -1, and Type into number based on allowedType
    input2[2] <- as.numeric(unlist(rapply(input2[2], function(x) 
                            ifelse(x == "BAU", -1, ifelse(x == "no", -2, x)), how = "replace")))
    input2[5] <- as.numeric(unlist(rapply(input2[5], function(x) match(x, allowedType), how = "replace")))
    # sort with c(1,4,2,3,5,6,7) to get region into first and years into second column
    x <- as.magpie(input2[c(1, 4, 2, 3, 5, 6, 7)], spatial = 1, temporal = 2, datacol = 3)
  }
  return(x)
}
