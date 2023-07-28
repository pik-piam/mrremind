#' Read EU Reference Scenario
#'
#' Read EU Reference Scenario .xlsx file as magpie object
#'
#' @return magpie object of EU reference scenario data by country. Units follow
#'   REMIND report conventions and conversion factor is defined in
#'   EUReferenceScenario2REMIND.xlsx file.
#' @param subtype data subtype. Either "techAssump.*", "2016" or "2020"
#' @author Renato Rodrigues, Falk Benke, Robin Hasse
#' @examples
#' \dontrun{
#' test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = FALSE)
#' }
#' @importFrom readxl excel_sheets read_excel
#' @importFrom openxlsx read.xlsx
#' @importFrom reshape2 melt
#' @importFrom stats aggregate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim
#' @importFrom tidyr pivot_longer
#' @export

readEU_ReferenceScenario <- function(subtype) {


  # Technology assumptions -----------------------------------------------------

  if (grepl("^techAssump\\..+$", subtype)) {

    subsubtype <- sub("^techAssump\\.", "", subtype)

    # read excel sheet
    rawdata <- read.xlsx(
      file.path("ref2020_technology_assumptions",
                "REF2020_Technology Assumptions_Energy.xlsx"),
      sheet = subsubtype, startRow = 2, colNames = FALSE, skipEmptyRows = TRUE,
      fillMergedCells = TRUE)

    # remove notes at the bottom
    data <- rawdata[cumsum(grepl("^Notes", rawdata[, 1])) == 0, ]



    if (subsubtype %in% c("Domestic", "Industry", "Power&Heat")) {

      ## Sectors ====

      # split subsector tables (Residential & Services)
      splitLines <- unlist(apply(data, 1, function(l) length(unique(l)) == 1))
      subsectors <- data[splitLines, 1]
      if (any(splitLines)) {
        splitLines <- seq_along(splitLines)[which(splitLines)]
        data <- lapply(seq_along(splitLines), function(i) {
          start <- splitLines[i] + 1
          end <- if (i == length(splitLines)) nrow(data) else splitLines[i + 1] - 1
          data[start:end, ]
        })
        names(data) <- subsectors
      } else {
        data <- list(subsubtype = data)
        names(data) <- subsubtype
      }

      # pivot variables to long format
      data <- lapply(data, function(chunk) {
        chunk[1, ] <- trimws(gsub("(\n.*$)|,", "", chunk[1, ]))
        variables <- unique(unlist(chunk[1, 2:ncol(chunk)]))
        varCols <- lapply(variables, function(var) which(chunk[1, ] == var))
        names(varCols) <- variables

        if (subsubtype %in% c("Domestic", "Industry")) {
          # identify group lines (end uses and units)
          groupLines <- do.call(cbind, lapply(varCols, function(cols) {
            data.frame(
              var = apply(chunk[, cols], 1, function(l) length(unique(l))))
          }))
          groupLines <- as.vector(apply(groupLines, 1, function(l) all(l == 1)))
          groupLines[1:3] <- FALSE

          # add end use column
          enduses <- chunk[groupLines, 1]
          enduseIndex <- cumsum(groupLines)
          enduseIndex[enduseIndex == 0] <- NA
          chunk[["enduse"]] <- enduses[enduseIndex]

          # save units
          if (subsubtype == "Domestic") {
            units <- do.call(cbind, lapply(varCols, function(cols) {
              dfCol <- data.frame(
                var = apply(chunk[groupLines, cols], 1, function(l) unique(l)))
              colnames(dfCol) <- chunk[1, head(cols, 1)]
              return(dfCol)
            }))
            units[["enduse"]] <- chunk[groupLines, 1]
            units <- pivot_longer(units, -"enduse",
                                  names_to = "variable",
                                  values_to = "unit")
            units[["unit"]] <- sub("^in ", "", units[["unit"]])
          }
          chunk <- chunk[!groupLines,]

          # treat regional heat pump data
          chunk <- chunk[!grepl("Heat pump air", chunk[, 1]), ]
          hpLines <- grepl("in .* countries", chunk[, 1], ignore.case = TRUE)
          chunk[["region"]] <- ifelse(hpLines, sub("in ", "", chunk[, 1]), NA)
          chunk[["region"]] <- sub("Countries", "countries", chunk[["region"]])
          chunk[, 1] <- ifelse(hpLines, "Heat pump air", chunk[, 1])
        }

        # pivot to long format
        fixCols <- c("technology", "enduse", "region")
        colnames <- apply(chunk[1:3, ], 2, paste, collapse = "_")
        colnames[which(colnames(chunk) %in% fixCols)] <-
          colnames(chunk)[which(colnames(chunk) %in% fixCols)]
        colnames[1] <- "technology"
        chunk <- chunk[4:nrow(chunk), ]
        colnames(chunk) <- colnames
        headerCols <- if (subsubtype == "Power&Heat") {
          c("variable", "unit", "period")
        } else {
          c("variable", "pointInTime", "level")
        }
        chunk <- pivot_longer(chunk, -intersect(colnames(chunk), fixCols),
                              names_to = headerCols,
                              names_sep = "_")
        chunk[["value"]] <- as.numeric(chunk[["value"]])
        if ("level" %in% colnames(chunk)) {
          chunk[chunk[["level"]] == "NA", "level"] <- "central"
        }
        if (subsubtype == "Power&Heat") {
          chunk[["period"]] = as.numeric(chunk[["period"]])
        }

        # add unit column
        if (subsubtype == "Domestic") {
          chunk <- left_join(chunk, units, by = c("variable", "enduse"))
        } else if (subsubtype == "Industry") {
          units <- "EUR\\/kW|equal to 1 in 2015"
          chunk[["unit"]] <- sub(paste0(".*(", units, ").*"), "\\1",
                                 chunk[["variable"]])
          chunk[["variable"]] <- trimws(gsub(paste0(units, "|\\(|\\)"), "",
                                             chunk[["variable"]]))
        }

        return(chunk)
      })

      # merge to one df with subsector column
      data <- do.call(rbind, lapply(names(data), function(subsector) {
        chunk <- data[[subsector]]
        chunk[["subsector"]] <- subsector
        return(chunk)
      }))

    } else if (subsubtype == "Renovation Costs") {

      ## Renovation Costs ====

      colnames(data) <- data[1, ]
      colnames(data)[1:2] <- c("region", "renovation")
      data <- data[2:nrow(data), ]

      data <- pivot_longer(data, -c("region", "renovation"),
                           names_to = "variable",
                           values_to = "value")
      data[["unit"]] <- sub(".*\\((.*)\\)$", "\\1", data[["variable"]])
      data[["variable"]] <- trimws(sub("\\(.*\\)$", "", data[["variable"]]))
      data[["value"]] <- as.numeric(data[["value"]])

    } else {

      stop("Invalid type of technology assumption: ", subsubtype)

    }

    # convert to magpie object
    if ("region" %in% colnames(data)) {
      data[is.na(data[["region"]]), "region"] <- "EUR"
    }
    x <- collapseDim(as.magpie(as.quitte(data)))

    return(x)

  }



  if (!subtype %in% c("2016", "2020")) {
    stop("Invalid subtype. Must be either 'techAssump.*', '2016' or '2020'")
  }



  # Results --------------------------------------------------------------------

  mapping <- NULL

  # load mapping and data
  mapping$A <- suppressMessages(read_excel(paste0("EUReferenceScenario2REMIND_", subtype, ".xlsx"), sheet = "A"))
  mapping$B <- suppressMessages(read_excel(paste0("EUReferenceScenario2REMIND_", subtype, ".xlsx"), sheet = "B"))

  if (subtype == "2016") {
    source_file <- "AppendixRefSce.xls"
    sheets <- excel_sheets(source_file)
    sheets <- sheets[-c(1, 2, 3, length(sheets))]
    no_rows <- 12
    columns <- c("REMIND")
  } else {
    source_file <- "ref2020_energy-transport-ghg.xlsx"
    sheets <- excel_sheets(source_file)
    sheets <- sheets[-c(1, 2, 3)]
    no_rows <- 11
    columns <- c("REMIND", "REMIND_2")
  }

  data <- NULL
  # looping through regions, filtering and converting values
  for (sheet in sheets) {
    for (column in columns) {
      type <- substr(sheet, nchar(sheet), nchar(sheet)) # A or B
      region <- substr(sheet, start = 1, stop = 2)
      countrySheet <- suppressMessages(read_excel(source_file, sheet = sheet, skip = 1))
      # cleaning sheet
      countrySheet <- countrySheet[, seq(1, no_rows)]
      # replace with remind mapping
      countrySheet$REMIND <- mapping[[type]][[column]][-1]
      # removing extra name column
      countrySheet <- countrySheet[, -1]
      # making sure the data is numeric
      countrySheet[, -length(colnames(countrySheet))] <- sapply(countrySheet[, -length(colnames(countrySheet))], as.numeric)
      # converting unit to REMIND unit
      countrySheet[, -length(colnames(countrySheet))] <- countrySheet[, -length(colnames(countrySheet))] * mapping[[type]]$factor[-1]
      # remove empty rows
      countrySheet <- countrySheet[-which(is.na(countrySheet$REMIND)), ]
      countrySheet <- cbind(region, countrySheet)
      countrySheet[is.na(countrySheet)] <- 0
      # merge repeated items
      countrySheet <- aggregate(. ~ REMIND + region, data = countrySheet, FUN = sum)
      data <- rbind(data, countrySheet)
    }
  }

  data <- aggregate(. ~ REMIND + region, data = data, FUN = sum)

  # long format
  data <- melt(data, id.vars = 1:2)
  colnames(data) <- c("variable", "region", "period", "value")
  # dump contents into magpie
  x <- as.magpie(data, spatial = 2, datacol = 4, temporal = 3)
  return(x)
}
