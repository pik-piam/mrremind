#' Read EU Reference Scenario
#'
#' Read EU Reference Scenario .xlsx file as magpie object
#'
#' @return magpie object of EU reference scenario data by country. Units follow REMIND report conventions and conversion factor is defined in EUReferenceScenario2REMIND.xlsx file.
#' @param subtype data subtype. Either "2016" or "2020"
#' @author Renato Rodrigues, Falk Benke
#' @examples
#' \dontrun{
#' test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = FALSE)
#' }
#' @importFrom readxl excel_sheets read_excel
#' @importFrom reshape2 melt
#' @importFrom stats aggregate

readEU_ReferenceScenario <- function(subtype) {
  if (!subtype %in% c("2016", "2020")) {
    stop("Invalid subtype. Must be either 2016 or 2020")
  }

  mapping <- NULL

  # load mapping and data
  mapping$A <- suppressMessages(read_excel(paste0("EUReferenceScenario2REMIND_", subtype, ".xlsx"), sheet = "A"))
  mapping$B <- suppressMessages(read_excel(paste0("EUReferenceScenario2REMIND_", subtype, ".xlsx"), sheet = "B"))

  if (subtype == "2016") {
    source_file <- "AppendixRefSce.xls"
    sheets <- excel_sheets(source_file)
    sheets <- sheets[-c(1, 2, 3, length(sheets))]
    no_rows <- 12
  } else {
    source_file <- "ref2020_energy-transport-ghg.xlsx"
    sheets <- excel_sheets(source_file)
    sheets <- sheets[-c(1, 2, 3)]
    no_rows <- 11
  }

  # looping through regions, filtering and converting values
  data <- lapply(sheets, function(sheet) {
    type <- substr(sheet, nchar(sheet), nchar(sheet)) # A or B
    region <- substr(sheet, start = 1, stop = 2)
    countrySheet <- suppressMessages(read_excel(source_file, sheet = sheet, skip = 1))
    # cleaning sheet
    countrySheet <- countrySheet[, seq(1, no_rows)]
    # replace with remind mapping
    countrySheet$REMIND <- mapping[[type]]$REMIND[-1]
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
    return(countrySheet)
  })
  # merge into single dataframe
  data <- do.call("rbind", data)
  # long format
  data <- melt(data, id.vars = 1:2)
  colnames(data) <- c("variable", "region", "period", "value")
  # dump contents into magpie
  x <- as.magpie(data, spatial = 2, datacol = 4, temporal = 3)
  return(x)
}
