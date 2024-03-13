#' @importFrom magclass new.magpie as.magpie getYears
#' @importFrom readxl read_excel


readNIR <- function(subtype) {

  sheets <- c("1B2" = "Table1.B.2")

  sheet <- toolSubtypeSelect(subtype, sheets)

  files <- list.files(pattern = ".xlsx$")
  countries <- unique(regmatches(files, regexpr("[A-Z]{3}", files)))
  years <- paste0("y", unique(substr(gsub("[^0-9]", "", files, ""), 5, 8)))

  if (subtype == "1B2") {
    data <- new.magpie(countries, years, names = c("Oil", "Gas", "VentingOil", "VentingGas",
                                                   "FlaringOil", "FlaringGas"), sets = c("region", "year", "emissions"))
    for (i in files) {
      country <- regmatches(i, regexpr("[A-Z]{3}", i))
      year <- paste0("y", substr(gsub("[^0-9]", "", i, ""), 5, 8))

      nir <- read_excel(i, sheet = sheet, skip = 4, .name_repair = "unique_quiet")
      nir <- unname(unlist(nir[, 10]))[c(4, 11, 20, 21, 24, 25)]
      nir[which(nir %in% c("NE", "IE", "NO", "NA", NA))] <- 0
      nir <- suppressWarnings(as.numeric(nir))

      data[country, year, ] <- nir
    }
  } else {
    stop("This it not a valid subtype for readNIR")
  }
  x <- as.magpie(data)
  x[is.na(x)] <- 0
  getYears(x) <- years
  return(x)
}
