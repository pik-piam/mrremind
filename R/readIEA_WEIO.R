#' IEA World Energy Investment Outlook
#'
#' Read 2015-2024 investments statistical data in energy sector (electricity, oil, gas)
#' IEA World Energy Investment Outlook (2024)
#' (https://www.iea.org/data-and-statistics/data-product/world-energy-investment-2024-datafile)
#'
#' @author Nicolas Bauer, Falk Benke
#'
readIEA_WEIO <- function() {

  dataFile <- file.path("2024", "WorldEnergyInvestment2024_DataFile.xlsx")
  sheets <- setdiff(readxl::excel_sheets(dataFile), c("Cover", "Notes_Web"))

  data <- NULL
  for (sheet in sheets) {

    # encode the section per row of read in data (after filtering empty rows)
    sections <- c(rep("Total", 2), rep("Fuels", 6), rep("Electricity", 9), rep("End-Use", 4), rep("Other", 2))

    d <- readxl::read_excel(path = dataFile, sheet = sheet, range = "B4:L31")
    d <- d %>%
      filter(!is.na(.data[[colnames(d)[1]]])) %>%
      mutate("section" = sections) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "period") %>%
      mutate("region" = sheet)


    colnames(d)[1] <- "name"

    d <- d %>%
      mutate("variable" = paste0(.data$section, "|", .data$name)) %>%
      select("region", "period", "variable", "value")

    data <- rbind(data, d)
  }

  x <- as.magpie(data, spatial = 1)
  return(x)
}
