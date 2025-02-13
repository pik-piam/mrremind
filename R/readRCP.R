#' Read RCP
#' Read in RCP data
#'
#' @param subtype Either 'Waste' or 'AviationShipping'
#' @return magpie object of the RCP data
#' @author Julian Oeser
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type="RCP", subtype="Waste")
#' }
#'
readRCP <- function(subtype) {

  dat <- as.data.frame(do.call(rbind, lapply(list.files(pattern = ".xls$"), readxl::read_excel)))

  if (subtype == "Waste") {
    dat <- dat %>%
      filter(.data$Region %in% c("R5ASIA", "R5LAM", "R5MAF", "R5OECD", "R5REF"), grepl("Waste", .data$Variable)) %>%
      tidyr::pivot_longer(cols = matches('^[0-9]*$'),
                          names_to = 'year',
                          names_transform = list(year = as.numeric)) %>%
      tidyr::separate("Variable", c("type", "source"), sep = " - ") %>%
      select(region = 'Region', 'year', scenario = 'Scenario', 'type', 'source', 'value') %>%
      mutate(region = sub('^R5', '', .data$region))

  } else if (subtype == "AviationShipping") {
    dat <- dat %>%
      filter(.data$Region == "World", grepl("Aviation|Shipping", .data$Variable)) %>%
      tidyr::pivot_longer(cols = matches('^[0-9]*$'),
                          names_to = 'year',
                          names_transform = list(year = as.numeric)) %>%
      mutate(Region = "GLO") %>%
      tidyr::separate("Variable", c("type", "source"), sep = " - ") %>%
      select(region = 'Region', 'year', scenario = 'Scenario', 'type', 'source', 'value')
  } else {
    stop("Invalid subtype. Must be 'Waste' or 'AviationShipping'")
  }

  dat$scenario <- dplyr::recode(dat$scenario,
                                `IMAGE - RCP3-PD (2.6)`="rcp26",
                                `MiniCAM - RCP 4.5` = "rcp45",
                                `AIM - RCP 6.0` = "rcp60",
                                `MESSAGE - RCP 8.5` = "rcp85")

  dat$source <- dplyr::recode(dat$source,
                              `Waste (landfills, wastewater, non-energy incineration)`="Waste",
                              `International Shipping`="InternationalShipping")

  dat$type <- dplyr::recode(dat$type,
                            `CH4 emissions`="ch4",
                            `Sulfur emissions`="so2",
                            `Black Carbon emissions`="bc",
                            `Organic Carbon Emissions`="oc",
                            `CO emissions`="CO",
                            `NOx emissions`="NOx",
                            `VOC emissions`="VOC",
                            `NH3 emissions`="nh3")

  dat <- dat[stats::complete.cases(dat),]

  as.magpie(dat, spatial = 1 , datacol = 6)
}
