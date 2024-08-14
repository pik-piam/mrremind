#' Read Global Energy Monitor data
#'
#' read GEM data for all available technologies and relevant statuses
#'
#' @author Rahel Mandaroux, Falk Benke, Pascal Weigmann
#'
#' @importFrom dplyr filter mutate select
#' @importFrom readxl read_xlsx
#' @importFrom rlang sym
#'
#' @export
readGlobalEnergyMonitor <- function() {
  # GEM GIPT 2024
  # file available after filling out questionnaire:
  # https://globalenergymonitor.org/projects/global-integrated-power-tracker/download-data/
  d <- read_excel("Global-Integrated-Power-June-2024.xlsx",
                  sheet = "Power facilities",
                  trim_ws = TRUE,
                  col_types = "text") %>%
    select(variable = "Type",
           tech = "Technology",
           region = "Country/area",
           value = "Capacity (MW)",
           status = "Status",
           start = "Start year",
           end = "Retired year") %>%
    filter(status %in% c("announced", "pre-construction", "construction", "operating")) %>%
    # ASSUMPTION: rows with empty start year are ignored
    # only look at pipeline until 2030
    filter(!is.na(start), start < 2031) %>%
    # Oil/Gas needs to be separated by technology or fuel (seems complicated?)
    # remove for now
    filter(variable != "oil/gas") %>%
    mutate(start = as.numeric(start), end = as.numeric(end), value = as.numeric(value))

  # no end year defined:
  # use average lifetime of technology (from generisdata_tech)
  lifetime <- c(coal = 40, bioenergy = 40, nuclear = 50, hydropower = 130,
                geothermal = 30, wind = 25, solar = 30)
  for (tech in names(lifetime)) {
    d[is.na(d$end) & d$variable == tech, "end"] <-
      d[is.na(d$end) & d$variable == tech, "start"] + lifetime[[tech]]
  }

  # use proper variable names
  d[d$variable == "coal", "variable"]       <- "Cap|Electricity|Coal"
  d[d$variable == "bioenergy", "variable"]  <- "Cap|Electricity|Biomass"
  d[d$variable == "nuclear", "variable"]    <- "Cap|Electricity|Nuclear"
  # pumped storage would be available as tech category if differentiation needed
  d[d$variable == "hydropower", "variable"] <- "Cap|Electricity|Hydro"
  d[d$variable == "geothermal", "variable"] <- "Cap|Electricity|Geothermal"

  # separate wind into on- and offshore, "Unknown"/"blank" are all onshore
  offshore_tech <-  c("Offshore hard mount", "Offshore floating", "Offshore mount unknown")
  d[d$variable == "wind" & d$tech %in% offshore_tech, "variable"] <- "Cap|Electricity|Wind|Offshore"
  d[d$variable == "wind", "variable"] <- "Cap|Electricity|Wind|Onshore"

  # separate solar into PV and CSP (called "Solar Thermal" by GEM)
  d[d$variable == "solar" & d$tech == "Solar Thermal", "variable"] <- "Cap|Electricity|Solar|CSP"
  d[d$variable == "solar", "variable"] <- "Cap|Electricity|Solar|PV"

  # transform data from list of capacities with start and end date
  # to sum of active capacities in 2025 and 2030
  d <- mutate(d, end = pmin(end, 2030))  # improves performance of next step
  tmp_list <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    tmp_list[[i]] <- data.frame(
      d[i, c("region", "value", "variable", "status")],
      period = seq(as.numeric(d[i, "start"]), as.numeric(d[i, "end"]), 1)
    )
  }
  tmp <- do.call(rbind, tmp_list) %>%
    group_by(region, variable, status, period) %>%
    summarise(value = sum(value)) %>%
    filter(period %in% c(2025, 2030))

  # convert to magclass object
  x <- as.magpie(tmp, spatial = "region")
  x[is.na(x)] <- 0

  # add solar and wind totals as variables
  x <- mbind(
    x,
    dimSums(x[, , c("Cap|Electricity|Solar|PV", "Cap|Electricity|Solar|CSP")], dim = 3.1) %>%
      add_dimension(dim = 3.1, add = "variable", nm = "Cap|Electricity|Solar"),
    dimSums(x[, , c("Cap|Electricity|Wind|Offshore", "Cap|Electricity|Wind|Onshore")], dim = 3.1) %>%
      add_dimension(dim = 3.1, add = "variable", nm = "Cap|Electricity|Wind")
  )

  # collapse variable and status dimension into one
  # getNames(x) <- gsub(pattern = "\\.", replacement = "|", x = getNames(x))

  # convert MW to GW
  x <- x / 1000
  x <- add_dimension(x, dim = 3.4, add = "unit", nm = "GW")
  x <- add_dimension(x, dim = 3.1, add = "model", nm = "GlobalEnergyMonitor")

  return(x)
}
