#' @importFrom magclass mbind
#' @importFrom dplyr %>%
#' @importFrom readxl read_xlsx
#' @importFrom reshape2 melt
#'
# TODO: rename and document
convertFORECAST <- function() {

  # read in FORECAST data ----
  vars <- c(
    "Production|Steel|Primary",
    "Production|Steel|Secondary",
    "Production|Non-Metallic Minerals|Cement"
  )

  x <- readSource("FORECAST")["DEU", , vars] %>% collapseDim()

  # calculate REMIND baseline trajectories for production variables ----

  # read in baseline-to-policy ratios
  factors <- suppressMessages(read_xlsx(
    toolGetMapping(type = "cell", name = "FORECAST_baseline-to-policy-ratio.xlsx", returnPathOnly = TRUE),
    range = "B3:H13"
  ))[, c(1, 4, 7)]
  names(factors) <- vars
  factors$period <- seq(2005, 2050, 5)
  factors$region <- "DEU"


  factors <- melt(factors, id.vars = c("region", "period")) %>% as.magpie()

  years <- intersect(getItems(x, dim = 2), getItems(factors, dim = 2))

  baseline <- x[, years, ] * factors[, years, ]

  # calculate total FE ----
  # multiply with FORECAST "Specific Energy Consumption": Production [Mt/yr] * SEC [GJ/t] = [PJ/yr] / 1000 = [EJ/yr]
  baseline <- mbind(
    setNames(baseline[, , "Production|Steel|Primary"] * 22 / 1000, "fe_steel_primary_ind"),
    setNames(baseline[, , "Production|Steel|Secondary"] * 4.5 / 1000, "fe_steel_secondary_ind"),
    setNames(baseline[, , "Production|Non-Metallic Minerals|Cement"] * 6.5 / 1000, "fe_cement_ind")
  )

  # calculate FE per energy carrier ----

  fe.carriers.cement <- c("feel_cement", "fega_cement", "feli_cement", "feso_cement")
  fe.carriers.steel <- c("fega_steel", "feli_steel", "feso_steel")
  fe.carriers.steel.split <- c("feel_steel_primary", "feel_steel_secondary")
  fe.carriers.steel.primary <- c(fe.carriers.steel, "feel_steel_primary")
  fe.carriers.steel.secondary <- c(fe.carriers.steel, "feel_steel_secondary")

  fe <- calcOutput("FEdemand", subtype = "FE", aggregate = FALSE)["DEU", , c(fe.carriers.cement, fe.carriers.steel, fe.carriers.steel.split)][, years, "gdp_SSP2EU"] %>% collapseDim()

  fe.shares <- mbind(
    setNames(fe[, , "feso_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feso_steel_primary"),
    setNames(fe[, , "feli_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feli_steel_primary"),
    setNames(fe[, , "fega_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_fega_steel_primary"),
    setNames(fe[, , "feel_steel_primary"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feel_steel_primary"),

    setNames(fe[, , "feso_steel"] / dimSums(fe[, , fe.carriers.steel.secondary], dim = 3), "share_feso_steel_secondary"),
    setNames(fe[, , "feli_steel"] / dimSums(fe[, , fe.carriers.steel.secondary], dim = 3), "share_feli_steel_secondary"),
    setNames(fe[, , "fega_steel"] / dimSums(fe[, , fe.carriers.steel.secondary], dim = 3), "share_fega_steel_secondary"),
    setNames(fe[, , "feel_steel_secondary"] / dimSums(fe[, , fe.carriers.steel.secondary], dim = 3), "share_feel_steel_secondary"),

    setNames(fe[, , "feso_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feso_cement"),
    setNames(fe[, , "feli_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feli_cement"),
    setNames(fe[, , "fega_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_fega_cement"),
    setNames(fe[, , "feel_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feel_cement")
  )

  baseline <- mbind(
    setNames(baseline[, , "fe_steel_primary_ind"] * fe.shares[,,"share_feso_steel_primary"], "feso_steel_primary_ind"),
    setNames(baseline[, , "fe_steel_primary_ind"] * fe.shares[,,"share_feli_steel_primary"], "feli_steel_primary_ind"),
    setNames(baseline[, , "fe_steel_primary_ind"] * fe.shares[,,"share_fega_steel_primary"], "fega_steel_primary_ind"),
    setNames(baseline[, , "fe_steel_primary_ind"] * fe.shares[,,"share_feel_steel_primary"], "feel_steel_primary_ind"),

    setNames(baseline[, , "fe_steel_secondary_ind"] * fe.shares[,,"share_feso_steel_secondary"], "feso_steel_secondary_ind"),
    setNames(baseline[, , "fe_steel_secondary_ind"] * fe.shares[,,"share_feli_steel_secondary"], "feli_steel_secondary_ind"),
    setNames(baseline[, , "fe_steel_secondary_ind"] * fe.shares[,,"share_fega_steel_secondary"], "fega_steel_secondary_ind"),
    setNames(baseline[, , "fe_steel_secondary_ind"] * fe.shares[,,"share_feel_steel_secondary"], "feel_steel_secondary_ind"),

    setNames(baseline[, , "fe_cement_ind"] * fe.shares[,,"share_feso_cement"], "feso_cement_ind"),
    setNames(baseline[, , "fe_cement_ind"] * fe.shares[,,"share_feli_cement"], "feli_cement_ind"),
    setNames(baseline[, , "fe_cement_ind"] * fe.shares[,,"share_fega_cement"], "fega_cement_ind"),
    setNames(baseline[, , "fe_cement_ind"] * fe.shares[,,"share_feel_cement"], "feel_cement_ind")
  )
}
