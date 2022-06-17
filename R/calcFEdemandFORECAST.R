#' @importFrom magclass mbind
#' @importFrom dplyr %>%
#' @importFrom readxl read_xlsx
#' @importFrom reshape2 melt
#'

calcFEdemandFORECAST <- function() {

  # 1. read in primary/secondary steel and cement from FORECAST (policy) run ----
  vars <- c(
    "Production|Steel|Primary",
    "Production|Steel|Secondary",
    "Production|Non-Metallic Minerals|Cement"
  )

  x <- readSource("FORECAST")["DEU", , vars] %>% collapseDim()

  # reduce COVID dip in 2020
  x[, 2020, ] <- (x[, 2020, ] + x[, 2018, ]) / 2

  # 2. calculate baseline trajectories for production variables ----

  # read in baseline-to-policy ratios from a previous REMIND run
  factors <- suppressMessages(read_xlsx(
    toolGetMapping(type = "cell", name = "FORECAST_baseline-to-policy-ratio.xlsx", returnPathOnly = TRUE),
    range = "B3:H11"
  ))[, c(1, 4, 7)]
  names(factors) <- vars
  factors$period <- c(2018, seq(2020, 2050, 5))
  factors$region <- "DEU"
  factors <- melt(factors, id.vars = c("region", "period")) %>% as.magpie()

  baseline <- x * factors
  getItems(baseline, dim = 3) <- c("ue_steel_primary", "ue_steel_secondary", "ue_cement")

  # 3. calculate total FE ----

  # calculate SEC cement from REMIND
  fe.carriers.cement <- c("feel_cement", "fega_cement", "feli_cement", "feso_cement")
  dem <- calcOutput("FEdemand", subtype = "FE", aggregate = FALSE)["DEU", 2015, "gdp_SSP2EU"]
  sec.cement <- as.numeric(dimSums(dem[, , fe.carriers.cement], dim = 3) / dem[, , "ue_cement"])

  # multiply with FORECAST "Specific Energy Consumption": Production [Mt/yr] * SEC [GJ/t] = [PJ/yr] / 1000 = [EJ/yr]
  fc.overwrite <- mbind(
    baseline,
    setNames(baseline[, , "ue_steel_primary"] * 22 / 1000, "fe_steel_primary"),
    setNames(baseline[, , "ue_steel_secondary"] * 4.5 / 1000, "fe_steel_secondary"),
    setNames(baseline[, , "ue_cement"] * sec.cement / 1000, "fe_cement")
  )

  # convert Production [Mt/yr] to [Gt/yr]
  fc.overwrite[, , c("ue_steel_primary", "ue_steel_secondary", "ue_cement")] <- fc.overwrite[, , c("ue_steel_primary", "ue_steel_secondary", "ue_cement")] / 1000

  # 4. calculate share of FE carriers from SSP2EU REMIND trajectories ----
  fe.carriers <- c(fe.carriers.cement, "fega_steel", "feli_steel", "feso_steel", "feel_steel_primary", "feel_steel_secondary")
  fe.carriers.steel.primary <- c("fega_steel", "feli_steel", "feso_steel", "feel_steel_primary")

  fe <- calcOutput("FEdemand", subtype = "FE", aggregate = FALSE)["DEU", , fe.carriers][, , "gdp_SSP2EU"] %>% collapseDim()

  fe.shares <- mbind(
    setNames(fe[, , "feso_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feso_steel_primary"),
    setNames(fe[, , "feli_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feli_steel_primary"),
    setNames(fe[, , "fega_steel"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_fega_steel_primary"),
    setNames(fe[, , "feel_steel_primary"] / dimSums(fe[, , fe.carriers.steel.primary], dim = 3), "share_feel_steel_primary"),
    setNames(fe[, , "feso_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feso_cement"),
    setNames(fe[, , "feli_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feli_cement"),
    setNames(fe[, , "fega_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_fega_cement"),
    setNames(fe[, , "feel_cement"] / dimSums(fe[, , fe.carriers.cement], dim = 3), "share_feel_cement")
  )

  # use 2015 shares for 2018
  fe.shares <- add_columns(fe.shares, addnm = "y2018", dim = 2, fill = NA)
  fe.shares[, 2018, ] <- fe.shares[, 2015, ]
  fe.shares <- fe.shares[, intersect(getItems(fe.shares, dim = 2), getItems(fc.overwrite, dim = 2))]

  fc.overwrite <- mbind(
    fc.overwrite,
    setNames(fc.overwrite[, , "fe_steel_primary"] * fe.shares[, , "share_feso_steel_primary"], "feso_steel"),
    setNames(fc.overwrite[, , "fe_steel_primary"] * fe.shares[, , "share_feli_steel_primary"], "feli_steel"),
    setNames(fc.overwrite[, , "fe_steel_primary"] * fe.shares[, , "share_fega_steel_primary"], "fega_steel"),
    setNames(fc.overwrite[, , "fe_steel_primary"] * fe.shares[, , "share_feel_steel_primary"], "feel_steel_primary"),
    setNames(fc.overwrite[, , "fe_steel_secondary"], "feel_steel_secondary"),
    setNames(fc.overwrite[, , "fe_cement"] * fe.shares[, , "share_feso_cement"], "feso_cement"),
    setNames(fc.overwrite[, , "fe_cement"] * fe.shares[, , "share_feli_cement"], "feli_cement"),
    setNames(fc.overwrite[, , "fe_cement"] * fe.shares[, , "share_fega_cement"], "fega_cement"),
    setNames(fc.overwrite[, , "fe_cement"] * fe.shares[, , "share_feel_cement"], "feel_cement")
  )


  # 5. create new SSP2EU-ariadne demand scenario from SSP2EU ----
  demand <- calcOutput("FEdemand", subtype = "FE", aggregate = FALSE)[, , "gdp_SSP2EU"]
  vars <- intersect(getNames(fc.overwrite), getNames(demand, dim = 2))
  getItems(demand, dim = 3.1) <- "gdp_SSP2EU-ariadne"
  demand["DEU", seq(2020, 2050, 5), vars] <- fc.overwrite[, seq(2020, 2050, 5), vars]
  demand["DEU", seq(2005, 2015, 5), vars] <- fc.overwrite[, 2018, vars]
  demand["DEU", seq(2055, 2150, 5), vars] <- fc.overwrite[, 2050, vars]

  return(list(
    x = demand, weight = NULL,
    unit = c("Gt/yr", "EJ/yr"),
    description = "demand pathways for final energy in buildings and industry, industry harmonized with FORECAST"
  ))
}
