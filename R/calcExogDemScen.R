#' calculate exogenuous FE and ES demand pathways
#' @description  prepare data for exogenuous FE and ES demand pathways that do not
#' come from EDGE models but from other sources and/or scenario literature.
#' REMIND can be fixed to those demand pathways if the switch cm_exogDem_scen is activated.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer
#'
#' @importFrom dplyr select

calcExogDemScen <- function() {
  # Exogenous industry production scenarios for Germany used in the Ariadne project
  # For steel and cement take production values in Mt/yr directly from FORECAST for 2030-2050
  # For chemicals and other industry production measures in gross value added, GVA, in REMIND),
  # we take 2030-2050 FORECAST values indexed to 2025 production
  # (because of different potential sectoral scopes of the model)
  #
  # Steps:
  # 1. Read data and define mappings
  # 2. Create trajectories for 2030-2050 for steel and cement
  # 3. Create trajectories for 2030-2050 for chemicals and other industry
  # 4. Linear interpolation from 2050 to REMIND values in 2100
  # 5. Convert to magclass format

  # ---- Step 1: Read data and define mappings ----

  data_forecast <- readSource("AriadneDB")

  # Filter FORECAST data for Germany only, keep in quitte format
  df_forecast <- quitte::as.quitte(data_forecast["DEU", , ])

  # Map FORECAST scenarios to REMIND scenario names
  scenario_mapping <- data.frame(
    scen.forecast = c("KN2045_Bal_v5", "KN2045_Elec_plus_v5"),
    scen.remind = c("ariadne_bal", "ariadne_reloc")
  )

  # Get REMIND industry production trajectories
  # Get FEdemand output for SSP2 Germany only (filter early for efficiency)
  # Multiple scenarios needed for cache, but only SSP2 used in output
  feDemScen <- c("SSPs", "SSP2IndiaDEAs", "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")
  remind_base <- calcOutput("FeDemandIndustry", scenarios = feDemScen, signif = 4, aggregate = F)["DEU", , "SSP2"]

  # industry production variables to adapt
  output_variables <- c(
    "ue_steel_primary",
    "ue_steel_secondary",
    "ue_cement",
    "ue_chemicals",
    "ue_otherInd"
  )

  # Extract REMIND production values needed for the FORECAST-based trajectories (2025 and 2100)
  df_remind_ssp2 <- quitte::as.quitte(remind_base) %>%
    dplyr::filter(.data$item %in% output_variables, .data$period %in% c(2025, 2100)) %>%
    select("variable" = "item", "period", "value")

  # Required mappings

  # Map steel/cement production variables from FORECAST to REMIND internal names
  variable_mapping <- data.frame(
    var.remind = c("ue_steel_primary", "ue_steel_secondary", "ue_cement"),
    var.forecast = c(
      "Production|Steel|Primary",
      "Production|Steel|Secondary",
      "Production|Non-Metallic Minerals|Cement"
    )
  )

  # Map FORECAST industry gross added value variables to REMIND variables
  # (6 sectors aggregated to ue_otherInd)
  gva_mapping <- data.frame(
    var.forecast = c(
      "Gross Value Added|Industry|Chemicals",
      "Gross Value Added|Industry|Vehicle Construction",
      "Gross Value Added|Industry|Pulp and Paper",
      "Gross Value Added|Industry|Other Sectors",
      "Gross Value Added|Industry|Non-Ferrous metals",
      "Gross Value Added|Industry|Food and Tobacco",
      "Gross Value Added|Industry|Engineering"
    ),
    var.remind = c(
      "ue_chemicals",
      rep("ue_otherInd", 6)
    )
  )

  # ---- Step 2: Create trajectories for 2030-2050 for steel and cement ----

  # time steps to adapt
  output_years <- sort(unique(quitte::remind_timesteps$period)) # REMIND timesteps
  output_years <- output_years[output_years >= 2005 & output_years <= 2100]
  years_2030_2050 <- output_years[output_years >= 2030 & output_years <= 2050]

  # Take steel/cement trajectories directly from FORECAST in 2030-2050 period
  # Map scenario/variable names and convert units (Mt -> Gt)
  df_steel_cement_2030_2050 <- df_forecast %>%
    dplyr::filter(
      .data$scenario %in% scenario_mapping$scen.forecast,
      .data$variable %in% variable_mapping$var.forecast,
      .data$period %in% years_2030_2050
    ) %>%
    dplyr::left_join(scenario_mapping, by = c("scenario" = "scen.forecast")) %>%
    dplyr::left_join(variable_mapping, by = c("variable" = "var.forecast")) %>%
    dplyr::transmute(
      scen.remind = .data$scen.remind,
      variable = .data$var.remind,
      period = .data$period,
      value = .data$value * 1e-3
    )


  # ---- Step 3: Create trajectories for 2030-2050 for chemicals and other industry ----

  # time steps to adapt
  years_2025_2050 <- output_years[output_years >= 2025 & output_years <= 2050]


  # Aggregate FORECAST industry gross value added (GVA) outside of steel, cement chemicals
  # to get to production of other industry sector in REMIND
  df_gva_2025_2050 <- df_forecast %>%
    dplyr::filter(
      .data$scenario %in% scenario_mapping$scen.forecast,
      .data$variable %in% gva_mapping$var.forecast,
      .data$period %in% years_2025_2050
    ) %>%
    dplyr::left_join(scenario_mapping, by = c("scenario" = "scen.forecast")) %>%
    dplyr::left_join(gva_mapping, by = c("variable" = "var.forecast")) %>%
    dplyr::group_by(.data$scen.remind, .data$var.remind, .data$period) %>%
    dplyr::summarise(value = sum(.data$value), .groups = "drop")

  # Extract 2025 GVA base values for indexing
  # Chemicals and Other Industry trajectories should start with REMIND value in 2025,
  # but then follow FORECAST trend until 2050
  df_gva_2025 <- df_gva_2025_2050 %>%
    dplyr::filter(.data$period == 2025) %>%
    dplyr::rename("value_2025" = "value") %>%
    select("scen.remind", "var.remind", "value_2025")

  # Convert GVA values to indices relative to 2025
  df_gva_index <- df_gva_2025_2050 %>%
    dplyr::left_join(df_gva_2025, by = c("scen.remind", "var.remind")) %>%
    dplyr::mutate(index = .data$value / .data$value_2025)


  # Scale chemicals/otherInd GVA to follow the the relative development of FORECAST trajectories in 2030-2050 on
  # (i.e. 2025 values are still the same as in REMIND)
  df_remind_2025 <- df_remind_ssp2 %>%
    dplyr::filter(.data$period == 2025) %>%
    dplyr::rename(
      "var.remind" = "variable",
      "remind_2025" = "value"
    ) %>%
    select("var.remind", "remind_2025")

  df_chem_other_2025_2050 <- df_gva_index %>%
    dplyr::left_join(df_remind_2025, by = "var.remind") %>%
    dplyr::transmute(
      scen.remind = .data$scen.remind,
      variable = .data$var.remind,
      period = .data$period,
      value = .data$remind_2025 * .data$index
    )


  # Merge all explicitly defined values up to 2050
  df_values_until_2050 <- dplyr::bind_rows(
    df_steel_cement_2030_2050,
    df_chem_other_2025_2050
  )

  # ---- Step 4: Linear interpolation from 2050 to REMIND values in 2100 ----

  # time steps to adapt
  years_after_2050 <- output_years[output_years > 2050]

  # Extract 2050 values as interpolation starting points
  df_2050 <- df_values_until_2050 %>%
    dplyr::filter(.data$period == 2050) %>%
    dplyr::rename("value_2050" = "value") %>%
    select("scen.remind", "variable", "value_2050")

  # Extract SSP2 2100 REMIND production as interpolation end points
  df_2100 <- df_remind_ssp2 %>%
    dplyr::filter(.data$period == 2100) %>%
    dplyr::rename("value_2100" = "value") %>%
    select("variable", "value_2100")

  # linearly interpolate production from FORECAST-derived values in 2050 to REMIND value in 2100
  df_after_2050 <- expand.grid(
    scen.remind = scenario_mapping$scen.remind,
    variable = output_variables,
    period = years_after_2050,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(df_2050, by = c("scen.remind", "variable")) %>%
    dplyr::left_join(df_2100, by = "variable") %>%
    dplyr::mutate(
      value = .data$value_2050 + (.data$value_2100 - .data$value_2050) * ((.data$period - 2050) / 50)
    ) %>%
    select("scen.remind", "variable", "period", "value")


  # ---- Step 5: Convert to magclass format ----

  # Merge all pre-2050 and post-2050 values and create final variable names
  df_deu_values <- dplyr::bind_rows(df_values_until_2050, df_after_2050) %>%
    mutate(region = "DEU") %>%
    rename("scenario" = "scen.remind") %>%
    select("region", "period", "scenario", "variable", "value") %>%
    # only use periods from 2030 on because that's where we start policy runs
    filter(.data$period >= 2030)


  # convert to magclass output, all other countries outside Germany set to 0
  out <- df_deu_values %>%
    as.data.frame() %>%
    as.magpie(
      spatial = 1,
      temporal = 2,
      datacol = 5
    ) %>%
    toolCountryFill(fill = 0, verbosity = 2)


  list(
    x = out,
    weight = NULL,
    unit = c("Gt for ue_steel_primary, ue_steel_secondary, ue_cement; units for ue_chemicals and ue_otherInd from FEdemand"),
    description = "Exogenous demand scenarios based on FORECAST v5 (steel/cement directly, chemicals/other via GVA scaling)"
  )
}
