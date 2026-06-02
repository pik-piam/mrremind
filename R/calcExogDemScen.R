#' calculate exogenuous FE and ES demand pathways
#' @description  prepare data for exogenuous FE and ES demand pathways that do not
#' come from EDGE models but from other sources and/or scenario literature.
#' REMIND can be fixed to those demand pathways if the switch cm_exogDem_scen is activated.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer

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

  # Map FORECAST scenarios to REMIND scenario names
  scenario_mapping <- data.frame(
    scen.forecast = c("KN2045_Bal_v5", "KN2045_Elec_plus_v5"),
    scen.remind = c("ariadne_bal", "ariadne_reloc")
  )

  # Map steel/cement production variables from FORECAST to REMIND internal names
  variable_mapping <- data.frame(
    var.remind = c("ue_steel_primary", "ue_steel_secondary", "ue_cement"),
    var.forecast = c(
      "Production|Steel|Primary",
      "Production|Steel|Secondary",
      "Production|Non-Metallic Minerals|Cement"
    )
  )

  # Map GVA sectors to REMIND industry variables (6 sectors aggregated to ue_otherInd)
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

  # Define output variables and REMIND timesteps
  output_variables <- c("ue_steel_primary", "ue_steel_secondary", "ue_cement",
                        "ue_chemicals", "ue_otherInd")
  output_years <- sort(unique(quitte::remind_timesteps$period))
  output_years <- output_years[output_years >= 2005 & output_years <= 2100]
  years_2025_2050 <- output_years[output_years >= 2025 & output_years <= 2050]
  years_2030_2050 <- output_years[output_years >= 2030 & output_years <= 2050]
  years_after_2050 <- output_years[output_years > 2050]

  # Create output variable names (scenario.variable combinations)
  output_names <- as.vector(outer(
    scenario_mapping$scen.remind,
    output_variables,
    FUN = function(x, y) paste0(x, ".", y)
  ))

  # Filter FORECAST data for Germany only, keep in quitte format
  df_forecast <- quitte::as.quitte(data_forecast["DEU",,])



  # Get REMIND industry production trajectories
  # Get FEdemand output for SSP2 Germany only (filter early for efficiency)
  # Multiple scenarios needed for cache, but only SSP2 used in output
  feDemScen <- c("SSPs", "SSP2IndiaDEAs", "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")
  remind_base <- calcOutput("FeDemandIndustry", scenarios = feDemScen, signif = 4, aggregate = F)["DEU", , "SSP2"]


  # Extract REMIND production anchors (2025 and 2100)
  df_remind_ssp2 <- quitte::as.quitte(remind_base) %>%
    dplyr::filter(.data$item %in% output_variables, .data$period %in% c(2025, 2100)) %>%
    dplyr::select(variable = .data$item, .data$period, .data$value)

  # Verify each variable-year combination has exactly one SSP2 anchor
  duplicate_check <- df_remind_ssp2 %>%
    dplyr::count(.data$variable, .data$period) %>%
    dplyr::filter(.data$n != 1)

  if (nrow(duplicate_check) > 0) {
    stop("Missing or non-unique SSP2 REMIND baseline values in calcOutput('FEdemand').")
  }

  # Keep distinct anchors for later joins
  df_remind_anchors <- df_remind_ssp2 %>%
    dplyr::distinct(.data$variable, .data$period, .keep_all = TRUE)

  # ---- Step 2: Create trajectories for 2030-2050 for steel and cement ----

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

  if (any(is.na(df_steel_cement_2030_2050$scen.remind)) ||
      any(is.na(df_steel_cement_2030_2050$variable))) {
    stop("Could not map FORECAST steel/cement data to REMIND scenarios or variables.")
  }

  # ---- Step 3: Create trajectories for 2030-2050 for chemicals and other industry ----

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
    dplyr::rename(value_2025 = .data$value) %>%
    dplyr::select(.data$scen.remind, .data$var.remind, .data$value_2025)

  # Convert GVA values to indices relative to 2025
  df_gva_index <- df_gva_2025_2050 %>%
    dplyr::left_join(df_gva_2025, by = c("scen.remind", "var.remind")) %>%
    dplyr::mutate(index = .data$value / .data$value_2025)

  if (any(is.na(df_gva_index$index))) {
    stop("Missing FORECAST 2025 values for GVA-based scaling.")
  }

  # Scale chemicals/otherInd with REMIND 2025 anchors to get 2025-2050 pathways
  df_remind_2025 <- df_remind_anchors %>%
    dplyr::filter(.data$period == 2025) %>%
    dplyr::rename(var.remind = .data$variable, remind_2025 = .data$value) %>%
    dplyr::select(.data$var.remind, .data$remind_2025)

  df_chem_other_2025_2050 <- df_gva_index %>%
    dplyr::left_join(df_remind_2025, by = "var.remind") %>%
    dplyr::transmute(
      scen.remind = .data$scen.remind,
      variable = .data$var.remind,
      period = .data$period,
      value = .data$remind_2025 * .data$index
    )

  if (any(is.na(df_chem_other_2025_2050$value))) {
    stop("Missing REMIND 2025 anchors for chemicals or other industry variables.")
  }

  # Merge all explicitly defined values up to 2050
  df_values_until_2050 <- dplyr::bind_rows(
    df_steel_cement_2030_2050,
    df_chem_other_2025_2050
  )

  # ---- Step 4: Linear interpolation from 2050 to REMIND values in 2100 ----

  # Extract 2050 values as interpolation start points
  df_2050 <- df_values_until_2050 %>%
    dplyr::filter(.data$period == 2050) %>%
    dplyr::rename(value_2050 = .data$value) %>%
    dplyr::select(.data$scen.remind, .data$variable, .data$value_2050)

  # Extract SSP2 2100 anchors as interpolation end points
  df_2100 <- df_remind_anchors %>%
    dplyr::filter(.data$period == 2100) %>%
    dplyr::rename(value_2100 = .data$value) %>%
    dplyr::select(.data$variable, .data$value_2100)

  # Generate all scenario-variable-year combinations after 2050 with linear interpolation
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
    dplyr::select(.data$scen.remind, .data$variable, .data$period, .data$value)

  if (any(is.na(df_after_2050$value))) {
    stop("Missing 2050 or 2100 anchors required for interpolation after 2050.")
  }

  # ---- Step 5: Convert to magclass format ----

  # Merge all pre-2050 and post-2050 values and create final variable names
  df_deu_values <- dplyr::bind_rows(df_values_until_2050, df_after_2050) %>%
                      mutate( region = "DEU") %>%
                      rename( scenario = .data$scen.remind) %>%
                      select( .data$region, .data$period, .data$scenario, .data$variable, .data$value) %>%
                      # only use periods from 2030 on because that's where we start policy runs
                      filter( .data$period >= 2030)

  # convert to magclass output, all other countries outside Germany set to 0
  out <- df_deu_values %>%
            as.magpie(spatial = 1,
                      temporal = 2,
                      datacol = 5) %>%
            toolCountryFill(fill = 0, verbosity = 2)


  list(
    x = out,
    weight = NULL,
    unit = c("Gt for ue_steel_primary, ue_steel_secondary, ue_cement; units for ue_chemicals and ue_otherInd from FEdemand"),
    description = "Exogenous demand scenarios based on FORECAST v5 (steel/cement directly, chemicals/other via GVA scaling)"
  )
}
