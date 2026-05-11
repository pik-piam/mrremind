#' calculate exogenuous FE and ES demand pathways
#' @description  prepare data for exogenuous FE and ES demand pathways that do not
#' come from EDGE models but from other sources and/or scenario literature.
#' REMIND can be fixed to those demand pathways if the switch cm_exogDem_scen is activated.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer

calcExogDemScen <- function() {
  # Main steps:
  # 1. Read data and define mappings
  # 2. Build FORECAST-based trajectories for steel/cement and GVA-based indices for chemicals/otherInd.
  # 3. Scale chemicals/otherInd with REMIND 2025 values and derive 2030-2050 values.
  # 4. Interpolate all variables linearly from 2050 to SSP2 values in 2100 on REMIND timesteps.
  # 5. Convert dataframe output back to magclass format expected by madrat.

  # 1. Read data and define mappings ----
  data_forecast <- readSource("AriadneDB")

  
  # map FORECAST production from balanced to ariadne_bal
  # map FORECAST production from elec_plus scenario to ariadne_reloc (relocation scenario)
  scenario_mapping <- data.frame(
    scen.forecast = c("KN2045_Bal_v5", "KN2045_Elec_plus_v5"),
    scen.remind = c("ariadne_bal", "ariadne_reloc")
  )

  # mapping of ARIADNE variables for industry production to REMIND industry production variables
  variable_mapping <- data.frame(
    var.remind = c("ue_steel_primary", "ue_steel_secondary", "ue_cement"),
    var.forecast = c(
      "Production|Steel|Primary",
      "Production|Steel|Secondary",
      "Production|Non-Metallic Minerals|Cement"
    )
  )

  # GVA (Gross Value Added), production variables for chemicals and other industry
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

  # define output variables and time periods for industry production adjustment
  output_variables <- c("ue_steel_primary", 
                        "ue_steel_secondary", 
                        "ue_cement", 
                        "ue_chemicals", 
                        "ue_otherInd")
  output_years <- sort(unique(quitte::remind_timesteps$period))
  output_years <- output_years[output_years >= 2005 & output_years <= 2100]
  years_2025_2050 <- output_years[output_years >= 2025 & output_years <= 2050]
  years_2030_2050 <- output_years[output_years >= 2030 & output_years <= 2050]
  years_after_2050 <- output_years[output_years > 2050]
  output_names <- as.vector(outer(
    scenario_mapping$scen.remind,
    output_variables,
    FUN = function(x, y) paste0(x, ".", y)
  ))

  # Convert FORECAST magclass to a clean long dataframe and keep Germany only
  df_forecast <- quitte::as.quitte(data_forecast) %>%
    dplyr::transmute(
      region = as.character(.data$region),
      scenario = as.character(.data$scenario),
      variable = as.character(.data$variable),
      period = as.integer(.data$period),
      value = as.numeric(.data$value)
    ) %>%
    dplyr::filter(.data$region == "DEU")

  # get REMIND industry production trajectories
  feDemScen <- c("SSPs", "SSP2IndiaDEAs", "SSP2_lowEn", "SSP2_highDemDEU", "SSP2_NAV_all")
  remind_base <- calcOutput("FEdemand", scenario = feDemScen, signif = 4, aggregate = FALSE, warnNA = FALSE)
  remind_base_quitte <- quitte::as.quitte(remind_base)

  scenario_raw <- if ("scenario" %in% colnames(remind_base_quitte)) {
    as.character(remind_base_quitte$scenario)
  } else {
    rep(NA_character_, nrow(remind_base_quitte))
  }
  variable_raw <- if ("variable" %in% colnames(remind_base_quitte)) {
    as.character(remind_base_quitte$variable)
  } else {
    rep(NA_character_, nrow(remind_base_quitte))
  }
  item_raw <- if ("item" %in% colnames(remind_base_quitte)) {
    as.character(remind_base_quitte$item)
  } else {
    rep(NA_character_, nrow(remind_base_quitte))
  }

  # Arrange REMIND industry production trajectories to a consistent dataframe and extract SSP2 anchors
  # for DEU and years 2025/2100, independent of whether names are in variable or item.
  df_remind_ssp2 <- data.frame(
    region = as.character(remind_base_quitte$region),
    period = as.integer(remind_base_quitte$period),
    scenario = scenario_raw,
    variable = variable_raw,
    item = item_raw,
    value = as.numeric(remind_base_quitte$value),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      scenario = dplyr::na_if(.data$scenario, "(Missing)"),
      variable = dplyr::na_if(.data$variable, "(Missing)"),
      item = dplyr::na_if(.data$item, "(Missing)"),
      var_name = dplyr::coalesce(.data$item, .data$variable),
      scenario = ifelse(is.na(.data$scenario) & grepl("^SSP2\\.", .data$var_name), "SSP2", .data$scenario),
      var_name = sub("^SSP2\\.", "", .data$var_name)
    ) %>%
    dplyr::filter(
      .data$region == "DEU",
      .data$scenario == "SSP2",
      .data$var_name %in% output_variables,
      .data$period %in% c(2025, 2100)
    ) %>%
    dplyr::select(.data$var_name, .data$period, .data$value)

  # Validate that each variable-year combination has exactly one SSP2 anchor.
  duplicate_remind_rows <- df_remind_ssp2 %>%
    dplyr::count(.data$var_name, .data$period) %>%
    dplyr::filter(.data$n != 1)
  if (nrow(duplicate_remind_rows) > 0) {
    stop("Missing or non-unique SSP2 REMIND baseline values in calcOutput('FEdemand').")
  }

  # Keep unique anchor rows for later joins (2025 scaling and 2100 interpolation target).
  df_remind_anchors <- df_remind_ssp2 %>%
    dplyr::distinct(.data$var_name, .data$period, .keep_all = TRUE)

  # 2. Build FORECAST trajectories and indices until 2050 ----
  # Build steel and cement trajectories directly from FORECAST (2030-2050),
  # map forecast scenario/variable names to REMIND names, and convert Mt -> Gt.
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
      var.remind = .data$var.remind,
      period = .data$period,
      value = .data$value * 1e-3
    )

  if (any(is.na(df_steel_cement_2030_2050$scen.remind)) || any(is.na(df_steel_cement_2030_2050$var.remind))) {
    stop("Could not map FORECAST steel/cement data to REMIND scenarios or variables.")
  }

  # Build aggregated FORECAST GVA trajectories per REMIND scenario/variable.
  # For ue_otherInd, six GVA sectors in gva_mapping are summed to get REMIND other industry sector
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

  # Extract 2025 GVA base values used as denominator for relative indices.
  df_gva_2025 <- df_gva_2025_2050 %>%
    dplyr::filter(.data$period == 2025) %>%
    dplyr::transmute(.data$scen.remind, .data$var.remind, value_2025 = .data$value)

  # Convert absolute GVA values into indices relative to 2025 (index = value/value_2025).
  df_gva_index_2025_2050 <- df_gva_2025_2050 %>%
    dplyr::left_join(df_gva_2025, by = c("scen.remind", "var.remind")) %>%
    dplyr::mutate(index = .data$value / .data$value_2025)

  if (any(is.na(df_gva_index_2025_2050$index))) {
    stop("Missing FORECAST 2025 values for GVA-based scaling.")
  }

  # 3. Scale chemicals/otherInd with REMIND 2025 anchors ----
  # Extract REMIND SSP2 production anchors in 2025 for scaling chemicals/otherInd.
  df_remind_2025 <- df_remind_anchors %>%
    dplyr::filter(.data$period == 2025) %>%
    dplyr::transmute(var.remind = .data$var_name, remind_2025 = .data$value)

  # Scale REMIND 2025 production values with FORECAST GVA indices
  # to derive 2025-2050 chemicals and other industry pathways.
  df_chem_other_2025_2050 <- df_gva_index_2025_2050 %>%
    dplyr::left_join(df_remind_2025, by = "var.remind") %>%
    dplyr::transmute(
      .data$scen.remind,
      .data$var.remind,
      .data$period,
      value = .data$remind_2025 * .data$index
    )

  if (any(is.na(df_chem_other_2025_2050$value))) {
    stop("Missing REMIND 2025 anchors for chemicals or other industry variables.")
  }

  # Combine all values that are explicitly defined up to 2050.
  df_values_until_2050 <- dplyr::bind_rows(
    df_steel_cement_2030_2050,
    df_chem_other_2025_2050
  )

  # 4. Interpolate from 2050 to SSP2 in 2100 on REMIND timesteps ----
  # Extract variable-specific 2050 values as interpolation start points.
  df_2050 <- df_values_until_2050 %>%
    dplyr::filter(.data$period == 2050) %>%
    dplyr::transmute(.data$scen.remind, .data$var.remind, value_2050 = .data$value)

  # Extract SSP2 2100 anchors as interpolation end points.
  df_2100 <- df_remind_anchors %>%
    dplyr::filter(.data$period == 2100) %>%
    dplyr::transmute(var.remind = .data$var_name, value_2100 = .data$value)

  # Generate all scenario-variable-year combinations after 2050 and
  # linearly interpolate between 2050 and 2100 anchors.
  df_after_2050 <- expand.grid(
    scen.remind = scenario_mapping$scen.remind,
    var.remind = output_variables,
    period = years_after_2050,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(df_2050, by = c("scen.remind", "var.remind")) %>%
    dplyr::left_join(df_2100, by = "var.remind") %>%
    dplyr::mutate(
      value = .data$value_2050 + (.data$value_2100 - .data$value_2050) * ((.data$period - 2050) / 50)
    ) %>%
    dplyr::transmute(.data$scen.remind, .data$var.remind, .data$period, .data$value)

  if (any(is.na(df_after_2050$value))) {
    stop("Missing 2050 or 2100 anchors required for interpolation after 2050.")
  }

  # 5. Convert to magclass and return ----
  # Merge pre-2050 and post-2050 values and prepare final REMIND-style series names.
  df_deu_values <- dplyr::bind_rows(df_values_until_2050, df_after_2050) %>%
    dplyr::mutate(name = paste0(.data$scen.remind, ".", .data$var.remind)) %>%
    dplyr::select(.data$period, .data$name, .data$value) %>%
    dplyr::distinct(.data$period, .data$name, .keep_all = TRUE)

  out <- new.magpie(
    cells_and_regions = getItems(data, dim = 1),
    years = output_years,
    names = output_names,
    fill = 0
  )

  for (series_name in output_names) {
    df_name <- df_deu_values %>% dplyr::filter(.data$name == .env$series_name)
    out["DEU", paste0("y", df_name$period), series_name] <- df_name$value
  }

  list(
    x = out,
    weight = NULL,
    unit = c("Gt for ue_steel_primary, ue_steel_secondary, ue_cement; units for ue_chemicals and ue_otherInd from FEdemand"),
    description = "Exogenous demand scenarios based on FORECAST v5 (steel/cement directly, chemicals/other via GVA scaling)"
  )
}
