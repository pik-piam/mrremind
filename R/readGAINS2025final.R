#' Read and combine air pollution emissions, activities and emission factors from GAINS data
#'
#' There's no associated convert function, as the disaggregation takes
#' a combination of subtypes, and it is easier to carry out most
#' calculations at the GAINS regional level first and then disaggregate
#' the results
#'
#' @return  Activity levels, emissions or emission factors at the level of
#'          25 GAINS regions, 35 GAINS sectors and 7 species:
#'          magclass object with dimensions region, year, and ssp.scenario.sectorGAINS.species
#' @author Gabriel Abrahao, Laurin Koehler-Schindler
#' @param subtype "emifacs", "emissions", "activities"
#' @importFrom stats na.omit
#' @importFrom utils timestamp globalVariables
#' @importFrom tidyr complete

utils::globalVariables(c(
  "EMF30_AGG", "EMF30_DET", "EMF30_MIXED", "Group_Region", "POLLUTANT_FRACTION",
  "SCENARIO", "VARIABLE", "VARIANT", "region", "scen", "scenario",
  "sectorGAINS", "species", "ssp", "value", "vartype", "year"
))


readGAINS2025final <- function(subtype) {
  # ================================================================================================
  # SECTOR INFORMATION =============================================================================
  # ================================================================================================
  sectors <- read.csv("GAINS2025_sectors_2025-07-02.csv")
  sectors_agg <- na.omit(unique(sectors$EMF30_AGG))
  # Aggregated GAINS sectors do not contain Waste, thus these selected sectors are added from the
  # detailed GAINS sectors
  sectors_det_selected <- c(
    "Waste_Solid_Municipal",
    "Waste_Water_Municipal"
  )
  # According to email "ScenarioMIP air pollution EFs - CORRECTION to Transformation"
  # from Zig (GAINS team) on July 4, the following sectors need to be corrected
  # for historical and ScenarioMIP emission factors
  sectors_to_correct <- c(
    "Transformations_Coal",
    "Transformations_NatGas"
  )
  # According to emails "ScenarioMIP air pollution EFs - CORRECTION to Transformation" and
  # "Re: ScenarioMIP air pollution EFs - 1 July - FINAL" from Zig (GAINS team) on July 4
  # the following sectors should be ignored
  sectors_to_ignore <- c(
    "CHEMBULK",
    "Transformations_HLF",
    "Transformations_LLF"
  )

  # Function to read in aggregated sectors except those to be ignored (sectors_to_ignore) and
  # selected detailed sectors (sectors_det_selected), and
  readSectors <- function(filepath_beginning, filepath_ending) {
    # Read in aggregated sectors except those to be ignored (sectors_to_ignore)
    df_agg_sectors <- read.csv(paste0(filepath_beginning, "_agg_", filepath_ending)) %>%
      rename(EMF30_MIXED = EMF30_AGG) %>%
      filter(!(EMF30_MIXED %in% sectors_to_ignore))
    # Read in selected detailed sectors (sectors_det_selected)
    df_sel_det_sectors <- read.csv(paste0(filepath_beginning, "_det_", filepath_ending)) %>%
      rename(EMF30_MIXED = EMF30_DET) %>%
      filter(EMF30_MIXED %in% sectors_det_selected)
    # Combine
    df_sectors <- rbind(df_agg_sectors, df_sel_det_sectors)
    return(df_sectors)
  }

  # Function to correct selected sectors (sectors_to_correct)
  correctSectors <- function(df_uncorrected, filepath_ending) {
    # Read in correct data
    df_NatGas <- read.csv(paste0("Transformations_NatGas_", filepath_ending)) %>%
      rename(EMF30_MIXED = EMF30_AGG)
    df_Coal <- read.csv(paste0("Transformations_Coal_", filepath_ending)) %>%
      rename(EMF30_MIXED = EMF30_AGG)
    # Remove sectors to correct from uncorrected dataframe
    df_uncorrected <- df_uncorrected %>% filter(!(EMF30_MIXED %in% sectors_to_correct))
    # Add correct data
    df_corrected <- rbind(df_uncorrected, df_Coal, df_NatGas)
  }

  # ================================================================================================
  # AIR POLLUTANT NAMES ============================================================================
  # ================================================================================================

  # Function to maps from GAINS2025 to REMIND (oldGAINS) air pollutant names
  fixPolNames <- function(mag) {
    polnamesmap <- c(
      "CO" = "CO",
      "NOx" = "NOX",
      "BC" = "PM_BC",
      "OC" = "PM_OC",
      "SO2" = "SO2",
      "NH3" = "NH3",
      "VOC" = "VOC"
    )
    mag <- mag[, , polnamesmap]
    getItems(mag, "species") <- names(polnamesmap)
    return(mag)
  }

  if (subtype == "emifacs") {
    # ================================================================================================
    # EMISSION FACTORS ===============================================================================
    # Read emission factors for historical timesteps, ScenarioMIP scenarios, CLE, SLE and MTFR =======
    # Combine aggregated sectors and selected detailed sectors (Waste) ===============================
    # Remove or correct erroneous data according to information from GAINS team ======================
    # ================================================================================================

    # HISTORICAL =====================================================================================
    histefs <- readSectors("emission_factors", "hist_final_2025-07-02.csv")
    histefs <- correctSectors(histefs, "hist-2025-07-04-CORRECTION.csv")

    # Convert to long format and standardize column names
    histefs <- pivot_longer(histefs, 7:length(names(histefs)), names_prefix = "X", names_to = "year") %>%
      select(-timestamp) %>%
      rename(
        ssp = scen,
        scenario = VARIANT,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION
      )

    # SCENARIOMIP ===================================================================================
    # These differ by SSP (1, 2, 3 and 5) and by scenario (H, HL, M, ML, L, VLHO, VLLO). But:
    # 1) All scenarios expect VLLO are identical and only differ by SSP,
    #    thus removing duplicates and calling this scenario "SMIPbySSP".
    # 2) For the VLLO scenario, all SSPs are identical,
    #    thus removing duplicates and calling the SSP "SMIPVLLO".
    smipefs <- readSectors("emission_factors", "ssp_variant_final_2025-07-02.csv")
    smipefs <- correctSectors(smipefs, "SSP_variant-2025-07-04-CORRECTION.csv")

    # Convert to long format and standardize column names
    smipefs <- pivot_longer(smipefs, 7:length(names(smipefs)), names_prefix = "X", names_to = "year") %>%
      select(-timestamp) %>%
      rename(
        ssp = scen,
        scenario = VARIANT,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION
      )

    # 1) Filtering for one scenario (not VLLO) per SSP, removing the others, and calling it "SMIPbySSP"
    smipefsSMIPbySSP <- smipefs %>%
      filter((ssp %in% c("SSP1", "SSP2", "SSP3") & scenario == "Medium") | (ssp == "SSP5" & scenario == "High")) %>%
      mutate(scenario = "SMIPbySSP")
    # 2) Filtering for VLLO and SSP1, removing the others, and calling it SMIPVLLO
    smipefsSMIPVLLO <- smipefs %>%
      filter(ssp == "SSP1" & scenario == "Very Low Limited Overshot") %>%
      mutate(ssp = "SMIPVLLO", scenario = "SMIPVLLO")
    # Combining the filtered dataframes
    smipefs <- rbind(smipefsSMIPbySSP, smipefsSMIPVLLO)
    rm(smipefsSMIPbySSP, smipefsSMIPVLLO)

    # Maximum Technically Feasible Reduction (MTFR) ==================================================
    # No corrections for Transformations_Coal and Transformations_NatGas
    mtfrefs <- readSectors("SSPs_IMAGE_emf", "mtfr_2025-07-02.csv")

    # Convert to long format and standardize column names
    mtfrefs <- pivot_longer(mtfrefs, 5:length(names(mtfrefs)), names_prefix = "X", names_to = "year") %>%
      rename(
        scenario = SCENARIO,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION
      ) %>%
      mutate(
        ssp = "MTFR",
        scenario = "MTFR"
      )

    # Strong Legislation Emissions (SLE) =============================================================
    # No corrections for Transformations_Coal and Transformations_NatGas
    sleefs <- readSectors("SSPs_IMAGE_emf", "middle_2025-07-02.csv")

    # Convert to long format and standardize column names
    sleefs <- pivot_longer(sleefs, 5:length(names(sleefs)), names_prefix = "X", names_to = "year") %>%
      rename(
        ssp = scen,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION
      ) %>%
      mutate(
        scenario = "SLE"
      )

    # Current Legislation Emissions (CLE) =============================================================
    # No corrections for Transformations_Coal and Transformations_NatGas
    # Also contains historical emission factors. Remove ???
    cleefs <- readSectors("SSPs_IMAGE_emf", "cle_rev_2025-07-02.csv")

    # Convert to long format and standardize column names
    cleefs <- pivot_longer(cleefs, 5:length(names(cleefs)), names_prefix = "X", names_to = "year") %>%
      rename(
        ssp = scen,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION
      ) %>%
      mutate(
        scenario = "CLE"
      )

    # Remove historical emissions factors and historical years.
    # These are read in from a different source file for scenario "historical".
    cleefs <- cleefs %>% filter(ssp != "historical", year >= 2025)

    # COMPLETE DATA FRAMES TO ENSURE THAT ALL SECTOR x SPECIES x REGION COMBINATIONS ARE PRESENT =====
    # Years 1990 - 2020 (7 time steps, 25 regions, 35 sectors, 7 species = 42875)
    histefs <- histefs %>%
      complete(ssp, scenario, region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year)
    # Years 2025 - 2100 (16 time steps, 25 regions, 35 sectors, 7 species = 98000)
    smipefs <- smipefs %>%
      group_by(ssp, scenario) %>%
      complete(region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year)
    cleefs <- cleefs %>%
      complete(ssp, scenario, region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year)
    # Years 2030 - 2100 (15 time steps, 25 regions, 35 sectors, 7 species = 91875)
    mtfrefs <- mtfrefs %>%
      complete(ssp, scenario, region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year)
    sleefs <- sleefs %>%
      complete(ssp, scenario, region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year)

    # COMBINE ALL SCENARIOS ===========================================================================
    allefs <- rbind(histefs, smipefs, mtfrefs, sleefs, cleefs) %>%
      select(ssp, scenario, sectorGAINS, species, region, year, value)

    out <- as.magpie(allefs, spatial = "region", temporal = "year")
    comment(out) <- "GAINS2025 emission factors for different scenarios and SSPs at the level of 25 GAINS regions, 35 GAINS sectors and 7 species."
  } else if (subtype %in% c("emissions", "activities")) {
    # ================================================================================================
    # EMISSIONS AND ACTIVITIES =======================================================================
    # Read emissions and activities for baseline scenario ============================================
    # Combine aggregated sectors and selected detailed sectors (Waste) ===============================
    # Remove or correct erroneous data according to information from GAINS team ======================
    # ================================================================================================

    # BASELINE =======================================================================================
    # No corrections for Transformations_Coal and Transformations_NatGas
    baseactemi <- readSectors("IMAGE_emf", "activity_emission_2025-07-02.csv")

    # Convert to long format and standardize column names
    baseactemi <- pivot_longer(baseactemi, 6:length(names(baseactemi)), names_prefix = "X", names_to = "year") %>%
      rename(
        scenario = SCENARIO,
        region = Group_Region,
        sectorGAINS = EMF30_MIXED,
        species = POLLUTANT_FRACTION,
        vartype = VARIABLE
      ) %>%
      mutate(
        ssp = "baseline",
        scenario = "baseline"
      )

    # COMPLETE DATA FRAME TO ENSURE THAT ALL SECTOR x SPECIES x REGION COMBINATIONS ARE PRESENT =====
    baseactemi <- baseactemi %>%
      complete(ssp, scenario, region, sectorGAINS = na.omit(unique(sectors$EMF30_MIXED)), species, year, vartype)

    if (subtype == "emissions") {
      baseactemi <- baseactemi %>% filter(vartype == "EMISSION")
    } else {
      baseactemi <- baseactemi %>% filter(vartype == "ACTIVITY")
    }

    baseactemi <- baseactemi %>%
      select(ssp, scenario, sectorGAINS, species, region, year, value)

    out <- as.magpie(baseactemi, spatial = "region", temporal = "year")
    if (subtype == "emissions") {
      comment(out) <- "GAINS2025 emissions for the baseline scenario at the level of 25 GAINS regions, 35 GAINS sectors and 7 species.."
    } else {
      comment(out) <- "GAINS2025 activities for the baseline scenario at the level of 25 GAINS regions, 35 GAINS sectors and 7 species.."
    }
  } else {
    stop(paste0("Unknown subtype: ", subtype))
  }

  # Change air pollutant names
  out <- fixPolNames(out)

  return(out)
}
