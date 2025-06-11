#' generate F-Gases based on IMAGE data
#'
#' @param subtype "IMAGElegacy" will use the old IMAGE data, "IMAGE2025" will use the new IMAGE2025 data.
#' @param interp "interpolate2025" will intepolate from EDGAR historical data from 2025 to interpyear.
#' To account for the very old IMAGE scenarios in IMAGElegacy,
#' this used to be 2050 but is now flexible. Any other interp value will ignore this step.
#' @param interpyear The year to interpolate to, default is 2030.
#' interpyear is only used if interp is set to "interpolate2025".
#'
#' @return magpie object with F-gases information
#' @author Lavinia Baumstark, Gabriel Abrahao
#' @examples
#' \dontrun{
#' x <- calcOutput("FGas")
#' }
calcFGas <- function(subtype = "IMAGE2025", interp = "interpolate2025", interpyear = 2030) {
  # Mapping for the REMIND variable names. Works with both versions
  # of the IMAGE data
  emi_mapping <- list(
    "Emissions|F-Gases" = "emiFgasTotal",
    "Emissions|HFC" = "emiFgasHFC",
    "Emissions|HFC|HFC125" = "emiFgasHFC125",
    "Emissions|HFC|HFC134a" = "emiFgasHFC134a",
    "Emissions|HFC|HFC143a" = "emiFgasHFC143a",
    "Emissions|HFC|HFC227ea" = "emiFgasHFC227ea",
    "Emissions|HFC|HFC23" = "emiFgasHFC23",
    "Emissions|HFC|HFC245fa" = "emiFgasHFC245fa",
    "Emissions|HFC|HFC32" = "emiFgasHFC32",
    "Emissions|HFC|HFC43-10" = "emiFgasHFC43-10",
    "Emissions|PFC" = "emiFgasPFC",
    "Emissions|SF6" = "emiFgasSF6",
    "Emissions|C2F6" = "emiFgasC2F6",
    "Emissions|C6F14" = "emiFgasC6F14",
    "Emissions|CF4" = "emiFgasCF4"
  )

  if (subtype == "IMAGElegacy") {
    # read in IMAGE data
    x <- readSource("IMAGE")
    # make the dimension needed for REMIND
    getNames(x, dim = 1) <- gsub("-", ".", getNames(x, dim = 1))
    x <- clean_magpie(x, what = "sets")
    x <- collapseNames(x)
    getSets(x)[2] <- "Year"
    getSets(x)[4] <- "rcp"
    getSets(x)[5] <- "SPAscen"
    getSets(x)[6] <- "Variable"
    getSets(x)[7] <- "Unit"

    # change names to the REMIND-set-names
    getNames(x, dim = 1) <- paste0("forcing_", getNames(x, dim = 1))
    getNames(x, dim = 2) <- gsub("34", "37", getNames(x, dim = 2))
    getNames(x, dim = 2) <- paste0("rcp", getNames(x, dim = 2))
    getNames(x, dim = 2) <- gsub("rcpRef", "none", getNames(x, dim = 2))
    getNames(x, dim = 3) <- gsub("[1-9]", "x", getNames(x, dim = 3))

    xCO2 <- x[, , "Emissions|CO2|Fossil Fuels and Industry"]
    x <- x[, , "Emissions|CO2|Fossil Fuels and Industry", invert = TRUE]


    getNames(x, dim = 4) <- emi_mapping[getNames(x, dim = 4)]

    # remove the unit dimension
    x <- collapseNames(x, collapsedim = 5)
    xCO2 <- collapseNames(xCO2, collapsedim = 4)

    # To calculate SSP4 and SSP5 policy emissions we scale SSP2 policy F-gas emissions
    # with SSP5 F-gas baselines and correct for the ratio of CO2 baseline
    # emissions which is a proxy for the CO2 mitigation effort and therefore
    # for the CO2 price.
    # Fgas_SSP5_pol = Fgas_SSP2_pol *
    # (Fgas_SSP5_ref / Fgas_SSP2_ref) *
    # (CO2_SSP5_ref / CO2_SSP2_ref)

    x_SSP5 <- new.magpie(getRegions(x), getYears(x), getNames(x[, , "forcing_SSP2"]))
    getNames(x_SSP5) <- gsub("forcing_SSP2", "forcing_SSP5", getNames(x_SSP5))
    x_SSP5 <- x_SSP5[, , "none", invert = TRUE]

    # loop over rcp scenarios
    for (r in getNames(x_SSP5, dim = 2)) {
      x_SSP5[, , "forcing_SSP5"][, , r] <- collapseNames(x[, , "forcing_SSP2"])[, , r] *
        (collapseNames(x[, , "forcing_SSP5"][, , "none"]) / collapseNames(x[, , "forcing_SSP2"][, , "none"])) *
        (collapseNames(xCO2[, , "forcing_SSP5"][, , "none"]) / collapseNames(xCO2[, , "forcing_SSP2"][, , "none"]))
    }
    x <- mbind(x, x_SSP5)
    x[is.na(x)] <- 0

    # interpolate time to match REMIND years
    rem_years <- c(seq(2015, 2105, 10), seq(2110, 2150, 5))
    rem_years <- seq(2005, 2150, 5)
    x <- time_interpolate(x, interpolated_year = rem_years, extrapolation_type = "constant")

    # add data for rcp 2.0, use data from rcp 2.6
    x_20 <- x[, , "rcp26"]
    getNames(x_20) <- gsub("rcp26", "rcp20", getNames(x_20))
    x <- mbind(x, x_20)

    # If we're not doing anything else, the output is just x
    outx <- x
  } else if (subtype == "IMAGE2025") {
    # Read the IMAGE2025 data
    mdata <- readSource("IMAGE2025")

    # For IMAGE2025, the C6F14 emissions are not available, so we remove it from the mapping
    emi_mapping <- emi_mapping[emi_mapping != "emiFgasC6F14"]

    # Keep only relevant variables
    mdata <- mdata[, , names(emi_mapping)]

    # Change variable names to the REMIND-set-names
    getNames(mdata, dim = "variable") <- emi_mapping[getNames(mdata, dim = "variable")]

    # Remove the unit dimension, refer to the original readIMAGE2025 object
    mdata <- collapseNames(mdata, collapsedim = "unit")

    # Collapse the data to a single scenario, and add dimensions for SSP, RCP and SPA
    formatScen <- function(scendata, outssp, outrcp, outspa = "SPA0") {
      # Collapse the data to a single scenario
      scen <- collapseDim(scendata)

      # Add dimensions for SSP, RCP and SPA
      scen <- add_dimension(scen, dim = 3.1, add = "Scenario", nm = outssp)
      scen <- add_dimension(scen, dim = 3.1, add = "rcp", nm = outrcp)
      scen <- add_dimension(scen, dim = 3.1, add = "SPAscen", nm = outspa)

      return(scen)
    }

    # Mix long-term and short-term scenarios, assuming that the short-term scenario
    # only has valid data until 2050 and always keeping the lower emissions between the two
    mixScenarios <- function(mdata, lngscen = "SSP1_SPA1_19I_D", shtscen = "CP_GI") {
      sht <- collapseDim(mdata[, , shtscen])
      lng <- collapseDim(mdata[, , lngscen])

      sht <- time_interpolate(
        sht[, (getYears(sht, as.integer = TRUE) <= 2050), ],
        interpolated_year = getYears(lng),
        extrapolation_type = "constant"
      )
      msk <- (lng <= sht)
      res <- sht
      res[msk] <- lng[msk]
      return(res)
    }

    # Combine scenarios for SSP2 and SSP1. The logic here is that all RCPs except RCP60
    # abide by the Kigali Amendment, by using the short-term development of the CP_GI
    # scenario until each original RCP leads to less emissions than the CP_GI scenario.
    out <- mbind(
      formatScen(mixScenarios(mdata, "SSP1_SPA1_19I_D"), "forcing_SSP1", "rcp20", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp26", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp37", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP1_SPA1_26I_D"), "forcing_SSP1", "rcp45", "SPA0"),
      formatScen(mdata[, , "SSP1_SPA1_34I_D"], "forcing_SSP1", "rcp60", "SPA0"),
      formatScen(mdata[, , "SSP1-baseline"], "forcing_SSP1", "none", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_19I_D"), "forcing_SSP2", "rcp20", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp26", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp37", "SPA0"),
      formatScen(mixScenarios(mdata, "SSP2_SPA2_26I_D"), "forcing_SSP2", "rcp45", "SPA0"),
      formatScen(mdata[, , "SSP2_SPA2_45I_D"], "forcing_SSP2", "rcp60", "SPA0"),
      formatScen(mdata[, , "SSP2-baseline"], "forcing_SSP2", "none", "SPA0")
    )

    # Fill SSP3-5 scenarios with SSP2 data, as they are not available in IMAGE2025
    out <- mbind(
      out,
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP3"),
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP4"),
      setItems(out[, , "forcing_SSP2"], "Scenario", "forcing_SSP5")
    )

    # # Also fill the RCP "none" with RCP60
    # out <- mbind(
    #   out,
    #   setItems(out[, , "rcp60"], "rcp", "none")
    # )


    # Reorder and adjust dimensions
    out <- dimOrder(out, perm = c(3, 2, 1, 4))
    getSets(out)[getSets(out) == "variable"] <- "Variable"

    # Extrapolate and subset periods to match REMIND years
    rem_years <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
    outx <- time_interpolate(out, interpolated_year = rem_years, extrapolation_type = "constant")

    # If we are not doing anything else, the output is just outx. If we are doing the interpolation,
    # keep the x naming
    x <- outx
  } else {
    stop("Invalid subtype. Please use 'IMAGE2025' or 'IMAGElegacy'")
  }


  if (interp == "interpolate2025") {
    # Read EDGAR7 F-gas emissions, already matching the standard used here
    edgar_all <- readSource("EDGAR7Fgases")
    getSets(edgar_all)[3] <- "Variable"

    # Years to interpolate the scenarios to
    scenyears <- getYears(x, as.integer = T)

    # Extrapolate 2025 with the 2010-2020 average growth
    edgar2025 <- setYears(edgar_all[, 2020, ] + 0.5 * (edgar_all[, 2020, ] - edgar_all[, 2010, ]), 2025)

    # Basis for interpolation
    xpts <- x[, c(scenyears[scenyears <= 2025 | scenyears >= interpyear]), ]

    # Fill the years for which we do have EDGAR data with it, and 2025
    commonyears <- intersect(getYears(edgar_all), getYears(xpts))
    commonvars <- intersect(getItems(edgar_all, "Variable"), getItems(xpts, "Variable"))
    xpts[, commonyears, commonvars] <- edgar_all[, commonyears, commonvars]
    xpts[, 2025, commonvars] <- edgar2025[, 2025, commonvars]

    # Interpolate the years between 2025 and interpyear, which
    # are the only ones not in xpts
    outx <- toolFillYears(xpts, scenyears)
  }

  return(list(
    x = outx,
    weight = NULL,
    unit = "Mt CO2-equiv/yr, kt HFC134a-equiv/yr, kt HFC125/yr, kt HFC134a/yr, kt HFC143a/yr, kt HFC227ea/yr, kt HFC23/yr, kt HFC245fa/yr, kt HFC32/yr, kt HFC43-10/yr, kt CF4-equiv/yr, kt SF6/yr, kt C2F6/yr, kt C6F14/yr, kt CF4/yr",
    description = "F-gases from IMAGE"
  ))
}
