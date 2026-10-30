#' Convert NDC emission targets from the PBL ELEVATE scenario protocol
#'
#' @description Translates the NDC target formulations into absolute emission
#' levels in the metric "Emi|GHG|w/o Bunkers|w/o Land-Use Change" (Mt CO2eq/yr).
#'
#' For subtype `detailed`, the target formulations are extracted from the magclass
#' input and passed to [toolCalcNDCTarget()] which handles data loading, target
#' calculation, and EU disaggregation to member states. For subtype `reduced`, the
#' pre-computed emission levels are only completed to ISO country level.
#'
#' @param x a magclass object as returned by [readPBL_NDC()]
#' @param subtype one of "detailed" or "reduced"
#' @author Felix Schreyer, Rahel Mandaroux
#' @seealso [readPBL_NDC()], [toolCalcNDCTarget()]
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_wider
convertPBL_NDC <- function(x, subtype) { # nolint: object_name_linter.
  if (subtype == "reduced") {
    return(toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = "EU"))
  }

  if (subtype == "detailed") {
    targets <- quitte::as.quitte(x, na.rm = FALSE) %>%
      select("iso3c" = "region", "year" = "period", "type", "scope", "variable", "value") %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      filter(!is.na(.data$target_value)) %>%
      mutate("year" = as.integer(as.character(.data$year)))

    out <- toolCalcNDCTarget(targets) %>%
      as.magpie(spatial = "iso3c", temporal = "year")

    return(toolCountryFill(out, fill = NA, verbosity = 2))
  }

  stop("Invalid subtype for convertPBL_NDC, please use 'detailed' or 'reduced'.")
}
