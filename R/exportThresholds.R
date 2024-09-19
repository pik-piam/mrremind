#' export validation thresholds
#'
#' assemble near-term thresholds from project pipelines and potentially other
#' data sources and export them to a file
#'
#' @author Pascal Weigmann
#' @param type choose either "config" to export thresholds as used in the
#'        validationConfig or "full" to export all pipeline data
#' @param years choose years to include, currently only 2025 and 2030 are
#'        available for all sources
#' @export
exportThresholds <- function(type = "config", years = c(2025, 2030)) {
  # extra regions for ELEVATE
  # setConfig(regionmapping = "regionmappingR10_elevate.csv")
  # setConfig(forcecache = "GDP")
  # extraRegions <- c("CHN", "IND", "IDN", "VNM", "PAK")

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.
  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional")),
      c("X", "CountryCode")
    )

    if (any(columns %in% rel)) {
      warning(
        "The following column(s) from ", mapping,
        " exist in another mapping an will be ignored: ",
        paste(columns[columns %in% rel], collapse = ", ")
      )
    }
    rel <- unique(c(rel, columns))
  }

  columnsForAggregation <- gsub(
    "RegionCode", "region",
    paste(rel, collapse = "+")
  )

  # the following magclass objects are expected to have the dimensions "status"
  # as dim 3.3
  ccs <- calcOutput("ProjectPipelines", subtype = "CCS",
                    aggregate = columnsForAggregation, round = 3,
                    warnNA = FALSE, try = FALSE, years = years)

  hydro <- calcOutput("ProjectPipelines", subtype = "hydro",
                    aggregate = columnsForAggregation, round = 3,
                    warnNA = FALSE, try = FALSE, years = years)

  nuclear <- calcOutput("ProjectPipelines", subtype = "nuclear",
                        aggregate = columnsForAggregation, round = 3,
                        warnNA = FALSE, try = FALSE, years = years)

  wind <- calcOutput("ProjectPipelines", subtype = "wind",
                      aggregate = columnsForAggregation, round = 3,
                      warnNA = FALSE, try = FALSE, years = years)

  solar <- calcOutput("ProjectPipelines", subtype = "solar",
                      aggregate = columnsForAggregation, round = 3,
                      warnNA = FALSE, try = FALSE, years = years)

  # combine and export data to madrat output folder

  out <- mbind(ccs, hydro, wind, solar)
  # remove ROW region, in case it exists
  out <- out["ROW", , invert = TRUE]

  # export
  if (type == "full") {
    # write report containing all available data, including all statuses and
    # thresholds attached to "variable"
    outfile <- "pipelines.mif"
    as.quitte(out) %>%
      mutate(variable = paste(variable, status, sep = "|")) %>%
      select(-scenario, -status) %>%
      as.magpie() %>%
      write.report(file = paste0(getConfig("outputfolder"), "/", outfile))

  } else if (type == "config") {
    # write report containing only the "min/max" thresholds in extra columns
    # (as used in a validationConfig)
    outfile <- "thresholds.mif"
    out <- out[, , c("min_", "max_"), pmatch = TRUE] %>%
      as.quitte() %>%
      pivot_wider(names_from = "status") %>%
      select(-scenario)
    # exclude rows without any threshold
    out[!(is.na(out$min_red)
          & is.na(out$min_yel)
          & is.na(out$max_yel)
          & is.na(out$max_red)), ] %>%
      write.csv(file = paste0(getConfig("outputfolder"), "/", outfile),
                row.names = FALSE, quote = FALSE)

  } else {
    warning("`type` must be either `full` or `config`")
  }

}
