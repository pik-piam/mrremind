#' export validation thresholds
#'
#' assemble near-term thresholds from project pipelines and potentially other
#' data sources and export them to a file
#'
#' @author Pascal Weigmann
#' @param type choose either "config" to export thresholds as used in the
#'        validationConfig or "full" to export all pipeline data
#'
#' @export
fullTHRESHOLDS <- function(type = "config") {

  # get region mappings for aggregation ----
  # Determines all regions data should be aggregated to by examining the columns
  # of the `regionmapping` and `extramappings` currently configured.
  rel <- "global" # always compute global aggregate
  for (mapping in c(getConfig("regionmapping"), getConfig("extramappings"))) {
    columns <- setdiff(
      colnames(toolGetMapping(mapping, "regional", where = "mappingfolder")),
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

  # time horizon, currently not flexible
  years <- c(2020, 2025, 2030)

  # assemble data ----
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
  out <- mbind(ccs, hydro, nuclear, wind, solar)
  # remove ROW region, in case it exists (from using extra region mappings)
  out <- out["ROW", , invert = TRUE]

  # additional calculations ----
  # threshold calculations after regional aggregation

  # harmonize status names first
  idx <- which(getNames(out, dim  = 3) == "operating")
  getNames(out, dim = 3)[idx] <- "operational"

  # ASSUMPTION
  # regional bounds should be less strict than global ones: use logic of
  # "red" bounds as "yel" regional bounds and remove "red" regional bounds
  # (exception for 2020, see below)
  regions <- getItems(out["GLO", , , invert = TRUE], dim = 1)
  out[regions, , "min_yel"] <- out[regions, , "min_red"]
  out[regions, , "max_yel"] <- out[regions, , "max_red"]
  out[regions, , "min_red"] <- NA
  out[regions, , "max_red"] <- NA


  ## Historic Data: 2020 ####

  # ASSUMPTION[2020] ALL Variables:
  # operational +- 5/10% (world) and +- 20/40% (regions)
  out["GLO", 2020, "min_red"] <- out["GLO", 2020, "operational"]*0.9
  out["GLO", 2020, "min_yel"] <- out["GLO", 2020, "operational"]*0.95
  out["GLO", 2020, "max_yel"] <- out["GLO", 2020, "operational"]*1.05
  out["GLO", 2020, "max_red"] <- out["GLO", 2020, "operational"]*1.1

  # EXCEPTION for 2020: use also red bounds for regions
  out[regions, 2020, "min_red"] <- out[regions, 2020, "operational"]*0.6
  out[regions, 2020, "min_yel"] <- out[regions, 2020, "operational"]*0.8
  out[regions, 2020, "max_yel"] <- out[regions, 2020, "operational"]*1.2
  out[regions, 2020, "max_red"] <- out[regions, 2020, "operational"]*1.4

  # EXCEPTION for Nuclear capacities
  # use min/max(Ember, ProjectPipeline) for min/max 2020 thresholds
  emb <- calcOutput("Ember", aggregate = columnsForAggregation, round = 3)

  # use average over years 2018 to 2022 as 2020 Ember value
  emb[, 2020, "Cap|Electricity|Nuclear", pmatch = T] <-
    dimSums(emb[, 2018:2022, "Cap|Electricity|Nuclear", pmatch = T], dim = 2)/5

  # global thresholds (scale with 10/20% instead of 5/10%)
  out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.min_red"] <-
    pmin(out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb["GLO", 2020, "Cap|Electricity|Nuclear", pmatch = T])*0.8
  out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.min_yel"] <-
    pmin(out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb["GLO", 2020, "Cap|Electricity|Nuclear", pmatch = T])*0.9
  out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.max_yel"] <-
    pmax(out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb["GLO", 2020, "Cap|Electricity|Nuclear", pmatch = T])*1.1
  out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.max_red"] <-
    pmax(out["GLO", 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb["GLO", 2020, "Cap|Electricity|Nuclear", pmatch = T])*1.2

  # regional thresholds (same scaling as other sources: 20/40%)
  out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.min_red"] <-
    pmin(out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb[regions, 2020, "Cap|Electricity|Nuclear", pmatch = T])*0.6
  out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.min_yel"] <-
    pmin(out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb[regions, 2020, "Cap|Electricity|Nuclear", pmatch = T])*0.8
  out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.max_yel"] <-
    pmax(out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb[regions, 2020, "Cap|Electricity|Nuclear", pmatch = T])*1.2
  out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.max_red"] <-
    pmax(out[regions, 2020, "IAEA_PRIS.Cap|Electricity|Nuclear.operational"],
         emb[regions, 2020, "Cap|Electricity|Nuclear", pmatch = T])*1.4


  ## Near-Term: 2025, 2030 ####

  # EXCEPTION
  # IAEA has a global estimate for high cap scenario, use this for max_red
  # https://www-pub.iaea.org/MTCD/Publications/PDF/RDS-1-44_web.pdf tab.3, p.19
  out["GLO", 2030, "Cap|Electricity|Nuclear.max_red"] <- 461

  # Additional tolerances for ALL Near-Term Thresholds
  #
  # ASSUMPTION[2025, 2030] red: additional buffer +-10% (world)
  # ASSUMPTION[2025, 2030] yel: additional buffer +- 5% (world)
  #                             additional buffer +-40% (regions)
  out["GLO", c(2025, 2030), "min_red"] <- out["GLO", c(2025, 2030), "min_red"]*0.9
  out["GLO", c(2025, 2030), "min_yel"] <- out["GLO", c(2025, 2030), "min_yel"]*0.95
  out["GLO", c(2025, 2030), "max_yel"] <- out["GLO", c(2025, 2030), "max_yel"]*1.05
  out["GLO", c(2025, 2030), "max_red"] <- out["GLO", c(2025, 2030), "max_red"]*1.1

  out[regions, c(2025, 2030), "min_yel"] <- out[regions, c(2025, 2030), "min_yel"]*0.6
  out[regions, c(2025, 2030), "max_yel"] <- out[regions, c(2025, 2030), "max_yel"]*1.4


  # rename "GLO" to "World"
  getItems(out, dim = 1) <- gsub("GLO", "World", getItems(out, dim = 1))

  # remove GEM as it is not up to date and shouldn't be used in this state
  out <- out[, , "GlobalEnergyMonitor", invert = TRUE]

  # export to file ----
  if (type == "full") {
    # write report containing all available data, including all statuses and
    # thresholds attached to "variable"
    outfile <- "pipelines.mif"
    quitte::as.quitte(out) %>%
      mutate("variable" = paste(.data$variable, .data$status, sep = "|")) %>%
      select(-"scenario", -"status") %>%
      as.magpie() %>%
      write.report(file = outfile)

  } else if (type == "config") {
    # write report containing only the "min/max" thresholds in extra columns
    # (as used in a validationConfig)
    outfile <- "thresholds.mif"
    out <- out[, , c("min_", "max_"), pmatch = TRUE] %>%
      quitte::as.quitte() %>%
      tidyr::pivot_wider(names_from = "status") %>%
      select(-"scenario")
    # exclude rows without any threshold
    out[!(is.na(out$min_red)
          & is.na(out$min_yel)
          & is.na(out$max_yel)
          & is.na(out$max_red)), ] %>%
      utils::write.csv(file = outfile, row.names = FALSE, quote = FALSE)

  } else {
    warning("`type` must be either `full` or `config`")
  }

}
