#' Read REMIND/EDGE-T iterative input data
#'
#' Run EDGE-Transport Standalone in all used scenario combinations to supply input data
#' to REMIND and the iterative EDGE-T script
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Johanna Hoppe, Alex K. Hagen
#' @param subtype REMIND/iterative EDGE-T input data subtypes, must be one of
#' f35_esCapCost, f35_fe2es, f35_demByTech, f29_trpdemand, weightESdemand,
#' shares_LDV_transport
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "EDGETransport")
#' }
#'
readEDGETransport <- function(subtype) {

  #############################################################
  ## Define all scenario combinations for which
  ## input data should be generated
  #############################################################

  allScens <- dplyr::bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of standard
    ## EDGE-T scenarios
    tidyr::expand_grid(
      SSPscen = c("SSP1", "SSP2", "SSP3", "SSP5", "SDP", "SSP2IndiaMedium", "SSP2IndiaHigh"),
      transportPolScen = c("Mix1", "Mix2", "Mix3", "Mix4"),
      isICEban = c(TRUE, FALSE),
      demScen = c("default")),
    # Specific project scenarios
    tibble::tribble(
      ~SSPscen,         ~transportPolScen,        ~isICEban,    ~demScen,
      "SSP2",          "Mix1",                    FALSE,      "SSP2_demRedStrong",
      "SSP2",          "Mix2",                    FALSE,      "SSP2_demRedStrong",
      "SSP2",          "Mix2",                    TRUE,       "SSP2_demRedStrong",
      "SSP2",          "Mix3",                    TRUE,       "SSP2_demRedStrong",
      "SSP2",          "Mix4",                    TRUE,       "SSP2_demRedStrong",
      "SSP2",          "Mix4",                    TRUE,       "SSP2_demDiffer",
      "SSP2",          "Mix1",                    FALSE,      "SSP2_demDiffer",
      "SSP2",          "Mix2",                    FALSE,      "SSP2_highDemDEU",
      "SSP2",          "Mix3",                    FALSE,      "SSP2_highDemDEU",
      "SSP2",          "Mix3",                    TRUE,       "SSP2_highDemDEU",
      "SDP_EI",        "Mix4",                    TRUE,       "default",
      "SDP_MC",        "Mix4",                    TRUE,       "default",
      "SDP_RC",        "Mix3",                    TRUE,       "default",
      "SSP2",          "HydrHype4",               TRUE,       "default",
      "SSP2",          "NAV_act",                 FALSE,      "SSP2_demRedStrong",
      "SSP2",          "NAV_tec",                 FALSE,      "default",
      "SSP2",          "NAV_ele",                 TRUE,       "default",
      "SSP2",          "NAV_all",                 TRUE,       "SSP2_demRedStrong",
      "SSP2",          "NAV_lce",                 FALSE,      "SSP2_demRedStrong",
      "SSP2",          "CAMP_lscWeak",            TRUE,       "SSP2_demRedWeak",
      "SSP2",          "CAMP_lscStrong",          TRUE,       "SSP2_demRedStrong",
      "SSP2",          "Mix2",                    TRUE,       "SSP2_lowAvShip",
      "SSP2",          "Mix4",                    TRUE,       "SSP2_lowAvShip",
      "SSP2",          "Mix2",                    TRUE,       "SSP2_justMIP",
      "SSP2",          "Mix4",                    TRUE,       "SSP2_justMIP",
      "SSP1",          "Mix2",                    TRUE,       "SSP1_justMIP",
      "SSP1",          "Mix4",                    TRUE,       "SSP1_justMIP"
    )
  )

  # generate list from data frame rows
  allScens <- split(allScens, seq_len(nrow(allScens)))

  #############################################################
  ## Run EDGE-Transport SA with all scenario combinations
  #############################################################

  EdgeTransportSAdata <- lapply(allScens,
                                function(x) {
                                  calcOutput(type = "EdgeTransportSA",
                                             aggregate = FALSE,
                                             supplementary = FALSE,
                                             SSPscen = x[["SSPscen"]],
                                             transportPolScen = x[["transportPolScen"]],
                                             isICEban = x[["isICEban"]],
                                             demScen = x[["demScen"]],
                                             isTransportReported = FALSE,
                                             isREMINDinputReported = TRUE,
                                             isStored = FALSE)
                                })

  types <- unique(unlist(lapply(EdgeTransportSAdata, names)))

  stopifnot(subtype %in% types)

  data <- lapply(EdgeTransportSAdata, function(x) as.data.frame(x[[subtype]])) %>%
    dplyr::bind_rows()

  ########################################################################
  ## Rename EDGE-Transport scenarios according to REMIND scenario levers
  ## from 'demScen', 'transportPolScen' in transport to
  ## 'cm_demScen', 'cm_EDGEtr_scen' in REMIND
  ##
  ## applied to all sectors simultaneously
  ########################################################################

  m <- toolGetMapping("helpersMappingEdgeTtoREMINDscen.csv", where = "edgeTransport")

  x <- data %>%
    left_join(m, by = c("DEM_scenario" = "demScen_edge", "EDGE_scenario" = "transportPolScen_edge")) %>%
    mutate(
      "DEM_scenario" = ifelse(is.na(.data$demScen_remind), .data$DEM_scenario, .data$demScen_remind),
      "EDGE_scenario" = ifelse(is.na(.data$EDGEtr_scen_remind), .data$EDGE_scenario, .data$EDGEtr_scen_remind)
    ) %>%
    select(-c("demScen_remind", "EDGEtr_scen_remind")) %>%
    as.magpie()

  return(x)

}
