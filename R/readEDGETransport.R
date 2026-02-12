#' Read REMIND/EDGE-T iterative input data
#'
#' Run EDGE-Transport Standalone in all used scenario combinations to supply input data
#' to REMIND and the iterative EDGE-T script
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Johanna Hoppe
#' @param subtype REMIND/iterative EDGE-T input data subtypes
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "EDGETransport")
#' }
#' @importFrom dplyr bind_rows
#' @importFrom data.table :=

readEDGETransport <- function(subtype) {

  EDGE_scenario <- DEM_scenario <- NULL
  #############################################################
  ## Define all scenario combinations for which
  ## input data should be generated
  #############################################################

  allScens <- bind_rows(
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
      "SSP2",          "Mix4",                    TRUE,       "SSP2_lowAvShip"
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
  # Bind rows of equally named subtypes
  EdgeTransportSAdata <- lapply(types, function(type, outerList) {
    listOfDataTables <- lapply(outerList, function(innerList) innerList[[type]])
    data.table::rbindlist(listOfDataTables)
  }, EdgeTransportSAdata)

  EdgeTransportSAdata <- setNames(EdgeTransportSAdata, types)

  ########################################################################
  ## Rename EDGE-Transport scenarios according to REMIND scenario levers
  ## from 'demScen', 'transportPolScen' in transport to
  ## 'cm_demScen', 'cm_EDGEtr_scen' in REMIND
  ##
  ## applied to all sectors simultaneously
  ########################################################################
  
  # load scenarioMapping from edgeTransport package data
  scenarioMapping <- fread(system.file("extdata/helpersMappingEdgeTtoREMINDscen.csv",
                                       package = "edgeTransport", mustWork = TRUE), header = TRUE)
  
  translateEdgeTransportScentoREMIND <- function(transportData, scenarioMap) {
    # changes the rows with matches of DEM_scenario, EDGE_scenario in the mapping
    # ! transportData changes outside the function !
    # no match: rows are left unchanged
    transportData[scenarioMap,
                  on = .(DEM_scenario = DEM_edge, EDGE_scenario = EDGE_scenario), # match values
                  `:=`(DEM_scenario = i.DEM_remind, # overwrite values if match
                       EDGE_scenario = i.EDGEtr_scen_remind)]
  }
  
  # in-place modification of DEM_scenario, EDGE_scenario in EdgeTransportSAdata
  lapply(EdgeTransportSAdata, translateEdgeTransportScentoREMIND, scenarioMapping)
  
  #############################################################
  ## Create magpie object for every subtype
  #############################################################

  validSubtypes <- names(EdgeTransportSAdata)
  if (!subtype %in% validSubtypes) stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))

  as.magpie(EdgeTransportSAdata[[subtype]])
}
