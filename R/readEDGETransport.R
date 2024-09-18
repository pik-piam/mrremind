#' Read REMIND/EDGE-T iterative input data
#'
#' Run EDGE-Transport Standalone in all used scenario combinations to supply input data
#' to REMIND and the iterative EDGE-T script
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Johanna Hoppe
#' @seealso \code{\link{readSource}}
#' @param subtype REMIND/iterative EDGE-T input data subtypes
#'
#' @examples
#' \dontrun{ a <- readSource(type = "EDGETransport")
#' }
#' @importFrom tidyr expand_grid
#' @importFrom dplyr bind_rows
#' @importFrom data.table data.table rbindlist := setnames setkeyv

readEDGETransport <- function(subtype) {

  #############################################################
  ## Define all scenario combinations for which
  ## input data should be generated
  #############################################################

  allScens <- bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of standard
    ## EDGE-T scenarios
    expand_grid(
      SSPscen = c("SSP2", "SSP3", "SSP5", "SDP"),
      transportPolScen = c("Mix1", "Mix2", "Mix3", "Mix4"),
      isICEban = c(TRUE, FALSE),
      demScen = c("default")),
    # Specific project scenarios
    tribble(
      ~SSPscen,         ~transportPolScen,        ~isICEban,    ~demScen,
      'SSP1',          'Mix1',                    FALSE,      'default',
      'SSP1',          'Mix2',                    FALSE,      'default',
      'SSP1',          'Mix3',                    TRUE,       'default',
      'SSP1',          'Mix4',                    TRUE,       'default',
      'SSP2',          'Mix1',                    FALSE,      'SSP2_demRedStrong',
      'SSP2',          'Mix2',                    FALSE,      'SSP2_demRedStrong',
      'SSP2',          'Mix3',                    TRUE,       'SSP2_demRedStrong',
      'SSP2',          'Mix4',                    TRUE,       'SSP2_demRedStrong',
      'SSP2',          'Mix4',                    TRUE,       'SSP2_demDiffer',
      'SDP_EI',        'Mix4',                    TRUE,       'default',
      'SDP_MC',        'Mix4',                    TRUE,       'default',
      'SDP_RC',        'Mix3',                    TRUE,       'default',
      'SSP2',          'HydrHype4',               TRUE,       'default',
      'SSP2',          'ECEMF_HighEl_HighEff',    TRUE,       'default',
      'SSP2',          'ECEMF_HighEl_LifestCha',  TRUE,       'SSP2_demRedStrong',
      'SSP2',          'ECEMF_HighEl_ModEff',     TRUE,       'default',
      'SSP2',          'ECEMF_HighH2_HighEff',    TRUE,       'default',
      'SSP2',          'ECEMF_HighH2_LifestCha',  TRUE,       'SSP2_demRedStrong',
      'SSP2',          'ECEMF_HighH2_ModEff',     TRUE,       'default',
      'SSP2',          'NAV_act',                 FALSE,      'SSP2_demRedStrong',
      'SSP2',          'NAV_tec',                 FALSE,      'default',
      'SSP2',          'NAV_ele',                 TRUE,       'default',
      'SSP2',          'NAV_all',                 TRUE,       'SSP2_demRedStrong',
      'SSP2',          'NAV_lce',                 FALSE,      'SSP2_demRedStrong',
      'SSP2',          'CAMP_lscWeak',            TRUE,       'SSP2_demRedWeak',
      'SSP2',          'CAMP_lscStrong',          TRUE,       'SSP2_demRedStrong'
    )
  )

  # generate list from data frame rows
  allScens <- split(allScens, seq(nrow(allScens)))

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
    result <- rbindlist(listOfDataTables)
  }, EdgeTransportSAdata)

  EdgeTransportSAdata <- setNames(EdgeTransportSAdata, types)

  #############################################################
  ## Rename EDGE-Transport demScens and map to REMIND demScens
  ## that are applied to all sectors simultaneously
  #############################################################
  translateEdgeTransportDemScentoREMIND <- function(dt) {
    dt[DEM_scenario == "gdp_SSP2_demDiffer" & EDGE_scenario == "Mix4ICEban", DEM_scenario := "gdp_SSP2_demDiffer_IKEA"]
    dt[DEM_scenario == "gdp_SSP2EU" & EDGE_scenario == "NAV_ele", DEM_scenario := "gdp_SSP2EU_NAV_ele"]
    dt[DEM_scenario == "gdp_SSP2EU" & EDGE_scenario == "NAV_tec", DEM_scenario := "gdp_SSP2EU_NAV_tec"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "NAV_act", DEM_scenario := "gdp_SSP2EU_NAV_act"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "NAV_all", DEM_scenario := "gdp_SSP2EU_NAV_all"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "NAV_lce", DEM_scenario := "gdp_SSP2EU_NAV_lce"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedWeak" & EDGE_scenario == "CAMP_lscWeak", DEM_scenario := "gdp_SSP2EU_CAMP_weak"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "CAMP_lscStrong", DEM_scenario := "gdp_SSP2EU_CAMP_strong"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "Mix1", DEM_scenario := "gdp_SSP2_lowEn"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "Mix2", DEM_scenario := "gdp_SSP2_lowEn"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "Mix3ICEban", DEM_scenario := "gdp_SSP2_lowEn"]
    dt[DEM_scenario == "gdp_SSP2EU_demRedStrong" & EDGE_scenario == "Mix4ICEban", DEM_scenario := "gdp_SSP2_lowEn"]
    return(dt)
  }
  EdgeTransportSAdata <- lapply(EdgeTransportSAdata, translateEdgeTransportDemScentoREMIND)
  #############################################################
  ## Create magpie object for every subtype
  #############################################################

  validSubtypes <- names(EdgeTransportSAdata)
  if (!subtype %in% validSubtypes) stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))

  magpieobj <- as.magpie(EdgeTransportSAdata[[subtype]])

  return(magpieobj)
}
