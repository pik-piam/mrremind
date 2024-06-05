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
      SSPscen = c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
      transportPolScen = c("Mix1", "Mix2", "Mix3", "Mix4"),
      isICEban = c(TRUE, FALSE),
      demScen = c("default")),
    # Specific project scenarios
    tribble(
     ~SSPscen,         ~transportPolScen,        ~isICEban,    ~demScen,
    'SDP_EI',        'Mix4',                    TRUE,       'default',
    'SDP_MC',        'Mix4',                    TRUE,       'default',
    'SDP_RC',        'Mix3',                    TRUE,       'default',
    'SSP2EU',        'HydrHype4',               TRUE,       'default',
    'SSP2EU',        'ECEMF_HighEl_HighEff',    TRUE,       'default',
    'SSP2EU',        'ECEMF_HighEl_LifestCha',  TRUE,       'SSP2EU_demRedStrong',
    'SSP2EU',        'ECEMF_HighEl_ModEff',     TRUE,       'default',
    'SSP2EU',        'ECEMF_HighH2_HighEff',    TRUE,       'default',
    'SSP2EU',        'ECEMF_HighH2_LifestCha',  TRUE,       'SSP2EU_demRedStrong',
    'SSP2EU',        'ECEMF_HighH2_ModEff',     TRUE,       'default',
    'SSP2EU',        'NAV_act',                 FALSE,      'SSP2EU_demRedStrong',
    'SSP2EU',        'NAV_tec',                 FALSE,      'default',
    'SSP2EU',        'NAV_ele',                 TRUE,       'default',
    'SSP2EU',        'NAV_all',                 TRUE,       'SSP2EU_demRedStrong',
    'SSP2EU',        'NAV_lce',                 FALSE,      'SSP2EU_demRedStrong',
    'SSP2EU',        'CAMP_lscLow',             TRUE,       'SSP2EU_demRedLow',
    'SSP2EU',        'CAMP_lscWeak',            TRUE,       'SSP2EU_demRedWeak',
    'SSP2EU',        'CAMP_lscStrong',          TRUE,       'SSP2EU_demRedStrong'
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

  createCopyDemScens <- function(dt) {
    setkeyv(dt, c("DEM_scenario", "GDP_scenario", "EDGE_scenario"))
    ## Workaround for NAVIGATE: copy-create demand scenarios which we do not supply by EDGE-T
    dt <- rbind(dt,
                dt[.("gdp_SSP2EU", "gdp_SSP2EU", "NAV_ele"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_ele"],
                dt[.("gdp_SSP2EU", "gdp_SSP2EU", "NAV_tec"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_tec"],
                dt[.("gdp_SSP2EU_demRedStrong", "gdp_SSP2EU", "NAV_act"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_act"],
                dt[.("gdp_SSP2EU_demRedStrong", "gdp_SSP2EU", "NAV_all"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_all"],
                dt[.("gdp_SSP2EU_demRedStrong", "gdp_SSP2EU", "NAV_lce"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_NAV_lce"],
                dt[.("gdp_SSP2EU_demRedWeak", "gdp_SSP2EU", "CAMP_lscWeak"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_CAMP_weak"],
                dt[.("gdp_SSP2EU_demRedStrong", "gdp_SSP2EU", "CAMP_lscStrong"), nomatch = NULL][
                  , DEM_scenario := "gdp_SSP2EU_CAMP_strong"]
    )
    setkeyv(dt, "DEM_scenario")
    dt[.("gdp_SSP2EU_demRedStrong"), DEM_scenario := "gdp_SSP2_lowEn"]
    return(dt)
  }

  EdgeTransportSAdata <- lapply(EdgeTransportSAdata, createCopyDemScens)

  #############################################################
  ## Create magpie object for every subtype
  #############################################################

  validSubtypes <- names(EdgeTransportSAdata)
  if (!subtype %in% validSubtypes) stop(sprintf("Subtype %s is not valid for EDGETransport.", subtype))

  magpieobj <- as.magpie(EdgeTransportSAdata[[subtype]])

  return(magpieobj)
}
