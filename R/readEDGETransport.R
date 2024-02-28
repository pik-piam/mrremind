#' Read EDGETransport inputs
#'
#' Read-in EDGETransport inputs csv file as magclass object
#'
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @author Marianna Rottoli, Alois Dirnaichner
#' @seealso \code{\link{readSource}}
#' @param subtype
#'
#' @examples
#' \dontrun{ a <- readSource(type = "EDGETransport")
#' }
#' @importFrom magclass read.magpie
#' @importFrom tydir expand_grid
#' @importFrom dplyr bind_rows
#' @importFrom data.table data.table rbindlist fread setcolorder := setnames setkeyv

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
      demScen = c("default", "SSP2EU_demRedWeak", "SSP2EU_demRedStrong")),

    ## Specific project scenarios
    tribble(
      ~SSPscen,   ~transportPolScen,                   ~demScen,
      'SDP_EI',    'Mix4',                       'default',
      'SDP_MC',    'Mix4',                       'default',
      'SDP_RC',    'Mix3',                       'default',
      'SSP2EU',    'HydrHype4',                  'default',
      'SSP2EU',    'ECEMF_HighEl_HighEff',       'default',
      'SSP2EU',    'ECEMF_HighEl_LifestCha',     'SSP2EU_demRedStrong',
      'SSP2EU',    'ECEMF_HighEl_ModEff',        'default',
      'SSP2EU',    'ECEMF_HighH2_HighEff',       'default',
      'SSP2EU',    'ECEMF_HighH2_LifestCha',     'SSP2EU_demRedStrong',
      'SSP2EU',    'ECEMF_HighH2_ModEff',        'default',
      'SSP2EU',    'NAV_act',                    'SSP2EU_demRedStrong',
      'SSP2EU',    'NAV_tec',                    'default',
      'SSP2EU',    'NAV_ele',                    'default',
      'SSP2EU',    'NAV_all',                    'SSP2EU_demRedStrong',
      'SSP2EU',    'NAV_lce',                    'SSP2EU_demRedStrong',
      'SSP2EU',    'PhOP',                       'default',
      'SSP2EU',    'CAMP_lscWeak',               'SSP2EU_demRedWeak',
      'SSP2EU',    'CAMP_lscStrong',             'SSP2EU_demRedStrong'
    )
  )

  # generate list from data frame rows
  allScens <- lapply(1:nrow(allScens), function(x) {
    setNames(unlist(allScens[x, ]), NULL)
  })

  #############################################################
  ## Run EDGE-Transport SA with all scenario combinations
  #############################################################

  EdgeTransportSAdata <- lapply(allScens,
                     function(x) {
                       toolEdgeTransport <- function(SSPscen = x[1],
                                                     transportPolScen = x[2],
                                                     demScen = x[3],
                                                     generateTransportData = FALSE,
                                                     generateREMINDinputData = TRUE)
                     })


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
