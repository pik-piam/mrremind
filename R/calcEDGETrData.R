#' @title Runs EDGE-T model
#'
#'
#' @return list of EDGEtransport iterative inputs
#' @author Marianna Rottoli
#' @seealso \code{\link{readEDGETransport}}, \code{\link[madrat]{calcOutput}}
#'
#' @examples
#' \dontrun{ a <- calcOutput(type="EDGETrData", aggregate = F)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tribble
#' @importFrom tidyr expand_grid

calcEDGETrData <- function() {

  if (!'edgeTransport' %in% unique(getCalculations('calc')$package)) {
    stop(paste('calcEDGETrData() requires package edgeTransport to be loaded',
               '(manually)'))
  }

  allscens <- bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
    expand_grid(
      SSP_scen = c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
      tech_scen = c("Mix1", "Mix2", "Mix3", "Mix4"),
      DEM_scenario = c("default", "SSP2EU_lowdem" )),

    ## SHAPE scenarios are coupled to specific technologies
    tribble(
      ~SSP_scen,   ~tech_scen,                   ~DEM_scenario,
      'SDP_EI',    'Mix4',                       'default',
      'SDP_MC',    'Mix4',                       'default',
      'SDP_RC',    'Mix3',                       'default',
      'SSP2EU',    'HydrHype4',                  'default',
      'SSP2EU',    'ECEMF_HighEl_HighEff',       'default',
      'SSP2EU',    'ECEMF_HighEl_LifestCha',     'SSP2EU_lowdem',
      'SSP2EU',    'ECEMF_HighEl_ModEff',        'default',
      'SSP2EU',    'ECEMF_HighH2_HighEff',       'default',
      'SSP2EU',    'ECEMF_HighH2_LifestCha',     'SSP2EU_lowdem',
      'SSP2EU',    'ECEMF_HighH2_ModEff',        'default',
      'SSP2EU',    'NAV_act',                    'SSP2EU_lowdem',
      'SSP2EU',    'NAV_tec',                    'default',
      'SSP2EU',    'NAV_ele',                    'default',
      'SSP2EU',    'NAV_all',                    'SSP2EU_lowdem'
      )
  )

  # generate list from data frame rows
  allscens <- lapply(1:nrow(allscens), function(x) {
    setNames(unlist(allscens[x, ]), NULL)
  })

  ## run EDGE-T
  EDGETdata = lapply(allscens,
    function(x) {
      calcOutput(type = 'generateEDGEdata',
                 aggregate = FALSE,
                 supplementary = FALSE,
                 input_folder = paste0(getConfig("sourcefolder"),
                                       "/EDGE-T_standalone/"),
                 output_folder = NULL,
                 SSP_scen = x[1],
                 tech_scen = x[2],
                 demScen = x[3],
                 storeRDS = FALSE)
    })

  return(list(x = EDGETdata,
              class = 'list',
              unit = NA,
              description = 'EDGEtransport iterative inputs'))


}
