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

  ## monitor edgeTransport functions
  "#! @monitor edgeTransport:::compScenEDGET"
  "#! @monitor edgeTransport:::generateEDGEdata"
  "#! @monitor edgeTransport:::calcgenerateEDGEdata"
  "#! @monitor edgeTransport:::collectScens"
  "#! @monitor edgeTransport:::Update_Validation_Excel_tool"
  "#! @monitor edgeTransport:::Update_sw_trend"
  "#! @monitor edgeTransport:::lvl2_generate_plotdata"

  ## monitor edgeTrpLib functions
  "#! @monitor edgeTrpLib:::calculate_capCosts"
  "#! @monitor edgeTrpLib:::calcVint"
  "#! @monitor edgeTrpLib:::compareScenarios_EDGET"
  "#! @monitor edgeTrpLib:::createRDS"
  "#! @monitor edgeTrpLib:::readREMINDdemand"
  "#! @monitor edgeTrpLib:::applylearning"
  "#! @monitor edgeTrpLib:::calc_num_vehicles_stations"
  "#! @monitor edgeTrpLib:::merge_prices"
  "#! @monitor edgeTrpLib:::prepare4REMIND"
  "#! @monitor edgeTrpLib:::reportEDGETransport"
  "#! @monitor edgeTrpLib:::loadInputData"
  "#! @monitor edgeTrpLib:::shares_intensity_and_demand"
  "#! @monitor edgeTrpLib:::calculate_logit_inconv_endog"

  allscens <- bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
    expand_grid(
      SSP_scen = c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
      tech_scen = c("Mix1", "Mix2", "Mix3", "Mix4"),
      smartlifestyle = 'FALSE'),

    ## SHAPE scenarios are coupled to specific technologies
    tribble(
      ~SSP_scen,   ~tech_scen,   ~smartlifestyle,
      'SDP_EI',    'Mix4',    'FALSE',
      'SDP_MC',    'Mix4',    'FALSE',
      'SDP_RC',    'Mix3',    'FALSE')
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
                 smartlifestyle = x[3],
                 storeRDS = FALSE)
    })

  return(list(x = EDGETdata,
              class = 'list',
              unit = NA,
              description = 'EDGEtransport iterative inputs'))


}
