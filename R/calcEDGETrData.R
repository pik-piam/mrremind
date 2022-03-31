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

  "!# @monitor edgeTransport:::generateEDGEdata"

  allscens <- bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
    expand_grid(
      SSP_scen = c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
      tech_scen = c("ConvCase", "ElecEra", "HydrHype",
                    "Mix", "Mix1", "Mix2", "Mix3", "Mix4"),
      smartlifestyle = 'FALSE'),

    ## SHAPE scenarios are coupled to specific technologies
    tribble(
      ~SSP_scen,   ~tech_scen,   ~smartlifestyle,
      'SDP_EI',    'ElecEra',    'FALSE',
      'SDP_MC',    'ElecEra',    'FALSE',
      'SDP_RC',    'ElecEra',    'FALSE')
  )

  # generate list from data frame rows
  allscens <- lapply(1:nrow(allscens), function(x) {
    setNames(unlist(allscens[x, ]), NULL)
  })

  ## run EDGE-T
  EDGETdata = lapply(allscens,
    function(x) {
      generateEDGEdata(input_folder = paste0(getConfig("sourcefolder"), "/EDGE-T_standalone/"),
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
