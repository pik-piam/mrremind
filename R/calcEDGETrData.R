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

calcEDGETrData <- function() {

  ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
  edgetScenarios <- strsplit(cartesian(
    c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
    c("ConvCase", "ElecEra", "HydrHype", "Mix",
      "Mix1", "Mix2", "Mix3", "Mix4")), split=".", fixed=TRUE)
  ## SHAPE scenarios are coupled to specific technologies
  allscens <- append(
    allscens,
    list(
      c("SDP_EI", "ElecEra", FALSE),
      c("SDP_MC", "ElecEra", FALSE),
      c("SDP_RC", "ElecEra", FALSE)))

  ## add default lifestyle variants for all scenarios (exclude lifestyle)
  allscens <- lapply(allscens, function(sc){c(sc, FALSE)})

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
