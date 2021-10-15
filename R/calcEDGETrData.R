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

  infoConfig = getConfig()
  regionmapping2use <- infoConfig$regionmapping
  setConfig(regionmapping = "2b1450bc.csv")

  ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
  edgetScenarios <- strsplit(cartesian(
    c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
    c("ElecEra", "HydrHype", "Mix", "ConvCase")), split=".", fixed=TRUE)

  ## add both smartlifestyle and default lifestyle variants for all scenarios
  allscens <- append(
    lapply(edgetScenarios, function(sc){c(sc, TRUE)}),
    lapply(edgetScenarios, function(sc){c(sc, FALSE)}))
  ## SHAPE scenarios are coupled to specific technologies
  allscens <- append(
    allscens,
    list(
      c("SDP_EI", "ElecEra", FALSE),
      c("SDP_MC", "ElecEra", TRUE),
      c("SDP_RC", "ElecEra", TRUE)))

  ## run EDGE-T
  EDGETdata = lapply(allscens,
    function(x) {
      generateEDGEdata(input_folder = paste0(getConfig("mainfolder"), "/sources/EDGE-T_standalone/"),
                       output_folder = NULL,
                       SSP_scen = x[1],
                       tech_scen = x[2],
                       smartlifestyle = x[3],
                       storeRDS = FALSE)
    })


  setConfig(regionmapping = regionmapping2use)


  return(list(x = EDGETdata,
              class = 'list',
              unit = NA,
              description = 'EDGEtransport iterative inputs'))


}
