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
  ## run EDGE-T
  EDGETdata = lapply(strsplit(cartesian(x = c("ConvCase", "ElecEra", "HydrHype",
                                       "ConvCaseWise", "ElecEraWise",
                                       "HydrHypeWise"),
                                        y = c(paste0('SSP', c(1, 2, 5)))),
                              '\\.'),
                     function(x) {
                       generateEDGEdata(input_folder = paste0(getConfig("mainfolder"), "/sources/EDGE-T_standalone/"),
                                        output_folder = NULL,
                                        EDGE_scenario = x[[1]],
                                        REMIND_scenario = x[[2]],
                                        IEAbal = calcOutput("IO", subtype = "IEA_output", aggregate = TRUE),
                                        GDP_country = calcOutput("GDPppp", aggregate = F),
                                        POP_country = calcOutput("Population", aggregate = F),
                                        saveRDS = FALSE)
                       })


  setConfig(regionmapping = regionmapping2use)


  return(list(x = EDGETdata,
              class = 'list',
              unit = NA,
              description = 'EDGEtransport iterative inputs'))


}
