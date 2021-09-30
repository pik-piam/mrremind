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
  EDGETdata = lapply(list(
    c("SSP1", "ElecEra", TRUE),
    c("SSP2", "Mix", FALSE),
    c("SSP5", "ConvCase", FALSE),
    c("SSP2EU", "Mix", FALSE),
    c("SDP", "ElecEraWise", TRUE)),
    function(x) {
      generateEDGEdata(input_folder = paste0(getConfig("mainfolder"), "/sources/EDGE-T_standalone/"),
                       output_folder = NULL,
                       SSP_scen = x[1],
                       tech_scen = x[2],
                       smartlifestyle = x[3],
                       IEAbal = calcOutput("IO", subtype = "IEA_output", aggregate = TRUE),
                       GDP_country = {
                         x <- calcOutput("GDPppp", aggregate = F)
                         getSets(x)[1] <- "ISO3"
                         getSets(x)[2] <- "Year"
                         x
                       },
                       POP_country = {
                         x <- calcOutput("Population", aggregate = F)
                         getSets(x)[1] <- "iso2c"
                         x
                       },
                       trsp_incent = readSource("TransportSubsidies", convert=T),
                       storeRDS = FALSE)
    })


  setConfig(regionmapping = regionmapping2use)


  return(list(x = EDGETdata,
              class = 'list',
              unit = NA,
              description = 'EDGEtransport iterative inputs'))


}
