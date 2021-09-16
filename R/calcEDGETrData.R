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
  EDGETdata = lapply(strsplit(c(cartesian(x = c("ElecEraWise"),   ## also SSP2-lowEn, when it will exist
                                          y = c("SDP", "SSP1")),
                                cartesian(x = c("ElecEra", "ConvCase", "HydrHype", "Mix"),
                                          y = c("SSP2")),
                                cartesian(x = c("Mix"),
                                          y = c("SSP2Ariadne")),
                                cartesian(x = c("ConvCase"),
                                          y = c("SSP5"))),
                              '\\.'),
                     function(x) {
                       generateEDGEdata(input_folder = paste0(getConfig("mainfolder"), "/sources/EDGE-T_standalone/"),
                                        output_folder = NULL,
                                        techscen = x[[1]],
                                        SSP_scen = x[[2]],
                                        IEAbal = calcOutput("IO", subtype = "IEA_output", aggregate = TRUE),
                                        GDP_country = {
                                            x <- calcOutput("GDPppp", aggregate = F)
                                            getSets(x)[1] <- "ISO3"
                                            getSets(x)[2] <- "Year"
                                            x
                                          },
                                        RatioPPP2MER_country = calcOutput("RatioPPP2MER", aggregate =F),
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
