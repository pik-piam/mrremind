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

  ## monitor edgeTransport functions
  "#! @monitor edgeTransport:::compScenEDGET"
  "#! @monitor edgeTransport:::lineplots_perCap"
  "#! @monitor edgeTransport:::level2path"
  "#! @monitor edgeTransport:::level1path"
  "#! @monitor edgeTransport:::level0path"
  "#! @monitor edgeTransport:::generateEDGEdata"
  "#! @monitor edgeTransport:::levelNpath"
  "#! @monitor edgeTransport:::level0path"
  "#! @monitor edgeTransport:::level1path"
  "#! @monitor edgeTransport:::level2path"
  "#! @monitor edgeTransport:::calcgenerateEDGEdata"
  "#! @monitor edgeTransport:::collectScens"
  "#! @monitor edgeTransport:::outpath"
  "#! @monitor edgeTransport:::Calc_pref_and_prices"
  "#! @monitor edgeTransport:::Update_Validation_Excel_tool"
  "#! @monitor edgeTransport:::Update_sw_trend"
  "#! @monitor edgeTransport:::lvl0_GCAMraw"
  "#! @monitor edgeTransport:::lvl0_VOTandExponents"
  "#! @monitor edgeTransport:::lvl0_IntAvPreparation"
  "#! @monitor edgeTransport:::lvl0_incocost"
  "#! @monitor edgeTransport:::lvl0_loadEU"
  "#! @monitor edgeTransport:::lvl0_loadUCD"
  "#! @monitor edgeTransport:::psi_file"
  "#! @monitor edgeTransport:::lvl0_mrremind"
  "#! @monitor edgeTransport:::lvl1_IEAharmonization"
  "#! @monitor edgeTransport:::lvl1_calibrateEDGEinconv"
  "#! @monitor edgeTransport:::opt_func"
  "#! @monitor edgeTransport:::jacobian"
  "#! @monitor edgeTransport:::root_func"
  "#! @monitor edgeTransport:::lvl1_preftrend"
  "#! @monitor edgeTransport:::filldt"
  "#! @monitor edgeTransport:::apply_logistic_trends"
  "#! @monitor edgeTransport:::logistic_trend"
  "#! @monitor edgeTransport:::lvl2_demandReg"
  "#! @monitor edgeTransport:::lvl2_demandRegNAVIGATEIntl"
  "#! @monitor edgeTransport:::lvl2_REMINDdemand"
  "#! @monitor edgeTransport:::lvl2_createoutput"
  "#! @monitor edgeTransport:::addScenarioCols"
  "#! @monitor edgeTransport:::lvl2_generate_plotdata"
  "#! @monitor edgeTransport:::level2path"
  "#! @monitor edgeTransport:::level1path"
  "#! @monitor edgeTransport:::level0path"
  "#! @monitor edgeTransport:::LogitCostplotdata"

  ## monitor edgeTrpLib functions
  "#! @monitor edgeTrpLib:::calculate_capCosts"
  "#! @monitor edgeTrpLib:::X2Xcalc"
  "#! @monitor edgeTrpLib:::F2Vcalc"
  "#! @monitor edgeTrpLib:::linDecrease"
  "#! @monitor edgeTrpLib:::linIncrease"
  "#! @monitor edgeTrpLib:::calcVint"
  "#! @monitor edgeTrpLib:::compareScenarios_EDGET"
  "#! @monitor edgeTrpLib:::createRDS"
  "#! @monitor edgeTrpLib:::loadInputData"
  "#! @monitor edgeTrpLib:::datapathForFile"
  "#! @monitor edgeTrpLib:::applylearning"
  "#! @monitor edgeTrpLib:::calc_num_vehicles_stations"
  "#! @monitor edgeTrpLib:::merge_prices"
  "#! @monitor edgeTrpLib:::prepare4REMIND"
  "#! @monitor edgeTrpLib:::reportEDGETransport"
  "#! @monitor edgeTrpLib:::datapath"
  "#! @monitor edgeTrpLib:::reporting"
  "#! @monitor edgeTrpLib:::reportingEmi"
  "#! @monitor edgeTrpLib:::reportingVehNum"
  "#! @monitor edgeTrpLib:::reportStockAndSales"
  "#! @monitor edgeTrpLib:::reportTotals"
  "#! @monitor edgeTrpLib:::LogitCostplotdata"
  "#! @monitor edgeTrpLib:::LogitCostplotdata_FV"
  "#! @monitor edgeTrpLib:::Calc_shares"
  "#! @monitor edgeTrpLib:::shares_intensity_and_demand"

  allscens <- bind_rows(
    ## for all "default" SSP variants we ship the whole zoo of EDGE-T scenarios
    expand_grid(
      SSP_scen = c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
      tech_scen = c("ConvCase", "ElecEra", "HydrHype",
                    "Mix", "Mix1", "Mix2", "Mix3", "Mix4"),
      smartlifestyle = 'FALSE')
    ## SHAPE scenarios are coupled to specific technologies
    ## tribble(
    ##   ~SSP_scen,   ~tech_scen,   ~smartlifestyle,
    ##   'SDP_EI',    'Mix4',    'FALSE',
    ##   'SDP_MC',    'Mix4',    'FALSE',
    ##   'SDP_RC',    'Mix3',    'FALSE')
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
