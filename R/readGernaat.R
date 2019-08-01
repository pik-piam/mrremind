#' Read Hydro-Potential data from Gernaat et. al 2017
#' 
#' Read privileged csv file sent by David Gernaat for global hydropotential data. Technical potential 
#' defined as <0.5 $/kWh and economic potential as <0.1 $/kWh.
#' 
#' @return magpie object of aggregated hydropotential data from Gernaat et. al 2017
#' @author Atreya Shankar, Renato Rodrigues
#' @source Obtained from Nature Energy volume 2 (2017), pp 821–828, with following source: https://www.nature.com/articles/s41560-017-0006-y
#' @examples
#' \dontrun{a <- readGernaat()}

readGernaat <- function(){
    a <- read.csv("Atreya_PIK_Remain_r7_1_mod.csv", stringsAsFactors = FALSE, check.names = FALSE)
    x <- as.magpie(a, spatial = 1)
    x <- x[,,which(!getNames(x) %in% c("Lake volume (m3)", "SysID (1=DiversionalCanalPower/2=RiverPower)"))]
    return(x)
}