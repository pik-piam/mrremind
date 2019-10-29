#' @title readLandfillCH4Capture
#' @description reads percentage of ch4 from landfills that is captured, from World Bank CURB Toolv2.1
#' @return Magpie object with results on global level.
#' @author David Chen
#' @seealso
#' \code{\link{readSource}}
#' @importFrom dplyr bind_cols
#' @importFrom stats na.exclude


readLandfillCH4Capture <- function() {


CH4_capt_rate <- read.csv("capture_rates.csv")
a <- toolCountry2isocode(CH4_capt_rate[,1])
a <- bind_cols(as.data.frame(a), CH4_capt_rate)
a <- a[,c(1,3)]
a <- na.exclude(a)
colnames(a) <- c("country", "capture_rate")
a <- as.magpie(a)

return(a)
}


