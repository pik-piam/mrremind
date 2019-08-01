#' Read Wirsenius Livestock Subsystems
#' 
#' Read in data on livestock subsystems structure. Source: Wirsenius.
#' 
#' @param subtype Type of subsystem information that should be read in. 
#' Available types are: "cattlemeat2milk", "meat2egg" and "feed_beefsys_ratio"
#' @return magpie object of selected livestock subsystem data
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("WirseniusSubsystems","cattlemeat2milk")
#' a <- readSource("WirseniusSubsystems","meat2egg")
#' a <- readSource("WirseniusSubsystems","feed_beefsys")
#' }
#' 

readWirseniusSubsystems <- function(subtype) {
  
  # read in files
  files <- c(cattlemeat2milk="cattlemeat2milk_ratio.csv",
             meat2egg="meat2egg_ratio.csv",
             feed_beefsys="feed_beefsys_ratio.csv"
             )
  
  file <- toolSubtypeSelect(subtype,files)
  
  w <- read.csv(file, sep=",", header=TRUE)
  w <- as.magpie(w, spatial=1,datacol=2)
  
  regions <- c("East Asia"="EAS", "East Europe"="EER", "Latin America & Carib."="LAC", "North Africa & W. Asia"="NAA", "North America & Oc."="NAO", "South & Central Asia"="SCA", "Sub-Saharan Africa"="AFR", "West Europe" ="WER")
  getRegions(w) <- regions
  getYears(w) <- "y1995"

  return(w)
} 
