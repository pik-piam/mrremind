#' @title downloadSoilGrids
#' @description download SoilGrids data from https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif
#'
#' @author Kristine Karstens
#' 
#' @seealso
#' \code{\link{readSoilGrids}}
#' 
#' @examples
#' \dontrun{ downloadSource("SoilGrids") }
#' 
#' @importFrom utils download.file tail

downloadSoilGrids <- function() {
  
  links <- c("https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif")
  
  ### download files
  fnames <- tail(strsplit(links, split = "/")[[1]], 1)
  download.file(links, destfile=fnames, mode="wb")
}