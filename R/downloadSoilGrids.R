#' @title downloadSoilGrids
#' @description This function download the raw SoilGrids data (available at https://files.isric.org/soilgrids/data/recent)
#' or if available the preprocessed raster layers.
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#' @param subtype  Switch between different input. Use predefined ones or any FileName specified in 'SoilGrids/META_GEOTIFF_1B.csv'
#'
#' @seealso
#' \code{\link{readSoilGrids}}
#'
#' @examples
#' \dontrun{ downloadSource("SoilGrids",subtype="carbon0_30") }
#'
#' @import madrat
#' @import magclass
#' @importFrom utils download.file tail
#' @importFrom madrat toolSubtypeSelect

downloadSoilGrids <- function(subtype="cstock_0_30") {

  if(grepl(".tif",subtype)){
    link  <- paste0("https://files.isric.org/soilgrids/data/recent/",subtype)
  } else{
    links <- c(cstock_0_30  = "OCSTHA_M_30cm_250m_ll.tif", # Soil organic carbon stock in tons per ha for depth interval 0.00 m - 0.30 m
               sandfrac_0   = "SNDPPT_M_sl1_250m_ll.tif",  # Sand content (50-2000 micro meter) mass fraction in % at depth 0.00 m
               sandfrac_5   = "SNDPPT_M_sl2_250m_ll.tif",	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.05 m
               sandfrac_15  = "SNDPPT_M_sl3_250m_ll.tif",	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.15 m
               sandfrac_30  = "SNDPPT_M_sl4_250m_ll.tif")	 # Sand content (50-2000 micro meter) mass fraction in % at depth 0.30 m
    link  <- paste0("https://files.isric.org/soilgrids/data/recent/",toolSubtypeSelect(subtype,links))
  }
  download.file(link, destfile=toolSubtypeSelect(subtype,links), mode="wb")
}
