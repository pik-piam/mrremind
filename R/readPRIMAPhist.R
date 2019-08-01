#' @title readPRIMAPhist
#' 
#' @description Read in an PRIMAP-hist data csv file as magclass object.
#' 
#' @param subtype data subtype. available subtypes are:
#' \itemize{
#' \item hist
#' \item hist_no_ex
#' }
#' @return magpie object of the PRIMAP-hist data.
#' 
#' @author Roman Popov
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readPRIMAPhist("PRIMAPhist","hist")
#' }
#' @importFrom magclass as.magpie
#' @importFrom reshape2 melt
#' 
readPRIMAPhist <- function(subtype) {
  
  files <- c(hist = "primap-hist_v1.2_data/PRIMAP-hist_v1.2_14-Dec-2017.csv",
             hist_no_ex = "primap-hist_v1.2_data/PRIMAP-hist_no_extrapolation_v1.2_14-Dec-2017.csv"
  )  
  
  file <- toolSubtypeSelect(subtype, files)
  
  x <- read.csv(file)
  
  x <- melt(x, id.vars = c("scenario", "country", "category", "entity", "unit"))
  
  x$value <- as.numeric(x$value)
  
  x <- as.magpie(x, spatial = 2, temporal = 6, datacol = 7)
  
  # year format is "x1850" and has to set to "y1850"
  getYears(x) <- paste0("y",substring(getYears(x),2))
  
  
  # rename emissions to lower case names
  old_names <- getNames(x, dim=3)
  new_names <- tolower(old_names)
  

  x <- x[,,old_names]
  getNames(x, dim=3) <- new_names
  

  return(x)
}  
