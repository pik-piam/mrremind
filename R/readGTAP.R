#' @title readGTAP
#' @description Read BaseData and BaseView in GTAP database that has been downlodaded from the GTAP wewbsite.
#'
#'
#' @param subtype Type of GTAP data that should be read. So far available are:
#'        \itemize{
#'        \item GTAP7:
#'        \itemize{
#'        \item \code{GTAP7_VIWS}: Trade - Bilateral Imports at World Prices
#'        \item \code{GTAP7_VIMS}: Trade - Bilateral Imports at Market Prices
#'        \item \code{GTAP7_VXWD}: Trade - Bilateral Exports at World Prices
#'        \item \code{GTAP7_VXMS}: Trade - Bilateral Exports at Market Prices
#'        \item \code{GTAP7_VDFM}: Intermediates - Firms' Domestic Purchases at Market Prices
#'        \item \code{GTAP7_VIFM}: Intermediates - Firms' Imports at Market Prices
#'        \item \code{GTAP7_VFM}: Endowments - Firms' Purchases at Market Prices
#'        \item \code{GTAP7_VOA}: Payment received by producers (fram gtate value)
#'        \item \code{GTAP7_VOM}: Value of output at dometic market prices
#'        }
#'        \item GTAP8:
#'        \itemize{
#'        \item \code{GTAP8_VIWS}: Trade - Bilateral Imports at World Prices
#'        \item \code{GTAP8_VIMS}: Trade - Bilateral Imports at Market Prices
#'        \item \code{GTAP8_VXWD}: Trade - Bilateral Exports at World Prices
#'        \item \code{GTAP8_VXMS}: Trade - Bilateral Exports at Market Prices
#'        \item \code{GTAP8_VDFM}: Intermediates - Firms' Domestic Purchases at Market Prices
#'        \item \code{GTAP8_VIFM}: Intermediates - Firms' Imports at Market Prices
#'        \item \code{GTAP8_VFM}: Endowments - Firms' Purchases at Market Prices
#'        \item \code{GTAP8_VOA}: Payment received by producers (fram gtate value)
#'        \item \code{GTAP8_VOM}: Value of output at dometic market prices
#'        }
#'      }
#'
#' @return GTAP data as a MAgPie-Object
#' @author Stephen Wirth, Xiaoxi Wang
#' @examples
#'   \dontrun{
#'     a <- readSource("GTAP7", "VIWS")
#'            }
#' @importFrom reshape2 acast
#' @importFrom magclass as.magpie getYears
readGTAP <- function(subtype = NULL) {
  files <- c(
    GTAP7_VIMS = "BaseData.csv",
    GTAP7_VXWD = "BaseData.csv",
    GTAP7_VIWS = "BaseData.csv",
    GTAP7_VXMD = "BaseData.csv",
    GTAP7_VDFM = "BaseData.csv",
    GTAP7_VIFM = "BaseData.csv",
    GTAP7_VFM  = "BaseData.csv",
    GTAP7_VOM  = "BaseView.csv",
    GTAP7_VOA  = "BaseView.csv",
    GTAP8_VIMS = "BaseData.csv",
    GTAP8_VXWD = "BaseData.csv",
    GTAP8_VIWS = "BaseData.csv",
    GTAP8_VXMD = "BaseData.csv",
    GTAP8_VDFM = "BaseData.csv",
    GTAP8_VIFM = "BaseData.csv",
    GTAP8_VFM  = "BaseData.csv",
    GTAP8_VOM  = "BaseView.csv",
    GTAP8_VOA  = "BaseView.csv"
  )
  
    file <- toolSubtypeSelect(subtype, files)
    
    path <-
      paste0(tolower(gsub("(?<=\\d)\\w{1,}", "", subtype, perl = T)), "/")
  if (grepl("BaseData.csv", file)) {
    x <-
      read.csv(
        paste0(path, file),
        header = FALSE,
        stringsAsFactors = FALSE,
        skip = 3
      )} else if (grepl("BaseView.csv", file)){
        x <-
          read.csv(
            paste0(path, file),
            header = FALSE,
            stringsAsFactors = FALSE,
            skip = 0)}
    
    all_start <- which(grepl("!Header:", x$V1))
    dims <- x$V2[which(grepl("dimensions", x$V2))]
    gtapval <- sub("[A-Z]{4}\\d_", "", subtype, perl = T)
    start_row <- which(grepl(paste("!Header:",gtapval,sep=" "), x$V1))
    if (start_row == max(all_start)) {
      endrow <- nrow(x)
    }else{
      count <- match(start_row, all_start)
      endrow <- all_start[count + 1] - 1
    }
    x <- x[c((start_row + 1):endrow), ]
    if (x[nrow(x), ncol(x)] == "") {
      x <- x[-nrow(x), ]
    }
    if (length(grep("REG", x[1, ])) > 1) {
      x[1, grep("REG", x[1, ])][1] <- "Regions"
    }
    else
      x[1, grep("REG", x[1, ])] <- "Regions"
    colnames(x) <- x[1, ]
    x <- x[-1, ]
    if (grep("Regions", colnames(x)) != 1)
    {
      x[, c(1, grep("Regions", colnames(x)))] <-
        x[, c(grep("Regions", colnames(x)), 1)]
      colnames(x)[c(1, grep("Regions", colnames(x)))] <-
        colnames(x)[c(grep("Regions", colnames(x)), 1)]
    }
    x$Value <- as.numeric(x$Value)
    out <- as.magpie(x, spatial = 1, tidy = TRUE)
    if("REG" %in% colnames(x)){
    x <- unwrap(out)
    x <- aperm(x, c(1, 2, 4, 3))
    x <- as.magpie(x, spatial = 1, temporal = 2)
    }
    else {x <- out}
    #is GTAP released every 3 years?
    if (grepl("GTAP7", subtype)) {
      year <- "y2004"
    }
    else if (grepl("GTAP8", subtype)) {
      year <- "y2007"
    }
    
    getYears(x) <- year
    getRegions(x) <- toupper(getRegions(x))
    return(x)
}
