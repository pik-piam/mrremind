#' @title readGTAPTaxRates
#' @description Read in Tax rates from BaseRate dataset of GTAP databases that has been downlodaded from the GTAP wewbsite.
#'
#'
#' @param subtype Type of GTAP data that should be read. So far available are:
#'        \itemize{
#'        \item GTAP7:
#'        \itemize{
#'        \item \code{GTAP7_RTMS}: Import tariffs ad valorem [%]
#'        \item \code{GTAP7_RTXS}: Export tariffs ad valorem [%]
#'        }
#'        \item GTAP8:
#'        \itemize{
#'        \item \code{GTAP8_RTMS}: Import tariffs ad valorem [%]
#'        \item \code{GTAP8_RTXS}: Export tariffs ad valorem [%]
#'        }
#'      }
#'
#' @return GTAP tax rates data as a MAgPie-Object
#' @author Xiaoxi Wang
#' @examples
#'   \dontrun{
#'     a <- readSource("GTAP7", "RTMS")
#'            }
#' @importFrom reshape2 acast
#' @importFrom magclass as.magpie getYears


readGTAPTaxRates <- function(subtype=NULL){
  
  files <- c(GTAP7_RTMS="BaseRate.csv",GTAP7_RTXS="BaseRate.csv",
             GTAP8_RTMS="BaseRate.csv",GTAP8_RTXS="BaseRate.csv")
  file <- toolSubtypeSelect(subtype, files)
  path <- paste0(tolower(gsub("(?<=\\d)\\w{1,}", "", subtype, perl = T)), "/")
  
  x <- read.csv(paste0(path, file),header = FALSE,stringsAsFactors = FALSE,skip = 3)
  
  .subsetGTAP<- function(x){
    all_start <- grep("!Header:", x$V1)
    dims <- x[all_start,"v2"]
    gtapval <- sub("[A-Z]{4}\\d_", "", subtype, perl = TRUE)
    start_row <- grep(gtapval,x$V1)
    
    if (start_row == max(all_start)) {
      endrow <- nrow(x)
    }else{
      count <- match(start_row, all_start)
      endrow <- all_start[count + 1] - 1
    }
    
    x <- x[c((start_row + 1):endrow), ]
    
    start_row<- grep("Value",x$V1)+2
    endrow <- nrow(x)
    
    ind_value <- c(start_row:endrow)[c(start_row:endrow) %%2==1]
    
    y <- x[-c(ind_value,grep("Value",x$V1)),]
    names(y) <- gsub("\ ", "_",gsub("^\ ", "",paste(y[1,],y[2,],sep=" ")))
    y <- y[-(1:2),]
    value <- x[ind_value,"V1"]
    y$value <- as.numeric(value)
    return(y)
  }
  
  x <- .subsetGTAP(x)
  
  x <- as.magpie(x[,c(3,2,1,4)],spatial=1,tidy=TRUE)
  #  x[is.na(x)] <- 0
  return(x)
}