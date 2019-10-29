#' @title readREMIND
#' @description Reads in a reporting mif file from REMIND
#' 
#' @param subtype Either "intensive" or "extensive"
#' @return MAgPIE object with regional aggregation of REMIND H12
#' @author David Klein
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("REMIND",aggregate=FALSE)
#' }
#' @importFrom magclass read.report

readREMIND <- function(subtype) {
  
  # /p/projects/remind/runs/r8473-trunk-C/output/r8473-trunk-C_*/report.mif
  
  file_list <- c("REMIND_generic_r8473-trunk-C_Budg600-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_Budg950-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_Budg1300-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_NDC-rem-5.mif",
                 "REMIND_generic_r8473-trunk-C_NPi-rem-5.mif")
  
  x <- NULL
  for(f in file_list) {
    x <- mbind(x,read.report(f,as.list = FALSE))
  }
  
  # remove model and variable name
  x <- collapseNames(x)
  
  # shorten names of the REMIND scenarios
  getNames(x) <- gsub("r8473-trunk-C_", "SSP2-",getNames(x))
  getNames(x) <- gsub("-rem-5","",getNames(x))
  getNames(x) <- paste0("R2M41-",getNames(x))

  return(x)

}