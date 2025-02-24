#' Returns the year associated with a given ieaVersion
#' @param ieaVersion Release version of IEA data, either 'default' (vetted and used in REMIND)
#' or 'latest'.
#' @author Falk Benke
#'
toolGetIEAYear <- function(ieaVersion) {
  if (ieaVersion == "default") {
    return(2024)
  } else {
    return(2024)
  }
}
