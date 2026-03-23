#' Calc Input Output for REMIND
#'
#' A wrapper around calcIO used to generate REMIND input data, created to apply
#' corrective steps in a cache-conscious manner by avoiding additional subtypes
#' in calcIO.
#'
#' @param subtype either "input", "output" or "trade"
#'
#' @author Falk Benke
calcIoRemind <- function(subtype) {
  x <- calcOutput("IO", subtype = subtype, corrected = TRUE, supplementary = TRUE)
  desc <- x$description
  x <- x$x

  # remove items only used for reporting
  if (subtype == "output") {
    x <- x[, , "rep", pmatch = TRUE, invert = TRUE]
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "EJ",
    description = x$description
  ))
}
