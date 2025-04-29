#' @title calc Effort Sharing Target
#' @description provides region specific Effort Sharing Emission target
#'
#' @return target data magpie object
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("EffortSharingTarget")
#' }
#'
calcEffortSharingTarget <- function() {

  target <- readSource("Eurostat_EffortSharing", subtype = "target")

  w <- calcOutput("EffortSharingRefEmi", subtype = "Eurostat_GHG", round = 6, aggregate = F)

  # avoid zero weights, as they cause a warning in aggregation
  w[w == 0] <- 1e-10

  getNames(target) <- NULL

  return(list(
    x = target, weight = w,
    unit = "percentage",
    description = "Effort sharing emission target in relation to 2005 reference emissions"
  ))
}
