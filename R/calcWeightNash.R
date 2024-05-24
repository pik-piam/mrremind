calcWeightNash <- function() {
  x <- readSource("REMIND_11Regi", subtype = "nashWeight")
  getNames(x) <- NULL

  return(list(
    x = x,
    weight = NULL,
    unit = "factor",
    description = paste0(
      "fallback Nash weights only to be used if due to infeasibilities ",
      "internal computation of weights does not work"
    )
  ))
}
