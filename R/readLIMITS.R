readLIMITS <- function(subtype) {
  if (is.null(subtype)) stop("Please provide one of the following subtypes: 'activities' or 'emissions'")

  x <- as.data.frame(read_excel("LIMITS_sector_act_emi.xlsx", sheet = subtype))

  if (subtype == "activities") {
    x <- as.magpie(x, spatial = 1, temporal = 4)
  } else if (subtype == "emissions") {
    x <- as.magpie(x, spatial = 1, temporal = 5, datacol = 6)
  }

  return(x)
}
