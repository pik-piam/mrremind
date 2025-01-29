
convertCDIAC <- function(x) {

  # Filter regions mapping to XXX
  remove <- getRegions(x)[toolCountry2isocode(getRegions(x)) == "XXX"]
  x <- x[remove, , , invert = TRUE]

  # Nations names to ISO country codes
  getItems(x, dim = 1) <- toolCountry2isocode(getRegions(x))

  # CDIAC(XIT) -> ISO(ITA + SMR)
  # CDIAC(XFR) -> ISO(FRA + MCO)
  m <- matrix(c(c("XIT", "XIT", "XFR", "XFR"), c("ITA", "SMR", "FRA", "MCO")), 4)
  w <- calcOutput("Population", scenario = "SSP2", years = 2005, aggregate = FALSE)[c("ITA", "SMR", "FRA", "MCO"), , ]
  x_split <- toolAggregate(x[c("XIT", "XFR"), , ], m, weight = w)

  # delete XIT and XFR from x
  x <- x[c("XIT", "XFR"), , invert = TRUE]
  x <- mbind(x, x_split)

  # sort years
  x <- x[, sort(getYears(x)), ]

  # split Germany before 2045
  x_DE <- x["DEU", , ]
  getItems(x_DE, dim = 1) <- "XDE"
  x_DE[getYears(x_DE, as.integer = TRUE) >= 1945]   <- 0   # set to 0 for the time after 1945
  x["DEU", , ][getYears(x, as.integer = TRUE) < 1945] <- 0   # set to 0 for the time before 1945
  x <- mbind(x, x_DE)

  # FIXME: ANT and XAN exist in CDIAC -> has to be distributed properly (for a starting point see ISOhistorical.csv)

  x[is.na(x)] <- 0
  x <- toolISOhistorical(x, overwrite = TRUE)
  x <- toolCountryFill(x, fill = 0, verbosity = 2, no_remove_warning = c("XAN"))
  getSets(x) <- getSets(new.magpie())

  return(x)
}
