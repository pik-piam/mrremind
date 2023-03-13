calcShareCHP <- function() {
  ieaBal <- readSource("IEA", subtype = "EnergyBalances")[, seq(2005, 2020, 5), ]

  x <- dimSums(ieaBal[, , c("ELMAINC", "ELAUTOC")], dim = 3, na.rm = T) /
    dimSums(ieaBal[, , "ELOUTPUT"], dim = 3, na.rm = T)

  x[is.na(x)] <- 0

  w <- dimSums(ieaBal[, , "ELOUTPUT"], dim = 3)

  return(list(
    x = x,
    weight = w,
    unit = "ratio",
    description = "share of electricity from chp on total electricity"
  ))
}
