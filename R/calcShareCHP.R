calcShareCHP <- function() {
  ieaBal <- readSource("IEA", subtype = "EnergyBalances")[, seq(2005, 2020, 5), ]

  y <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  x <- new.magpie(
    cells_and_regions = getItems(ieaBal, dim = 1),
    years = y,
    names = NULL
  )

  s <- dimSums(ieaBal[, , c("ELMAINC", "ELAUTOC")], dim = 3, na.rm = T) /
    dimSums(ieaBal[, , "ELOUTPUT"], dim = 3, na.rm = T)

  s[is.na(s)] <- 0

  x[, seq(2005, 2020, 5), ] <- s
  x[, y[y >= 2050], ] <- x[, 2020, ] + 0.2
  x[, seq(2020, 2050, 5), ] <- toolFillYears(x[, c(2020, 2050), ], seq(2020, 2050, 5))
  x[x > 0.75] <- 0.75

  w <- new.magpie(
    cells_and_regions = getItems(ieaBal, dim = 1),
    years = y,
    names = NULL
  )

  w[, seq(2005, 2020, 5), ] <- dimSums(ieaBal[, , "ELOUTPUT"], dim = 3)
  w[, y[y > 2020], ] <- w[, 2020, ]

  return(list(
    x = x,
    weight = w,
    unit = "ratio",
    description = "share of electricity from chp on total electricity"
  ))
}
