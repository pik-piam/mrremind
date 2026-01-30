calcPHI <- function() {
  x <- readSource("IMF_PHI")

  return(
    list(
      x = x,
      unit = "-",
      description = paste(
        "Share of investments in each region that are not transformed into productive capital."
      ),
      isocountries = FALSE,
      aggregate = FALSE
    )
  )
}
