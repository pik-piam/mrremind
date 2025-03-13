convertIEA_InvestDatav2024 <- function(x){
  x <- x[c("World", "China"),,]
  getItems(x, dim = 1) <- c("GLO", "CHA")
  y <- toolCountryFill(x, fill = NA)
}
