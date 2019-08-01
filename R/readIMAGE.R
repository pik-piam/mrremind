readIMAGE <- function() {
  x <- as.data.frame(read_excel("SSP_V13_F-gas_IMAGE_v2.xlsx", sheet="F-gas"))
  x <- x[c(2:3,5:17)]
  x <- x[c(2,1,3:15)]
  x[is.na(x)] <- 0
  rid <- x["Region"]=="World"
  x <- x[!(rid),]
  x <- as.magpie(x, spatial=1)
  return(x) 
}