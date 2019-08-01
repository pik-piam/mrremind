
#' calcFAOLand
#' 
#' Returns either physical land areas from FAOSTAT.
#' 
#' @importFrom magpiesets findset 
#' @return land areas from FAOSTAT and weight
#' @author Ulrich Kreidenweis
calcFAOLand <- function() {
    selectyears <- findset("past")
    data <- readSource("FAO","Land")[,selectyears,]
    data <- data/10^6
    return(list(x=data,weight=NULL, unit="mio. ha", description = "land-use categories from FAOSTAT"))
}

