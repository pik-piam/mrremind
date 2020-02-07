#' @importFrom magclass getYears<-

calcCapacityOffset <- function() {
    x <- readSource("REMIND_11Regi", subtype="deltacapoffset")
    getYears(x) <- "y2010"
    return(list(x=x,weight=NULL,
                unit="TW", 
                description="global offset of 200MW multiplied with the regional share of PE2SE capacities"
    ))
}