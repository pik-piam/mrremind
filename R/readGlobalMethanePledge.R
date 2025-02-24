#' Countries that commited to the Global Methane Pledge by 23.02.2025
#'
#' Data scraped from the map at https://www.globalmethanepledge.org/
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Gabriel Abrahao
readGlobalMethanePledge <- function() {
    dfgmp <- read.table("global_methane_pledge_members.csv", sep = ";", header = TRUE)
    colnames(dfgmp) <- c("iso2", "country", "data")
    mpgmp <- as.magpie(dfgmp[c("country", "data")], spatial = "country")
    return(list(
        x = mpgmp,
        weight = NULL,
        unit = "binary",
        description = "Countries that commited to the Global Methane Pledge by 23.02.2025, 1 for commited, 0 for not"
    ))
}
