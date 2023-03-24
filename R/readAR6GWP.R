#' Read GWP (or other metrics) from the AR6 WGIII Table SM7 per GHG species
#' @md
#'
#' @return A data.frame with two columns, "Gas", with the common name of the GHG species, and "GWP", with the selected GWP
#'
#' @author Gabriel Abrahao
#'
#' @param subtype data subtype. Currently just "GWP100", but other metrics are also available in the input data
#' @importFrom readxl read_xlsx
#'
#' @export
readAR6GWP <- function(subtype = "GWP100") {
    if (subtype == "GWP100") {
        # intable <- as.data.frame(read_excel("/p/projects/rd3mod/inputdata/sources/AR6/AR6_WGIII_TableSM7.xlsx", sheet = "table"))
        intable <- as.data.frame(read_excel("AR6_WGIII_TableSM7.xlsx", sheet = "table"))

        x <- intable[,c("Name","GWP 100")]
        colnames(x) <- c("Gas","GWP")
        out <- list(class = "data.frame", x = x)
    }
    return(out)
}