#'
#' Reads country-specific damage coefficients for tropical cyclones from
#' Krichene et al. (in prep.). Data has been provided by the authors,
#' but will be made publicly available as well.
#' This contains data for 41 countries (those exposed to tropical cyclones),
#' and two coefficients (constant and linear temperature)
#'
#' @return TC damage coefficients
#' @author Franziska Piontek
#' @param subtype data subtype. Either "const" or "tasK"

readTCdamageKrichene <- function(subtype) {

        if (subtype == "const") {
                a <- read.csv(file = "./TC_df_parameters_const.csv",sep=";")
        } else if (subtype == "tasK") {
                a <- read.csv(file = "./TC_df_parameters_tasK.csv",sep=";")
        }

        quant <- gsub("estimates_", "", colnames(a)[grepl("estimates", colnames(a))])
                for (q in quant) {
                        col <- which(colnames(a) == paste0("estimates_", q))
                        if (q == quant[1]) {
                                out <- new.magpie(a$iso, NULL, q, a[, col], sets = c("CountryCode", "year", "quantile"))
                        } else {
			out <- mbind(out, new.magpie(a$iso, NULL, q, a[, col], sets = c("CountryCode", "year", "quantile")))
                        }
                }
        return(out)
}
