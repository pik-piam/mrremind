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
                a <- read.csv(file = "./TC_df_parameters_const.csv")
        } else if (subtype == "tasK") {
                a <- read.csv(file = "./TC_df_parameters_tasK.csv")
        }

        quant <- gsub("estimates_", "", colnames(a)[grepl("estimates", colnames(a))])
        for (p in unique(a$persistence)) {
                for (q in quant) {
                        a2 <- a[which(a$persistence == p), ]
                        col <- which(colnames(a2) == paste0("estimates_", q))
                        if (p == 0 && q == quant[1]) {
                                out <- new.magpie(a2$iso, NULL, paste0(p, ".", q), a2[, col], sets = c("CountryCode", "year", "persistence.quantile"))
                        } else {
                                out <- mbind(out, new.magpie(a2$iso, NULL, paste0(p, ".", q), a2[, col], sets = c("CountryCode", "year", "persistence.quantile")))
                        }
                }
    }
        return(out)
}
