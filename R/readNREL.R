#' Read NREL
#'
#' Read-in NREL xlsx file as magclass object
#'
#' @param subtype type either "onshore" or "offshore"
#' @return magpie object of NREL
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' a <- readSource(type = "NREL", subtype = "onshore")
#' }
#'
readNREL <- function(subtype) {

  if (subtype == "onshore") {

    # read in data
    x <- as.data.frame(read_excel("nrelcfddawindsc20130603.xlsx", sheet = "Onshore Energy",
                                  skip = 2, .name_repair = "unique_quiet"))
    x <- x[1:182, 1:32]
    x <- x[-length(names(x))]
    names(x) <- sub("\\.[0-9]*$", "", names(x))
    names(x) <- gsub(" ", ".", names(x))
    colnames(x)[2:11] <- paste("near", colnames(x)[2:11], sep = ".")
    colnames(x)[12:21] <- paste("transitional ", colnames(x)[12:21], sep = ".")
    colnames(x)[22:31] <- paste("far", colnames(x)[22:31], sep = ".")

  } else if (subtype == "offshore") {

    # read in data
    x <- as.data.frame(read_excel("nrelcfddawindsc20130603.xlsx",
                                  sheet = "Offshore Energy", skip = 3,
                                  .name_repair = "unique_quiet"))
    x <- x[1:560, 1:33]
    x <- x[-length(names(x))]
    names(x) <- sub("\\.[0-9]*$", "", names(x))
    names(x) <- gsub(" ", ".", names(x))
    colnames(x)[3:12] <- paste("near", colnames(x)[3:12], sep = ".")
    colnames(x)[13:22] <- paste("transitional ", colnames(x)[13:22], sep = ".")
    colnames(x)[23:32] <- paste("far", colnames(x)[23:32], sep = ".")
    x$depth_class[is.na(x$depth_class)] <- "Total"
    x$IAM.country <- gsub(" Total", "", x$IAM.country)
    x$IAM.country[is.na(x$IAM.country)] <- x$IAM.country[which(is.na(x$IAM.country)) - 1]
    x$IAM.country[is.na(x$IAM.country)] <- x$IAM.country[which(is.na(x$IAM.country)) - 1]
  }

  # turn into a magclass object
  x <- as.magpie(x, spatial = 1)

  if (subtype == "offshore") {
    # FIXME find better solution how to deal with this part of ANT
    x <- x["Guadeloupe and Martinique", , , invert = TRUE]
  }

  return(x)
}
