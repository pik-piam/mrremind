#' Fills years in a magpie object with their closest temporal neighbor
#' When a NA is in the middle of two valid years, choose the previous one
#'
#' Does not do anything for slices where all timesteps are NA
#' 
#' @param inx a magclass object with NAs
#' @returns the magclass object with some of those NAs filled

# Fills years in a magpie object with their closest temporal neighbor
# When a NA is in the middle of two valid years, choose the previous one
toolFillYearsWithClosest <- function(inx) {
    outx <- inx
    tall <- getYears(inx, as.integer = T)
    tfirst <- min(tall)
    # Reference FALSE vectors with the full size and slice size
    refxbig <- inx
    refxbig[, , ] <- FALSE
    refxslice <- setYears(refxbig[, tfirst, ])

    # Year used to fill each cell
    fillyears <- refxbig
    fillyears[, , ] <- NA

    lastvalid <- refxslice
    lastvalidyear <- refxslice
    lastvalid[, , ] <- NA
    lastvalidyear[, , ] <- NA
    for (ti in tall) {
        isna_now <- setYears(is.na(inx[, ti, ]))
        isfilled <- refxslice
        for (tj in tall[tall > ti]) {
            # if(tj == 2040) {break}
            isna_j <- setYears(is.na(inx[, tj, ]))
            fillwithj <- isna_now & !isna_j & !isfilled
            outx[, ti, ] <- ifelse(fillwithj, inx[, tj, ], outx[, ti, ])
            fillyears[, ti, ] <- ifelse(fillwithj, tj, fillyears[, ti, ])
            isfilled <- isfilled | fillwithj
        }
        lastvalid <- ifelse(!isna_now, setYears(inx[, ti, ]), lastvalid)
        lastvalidyear <- ifelse(!isna_now, ti, lastvalidyear)
        islastcloser <- abs(lastvalidyear - ti) <= abs(fillyears[, ti, ] - ti)
        islastcloser[!isfilled] <- TRUE # If there was no future year, last is closer
        fillwithlast <- isna_now & !is.na(lastvalid) & islastcloser
        outx[, ti, ] <- ifelse(fillwithlast, lastvalid, outx[, ti, ])
        fillyears[, ti, ] <- ifelse(fillwithlast, lastvalidyear, fillyears[, ti, ])
    }
    return(outx)
}
