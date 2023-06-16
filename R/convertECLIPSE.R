#' @importFrom dplyr group_by_ min_rank select_ mutate_
#' @importFrom quitte as.quitte

convertECLIPSE <- function(x, subtype) {

  # Parameter definitions
  downscaling <- TRUE

  # TODO: Switch to EDGAR data
  p_dagg_year <- 2005
  p_dagg_pop  <- "pop_SSP2"

  # Initialisation
  # Local functions
  closest                 <- function(xv, xv2, fun.ties = min, place = "all") {

    if (!(place %in% c("all", "above", "below"))) stop('place must be in c("all", "above", "below")')

    # returns the row number within the group (if the vectors are grouped)
    # of the closest xv value, where xv2 is not NA
    result <- NULL
    if (length(xv[!is.na(xv2)]) == 1) { # case there is only one non NA in the group
      result <- which(!is.na(xv2))
    } else if (length(xv[!is.na(xv2)]) == 0) { # case there is no non NA in the group
      stop("no xv2 with non NA value in the group considered")
    } else {
      for (i in 1:length(xv)) {
        sv <- xv[i]  # value of xv for this line
        xvj <- xv[-i]  # subvector of xv excluding the line considered
        xv2j <- xv2[-i]  # subvector of xv2 excluding the line considered
        xvjNAbar <- xvj[!(is.na(xv2j))]
        xv2jNAbar <- xv2j[!(is.na(xv2j))]


        if (place == "below") resulti <- which(xvjNAbar == max(xvjNAbar[(xvjNAbar <= max(sv, min(xvjNAbar)))])) # returns the closest rank in the subset of xvjNAbar below the value sv, if there are only values above, it takes the first value of the xvjNAbar
        if (place == "above") resulti <- which(xvjNAbar == min(xvjNAbar[(xvjNAbar >= min(sv, max(xvjNAbar)))])) # returns the closest rank in the subset of xvjNAbar above the value sv, if there are only values above, it takes the first value of the xvjNAbar
        if (place == "all") {
          resultiNB <- fun.ties(xvjNAbar[which(abs(xvjNAbar - sv) == min(abs(xvjNAbar - sv)))]) # gives the value that is the closest to sv, if ties, it takes the min by default.
          resulti <- which(xvjNAbar == resultiNB) # gives the line in xvjNAbar where the xv value is equal to resultiNB,
          resulti <- which(xvjNAbar == resultiNB & xv2jNAbar ==  fun.ties(xv2jNAbar[resulti])) # if there are ties, then takes the min (by default) of xv2
        }
        if (length(resulti) > 1) stop("cases where both xv and xv2 are similar in non NA cases")

        resulti <- which(xvj == xvjNAbar[resulti] & xvj %in% xvjNAbar & xv2j == xv2jNAbar[resulti]) # returns the corresponding row number in xvj

        if (resulti >= i) resulti <- resulti + 1 # returns the corresponding row number in xv

        result[i] <- resulti
      }
    }
    return(as.numeric(result))
  }

  fill_Zeros_between      <- function(mdata) {
    # for regions where not everything is 0, take the non 0 value closest in time or the mean of the two closest
    # For instance "End_Use_Residential_Coal" is Null in the US in 2010, but not in 2005 or 2020
    keepNames <- getSets(mdata)[3:length(getSets(mdata))]

    tmp <- mdata %>%
      as.quitte()


    tmp <- tmp[, colSums(is.na(tmp)) < nrow(tmp)]
    grp_cols <- setdiff(colnames(tmp), c("value", "period"))

    output <- tmp  %>%
      as.quitte() %>%
      # mutate(period = as.integer(format(period,"%Y"))) %>%
      mutate(value = ifelse(.data$value == 0, NA, .data$value)) %>%
      # group_by(region, sector,variable,scenario) %>%
      group_by(!!!syms(grp_cols)) %>%
      mutate(rank = min_rank(.data$period),
             value = ifelse(  all(is.na(.data$value))
                            | !any(is.na(.data$value))
                            | !is.na(.data$value),
                            .data$value,
                            ifelse(.data$rank < min(.data$rank[!is.na(.data$value)]),
                                   .data$value[.data$rank == min(.data$rank[!is.na(.data$value)])],
                                   # ifelse(rank > max(rank[!is.na(value)]), value[rank == max(rank[!is.na(value)])],
                                   ifelse(.data$rank > max(.data$rank[!is.na(.data$value)]), 0,
                                          (.data$value[closest(.data$period, .data$value, place = "above")] +
                                             .data$value[closest(.data$period, .data$value, place = "below")]) / 2))),
             value = ifelse(is.na(.data$value), 0, .data$value)) %>%
      ungroup() %>%
      select(-'rank') %>%
      as.data.frame() %>%
      as.quitte()

    output <- output[c(setdiff(colnames(output), c(keepNames, "value")), keepNames, "value")] # ensure that the order of the names is the same as in mdata


    return(as.magpie(output))
  }

  # Check errors
  if (is.null(subtype) | !subtype %in% c("activities.aggregated", "activities.extended", "emissions.aggregated", "emissions.extended", "shipping.emi", "shipping.ef"))
    stop("Please provide one of the following subtypes: 'activities.aggregated', 'activities.extended', 'emissions.aggregated', 'emissions.extended', 'shipping.emi' or 'shipping.ef'")

  # For now, this is useless as the processing for activities and emissions is the same
  if (subtype == "activities.aggregated" || subtype == "activities.extended") {

    #-- Data checks ------------------------
    x <- fill_Zeros_between(x)

    #-- Regional downscaling ---------------
    if (downscaling) {
      m <- toolGetMapping(type = "regional", name = "regionmappingGAINS.csv", returnPathOnly = TRUE, where = "mappingfolder")

      # Get GAINS regional mapping
      map <- read.csv2(m)
      map <- map[!(map$RegionCode == "" | map$CountryCode == "ANT"), c(2, 3)]
      map <- map %>%
        mutate(RegionCode = gsub("\\ \\+", "\\+", gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$RegionCode))))
      map$CountryCode <- factor(map$CountryCode)
      map$RegionCode  <- factor(map$RegionCode)

      # TODO: Use EDGAR data as weight
      w <- calcOutput("Population", aggregate = FALSE)[levels(map$CountryCode), p_dagg_year, p_dagg_pop]
      x <- toolAggregate(x[, , ], map, weight = w)

      # fill all missing countries with 0 (add Antarctica)
      x <- toolCountryFill(x, fill = 0)
    }
  }

  if (subtype == "emissions.aggregated" || subtype == "emissions.extended") {

    #-- Data checks ------------------------
    x <- fill_Zeros_between(x)

    #-- Regional downscaling ---------------
    if (downscaling) {
      m <- toolGetMapping(type = "regional", name = "regionmappingGAINS.csv", returnPathOnly = TRUE, where = "mappingfolder")

      # Get GAINS regional mapping
      map <- read.csv2(m)
      map <- map[!(map$RegionCode == "" | map$CountryCode == "ANT"), c(2, 3)]
      map  <- map %>%
        mutate(RegionCode = gsub("\\ \\+", "\\+", gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", .data$RegionCode))))
      map$CountryCode <- factor(map$CountryCode)
      map$RegionCode  <- factor(map$RegionCode)

      # TODO: Use EDGAR data as weight
      w <- calcOutput("Population", aggregate = FALSE)[levels(map$CountryCode), p_dagg_year, p_dagg_pop]
      x <- toolAggregate(x[, , ], map, weight = w)

      # fill all missing countries with 0 (add Antarctica)
      x <- toolCountryFill(x, fill = 0)
    }
  }


  return(x)
}
