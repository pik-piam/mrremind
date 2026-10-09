#' Fractions of the 2020 HFC emissions that remain under the Kigali Amendment, per country
#'
#' For each country, the Kigali phase-down steps of its group (readSource("KAReductions", "schedule")) are linearly
#' interpolated between the step years (constant before the first and after the last step), applied to the country
#' baseline and expressed as a fraction of the country emissions in the reference year 2020, i.e. the allowed share of
#' the baseline times the baseline relative to the reference year:
#' `fraction(y) = min(1, (1 - reduction(group, y) / 100) x baseline / emissions(2020))`.
#' Emissions are the EDGAR7 emissions of the Annex F Group I HFCs in Mt CO2eq
#' (AR6 GWP100), used as a proxy for the capped consumption. Group I is the only group capped; HFC-23, PFC, SF6 and
#' NF3 are not (fraction 1, not returned).
#'
#' Because the steps are interpolated, the reduction reaches the legal value of a step in the step year and is larger
#' than the legal one between steps (the law holds the previous step until the next one).
#'
#' Baseline years (HFC part of the legal baselines, from the Kigali Amendment, see README.txt of the source folder):
#' non-Article 5 (also the special group) 2011-2013, Article 5 Group 1 2020-2022, Article 5 Group 2 2024-2026. Only the
#' years available in EDGAR7 (up to 2021) are used; if none of the legal years is available (Group 2) the fallback
#' 2020-2021 is used, which is probably too low. The assumptions of each group are listed in its block in the code.
#'
#' The HCFC part of the legal baseline (+65% / +15% / +25% of the HCFC
#' baseline) is NOT included, so the caps are tighter than the legal ones. Countries without a group get 1.
#'
#' The fractions are intensive: when aggregating to regions they are weighted by the 2020 Group I emissions.
#'
#' @return list for [madrat::calcOutput()]: magpie object (country, years 2020-2100, fraction_hfc_I) and weight
#' @author Gabriel Abrahao
#' @seealso [readKAReductions()]
calcKAReductionFractions <- function() {
  # Everything is expressed relative to the emissions of the reference year, for the years 2020 to 2100
  refYear <- 2020
  outYears <- 2020:2100
  # The capped HFCs (Annex F Group I) that EDGAR7 reports, with the names EDGAR7 uses. HFC-23 is left out on purpose:
  # it is Annex F Group II, which has a destruction duty but no cap. The treaty lists 17 Group I HFCs, EDGAR7 only
  # reports these 13.
  hfcGroupI <- c(
    "HFC-32", "HFC-125", "HFC-134a", "HFC-143a", "HFC-152a", "HFC-227ea", "HFC-245fa", "HFC-365mfc",
    "HFC-134", "HFC-143", "HFC-236fa", "HFC-41", "HFC-43-10-mee"
  )

  # Emissions of the capped HFCs per country in Mt CO2eq, using the AR6 GWPs. EDGAR7 gives kt of each gas
  edgar <- readSource("EDGAR7Fgases", convert = FALSE)
  edgar[is.na(edgar)] <- 0
  edgar <- toolCountryFill(edgar, fill = 0, verbosity = 2, no_remove_warning = "SCG")
  gwpAR6 <- readSource("AR6GWP")
  # AR6GWP and EDGAR7 spell this gas differently
  gwpAR6$Gas[gwpAR6$Gas == "HFC-43-10mee"] <- "HFC-43-10-mee"
  gwp <- setNames(gwpAR6$GWP[match(hfcGroupI, gwpAR6$Gas)], hfcGroupI)
  # To get CO2eq we copy the emissions, fill the copy with the GWP of each gas, multiply and add up the gases. The gas
  # is the slowest-changing dimension, so each GWP is repeated for all country-year cells
  edgarI <- edgar[, , hfcGroupI]
  gwpI <- edgarI
  gwpI[] <- rep(gwp[getItems(edgarI, dim = 3)], each = prod(dim(edgarI)[1:2]))
  em <- dimSums(edgarI * gwpI, dim = 3) / 1000
  # Emissions in the reference year. Summing over the single year drops the year dimension, which is what we want:
  # the baselines below have no year dimension either, so the two can be combined directly
  ref <- dimSums(em[, refYear, ], dim = 2)

  # The Kigali group of each country, as masks: 1 if the country is in the group, 0 if not
  grp <- readSource("KAReductions", subtype = "groups")
  isNonA5 <- grp[, , "nonA5"]
  isNonA5Special <- grp[, , "nonA5_special"]
  isA5G1 <- grp[, , "A5G1"]
  isA5G2 <- grp[, , "A5G2"]
  # Countries that are in none of the groups get no reduction at all (fraction 1)
  isNoGroup <- 1 - isNonA5 - isNonA5Special - isA5G1 - isA5G2
  sched <- readSource("KAReductions", subtype = "schedule", convert = FALSE)

  # What we want is a fraction of the emissions in the reference year (2020), but the treaty is written relative to a
  # baseline that depends on the group, for example the average of 2011-2013 for developed countries. So each group
  # below is done in two steps:
  #  1. the share of its baseline that the group may still emit in each year (1 minus the reduction). The reductions
  #     are the steps of the group from KASchedule.csv, interpolated linearly between the step years and kept constant
  #     before the first and after the last step
  #  2. the baseline relative to the reference year: the mean emissions of the country over the baseline years of the
  #     group, divided by its emissions in the reference year
  # Multiplying the two gives the allowed fraction of the reference-year emissions. Every group is calculated for all
  # countries, and the masks pick the right ones at the end.

  # Non-Article 5 (developed countries), Article 2J paragraph 1
  #  - the baseline is the mean of 2011-2013, which EDGAR7 covers
  #  - there is no freeze, the first step (-10%) already applies in 2019. Because of the interpolation the reduction
  #    in 2020 is therefore already higher than 10%
  baseYearsNonA5 <- 2011:2013
  baselineNonA5 <- dimSums(em[, baseYearsNonA5, ], dim = 2) / length(baseYearsNonA5)
  stepsNonA5 <- sched[sched$group == "nonA5", ]
  # The steps of the group, interpolated linearly between the step years
  reductionNonA5 <- as.magpie(data.frame(region = "GLO", year = stepsNonA5$year, value = stepsNonA5$reduction_pct),
                              spatial = 1, temporal = 2)
  reductionNonA5 <- time_interpolate(reductionNonA5, outYears, integrate_interpolated_years = FALSE,
                                     extrapolation_type = "constant")
  # Step 1: the share of the baseline that is still allowed in each year
  shareOfBaselineNonA5 <- 1 - reductionNonA5 / 100
  # Step 2: how large the baseline is compared with the reference year
  baselineToRefNonA5 <- baselineNonA5 / ref
  fractionNonA5 <- shareOfBaselineNonA5 * baselineToRefNonA5
  # Above 1 means a cap looser than today's emissions, which is no reason to increase them. NA means no emissions in
  # the reference year, so there is nothing to reduce
  fractionNonA5[is.na(fractionNonA5) | fractionNonA5 > 1] <- 1

  # Non-Article 5 countries with special early steps: Belarus, Russia, Kazakhstan, Tajikistan and Uzbekistan
  # (Article 2J paragraph 2, applied to them by Decision XXVIII/2)
  #  - the baseline is the mean of 2011-2013, like for the other non-Article 5 countries. The 25% HCFC component of
  #    their legal baseline is not included, as for every group
  #  - the first steps are -5% (2020) and -35% (2025), after that the steps are the same as for non-Article 5
  baseYearsNonA5Special <- 2011:2013
  baselineNonA5Special <- dimSums(em[, baseYearsNonA5Special, ], dim = 2) / length(baseYearsNonA5Special)
  stepsNonA5Special <- sched[sched$group == "nonA5_special", ]
  # The steps of the group, interpolated linearly between the step years
  reductionNonA5Special <- as.magpie(data.frame(region = "GLO", year = stepsNonA5Special$year,
                                                value = stepsNonA5Special$reduction_pct),
                                     spatial = 1, temporal = 2)
  reductionNonA5Special <- time_interpolate(reductionNonA5Special, outYears, integrate_interpolated_years = FALSE,
                                            extrapolation_type = "constant")
  # Step 1: the share of the baseline that is still allowed in each year
  shareOfBaselineNonA5Special <- 1 - reductionNonA5Special / 100
  # Step 2: how large the baseline is compared with the reference year
  baselineToRefNonA5Special <- baselineNonA5Special / ref
  fractionNonA5Special <- shareOfBaselineNonA5Special * baselineToRefNonA5Special
  # Above 1 means a cap looser than today's emissions, which is no reason to increase them. NA means no emissions in
  # the reference year, so there is nothing to reduce
  fractionNonA5Special[is.na(fractionNonA5Special) | fractionNonA5Special > 1] <- 1

  # Article 5 Group 1 (developing countries not in Group 2, this includes China), Article 5 paragraph 8 qua
  #  - the legal baseline is the mean of 2020-2022, but EDGAR7 stops in 2021, so only 2020-2021 are used
  #  - emissions are frozen from 2024 and the first cut (-10%) comes in 2029. Because of the interpolation the cuts
  #    start right after 2024
  baseYearsA5G1 <- 2020:2021
  baselineA5G1 <- dimSums(em[, baseYearsA5G1, ], dim = 2) / length(baseYearsA5G1)
  stepsA5G1 <- sched[sched$group == "A5G1", ]
  # The steps of the group, interpolated linearly between the step years
  reductionA5G1 <- as.magpie(data.frame(region = "GLO", year = stepsA5G1$year, value = stepsA5G1$reduction_pct),
                             spatial = 1, temporal = 2)
  reductionA5G1 <- time_interpolate(reductionA5G1, outYears, integrate_interpolated_years = FALSE,
                                    extrapolation_type = "constant")
  # Step 1: the share of the baseline that is still allowed in each year
  shareOfBaselineA5G1 <- 1 - reductionA5G1 / 100
  # Step 2: how large the baseline is compared with the reference year
  baselineToRefA5G1 <- baselineA5G1 / ref
  fractionA5G1 <- shareOfBaselineA5G1 * baselineToRefA5G1
  # Above 1 means a cap looser than today's emissions, which is no reason to increase them. NA means no emissions in
  # the reference year, so there is nothing to reduce
  fractionA5G1[is.na(fractionA5G1) | fractionA5G1 > 1] <- 1

  # Article 5 Group 2 (India, Pakistan, Iran, Iraq, Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, UAE)
  #  - the legal baseline is the mean of 2024-2026, which EDGAR7 does not have at all, so 2020-2021 is used instead.
  #    This is probably too low
  #  - emissions are frozen from 2028 and the first cut (-10%) comes in 2032
  baseYearsA5G2 <- 2020:2021
  baselineA5G2 <- dimSums(em[, baseYearsA5G2, ], dim = 2) / length(baseYearsA5G2)
  stepsA5G2 <- sched[sched$group == "A5G2", ]
  # The steps of the group, interpolated linearly between the step years
  reductionA5G2 <- as.magpie(data.frame(region = "GLO", year = stepsA5G2$year, value = stepsA5G2$reduction_pct),
                             spatial = 1, temporal = 2)
  reductionA5G2 <- time_interpolate(reductionA5G2, outYears, integrate_interpolated_years = FALSE,
                                    extrapolation_type = "constant")
  # Step 1: the share of the baseline that is still allowed in each year
  shareOfBaselineA5G2 <- 1 - reductionA5G2 / 100
  # Step 2: how large the baseline is compared with the common reference year
  baselineToRefA5G2 <- baselineA5G2 / ref
  fractionA5G2 <- shareOfBaselineA5G2 * baselineToRefA5G2
  # Above 1 means a cap looser than today's emissions, which is no reason to increase them. NA means no emissions in
  # the reference year, so there is nothing to reduce
  fractionA5G2[is.na(fractionA5G2) | fractionA5G2 > 1] <- 1

  # Every country takes the fraction of its own group
  fraction <- isNonA5 * fractionNonA5 + isNonA5Special * fractionNonA5Special +
    isA5G1 * fractionA5G1 + isA5G2 * fractionA5G2 + isNoGroup * 1
  getNames(fraction) <- "fraction_hfc_I"
  getSets(fraction) <- c("region", "year", "data")

  # The fractions are shares, so they cannot simply be added up when countries are combined into regions. calcOutput
  # takes a weighted average instead, and this is the weight: each country counts in proportion to its emissions in
  # the reference year. A region's fraction is then its total allowed emissions divided by its total reference-year
  # emissions, and a large emitter like China counts far more than a small island state
  weight <- magpie_expand(ref, fraction)
  getNames(weight) <- "fraction_hfc_I"
  getSets(weight) <- getSets(fraction)

  list(x = fraction, weight = weight, unit = "1",
       description = "Fraction of the 2020 Annex F Group I HFC emissions remaining under the Kigali schedules",
       isocountries = TRUE)
}
