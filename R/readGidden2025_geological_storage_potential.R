#' Read geological storage potential
#'
#' @return A magpie object with geological storage potential. Off = offshore; On = onshore; TechPot = technical potential
#' without any exclusion layers applied; PlanetLim = applying all exclusion layers described in Table S1 (e.g. protected
#' areas, population centers, max and min depth, etc.)
#' @author David Klein
#'
readGidden2025_geological_storage_potential <- function() {
  intable <- readxl::read_excel("gidden_et_al_2025_supplemental_data.xlsx", sheet = "S5", skip = 1)

  intable <- intable[-1 , -c(2, 3, 6, 9, 10, 11, 12)]
  colnames(intable) <- c("regi","potTechOff", "potTechOn", "potLimOff", "potLimOn")

  # 194 Total
  # 195 Non Mapped Countries

  # 230 Total
  # 231 Final Total

  # remove rows with no regional data listed above
  intable <- intable[-c(194, 195, 230, 231),]

  longtable <- tidyr::pivot_longer(intable,
                                   cols = tidyselect::contains(c("Tech","Lim")),
                                   names_to = "category",
                                   values_to = "value")

  as.magpie(longtable)
}
