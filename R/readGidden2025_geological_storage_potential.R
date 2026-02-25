#' Read geological storage potential
#'
#' @return A magpie object with geological storage potential. Off = offshore; On = onshore; potTech = technical potential
#' without any exclusion layers applied; potLim = applying all exclusion layers described in Table S1 (e.g. protected
#' areas, population centers, max and min depth, etc.)
#' @author David Klein
#'
readGidden2025_geological_storage_potential <- function() {

  intable <- readxl::read_excel("gidden_et_al_2025_supplemental_data.xlsx", sheet = "S5", skip = 1) |>
             dplyr::select(c("regi"       = "...1",         # select relevant columns and rename them
                             "potTechOff" = "Offshore...4",
                             "potTechOn"  = "Onshore...5",
                             "potLimOff"  = "Offshore...7",
                             "potLimOn"   = "Onshore...8"))

  # remove rows with no regional data
  #   1 NODE
  # 195 Total
  # 196 Non Mapped Countries
  # 231 Total
  # 232 Final Total

  intable <- intable[-c(1, 195, 196, 231, 232),]

  longtable <- tidyr::pivot_longer(intable,
                                   cols = tidyselect::contains(c("Tech","Lim")),
                                   names_to = "category",
                                   values_to = "value")

  as.magpie(longtable)
}
