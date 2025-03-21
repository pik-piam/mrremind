#' Read emission factors from a PBL IMAGE baseline (no mitigation) scenario
#' for a specific sector and gas species (CH4 or N2O)
#'
#' @md
#' @param subtype gas and subsector combination string. One of: 
#' c("CH4_entf", "CH4_gasp", "CH4_landf", "CH4_manu", "CH4_oilp", 
#'   "CH4_rice", "CH4_sewa", "N2O_adip", "N2O_fert", "N2O_manu", 
#'   "N2O_nitr", "N2O_sewa", "N2O tran")
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#'
#' @export
readPBL_EFsBaseline <- function(subtype) {

    # IMAGE regions in the order they appear in the Excel file
    # For some reason, the mapping in regionmapping_IMAGE_PBL_Stegmann2022.csv has
    # "Ukraine region" instead of "Ukraine", so we use the same here
    IMAGE_regions <- c(
        "Canada",
        "USA",
        "Mexico",
        "Central America",
        "Brazil",
        "Rest of South-America",
        "North Africa",
        "West Africa",
        "East Africa",
        "South Africa",
        "West Europe",
        "Central Europe",
        "Turkey",
        "Ukraine region",
        "Kazachstan",
        "Russia",
        "Middle East",
        "India",
        "Korea",
        "China+",
        "South East Asia",
        "Indonesia",
        "Japan",
        "Oceania",
        "Rest of South Asia",
        "Rest of South Africa"
    )

    intable <- read_excel("NonCO2_Emission_Factors_&_SSP2-dependent_Pessimistic_MAC_IMAGE.xlsx", sheet = paste0("EF_",subtype), skip = 2)
    colnames(intable) <- c("year", IMAGE_regions)

    x <- as.magpie(tidyr::pivot_longer(intable, cols = -1, names_to = "region", values_to = "value"), spatial = "region")

    # Add the subtype in the third, singleton dimension
    names(dimnames(x))[3] <- "SRC"
    getItems(x, dim = 3) <- subtype

    return(x)
}
