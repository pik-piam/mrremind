#' Read PlasticsEoL
#'
#' Read-in data for the End-of-Life fate of plastics
#' from 1.Stegmann, P., Daioglou, V., Londo, M., van Vuuren,
#' D. P. & Junginger, M. Plastic futures and their CO2 emissions.
#' Nature 612, 272–276 (2022).
#' https://www.nature.com/articles/s41586-022-05422-5
#' Link to SI:
#' https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-022-05422-5/MediaObjects/41586_2022_5422_MOESM1_ESM.xlsx #nolint
#'
#' @md
#' @return magpie object of the data
#' @author Falk Benke, Simón Moreno
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Stegmann2022")
#' }
#'
#' @importFrom readxl read_xlsx

readStegmann2022 <- function() {

  data <- read_xlsx("41586_2022_5422_MOESM1_ESM.xlsx", sheet = "Data")

  reshape2::melt(data, id.vars = seq(1, 5), variable.name = "period") %>%
    as.magpie(spatial = 3)
}
