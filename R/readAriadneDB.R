#' Ariadne database scenario data
#' @description  Scenario data from the Ariadne modeling intercomparison project for Germany.
#' See README in input file for more details.
#'
#' @return A [`magpie`][magclass::magclass] object.
#' @author Felix Schreyer
#' @importFrom tidyr gather
#' @importFrom rlang sym
#' @importFrom dplyr filter %>% mutate
#' @importFrom readxl read_excel

readAriadneDB <- function() {
  filename <- "IASA_DB_Complete_12_09_2022.xlsx"

  # read in file
  data <- read_excel(filename,
    sheet = "data",
    col_types = c(
      rep("text", 6),
      rep("numeric", 19)
    )
  )

  # rearrange and convert to magclass object
  out <- data %>%
    gather(!!sym("period"), !!sym("value"), -!!sym("model"), -!!sym("scenario"),
           -!!sym("region"), -!!sym("variable"), -!!sym("unit"), -!!sym("subannual")) %>%
    filter(!is.na(!!sym("value"))) %>%
    as.magpie(temporal = 7, spatial = 3, datacol = 8)

  return(out)
}
