#' Read UNFCCC data
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble tibble
#' @export
#'
readUNFCCC <- function() {

  # structural definition of the source ----
  # nolint start
  sheets <- list(
    "Table1" = list(
      type = "identifier",
      cols = c(1:8),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O", "NOX", "CO", "NMVOC", "SO2"))
    ),
    "Table1.A(a)s1" = list(
      type = "identifier",
      cols = c(1, 7, 8, 9),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Table1.A(a)s2" = list(
      type = "identifier",
      cols = c(1, 7, 8, 9),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Table1.A(b)" = list(
      type = "range",
      range = "T11:T29",
      colnames = "kt CO2",
      rows = tibble(
        name = {
          c(
            "Fuel Types|Liquid fossil|Primary fuels|Crude oil",
            "Fuel Types|Liquid fossil|Primary fuels|Orimulsion",
            "Fuel Types|Liquid fossil|Primary fuels|Natural gas liquids",
            NA,
            "Fuel Types|Liquid fossil|Secondary fuels|Gasoline",
            "Fuel Types|Liquid fossil|Secondary fuels|Jet kerosene",
            "Fuel Types|Liquid fossil|Secondary fuels|Other kerosene",
            "Fuel Types|Liquid fossil|Secondary fuels|Shale oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Gas/diesel oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Residual fuel oil",
            "Fuel Types|Liquid fossil|Secondary fuels|Liquefied petroleum gases (LPG)",
            "Fuel Types|Liquid fossil|Secondary fuels|Ethane",
            "Fuel Types|Liquid fossil|Secondary fuels|Naphtha",
            "Fuel Types|Liquid fossil|Secondary fuels|Bitumen",
            "Fuel Types|Liquid fossil|Secondary fuels|Lubricants",
            "Fuel Types|Liquid fossil|Secondary fuels|Petroleum coke",
            "Fuel Types|Liquid fossil|Secondary fuels|Refinery feedstocks",
            "Fuel Types|Liquid fossil|Secondary fuels|Other oil",
            "Fuel Types|Other liquid fossil"
          )
        }
      )
    ),
    "Table2(I)" = list(
      type = "identifier",
      cols = c(1, 2, 3, 4),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Table3" = list(
      type = "identifier",
      cols = c(1, 2, 3, 4),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Table4" = list(
      type = "identifier",
      cols = c(1, 2, 3, 4),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Table5" = list(
      type = "identifier",
      cols = c(1, 2, 3, 4),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    ),
    "Summary1" = list(
      type = "identifier",
      cols = c(1, 2, 3, 4),
      colnames = paste0("kt ", c("CO2", "CH4", "N2O"))
    )
  )
  # nolint end

  # parse directories ----

  dirs <- list.files(path = "2024")

  tmp <- NULL

  for (dir in dirs) {
    tmpCtry <- NULL

    files <- list.files(path = file.path(".", "2024", dir))
    region <- toupper(sub("\\-.*", "\\1", dir))

    message("Reading in ", region, "\n\n")

    for (file in files) {

      rx <- dplyr::case_when(
        region == "AUS" ~ "AUS_2024_([0-9]{4})_.*",
        region == "EUA" ~ "EUA_CRT_([0-9]{4}).*",
        .default = ".{3}-CRT-2024-V[0-9].[0-9]-([0-9]{4})-.*"
      )

      year <- suppressWarnings(as.integer(sub(rx, "\\1", file)))
      if (is.na(year)) {
        stop("No year found in filename ", file)
      }

      availableSheets <- readxl::excel_sheets(file.path(".", "2024", dir, file))

      if (length(intersect(names(sheets), availableSheets)) == 0) {
        stop("No matching sheets found in ", file)
      }

      for (i in seq_along(sheets)) {

        # read in data via range
        if (sheets[[i]][["type"]] == "range") {

          s <- suppressMessages(
            readxl::read_xlsx(
              path = file.path("2024", dir, file),
              sheet = names(sheets[i]),
              range = sheets[[i]][["range"]],
              col_names = sheets[[i]][["colnames"]]
            )
          )

          if (nrow(s) == 0 ){
            message("No data found in ", names(sheets[i]), " for ", region, " ", year, ".")
          }

          s <- suppressMessages(
            suppressWarnings(
              s %>%
                dplyr::bind_cols(sheets[[i]]$rows, year = year, region = region) %>%
                filter(!is.na(.data$name)) %>%
                pivot_longer(cols = dplyr::starts_with("kt "), names_to = "unit") %>%
                mutate(
                  "value" = suppressWarnings(as.double(.data$value)),
                  "variable" = paste0(sub("\\.", "_", names(sheets[i])),
                                      "|", .data$name, "|", sub(".+ ", "", .data$unit))
                ) %>%
                select("region", "year", "variable", "unit", "value") %>%
                filter(!is.na(.data$value))
            )
          )

          # read in data via identifiers (up to level 1.A.1.a.)
        } else {

          s <- suppressMessages(
            readxl::read_xlsx(path = file.path("2024", dir, file), sheet = names(sheets[i]))
          )

          # drop first column, when it is not the item colum
          if (all(is.na(s[, 1]))) {
            s <- s[, -1]
          }

          s <- s %>%
            select(sheets[[i]][["cols"]])

          colnames(s) <- c("variable", sheets[[i]][["colnames"]])

          s <- s %>%
            filter(grepl("[0-9]\\.([A-Z]\\.)?([0-9]\\.)?([a-z]\\.)? ", .data$variable)) %>%
            dplyr::bind_cols(year = year, region = region) %>%
            pivot_longer(cols = dplyr::starts_with("kt "), names_to = "unit") %>%
            mutate(
              "value" = suppressWarnings(as.double(.data$value)),
              # remove appended remarks in brackets from entry
              "variable" = sub("\\(.*\\) *$", "", .data$variable),
              "variable" = sub("  ", " ", .data$variable),
              "variable" = paste0(.data$variable, "|", sub("kt ", "", .data$unit))
            ) %>%
            filter(!is.na(.data$value)) %>%
            select("region", "year", "variable", "unit", "value")

        }

        tmpCtry <- bind_rows(tmpCtry, s)

        # duplicates can be introduced because of repetition in the sheets
        if (any(duplicated(tmpCtry))) {
          tmpCtry <- tmpCtry[-which(duplicated(tmpCtry[,-5])), ]
        }
      }
    }


    tmp <- bind_rows(tmp, tmpCtry)
  }

  tmp %>%
    as.magpie(tidy = TRUE) %>%
    return()
}
