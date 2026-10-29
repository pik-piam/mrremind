#' Calculate Capacity Targets
#'
#' @description Capacity targets (GW) at regional level from the NewClimate NPI policy database.
#'
#' @author Aman Malik, Oliver Richters, Rahel Mandaroux, Léa Hayez
#'
calcCapTarget <- function() {

  listCapacities <- list(
    "2024_cond"   = readSource("NewClimate", subtype = "Capacity_2025_cond"),
    "2024_uncond" = readSource("NewClimate", subtype = "Capacity_2025_uncond")
  )

  # ensure that all magclass objects in the list have matching years so they can be bound together
  listYears <- lapply(listCapacities, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  capacities <- purrr::map(listCapacities,
                    ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2))
  capacities <- mbind(capacities)

  capacities <- capacities[, sort(getYears(capacities)), ]
  capacities[is.na(capacities)] <- 0

  return(list(x = capacities,
              weight = NULL,
              unit = "GW",
              description = "Capacity targets combined from NewClimate Database for Current Policy Scenarios")
  )
}
