#' @importFrom madrat vcat

.onAttach <- function(libname, pkgname) {
  madrat::madratAttach(pkgname)
}

.onDetach <- function(libpath) {
  madrat::madratDetach(libpath)
}

# redirect standard messaging functions to vcat
cat     <- function(...) vcat(1, ...)
message <- function(...) vcat(1, ...)
warning <- function(...) vcat(0, ...)
stop    <- function(...) vcat(-1, ...)
