.onLoad <- function(libname, pkgname) {
  madrat::madratAttach(c(pkgname, "edgeTransport", "GDPuc", "mrcommons", "mrdrivers"))
  madrat::setConfig(
    nolabels = c(madrat::getConfig("nolabels"), "REMIND", "VALIDATIONREMIND")
  )
}


.onUnload <- function(libpath) {
  madrat::madratDetach(c(libpath, "edgeTransport", "GDPuc", "mrcommons", "mrdrivers"))
}

# redirect standard messaging functions to vcat
cat <- function(...) vcat(1, ...)
message <- function(...) vcat(1, ...)
warning <- function(...) vcat(0, ...)
stop <- function(...) vcat(-1, ...)
