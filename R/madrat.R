.onLoad <- function(libname, pkgname) {
  madrat::madratAttach(c(pkgname, "edgeTransport", "GDPuc", "mrcommons", "mrdrivers", "mrindustry"))
  madrat::setConfig(
    nolabels = c("REMIND", "VALIDATIONREMIND"),
    .cfgchecks = FALSE, .verbose = FALSE
  )
}


.onUnload <- function(libpath) {
  madrat::madratDetach(c(libpath, "edgeTransport", "GDPuc", "mrcommons", "mrdrivers", "mrindustry"))
}

# redirect standard messaging functions to vcat
cat <- function(...) vcat(1, ...)
message <- function(...) vcat(1, ...)
warning <- function(...) vcat(0, ...)
stop <- function(...) vcat(-1, ...)
