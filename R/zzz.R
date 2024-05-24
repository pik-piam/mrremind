.onLoad <- function(libname, pkgname) {
  madrat::setConfig(
    nolabels = c(madrat::getConfig("nolabels"), "REMIND", "VALIDATIONREMIND"),
    .cfgchecks = FALSE, .verbose = FALSE
  )
}
