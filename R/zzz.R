.onLoad <- function(libname, pkgname) {
  madrat::setConfig(nolabels = c(madrat::getConfig("nolabels"), "REMIND"), .cfgchecks = FALSE, .verbose = FALSE)
}
