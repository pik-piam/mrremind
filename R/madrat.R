#' @importFrom madrat vcat

.onLoad <- function(libname, pkgname){
  madrat::setConfig(packages=c(madrat::getConfig("packages"),pkgname), .cfgchecks=FALSE, .verbose=FALSE)
}

#create an own warning function which redirects calls to vcat (package internal)
warning <- function(...) vcat(0,...)

# create a own stop function which redirects calls to stop (package internal)
stop <- function(...) vcat(-1,...)

# create an own cat function which redirects calls to cat (package internal)
cat <- function(...) vcat(1,...)