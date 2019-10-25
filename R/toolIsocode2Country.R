#' toolIsocode2Country
#' 
#' Translate iso country code to country names
#' @param x Array of iso country codes
#' @return return array of country names
#' @author Kristine Karstens
#' 
#' @export


toolIsocode2Country <- function(x){
  
  iso2Country <- toolGetMapping("iso_country.csv",where="moinput")
  noIso       <- which(!(x%in%iso2Country[,2]))
  
  if(length(noIso) != 0) {
    cat("Following items are no iso country code: ", x[noIso],". They will be returned unmodifed.\n")
    noMod <- x[noIso]
    toMod <- x[-noIso]

  } else {
    noMod <- NULL
    toMod <- x
  }

  Mod <- iso2Country[match(toMod,iso2Country[,2]),1]

  for(i in noIso){
    Mod <- append(Mod, x[i], i-1)
  }
  
  if(length(x)==length(Mod)) return(Mod)
  else stop("Something went wrong.")
}
