#' @title readCRU
#' @description Read CRU content
#' @param subtype Switch between diffrent input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readLPJmL}},
#' \code{\link{read.LPJ_input}}
#' @examples
#'
#' \dontrun{
#'   readSource("CRU", subtype="precipitation")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input
#' @importFrom ncdf4 nc_open ncvar_get

readCRU <- function(subtype="precipitation"){

  types  <- c(precipitation   = "pre",
              temperature     = "tmp",
              potential_evap  = "pet")

  type   <- toolSubtypeSelect(subtype,types)
  folder <- "CRU4p02/"
  file   <- grep(type, list.files(folder), value=TRUE)
  years  <- as.numeric(unlist(regmatches(file, gregexpr("\\d{4}",file))))
  format <- unlist(regmatches(file, gregexpr("\\.[a-z]*$",file)))

  print(file)
  print(years)

  if(format == ".clm"){

    x    <- read.LPJ_input(file_name=paste0(folder,file), out_years=paste0("y",years[1]:years[2]))
    x    <- collapseNames(as.magpie(x))
    getNames(x) <- c("jan","feb","mar","apr","mai","jun","jul","aug","sep","oct","nov","dec")

  } else if(format == ".nc"){

    nc.file <- ncdf4::nc_open(paste0(folder,file))
    nc.data <- ncdf4::ncvar_get(nc.file, type)
    nc.lon  <- ncdf4::ncvar_get(nc.file,"lon")
    nc.lat  <- ncdf4::ncvar_get(nc.file,"lat")

    #Load celliso names for 1:59199 magpie cells
    mapping   <- toolMappingFile(type="cell",name="CountryToCellMapping.csv", readcsv=TRUE)
    cellNames <- mapping$celliso
    ndays     <- clean_magpie(as.magpie(c( jan=31, feb=28, mar=31, apr=30, mai=31, jun=30, jul=31, aug=31, sep=30, oct=31, nov=30, dec=31)))
    years     <- seq(years[1],years[2],1)
    x         <- as.array(new.magpie(cellNames, years, getNames(ndays) ,fill=NA))

    for (j in 1:59199) {
      ilon <- which(magpie_coord[j,1]==nc.lon)
      ilat <- which(magpie_coord[j,2]==nc.lat)
      for(a in 1:length(years)){
        x[j,a,] <- nc.data[ilon, ilat, (a-1)*12+(1:12)]
      }
    }

    # multiply number of daye per month to get monthly data
    x <- as.magpie(x)*ndays
  }

  return(x)
}
