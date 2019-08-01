#' calcEORA
#'
#' @description harmonizes the sectors of EORA by calculation, aggregation and disaggregation
#' @param subtype - year (1990-2013)
#' @return list of MAgPIE object of the harmonized EORA data, weight, unit, description and as note a vector
#' consisting of the three lost values
#' @author Debbora Leip
#' @importFrom data.table := fread dcast as.data.table
#' @importClassesFrom data.table data.table
#' @importFrom plyr round_any
#' @note \strong{Some information about the EORA database in general:}
#' The database is a multi-region input-output table (MRIO) database. 187 individual countries represented 
#' by a total of 15,909 sectors. This makes the database detailed but also difficult to handle, as the sectoral
#' division isn't the same for the different countries. Furthermore, sectors
#' can be either Commodities or Industries, but some countries even report both.
#' 
#' \strong{What the calcEORA function does:}
#' The aim was to harmonize the sectors of the EORA database and map them to magpie categories. We did this in
#' 3 main steps: removing the distinction between Commodities and Industries (see \code{\link{calcCommoditiesEORA}}
#' for an explanation on how to do that), aggregating the sectors and finally disaggregating them to magpie categories
#' using the FAO production times IniFoodPrice.
#' The last two steps are done for each input country (if value flows from country A to country B, the inputcountry would
#' be A according to my use of this word...) individually and the resulting magpie objects are then bound toghether. 
#' (This means that it should be easy to parallelize that part of the function, if you have some spare time 
#' feel free to do so!)
#' 
#' At three points in this function the total value of the table is reduced: calcCommoditiesEORA looses some
#' value due to the rounding in \code{\link{readEORA}} and removes some value intentionally (see 
#' \code{\link{calcCommoditiesEORA}} for an explanation) and we have to remove ANT, USR and ROW as 
#' inputcountries as they don't exist in the magpie-countries. Those three lost values are returned in the
#' note part of the returned list, in order to be able to compare the original value of the 
#' table with the value of the result to check if everything went right. Sometimes there will still be a difference
#' of a (small) multiple of 16, this occurs if at one point the readSource function loads the EORA table from the cache
#' and at one point really reads it in. (If you find out why exactly this makes a difference, please tell me!)
#' 
#' \strong{Structure of returned MAgPIE object:} the 1st dimension is the outputcountry, 2nd is as always the year and the 3rd is divided in three: 
#' the input country, the input sector and the output sector.
#' 
#' @seealso \code{\link{calcCommoditiesEORA}}, \code{\link{readEORA}}


calcEORA <-function(subtype){
  
  # defining subfunctions
  aggEORA_inout <- function(x, frame, output_country, input_country){
    rownames_main <- fread(toolMappingFile("sectoral","rownamesEORA_main.csv"))
    rownames_country <- rownames_main[rownames_main[["CountryA3"]]==output_country,]
    entities <- levels(as.factor(rownames_country[["Entity"]]))
    rm(rownames_main)
    rm(rownames_country)
    
    categorisation <- fread(toolMappingFile("sectoral","categoriesEORA_output.csv"))
    categories <- unique(categorisation[["Category"]])
    
    for(k in categories){
      sectors <- categorisation[categorisation[["Category"]]==k &
                                  categorisation[["CountryA3"]]==output_country,]
      sectors <- sectors[["Sector"]]
      sectors <- sectors[sectors%in%getNames(x, dim=3)]
      if(length(sectors)==0){next()}
      sectors <- paste0("[.]", sectors,"$")
      sectors <- gsub("\\(", "\\\\(", sectors)
      sectors <- gsub("\\)", "\\\\)", sectors)
      sectors <- gsub("\\*","\\\\*", sectors)
      sectors <- gsub("\\+", "\\\\+", sectors)
      relevant <- x[,,getNames(x)[grep(paste(sectors, collapse="|"),getNames(x))]]
      relevant <- dimSums(relevant, dim=3.3)
      getNames(relevant) <- paste(getNames(relevant), k, sep=".")
      frame[,,getNames(relevant)[grep(paste0("[.]", k,"$"),getNames(relevant))]] <-
        relevant[,,getNames(relevant)[grep(paste0("[.]", k,"$"),getNames(relevant))]]
    }
    return(frame)
  }
  aggEORA_input <- function(x, input_country, countries, year, counter){
    vcat(verbosity = 2, paste0("Aggregating input country ", input_country, " (", counter, ")"))
    
    # getting categorisation 
    categorisation <- fread(toolMappingFile("sectoral","categoriesEORA_input.csv"))
    categories_input <- unique(categorisation[["Category"]])
    
    # creating empty magpie to fill
    output <- getNames(x, dim=3)
    input <- paste(input_country, categories_input, sep=".")
    combinations <- levels(interaction(input, output))
    empt <- new.magpie(countries, year, combinations, fill=0)
    rm(output)
    rm(input)
    rm(combinations)
    
    # for each categorie we add up the corresponding sectors (and delete them in x)
    for(k in categories_input){
      vcat(verbosity = 2, k)
      sectors <- categorisation[categorisation[["Category"]]==k &
                                  categorisation[["CountryA3"]]==input_country,]
      sectors <- sectors[["Sector"]]
      sectors <- sectors[sectors%in%getNames(x, dim=2)]
      if(length(sectors)==0){next()}
      relevant <- x[,,paste(input_country, sectors, sep=".")]
      relevant <- dimSums(relevant, dim=3.2)
      names <- do.call(rbind, strsplit(getNames(relevant),"[.]"))
      getNames(relevant) <- paste(names[,1], k, names[,2], sep=".")
      empt[,,getNames(relevant[,,paste(input_country, k, sep=".")])] <-
        relevant[,,paste(input_country, k, sep=".")]
    }
    return(empt)
  }
  aggEORA_outputs <- function(x, input_country, countries, year, counter){
    
    # getting categorisation
    categorisation_input <- fread(toolMappingFile("sectoral","categoriesEORA_input.csv"))
    categories_input <- unique(categorisation_input[["Category"]])
    rm(categorisation_input)
    categorisation_output <- fread(toolMappingFile("sectoral","categoriesEORA_output.csv"))
    categories_output <- unique(categorisation_output[["Category"]])
    rm(categorisation_output)
    
    # creating empty magpie to mbind with the calculated
    input <- paste(input_country, categories_input, sep=".")
    combinations <- levels(interaction(input, categories_output))
    empt <- new.magpie("bla", year, combinations, fill=0)
    rm(input)
    
    tmp <- function(x) return(all(x==0))
    
    
    for(c in countries){
      vcat(verbosity=2, paste0(" Aggregating outputcountry ", c, " for inputcountry ", as.character(input_country), " (", counter, ")"))
      loop <- x[c,,]
      empt_loop <- new.magpie(c, year, combinations, fill=0)
      
      # kick out empty columns
      all_zeros <- apply(loop, 3, tmp)
      
      # if nothing's left there's nothing to do
      if(sum(!all_zeros)==0){
        vcat(verbosity = 2, "\t no data")
        empt <- mbind(empt, empt_loop)
        next()
      }
      
      all_zeros <- which(!all_zeros)
      loop <- loop[,,all_zeros]
      
      # but if there is something left, we have to aggregate it!
      empt_loop <- aggEORA_inout(loop, empt_loop, c, input_country)
      empt <- mbind(empt, empt_loop)
    }
    empt <- empt["bla",,invert=TRUE]
    return(empt)
  }
  disaggEORA_inout <- function(x, frame, input_country, output_country, year, data){
    # get disaggregation info
    disaggregation_output <- fread(toolMappingFile("sectoral", "disaggregationEORA_output.csv"))
    magpie_cats_output <- disaggregation_output[["Step1"]] 
    
    
    # aggregated input-categories of this country
    categorisation_output <- fread(toolMappingFile("sectoral", "categoriesEORA_output.csv"))
    categories <- categorisation_output[categorisation_output[["CountryA3"]]==output_country,]
    categories <- unique(categories[["Category"]])
    rm(categorisation_output)
    
    done <- c()
    
    # fill first level into frame (no disaggregation needed here)
    for(l in categories[categories %in% magpie_cats_output]){
      frame[,,getNames(x)[grep(paste0("[.]", l,"$"), getNames(x))]] <- x[,,getNames(x)[grep(paste0("[.]", l,"$"), getNames(x))]]
      done <- c(done, l)
      x <- x[,,getNames(x)[grep(paste0("[.]",l,"$"), getNames(x))], invert=TRUE]
      categories <- categories[categories!=l]
    }
    
    # delete the other columns of first level, which exist because of magpie structure, but are not relevant for this country
    names <- getNames(x, dim=3)
    for(n in names[names %in% magpie_cats_output]){
      x <- x[,,getNames(x)[grep(paste0("[.]",n, "$"), getNames(x))], invert=TRUE]
    }
    
    # fill second level disaggregation into frame
    for(l in categories[categories %in% disaggregation_output[["Step2"]]]){
      sub <- disaggregation_output[disaggregation_output[["Step2"]]==l, ]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation_output[disaggregation_output[["Step2"]]==l,]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        mult <- const(sub, s, data, year, output_country) 
        getNames(x_loop, dim=3)[getNames(x_loop, dim=3)==l] <- s
        frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] <-  
          frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] +
          (mult * x_loop[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]])
      }
      x <- x[,,getNames(x)[grep(paste0("[.]",l,"$"), getNames(x))], invert=TRUE]
      categories <- categories[categories!=l]
      done <- unique(c(done, sub))
    }
    
    # fill third level disaggregation into frame
    for(l in categories[categories %in% disaggregation_output[["Step3"]]]){
      sub <- disaggregation_output[disaggregation_output[["Step3"]]==l, ]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation_output[disaggregation_output[["Step3"]]==l,]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        mult <- const(sub, s, data, year, output_country) 
        getNames(x_loop, dim=3)[getNames(x_loop, dim=3)==l] <- s
        frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] <-  
          frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] +
          (mult * x_loop[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]])
      }
      x <- x[,,getNames(x)[grep(paste0("[.]", l,"$"), getNames(x))], invert=TRUE]
      categories <- categories[categories!=l]
      done <- unique(c(done, sub))
    }
    
    # fill fourth level disaggregation into frame
    for(l in categories[categories %in% disaggregation_output[["Step4"]]]){
      sub <- disaggregation_output[disaggregation_output[["Step4"]]==l,]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation_output[disaggregation_output[["Step4"]]=="agriculture",]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        mult <- const(sub, s, data, year, output_country) 
        getNames(x_loop, dim=3)[getNames(x_loop, dim=3)==l] <- s
        frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] <- 
          frame[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]] + 
          (mult * x_loop[,,getNames(x_loop)[grep(paste0("[.]", s,"$"), getNames(x_loop))]])
      }
      x <- x[,,getNames(x)[grep(paste0("[.]", l,"$"), getNames(x))], invert=TRUE]
      categories <- categories[categories!=l]
      done <- unique(c(done, sub))
    }
    
    magpie_cats_wo_fd <- magpie_cats_output[-grep("^Final Demand", magpie_cats_output)]
    vcat(verbosity = 2, paste0(" missing categories: ", paste(magpie_cats_wo_fd[!(magpie_cats_wo_fd %in% done)],collapse=", ")))
    return(frame)
  }
  disaggEORA_input <- function(x, input_country, countries, year, data, counter){
    vcat(verbosity = 2, paste0("Disaggregating input country ", input_country, " (", counter, ")"))
    
    # getting disaggregation info and create empty magpie to fill
    disaggregation <- fread(toolMappingFile("sectoral", "disaggregationEORA_input.csv"))
    magpie_cats <- disaggregation[["Step1"]]
    newcombinations <- levels(interaction(input_country, magpie_cats, getNames(x, dim=3)))
    
    frame <- new.magpie(countries, year, newcombinations, fill=0)
    rm(newcombinations)
    
    # aggregated input-categories of this country
    categorisation_input <- fread(toolMappingFile("sectoral", "categoriesEORA_input.csv"))
    categories <- categorisation_input[categorisation_input[["CountryA3"]]==input_country,]
    categories <- unique(categories[["Category"]])
    rm(categorisation_input)
    
    done <- c()
    
    # fill first level into frame (no disaggregation needed here)
    for(l in categories[categories %in% magpie_cats]){
      frame[,,getNames(x[,,paste(input_country, l, sep=".")])] <- x[,,paste(input_country, l, sep=".")]
      done <- c(done, l)
      x <- x[,,getNames(x[,,paste(input_country, l, sep=".")]), invert=TRUE]
    }
    
    # delete the other columns of first level, which exist because of magpie structure, but are not relevant for this country
    names <- getNames(x, dim=2)
    for(n in names[names %in% magpie_cats]){
      x <- x[,,paste(input_country, n, sep="."), invert=TRUE]
    }
    
    # fill second level disaggregation into frame
    for(l in categories[categories %in% disaggregation[["Step2"]]]){
      sub <- disaggregation[disaggregation[["Step2"]]==l,]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation[disaggregation[["Step2"]]==l,]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        getNames(x_loop, dim=2)[getNames(x_loop, dim=2)==l] <- s
        mult <- const(sub, s, data, year, input_country)
        frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] <- 
          frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] + 
          (mult * x_loop[,,paste(input_country, s, sep=".")])
      }
      x <- x[,,getNames(x[,,paste(input_country, l, sep=".")]), invert=TRUE]
      done <- unique(c(done, sub))
    }
    
    # fill third level disaggregation into frame
    for(l in categories[categories %in% disaggregation[["Step3"]]]){
      sub <- disaggregation[disaggregation[["Step3"]]==l, ]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation[disaggregation[["Step3"]]==l,]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        getNames(x_loop, dim=2)[getNames(x_loop, dim=2)==l] <- s
        mult <- const(sub, s, data, year, input_country) 
        frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] <- 
          frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] + 
          (mult * x_loop[,,paste(input_country, s, sep=".")])
      }
      x <- x[,,getNames(x[,,paste(input_country, l, sep=".")]), invert=TRUE]
      done <- unique(c(done, sub))
    }
    
    # fill fourth level disaggregation into frame
    for(l in categories[categories %in% disaggregation[["Step4"]]]){
      sub <- disaggregation[disaggregation[["Step4"]]==l,]
      sub <- sub[["Step1"]]
      sub <- sub[!(sub %in% done)]
      if(length(sub)==0){
        sub <- disaggregation[disaggregation[["Step4"]]=="agriculture",]
        sub <- sub[["Step1"]]
      }
      for(s in sub){
        x_loop <- x
        getNames(x_loop, dim=2)[getNames(x_loop, dim=2)==l] <- s
        mult <- const(sub, s, data, year, input_country)
        frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] <- 
          frame[,,getNames(x_loop[,,paste(input_country, s, sep=".")])] +
          (mult * x_loop[,,paste(input_country, s, sep=".")])
      }
      x <- x[,,getNames(x[,,paste(input_country, l, sep=".")]), invert=TRUE]
      done <- unique(c(done, sub))
    }
    
    vcat(verbosity = 2, paste("missing categories:", paste(magpie_cats[!(magpie_cats %in% done)],collapse=", ")))
    return(frame)
    
  }
  disaggEORA_outputs <- function(x, input_country, countries, year, data, counter){
    
    # getting disaggregation info and create empty magpie to mbind
    disaggregation_input <- fread(toolMappingFile("sectoral", "disaggregationEORA_input.csv"))
    magpie_cats_input <- disaggregation_input[["Step1"]] 
    disaggregation_output <- fread(toolMappingFile("sectoral", "disaggregationEORA_output.csv"))
    magpie_cats_output <- disaggregation_output[["Step1"]] 
    combinations <- levels(interaction(input_country, magpie_cats_input, magpie_cats_output))
    empt <- new.magpie("bla", year, combinations, fill=0)
    rm(disaggregation_input)
    rm(disaggregation_output)
    rm(magpie_cats_input)
    rm(magpie_cats_output)
    
    tmp <- function(x) return(all(x==0))
    
    for(c in countries){
      vcat(verbosity = 2, paste0(" Disaggregating outputcountry ", c, " for inputcountry ", as.character(input_country), " (", counter, ")"))
      x_loop <- x[c,,]
      frame <- new.magpie(c, year, combinations, fill=0)
      
      all_zero <- apply(x_loop, 3, tmp)
      
      if(sum(!all_zero)!=0){
        frame <- disaggEORA_inout(x_loop, frame, input_country, c, year, data)
      }
      empt <- mbind(empt, frame)
    }
    empt <- empt["bla",,,invert=TRUE]
    return(empt)
  }
  const <- function(sub, s, data, year, country){
    sum <- dimSums(data[country,,sub],dim=c(3))
    if(as.numeric(sum)==0){
      return(0)
    }
    part <- data[country,, s]
    share <- part/sum
    share <- as.numeric(share)
    return(share)
  }
  
  
  ptm_begin <- proc.time()
  
  # getting rid of distinction between commodities and industries
  vcat(verbosity = 2, " calcCommoditiesEORA\n")
  y <- calcOutput("CommoditiesEORA", subtype=subtype, aggregate = FALSE, supplementary = TRUE)
  x <- y[[1]]
  lost_calc <- y[[5]][1]
  removed_calc <- y[[5]][2]
  vcat(verbosity = 2, "\n duration calcCommodietiesEORA",paste(proc.time() - ptm_begin))
  ptm_begin <- proc.time()
  
  vcat(verbosity = 2, "\n\n\n starting aggregation/disaggregation")
  

  countries <- getRegions(x)
  year <- subtype

  # create empty magpie out
  out <- new.magpie(countries, year, "a.b.c", fill=0)
  
  countries_input <- getNames(x, dim=1)
  countries_input <- countries_input[countries_input%in%countries]
  
  removing <- c("ROW", "ANT", "USR")
  removing <- removing[removing%in%getNames(x, dim=1)]
  lost_row_ant_usr <- sum(x[,,removing])

  # get data for disaggregation (production from FAO times IniFoodPrice)
  data <- calcOutput("FAOmassbalance",aggregate = F)[,,"production"][,,"dm"]
  data <- time_interpolate(data, year)
  neededcommodities <- c("maiz", "rice_pro", "tece", "trce", "groundnut", "oilpalm", "rapeseed", "soybean", "sunflower", "cassav_sp",
                         "cottn_pro", "foddr", "potato", "puls_pro", "others", "sugr_beet", "sugr_cane", "livst_chick", "livst_egg",
                         "livst_milk", "livst_pig", "livst_rum")
  prices<-calcOutput("IniFoodPrice", aggregate=FALSE)
  data <- collapseNames(data)
  data <- data[,,neededcommodities]
  prices <- prices[,,neededcommodities]
  data <- data*prices
  data <- data+10^(-20)
  
  counter <- 0
  
  
  for(i in countries_input){
    ptm <- proc.time()
    counter <- counter+1
    vcat(verbosity = 2, paste0("\nInputcountry ", i, " (",counter,")"))
    
    x_loop <- x[,,i]

    # aggregation
    x_loop <- aggEORA_input(x_loop, i, countries, year, counter)
    x_loop <- aggEORA_outputs(x_loop, i, countries, year, counter)

    # disaggregation
    x_loop <- disaggEORA_input(x_loop, i, countries, year, data, counter)
    x_loop <- disaggEORA_outputs(x_loop, i, countries, year, data, counter)


    out <- mbind(out, x_loop)

    gc()
    vcat(verbosity = 2, " duration loop",paste(proc.time() - ptm))
  }
  out <- out[,,"a.b.c", invert=TRUE]
  vcat(verbosity = 2, "\n duration calcEORA (rest)",paste(proc.time() - ptm_begin))
  vcat(verbosity = 1, "\n value lost due to calc_commodities:", lost_calc)
  vcat(verbosity = 1, "\n value removed during calc_commodities:", removed_calc)
  vcat(verbosity = 1, "\n value lost due to ROW and ANT (and sometimes USR):", lost_row_ant_usr)

  return(list(x=out,
              weight=NULL,
              unit="1000USD",
              description="EORA data",
              note=c(lost_calc, removed_calc, lost_row_ant_usr)))
}