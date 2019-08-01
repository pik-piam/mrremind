#' calcCommoditiesEORA
#' 
#' 
#' @description removes the distinction between commodities and industries in EORA
#' @param subtype - year (1990-2013)
#' @return a list consisting of a MAgPIE object of the EORA data, weight, unit, description and as note a vector of
#' the lost value and the removed value
#' @author Debbora Leip
#' @importFrom data.table := fread dcast
#' @importFrom magclass as.data.frame
#' @importClassesFrom data.table data.table
#' @note  For the countries that report both commodities and industries we want to translate the industries into 
#' commodities (as the commodities are more detailed). So here is what we do:
#' \tabular{ccccc}{
#' \tab other countries \tab Industries \tab Commodities \tab other countries \cr
#' other countries \tab x \tab A \tab C \tab x \cr
#' Industries \tab  \tab  \tab B \tab  \cr
#' Commodities \tab x \tab A \tab C \tab x \cr
#' other countries \tab x \tab A \tab C \tab x\cr
#' }
#' \emph{Here we focus on one country and its industries and commodities.  Values flow 
#' from the inputsectors, which are the rownames, into the outputsectors, which are the columnnames. 
#' Where nothing is written no numbers are in the table, in all other places every value is possible. 
#' The columns/rows "Commodities"/"Industries" stand for all commodities/industries of the country.}
#'  
#' We want to be able to remove part A and part B. To achieve this we redistribute the 
#' value of part B by adding it in a certain way onto part C, which is already sufficient to 
#' remove A as well as B (see below for reason). We use the informaiton of part A to decide how to 
#' redistribute the value, as it tells us from which sectors the value in 
#' the industries origanally came from. So we want to use part A to calculate the shares 
#' describing how much of each value should be added where. Mathematically this ist a matrix multiplication:
#' mat1 shall be the industrie columns but whith each entry divided by the sum of the corresponding 
#' column and mat2 shall be part B. Then mat1*mat2 gives us a matrix with the same rows as mat1 but the commodities 
#' as columns, which we can add onto the commoditie-columns.
#' 
#' Reason: This is the only step in which we want to reduce the value of the total table. Before this 
#' calculation, the value which is in part A column appears twice in the complete table, once as
#'  value flowing into the industries (A) and once as value flowing out of the industries (B). As we remove the step in
#'  which value flows through industries, after the calculation this value sholud appear only once. 
#'  Therefore we can remove both parts even though we redistribute only one of them.
#'  
#' To consider: We also reduce the total value unintentionally, because due to the rounding in readEORA, some industries 
#' exist as input but not as output (or vice versa) which means that we also have to remove it 
#' as input to be able to do the matrix multiplication. 
#' 
#' Both lost values are printed out in the end, in order to be able to compare the original value of the 
#' table with the value of the result to check if everything went right.
#' 
#' @seealso \code{\link{calcEORA}}



calcCommoditiesEORA <- function(subtype){
  # read in data
  x <- readSource("EORA", subtype = subtype)
  
  # some background information
  year <- subtype
  countries <- getRegions(x)
  inputcountries <- getNames(x, dim=1)
  inputcountries <- inputcountries[inputcountries%in%countries]
  
  rownames <- fread(toolMappingFile("sectoral","rownamesEORA_main.csv"))
  rownames[,"Sector" := gsub("[.]","_",rownames[["Sector"]]), with=FALSE]
  
  missingvalue <- 0
  removedvalue <- 0
  
  tmp <- function(x){return(all(x==0))} # 1 ist zeilen-dim, 2 ist spalten-dim
  
  count <- 0
  
  for(i in inputcountries){
    entities <- rownames[rownames[["CountryA3"]]==i,]
    entities <- entities[["Entity"]]
    if(length(unique(entities))==2){
      count <- count+1
      vcat(verbosity = 2, paste0("\n\n",i, " (", count, ") "))
      x_countryout <- x[i,,]
      x_countryin <- x[,,i]
      indnames <- rownames[rownames[["CountryA3"]]==i & rownames[["Entity"]]=="Industries",]
      indnames <- indnames[["Sector"]]
      indnames <- paste("Industries",indnames, sep="_")
      indnames[duplicated(indnames)] <- paste(indnames[duplicated(indnames)], 1, sep="_")
      vcat(verbosity = 2, " Length indnames:", length(indnames))
      commnames <- rownames[rownames[["CountryA3"]]==i & rownames[["Entity"]]=="Commodities",]
      commnames <- commnames[["Sector"]]
      commnames <- paste("Commodities", commnames, sep="_")
      commnames[duplicated(commnames)] <- paste(commnames[duplicated(commnames)], 1, sep="_")
      indnamesout <- indnames[indnames%in%getNames(x_countryout, dim=3)]
      vcat(verbosity = 2, " Length indnamesout:", length(indnamesout))
      indnamesin <- indnames[indnames%in%getNames(x_countryin, dim=2)]
      vcat(verbosity = 2, " Length indnamesin:", length(indnamesin))
      missingind <- indnames[!indnames%in%intersect(indnamesin, indnamesout)]
      indnames <- intersect(indnamesin, indnamesout)
      vcat(verbosity = 2, " Length new indnames:", length(indnames))
      commnamesout <- commnames[commnames%in%getNames(x_countryout[,,i], dim=3)]
      rm(commnames)
      
      # removed value
      removedvalue <- removedvalue + sum(mselect(x_countryout, Output=indnamesout))
      
      #calculate lost value
      missingindin <- indnamesin[!indnamesin%in%indnames]
      missingindout <- indnamesout[!indnamesout%in%indnames]
      missingvalue_loop <- sum(mselect(x_countryin, Input=missingindin))

      rm(missingindin)
      rm(missingindout)
      rm(x_countryin)
      
      
      # get iout
      iout <- mselect(x_countryout, Output=indnames)
      iout <- as.data.frame(iout)
      iout <- dcast(iout, Data1 + Data2 ~ Data3, value.var = "Value")
      row.names(iout) <- paste(iout[,1], iout[,2], sep=".")
      iout <- as.data.frame(iout[,-c(1:2), drop=FALSE])
      iout[is.na(iout)] <- 0

      # get cout iin
      cout_iin <- mselect(x_countryout[,,i], Input=indnames)
      cout_iin <- mselect(cout_iin, Output=commnamesout)
      cout_iin <- as.data.frame(cout_iin)
      cout_iin <- dcast(cout_iin, Data1 + Data2 ~ Data3, value.var = "Value")
      row.names(cout_iin) <- cout_iin[,2]
      cout_iin <- as.data.frame(cout_iin[,-c(1:2), drop=FALSE])
      cout_iin[is.na(cout_iin)] <- 0
      rm(x_countryout)
      rm(commnamesout)

      # new missing ind (because no commodities with this ind as input)
      newmissing <- indnames[!(indnames%in%rownames(cout_iin))]
      if(length(newmissing)!=0){
        warning("something in newmissing", immediate. = TRUE)
        missingind <- c(missingind, newmissing)
        
        vcat(verbosity = 2, " Newmissing:", paste(newmissing, collapse = ", "))
        remove <- which(colnames(iout)==newmissing)
        iout <- iout[,-remove]
      }
      rm(newmissing)
      
      
      
      # sort
      iout <- iout[,row.names(cout_iin), drop=FALSE]
      rownames_result <- row.names(iout)
      rownames_country <- substr(rownames_result, 1, 3)
      rownames_sector <- substr(rownames_result, 5, nchar(rownames_result))
      colnames_result <- colnames(cout_iin)
      iout <- as.matrix(iout)
      cout_iin <- as.matrix(cout_iin)
      rm(rownames_result)

      # find rows/cols that have only zeros
      deleteout <- which(apply(iout, 2, tmp))
      deletein <- which(apply(cout_iin, 1, tmp))
      delete <- unique(c(deleteout, deletein))
      vcat(verbosity = 2, " Length delete:", length(delete), "\n")
      
      if(length(delete)!=0){
        # calculate new lost value
        missingvalue_loop <- missingvalue_loop + sum(cout_iin[delete,])
        # delete rows/cols with only zeros
        iout <- iout[,-delete]
        cout_iin <- cout_iin[-delete,]
      }
      
      # announce missed value
      vcat(verbosity = 2, " Missing Value:", missingvalue_loop)
      missingvalue <- missingvalue + missingvalue_loop
      missingind <- c(missingind, indnames[!indnames%in%colnames(iout)])
      vcat(verbosity = 2, paste0(" Missing industries: ", paste(missingind,collapse=", ")))
      
      
      # weight iout 
      cancel <- c()
      vcat(verbosity = 2, " Remaining industries:",ncol(iout))
      for(j in 1:ncol(iout)){
        sum <- sum(iout[,j])
        if(sum>0){
            iout[,j] <- iout[,j]/sum
        }
        else{
          iout[,j] <- 0
          cancel <- c(cancel, j)
          warning(paste("\n still a non positiv collumn:", sum),immediate. = TRUE)
        } 
      }
      missingduetocanceling <- sum(cout_iin[cancel,])
      
      
      # missing value due to cancelling out
      vcat(verbosity = 2, " Missing du to cancelling:", missingduetocanceling, "\n")
      missingvalue <- missingvalue + missingduetocanceling
      
      # iout weighted * cout iin (to remove industries as input) = res1
      res1 <- iout %*% cout_iin
      res1 <- as.data.table(res1)
      res1 <- cbind(rownames_country, rownames_sector, res1)
      colnames(res1) <- c("Input Country", "Input", colnames_result)

      res1 <-  melt(res1, id.vars=c("Input Country","Input"))
      res1 <- res1[res1[["value"]]!=0,,]
      input <- as.data.frame(res1[["Input"]])
      output <- as.data.frame(res1[["variable"]])
      input_country <- as.data.frame(res1[["Input Country"]])
      res1 <- cbind(i, year, input_country, input, output, res1[["value"]])
      colnames(res1) <- c("Country","Year", "Input Coutnry", "Input","Output","Value")
      res1 <- as.magpie(res1, spatial=1 ,temporal=2, datacol=6)

      
      rm(iout)
      rm(cout_iin)
      
      # ind parts to zero
      mselect(x[i,,], Output=indnamesout) <- 0
      mselect(x[,,i], Input=indnamesin) <- 0
      delete <- which(apply(x, 3, tmp))
      x <- x[,,-delete]
      vcat(verbosity = 2, " Deleted unused 3rd dimension")
      
      # get newcombinations (compare getNames(res), getNames(x))
      names_res1 <- getNames(res1)
      names_before <- getNames(x)
      new_combinations <- names_res1[!(names_res1%in%names_before)]

      # mbind those newcpmbinations to x
      addcombinations <- new.magpie(countries, year, new_combinations, fill=0)
      x <- mbind(x, addcombinations)

      # add res1 and res2 to corresponding part of x
      x[i,,names_res1] <- x[i,,names_res1] + res1
      
      gc()
    }
  }
  
  vcat(verbosity = 1, "\n Total lost value:", missingvalue)
  vcat(verbosity = 1, "\n Total removed value:", removedvalue)
  

  dimnames(x)[[3]] <- gsub("(Commodities_|Industries_)","",dimnames(x)[[3]])
  vcat(verbosity = 2, "\n deleted Commodities/Industries\n\n")
  return(list(x=x,
              weight=NULL,
              unit="1000USD",
              description="EORA data",
              note=c(missingvalue, removedvalue)))
}