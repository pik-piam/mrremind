#' readEORA
#'
#' @description Read in EORA data of one year
#' @param subtype - year (1990-2013)
#' @return MAgPIE object of the EORA data
#' @author Debbora Leip
#' @seealso \code{\link{readSource}}, \code{\link{calcEORA}}, \code{\link{calcCommoditiesEORA}}
#' @importFrom data.table := fread melt.data.table
#' @importClassesFrom data.table data.table
#' @importFrom plyr round_any
#' @note With about 15000*15000 entries the EORA database is extremely big. Therefore, it wasn't possible
#' to create a MAgPIE object containing the full information. Instead every value is rounded to the nearest
#' 250 000 USD.


readEORA <-  function(subtype){

  year <- subtype
  
  # Read data
  filename_main <- paste("eora_main_",year,".csv",sep="")
  eora_main <- fread(filename_main, header=FALSE)
  filename_pi <- paste("eora_pi_",year,".csv",sep="")
  eora_pi <- fread(filename_pi, header=FALSE)
  filename_fd <- paste("eora_fd_",year,".csv",sep="")
  eora_fd <- fread(filename_fd, header=FALSE)
  vcat(verbosity = 2, " read data")
  
  # Read row- and colnames
  rownames_main <- fread("rownames_main.csv")
  rownames_pi <- fread("rownames_pi.csv")
  colnames_fd <- fread("colnames_fd.csv")
  rownames_main[,"Sector" := gsub("[.]","_",rownames_main[["Sector"]]), with=FALSE]
  rownames_pi[,"Sector" := gsub("[.]","_",rownames_pi[["Sector"]]), with=FALSE]
  colnames_fd[,"Sector" := gsub("[.]","_",colnames_fd[["Sector"]]), with=FALSE]

  # Put row- and colnames to data.frame
  eora_main <- cbind(paste(paste(rownames_main[["CountryA3"]],
                           rownames_main[["Entity"]],sep="."),
                           rownames_main[["Sector"]], 
                           sep="_"),eora_main)
  eora_fd <- cbind(paste(paste(rownames_main[["CountryA3"]],
                         rownames_main[["Entity"]],sep="."),
                         rownames_main[["Sector"]],
                         sep="_"),eora_fd)
  eora_pi <- cbind(paste(paste(rownames_pi[["CountryA3"]],
                         rownames_pi[["Entity"]],sep="."),
                         rownames_pi[["Sector"]],
                         sep="_"),eora_pi)
  
  colnames_eora <- c("input",
                     paste(rownames_main[["CountryA3"]],rownames_main[["Entity"]],
                           rownames_main[["Sector"]],sep="$"))
  colnames(eora_main) <- colnames_eora
  colnames(eora_pi) <- colnames_eora
  
  colnames_eora_fd <- c("input",paste(colnames_fd[["CountryA3"]],
                                      colnames_fd[["Entity"]],
                                      colnames_fd[["Sector"]],sep="$"))
  colnames(eora_fd) <- colnames_eora_fd

  # Change names of duplicated colnames
  colnames(eora_main)[duplicated(colnames(eora_main))] <-
    paste(colnames(eora_main)[duplicated(colnames(eora_main))],1,sep="_")
  colnames(eora_pi)[duplicated(colnames(eora_pi))] <-
    paste(colnames(eora_pi)[duplicated(colnames(eora_pi))],1,sep="_")
  
  # Change names of duplicated rownames
  rownames_main <- eora_main[["input"]]
  rownames_main[duplicated(rownames_main)] <- paste(rownames_main[duplicated(rownames_main)],1,sep="_")
  eora_main[,"input" := rownames_main, with=FALSE]
  rownames_fd <- eora_fd[["input"]]
  rownames_fd[duplicated(rownames_fd)] <- paste(rownames_fd[duplicated(rownames_fd)],1,sep="_")
  eora_fd[,"input":=rownames_fd, with=FALSE]
  vcat(verbosity = 2, " added names")
  
  # inputs as factor
  eora_main[,"input" := as.factor(eora_main[["input"]]), with=FALSE]
  eora_pi[,"input" := as.factor(eora_pi[["input"]]), with=FALSE]
  
  
  # Melt data.frame
  eora_main_part_0 <- melt(eora_main[1:5000,,], id.vars="input", na.rm = TRUE)
  eora_main_part_0 <- eora_main_part_0[eora_main_part_0[["value"]]!=0,]
  eora_main_part_0[,"value":=round_any(eora_main_part_0[["value"]],10000), with=FALSE]
  eora_main_part_0 <- eora_main_part_0[eora_main_part_0[["value"]]!=0,]
  eora_main_part_1 <- melt(eora_main[5001:10000,,], id.vars="input", na.rm = TRUE)
  eora_main_part_1 <- eora_main_part_1[eora_main_part_1[["value"]]!=0,]
  eora_main_part_1[,"value":=round_any(eora_main_part_1[["value"]],10000), with=FALSE]
  eora_main_part_1 <- eora_main_part_1[eora_main_part_1[["value"]]!=0,]
  eora_main_part_2 <- melt(eora_main[10001:nrow(eora_main),,], id.vars="input", na.rm = TRUE)
  eora_main_part_2 <- eora_main_part_2[eora_main_part_2[["value"]]!=0,]
  eora_main_part_2[,"value":=round_any(eora_main_part_2[["value"]],10000),with=FALSE]
  eora_main_part_2 <- eora_main_part_2[eora_main_part_2[["value"]]!=0,]
  eora_pi <- melt(eora_pi, id.vars="input", na.rm = TRUE)
  eora_pi[,"value":=round_any(eora_pi[["value"]],10000),with=FALSE]
  eora_pi <- eora_pi[eora_pi[["value"]]!=0,]
  eora_fd <- melt(eora_fd, id.vars="input", na.rm = TRUE)
  eora_fd[,"value":=round_any(eora_fd[["value"]],10000), with=FALSE]
  eora_fd <- eora_fd[eora_fd[["value"]]!=0,]
  vcat(verbosity = 2, " molten and round")
  eora <- rbind(eora_main_part_0,eora_main_part_1,eora_main_part_2,eora_pi,eora_fd)
  output_information <- do.call(rbind, strsplit(as.character(eora[["variable"]]),"[$]"))
  input <- eora[["input"]]
  input <- do.call(rbind,strsplit(as.character(input),"[.]"))
  input_countries <- input[,1]
  input <- input[,2]
  output <- paste(output_information[,2],output_information[,3],sep="_")
  countries <- as.data.frame(output_information[,1])
  years <- as.data.frame(c(rep(year,length(countries))))
  eora <- cbind(countries,years,input_countries, input, output, eora[["value"]])
  colnames(eora) <- c("Country","Year","Input_Country","Input","Output","value")

  # as MAgPIE object
  eora <- as.magpie(eora,spatial=1,temporal=2, datacol=6)
  vcat(verbosity = 2, " Converted to magpie object")
  vcat(verbosity = 1, " Data round to nearest 1000000USD")
  
  return(eora)
}