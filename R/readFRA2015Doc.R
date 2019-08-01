#' Read FRA2015Doc
#' 
#' Read-in an FRA data from 2015 (forest resource assessment) pdf file. 
#' 
#' 
#' @param subtype data subtype. 
#' @return magpie object of the FRA 2015 data
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("FRA2015Doc","forest_area")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect
#' @export

readFRA2015Doc <- function(subtype){
  
  ## Mapping file
  iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "moinput"),row.names=NULL)
  
  ## Original data file name
  location <- "FRA2015_data.xlsx"
  
  ### Aggregated region rows in original data which have to be removed
  remove_region_rows <- c("Eastern and Southern Africa","Northern Africa","Western and Central Africa",
                          "East Asia","South and Southeast Asia","Western and Central Asia",
                          "Europe","Caribbean","Central America","North America","Oceania","South America")
  
  ## Year Names
  column_names <- c("Country",paste0("y",c(1990,2000,2005,2010,2015)))
  
  ## Define standard function
  
  fra_reader <- function(file_name,sheet_location,skip_rows,skip_cols,rows_delete,col_vector){
    dat <- as.data.frame(read_xlsx(path = file_name,sheet = sheet_location,skip = skip_rows,n_max = skip_cols))
    
    dat <- dat[,-c(2:10,16:ncol(dat))]
    
    ## Rename columns
    colnames(dat) <- col_vector
    
    ## Remove rows we don't need
    dat <- dat[!(dat[,1] %in% rows_delete),]
    
    return(dat)
  }
  
  ############ FOREST AREA ############
 
  if (subtype=="forest_area") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "1.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="other_wooded_land") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "1.2",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  ############ FOREST CHARACTERISTICS ############
  
  else if (subtype=="primary_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "2.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="secondary_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "2.2",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="plantation_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "2.3",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="mangrove_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "2.4",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  ############ GROWING STOCK, BIOMASS AND CARBON ############
  
  else if (subtype=="forest_gs") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. m3
  }
  
  else if (subtype=="abvg_bm") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.2",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="belg_bm") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.3",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="dead_wood") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.4",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="c_abvg_bm") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.5",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="c_belg_bm") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.6",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="c_dead_wood") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.7",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="c_litter") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.8",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  else if (subtype=="c_soil") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "3.9",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    ## This is already in mio. tonnes
  }
  
  ############ PRODUCTION AND MULTIPLE USE ############
  
  else if (subtype=="production_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "4.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="multiple_use_forest") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "4.2",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  ############ BIODIVERSITY AND PROTECTED AREAS ############
  
  else if (subtype=="biodiversity_conservation") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "5.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="forest_within_protected") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "5.2",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  ############ OWNERSHIP OF FORESTS ############
  
  else if (subtype=="public_own") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="private_own") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="unknown_own") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  ############ MANAGEMENT RIGHTS OF PUBLIC FORESTS ############
  
  else if (subtype=="public_admin") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="individual_admin") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="private_admin") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="community_admin") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }
  
  else if (subtype=="other_admin") {
    ## Read using pre-define function
    x <- fra_reader(file_name = location,sheet_location = "6.1",
                    skip_rows = 3,skip_cols = 245,rows_delete = remove_region_rows,col_vector = column_names)
    
    ## Data for 2015 doesn't exist but is read in using the dtandard function we defined earlier
    ## This results in reading an additional column we don't need so we drop it here.
    
    x <- x[,-ncol(x)]
    
    ## Convert to mio. ha
    x[,2:ncol(x)] <- x[,2:ncol(x)]/1000
  }

  else {stop("Invalid subtype ", subtype)}
  
  mag_iso <- merge(x, iso_country, by.x = "Country", by.y = "X")
  mo_final <- magpiesort(as.magpie(mag_iso[,-1])) ## Dropping the first column because we now have ISO codes in last column
  mo_final <- round(mo_final,2)
  getNames(mo_final) <- subtype
  mo_final_1995 <- setYears((setYears(mo_final[,"y1990",],NULL) + setYears(mo_final[,"y2000",],NULL))/2,"y1995")
  mo_final <- mbind(mo_final,mo_final_1995)
  mo_final <- magpiesort(mo_final)
  return(mo_final)
} 