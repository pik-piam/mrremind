#' @title calcNlWasteDistrib
#' @description 
#' non-linear optimization distributes waste by composition type to disposal type. 
#' returns magpie object, share of total disposal
#' @return Magpie object of waste types to waste distribution,  share
#' @author David Chen
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="NlWasteDistrib")
#' }
#' @importFrom alabama auglag

calcNlWasteDistrib <- function(){
#used for result visualization
########pre-processing

comp <- readSource("Waste",subtype="Composition")
treat <- readSource("Waste",subtype="Treatment")
gen <-readSource("Waste", subtype="Generation", convert=T)

comp_shr <- comp/gen
treat_shr <- treat/gen
comp_shr <- collapseNames(dimSums(comp_shr, dim=2, na.rm=T))
treat_shr <- collapseNames(dimSums(treat_shr, dim=2, na.rm=T))

########## nl model

nlwaste <- function(iso,comp_shr,treat_shr){
#preprocess
  type<-as.data.frame(comp_shr[iso,,])
  type$Value[which(is.na(type$Value))] <- 0
  type$Value <- type$Value/sum(type$Value)
  treatment<-as.data.frame(treat_shr[iso,,])
  treatment$Value[which(is.na(treatment$Value))] <- 0
  treatment$Value <- treatment$Value/sum(treatment$Value)
  rhs <- as.matrix(outer(treatment$Value, type$Value))
  rhs1 <- as.vector(t(rhs))  

#obj fn  
f=function(x)((x[1]-1)^2+(x[2]-rhs1[2])^2+(x[3]-rhs1[3])^2+(x[4]-rhs1[4])^2+(x[5]-rhs1[5])^2+(x[6]-rhs1[6])^2+(x[7]-rhs1[7])^2+(x[8]-rhs1[8])^2+
               (x[9]-rhs1[9])^2+(x[10]-rhs1[10])^2+(x[11]-rhs1[11])^2+(x[12]-rhs1[12])^2+(x[13]-rhs1[13])^2+(x[14]-rhs1[14])^2+(x[15]-rhs1[15])^2+(x[16]-rhs1[16])^2+
               (x[17]-rhs1[17])^2+(x[18]-rhs1[18])^2+(x[19]-rhs1[19])^2+(x[20]-rhs1[20])^2+(x[21]-rhs1[21])^2+(x[22]-rhs1[22])^2+(x[23]-rhs1[23])^2+(x[24]-rhs1[24])^2+
               (x[25]-rhs1[25])^2+(x[26]-rhs1[26])^2+(x[27]-rhs1[27])^2+(x[28]-rhs1[28])^2+(x[29]-rhs1[29])^2+(x[30]-rhs1[30])^2+(x[31]-rhs1[31])^2+(x[32]-rhs1[32])^2+
               (x[33]-rhs1[33])^2+(x[34]-rhs1[34])^2+(x[35]-rhs1[35])^2+(x[36]-rhs1[36])^2+(x[37]-rhs1[37])^2+(x[38]-rhs1[38])^2+(x[39]-rhs1[39])^2+(x[40]-rhs1[40])^2)

#equality constraint function - row and column sums, 0's
eq=function(x){
  h <- rep(NA, 1)
  h[1] <- sum(x[1:8])-treatment$Value[1]
  h[2] <-  sum(x[9:16])-treatment$Value[2]
  h[3] <- sum(x[17:24])-treatment$Value[3]
  h[4] <- sum(x[25:32])-treatment$Value[4]
  h[5] <- sum(x[33:40])-treatment$Value[5]
  h[6] <- sum(x[c(1,9,17,25,33)])-type$Value[1]
  h[7] <- sum(x[c(2,10,18,26,34)])-type$Value[2]
  h[8] <- sum(x[c(3,11,19,27,35)])-type$Value[3]
  h[9] <- sum(x[c(4,12,20,28,36)])-type$Value[4]
  h[10] <- sum(x[c(5,13,21,29,37)])-type$Value[5]
  h[11] <- sum(x[c(6,14,22,30,38)])-type$Value[6]
  h[12] <- sum(x[c(7,15,23,31,39)])-type$Value[7]
  h[13] <- sum(x[c(8,16,24,32,40)])-type$Value[8]
  h[14] <- x[2]
  h[15] <- x[3]
  h[16] <- x[4]
  h[17] <- x[6]
  h[18] <- x[7]
  h[19] <- x[9]
  h[20] <- x[18]
  h[21] <- x[19]
  h
}
#inequality constraints - greater than 0 for everything except for the ones that are =0
ineq=function(x){
  h <- rep(NA, 1)
  h[1] <- x[1]
  h[2] <-  x[5]
  h[3] <- x[8]
  h[4] <- x[10]
  h[5] <- x[11]
  h[6] <- x[12]
  h[7] <- x[13]
  h[8] <- x[14]
  h[9] <- x[15]
  h[10] <- x[16]
  h[11] <- x[17]
  h[12] <- x[20]
  h[13] <- x[21]
  h[14] <- x[22]
  h[15] <- x[23]
  h[16] <- x[24]
  h[17] <- x[25]
  h[18] <- x[26]
  h[19] <- x[27]
  h[20] <- x[28]
  h[21] <- x[29]
  h[22] <- x[30]
  h[23] <- x[31]
  h[24] <- x[32]
  h[25] <- x[33]
  h[26] <- x[34]
  h[27] <- x[35]
  h[28] <- x[36]
  h[29] <- x[37]
  h[30] <- x[38]
  h[31] <- x[39]
  h[32] <- x[40]
  h
}

#iterations
set.seed(20)
#initial values at multiplication
p0 <- rhs1
#augmented langrangian nl optim
ans2 <- auglag(par=p0, fn=f, heq=eq, hin=ineq)
return(ans2)
}

#list of countries where both treat and type exist,, for those for which only one exist...calibration?
tmp<- which(dimSums(treat_shr,dim=3)==0)
tmp2 <- which(dimSums(comp_shr,dim=3)==0)
valid <- getRegions(treat_shr[-tmp,,])
valid2 <- getRegions(comp_shr[-tmp2,,])
v <- intersect(valid, valid2)

#applied on all regions
models <- lapply(v, nlwaste, comp_shr=comp_shr, treat_shr=treat_shr)
names(models) <- paste0(v)

#to compare countries one on one; remember change iso to change rhs

# a <- matrix((models$AND$par), nrow=5, ncol=8, byrow=T)
# colnames(a) <- c("organic","glass","metal","other","paper","plastic","rubber","wood")
# rownames(a) <- c("compost", "recyc", "incineration","landfills","dumps")
# b <- round(rhs,3)
# colnames(b) <- c("organic","glass","metal","other","paper","plastic","rubber","wood")
# rownames(b) <- c("compost", "recyc", "incineration","landfills","dumps")
# #
# a-b

names <- list(c("compost", "recyc", "incineration","landfills","dumps"),
              c("organic","glass","metal","other","paper","plastic","rubber","wood"),
              v)

pars <- sapply(models,`[`,1) 
pars <- lapply(pars, matrix,nrow=5, ncol=8, byrow=T)
pars <- simplify2array(pars)
dimnames(pars) <- names
pars <- as.data.frame.table(pars)
colnames(pars) <- c("treatment", "type","region","value")


years <-readSource("Waste", subtype="Generation", convert=F)
years <-as.data.frame(where(is.na(years))$false$`individual`)
# year that total msw generated is measured
years <- years[,c(1:2)]
pars <- merge(pars, years)

pars <- pars[,c(1,5,2,3,4)]


x <- as.magpie(pars, spatial=1, temporal=2, tidy=TRUE)

return(list(
  x=x,
  weight=NULL,
  unit="percentage",
  description="share of waste generation",
  isocountries=FALSE
))
  
  }