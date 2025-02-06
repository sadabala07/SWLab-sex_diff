# the below code will read in values given after the RScript command
## cluster, when you run 50*3*13 jobs
args <- commandArgs(trailingOnly = TRUE)
job_id <- 26
a<-2
b<-1
# c<-as.numeric(args[3])
# this code runs completely-yay!
library(abind)
library(MASS)
library(magic)
#library(psych)
library(coda)
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) {
  files <- list.files(folderName, full.names=TRUE)
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}
## set directory of code
sourceEntireFolder("/N/u/sadabala/BigRed200/SWLab-sex_diff/R", verbose=FALSE, showWarnings=TRUE)
# choose the folder that has all the R files we want to use
#read data
setwd("/N/u/sadabala/BigRed200/SWLab-sex_diff")
alltask <- c("rest")
setwd("/N/u/sadabala/BigRed200/SWLab-sex_diff")
## this is tau-PET information
df<-read.csv("/N/u/sadabala/BigRed200/SWLab-sex_diff/a4vars_fmripet_n394.csv") # reads data into a dataframe
## includes all regional tau-PET information
allbehav <- c("PACCRN", colnames(df)[19:30]) # vector of behavioral variables from dataframe
task<-alltask[1]
behaviour <-as.character(allbehav[b]) # b should be which column we want to look at.
X<- readRDS(paste0("/N/u/sadabala/BigRed200/SWLab-sex_diff/X.",task,".rds")) # loads data to a list
Y<- df[df$sex==a,c("subj_id", allbehav)]# a = 1 or 2 representing sex,
if(a==3){
  Y<- df[,c("subj_id", allbehav)]# a = 1 or 2 representing sex,
}
# filters df to create a subset df specific sex
rownames(Y) <- Y$'subj_id' # sets rownames to subj_id
X <- X[names(X) %in% Y$'subj_id'] # X now only has the same ids that Y has
#### N is the sample size/number of subjects
N<-length(X)
N
## can replace ids with actual subject IDs
ids=names(X)
## V is the number of brain regions
V<-nrow(X[[1]])
## P is the number of variables for behavior/pathology
P<-1
##############  data
## X is a list of length N, and each element of the list is a V by V connectivity matrix
#X<-connmats.z[,,a,]
#X<-lapply(seq(dim(X)[3]), function(x) X[ , , x])
names(X)=ids
## Y is a matrix of N by P, N subjects' P behavior/pathology information
#Y<-as.matrix(clindata[,b],ncol=P)
Y<-as.matrix(Y[,behaviour],ncol=P)
rownames(Y)=ids
## randomly sample subject ids to keep as training and test data, here we use 10% test data
if(sum(is.na(Y))>0){
  sampled.id=sample(ids[-which(is.na(Y))], length(ids)*.1, replace = FALSE)
}else{sampled.id=sample(ids, length(ids)*.1, replace = FALSE)}
train.id=ids[!ids %in% sampled.id] # training ids
## full data X_full
X_full=X # list
## full data Y_full
Y_full=Y # array
## training data Y
Y[sampled.id,]=NA
## family_behavior indicates the distribution of behavior,"nrm" represents normal
## nscan = number of iterations, longer nscan takes longer
## burn = number of iterations before model converges
#need to change nscan how many iterations in MCMC, 2000 is enough for smaller dataset. #burn= burn-in period
#look at the trace plot and see whether the data converge
# run the model
model1=latentSNA(X=X, Y=Y, W=NULL, H=NULL,
                 seed = 1, nscan = 1, burn = 1, odens = 1,
                 prior=list(), job = job_id)
# save results
res=list("model"=model1, "X_full"=X_full, "sampled.id"=sampled.id,"Y_full"=Y_full)
#out_model = paste0(paste0("/N/slate/thjaya/MRI/Sex_differences/A4_code","/",behaviour, "/"), "sex=",1,",","feature=",behaviour,1,".rds") # changed c to 1
#dir.create(file.path(paste0("/N/slate/thjaya/MRI/Sex_differences/A4_code","/",behaviour)))
#setwd(paste0("/N/slate/thjaya/MRI/Sex_differences/A4_code",behaviour))
#saveRDS(res,out_model)
#out_model = paste0("/N/slate/thjaya/MRI/Sex_differences/A4_code/job_", job_id, behaviour, "/sex=", 2, ",feature=", behaviour, "1.rds")
#dir.create(file.path("/N/slate/thjaya/MRI/Sex_differences/A4_code/job_", job_id, behaviour), showWarnings = FALSE, recursive = TRUE)
#setwd(file.path("/N/slate/thjaya/MRI/Sex_differences/A4_code/job_", job_id, behaviour))
#saveRDS(res, out_model)
# Define the output model path
out_model <- file.path(
  "/N/u/sadabala/BigRed200/SWLab-sex_diff",
  paste0("job_", job_id),
  behaviour,
  paste0("sex=2,feature=", behaviour, "1.rds")
)
# Ensure the directory exists
dir.create(
  file.path("/N/u/sadabala/BigRed200/SWLab-sex_diff", paste0("job_", job_id), behaviour),
  showWarnings = FALSE,
  recursive = TRUE
)
# Save the RDS file
saveRDS(res, out_model)
