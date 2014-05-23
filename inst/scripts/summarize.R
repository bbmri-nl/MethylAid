#########################################################################################
##MethylAid BiocParallel example
##M. van Iterson
##2014-04-17
#########################################################################################


#########################################################################################
library(MethylAid)
library(BiocParallel) ##for BatchJobsParam
##requires valid minfi targets data.frame
##uses 10 workers (Cores)
##summarizes the data in batches of size 50

##prepare for batch job submission
BPPARAM <- BatchJobsParam(workers = 10, 
                     progressbar = FALSE, 
                     conffile = file.path(path.package("MethylAid"), "scripts", "config.R"))

summarize(targets, batchSize = 50, BPPARAM = BPPARAM, file="sData.RData")
#########################################################################################


