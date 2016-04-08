test_summarize <- function() {

  library(minfiData)

  ##prepare targets
  baseDir <- system.file("extdata", package = "minfiData")
  targets <- read.metharray.sheet(baseDir)
  rownames(targets) <- paste(targets$Slide, targets$Array, sep="_")

  ##summarize data some sanity checks
  ##summarize 1 sample as 1 batch
  data <- summarize(targets[1,], batchSize=1, verbose=FALSE)
  checkEquals(nrow(data@targets) , 1)
  
  ##summarize 2 sample in one go
  data <- summarize(targets[1:2,], verbose=FALSE)
  checkEquals(nrow(data@targets) , 2)
  
  ##summarize 2 sample each in a single batch
  data <- summarize(targets[1:2,], batchSize=1, verbose=FALSE)
  checkEquals(nrow(data@targets) , 2)
  
  ##summarize all samples in one go
  data <- summarize(targets)
  checkEquals(nrow(data@targets) , nrow(targets))    

}
