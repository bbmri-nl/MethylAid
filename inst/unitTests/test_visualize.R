
check_summarizedData <- function(sData, numberOfSamples, numberOfTargetColumns) {

    ##check if summarizedData is create correctly
    checkTrue(all(slotNames(sData) %in%
                  c("targets", "controls", "Rcontrols", "Gcontrols", "DPfreq", "MU", "plotdata")))

    ##and if all dimension are as expected
    checkEquals(nrow(sData@targets), numberOfSamples)
    checkEquals(ncol(sData@targets), numberOfTargetColumns)

    checkEquals(nrow(sData@controls), 848)
    checkEquals(ncol(sData@controls), 4)

    checkEquals(nrow(sData@Rcontrols), 848)
    checkEquals(ncol(sData@Rcontrols), numberOfSamples)

    checkEquals(nrow(sData@Gcontrols), 848)
    checkEquals(ncol(sData@Gcontrols), numberOfSamples)

    checkEquals(length(sData@DPfreq), numberOfSamples)

    checkEquals(nrow(sData@MU), 2)
    checkEquals(ncol(sData@MU), numberOfSamples)

    checkEquals(nrow(sData@plotdata), 662*numberOfSamples)
    checkEquals(ncol(sData@plotdata), 7)

  }


test_visualize <- function() {

  ##check example data
  data(exampleData)
  check_summarizedData(exampleData, 500, 7)

  ##check summarized minfiData
  library(minfiData)

  ##prepare targets
  baseDir <- system.file("extdata", package = "minfiData")
  targets <- read.metharray.sheet(baseDir)
  rownames(targets) <- paste(targets$Slide, targets$Array, sep="_")
  data <- summarize(targets)

  check_summarizedData(data, nrow(targets), ncol(targets)+1)

}
