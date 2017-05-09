##' launch a shiny app for visualization of the summarized Illumina
##' Human DNA Methylation array data
##'
##' Outliers are detected based on a set of default thresholds.  To
##' use a use-defined set of thresholds use the thresholds argument.
##' @title visualize the summarized Illumina Human DNA Methylation
##'     array data
##' @param object summarizedData object
##' @param thresholds default thresholds
##' @param background optional summarizedData-object used as
##'     background in filter control plots
##' @param ... for future use
##' @return lauches a web browser with the shiny application and
##'     returns a data.frame with detected outliers
##' @import shiny ggplot2 RColorBrewer
##' @importFrom hexbin hexbin grid.hexagons
##' @importFrom grid pushViewport popViewport
##' @importFrom gridBase baseViewports
##' @importFrom grDevices dev.off pdf blues9 colorRampPalette
##' @importFrom graphics abline axTicks axis legend par plot points
##'     rect text
##' @importFrom stats na.omit pnorm sd
##' @importFrom utils str write.csv
##' @export
##' @docType methods
##' @examples
##' library(minfiData)
##' baseDir <- system.file("extdata", package="minfiData")
##' targets <- read.metharray.sheet(baseDir)
##' data <- summarize(targets)
##' \dontrun{
##' visualize(data)
##' }
setGeneric("visualize",
           function(object,
                    thresholds = list(hm450k=list(MU = 10.50, OP = 11.75, BS = 12.75, HC = 13.25, DP = 0.95),
                                      epic=list(MU = 10.5, OP = 12, BS = 12, HC = 13, DP = 0.95)),
                    background = NULL, ...)
               standardGeneric("visualize")
           )


.arrayType <- function(object) {
    ##little bit danger test
    ifelse(any(object@plotdata$ExtendedType == "BS Conversion I-C6"), "hm450k", "epic")
}

##' @rdname visualize
setMethod("visualize", "summarizedData",
          function(object,
                   thresholds = list(hm450k=list(MU = 10.50, OP = 11.75, BS = 12.75, HC = 13.25, DP = 0.95),
                                     epic=list(MU = 10.5, OP = 12, BS = 12, HC = 13, DP = 0.95)),
                   background = NULL, ...){

              object <- updateObject(object) ##for visualize MethylAid version 1.5.1
              background <- updateObject(background)

              arrayType <- .arrayType(object)

              if(!is.null(background)) {
                  if(arrayType != .arrayType(background))
                      message("Array-types differ between background and 'summarizedData'!")
              }

              thresholds <- thresholds[[arrayType]]

              app <- list(ui=ui(object),
                          server=server(object, thresholds = thresholds, background = background, ...))
              invisible(runApp(app))
          })
