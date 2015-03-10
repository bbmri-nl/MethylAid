setGeneric("visualize",
           function(object, ...)
           standardGeneric("visualize")
           )

##' launch a shiny app for visualization of the summarized 450k data
##'
##' Outliers are detected based on a set of default thresholds.
##' To use a use-defined set of thresholds use the thresholds
##' argument.
##' @title visualize the summarized 450k data
##' @param object summarizedData object
##' @param thresholds default thresholds
##' @return lauches a web browser with the shiny application and returns a
##' data.frame with detected outliers
##' @import shiny ggplot2 RColorBrewer
##' @export
##' @docType methods
##' @rdname visualize-methods
##' @examples
##' library(minfiData)
##' baseDir <- system.file("extdata", package="minfiData")
##' targets <- read.450k.sheet(baseDir)
##' data <- summarize(targets)
##' \dontrun{
##' visualize(data)
##' }
setMethod("visualize", "summarizedData",
          function(object,
                   thresholds = list(MU = 10.50, OP = 11.75, BS = 12.75, HC = 13.25, DP = 0.95))
          {
            app <- list(ui=ui450k(object),
                        server=server450k(object, thresholds = thresholds))
            invisible(runApp(app))
          })
