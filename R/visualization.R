setGeneric("visualize",
           function(object)
           standardGeneric("visualize")
           )

##' launch a shiny app for visualization of the summarized 450k data
##'
##' details
##' @title visualize the summarized 450k data
##' @param object summarizedData object
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
setMethod("visualize", signature="summarizedData",
          function(object)
          {                        
            app <- list(ui=ui450k(object), server=server450k(object))                                   
            invisible(runApp(app))
          })
