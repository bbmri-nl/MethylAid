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
            ##construct container for the outliers
            outliers <- matrix(FALSE, nrow=nrow(object@targets), ncol=5,
                               dimnames=list(row.names(object@targets),
                                 c("MU", "BS", "OP", "HC", "DP")))
            assign("outliers", outliers, envir=globalenv())

            app <- list(ui=ui450k(object), server=server450k(object))
            runApp(app)

            ##get outliers for return value
            outliers <- get("outliers", envir=globalenv())

            ##clear global envirnoment
            for(obj in c("outliers", "highlight"))
              {
                if(exists(obj, envir=globalenv()))
                  rm(list=obj, envir=globalenv())
              }

            ##return outliers with information from targets file
            targets <- object@targets
            outliers <- targets[rownames(targets) %in%
                                rownames(outliers[rowSums(outliers) > 0,,
                                                  drop=FALSE]),]
            invisible(outliers)
          })
