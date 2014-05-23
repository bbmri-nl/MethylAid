##' container for summarized 450k Illumina Human Methylation data
##'
##'@section Slots:
##'    \describe{
##'      \item{\code{targets}:}{Object of class \code{"data.frame"}
##' containing targets information.}
##'      \item{\code{controls}:}{Object of class \code{"data.frame"}
##' containing quality control probe information.}
##'      \item{\code{Rcontrols}:}{Object of class \code{"matrix"}
##' containing quality control probe intensities for the Red channel.}
##'      \item{\code{Gcontrols}:}{Object of class \code{"matrix"}
##' containing quality control probe  intensities for the Grn channel.}
##'      \item{\code{DPfreq}:}{Object of class \code{"vector"}
##' containing frequencies of probes above background.}
##'      \item{\code{MU}:}{Object of class \code{"matrix"}
##' containing Methylated and Unmethylated internsities.}
##'      \item{\code{plotdata}:}{Object of class \code{"list"}
##' containing data to make plotting efficient.}
##'    }
##' @title container for summarized 450k Illumina Human Methylation data
##' @name summarizedData-class
##' @rdname summarizedData-class
##' @exportClass summarizedData
##' @import methods
setClass(
         Class="summarizedData",
         representation=representation(
           targets="data.frame",
           controls="data.frame",
           Rcontrols="matrix",
           Gcontrols="matrix",
           DPfreq="vector",
           MU="matrix",
           plotdata="list"
           )
         )

print.summarizedData <- function(object)
{
  nSamples <- nrow(object@targets)
  txt <- paste("summarizedData on", nSamples, "samples.")
  print(txt)
  return(invisible(txt))
}

##' show method for summarized 450k Illumina Human Methylation data
##'
##' @title show method for summarized 450k Illumina Human Methylation data
##' @param object summarizedData object
##' @return print short summary summarizedData object
##' @import methods
##' @export
##' @docType methods
##' @rdname summarizedData-methods
setMethod("show", "summarizedData",
          function(object) print.summarizedData(object))

reduce <- function(summarizedDataList)
  {
    targets <- controls <- Rcontrols <- Gcontrols <- MU <- DPfreq <- c()
    n <- length(summarizedDataList)
    for(i in 1:n)
      {
        summarizedData <- summarizedDataList[[i]]
        targets <- rbind(targets, summarizedData@targets)
        Rcontrols <- cbind(Rcontrols, summarizedData@Rcontrols)
        Gcontrols <- cbind(Gcontrols, summarizedData@Gcontrols)
        MU <- cbind(MU, summarizedData@MU)
        DPfreq <- c(DPfreq, summarizedData@DPfreq)
      }

    sData <- new("summarizedData",
                 targets=as.data.frame(targets),
                 controls=summarizedData@controls,
                 Rcontrols=as.matrix(Rcontrols),
                 Gcontrols=as.matrix(Gcontrols),
                 MU=as.matrix(MU),
                 DPfreq=DPfreq) ##not as.vector otherwise names attribute is dropped
    sData
  }
