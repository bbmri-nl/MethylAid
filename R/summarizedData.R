##' container for summarized Illumina Human DNA Methylation array data
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
##' @title container for summarized Illumina Human DNA Methylation array data
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

printSummarizedData <- function(object) {
    nSamples <- nrow(object@targets)
    cat(class(object), " object with ", nSamples, " samples.\n",
        "Containing: median Methylated and Unmethylation values,\n",
        "            detection P-values\n",
        "            and all quality control probe intensities.\n", sep="")
}

##' show method for summarized Illumina Human DNA Methylation array data
##'
##' @title show method for Illumina Human DNA Methylation array data
##' @param object summarizedData object
##' @return print short summary summarizedData object
##' @import methods
##' @export
##' @docType methods
##' @rdname summarizedData-methods
##' @examples
##' data(exampleData)
##' exampleData
setMethod("show", "summarizedData",
          function(object) printSummarizedData(object))

reduce <- function(summarizedDataList) {
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

.combine <- function(x, y, by=c("identical", "overlap")) {
    message("combining summarizedData objects")

    by <- match.arg(by)

    summarizedDataList <- c(x, y)
    names(summarizedDataList) <- as.character(as.list(substitute(summarizedDataList))[-1])

    ##message(paste(str(summarizedDataList)))

    ##check and adjust target info of the summarizedData objects
    colNamesTargets <- lapply(summarizedDataList, function(x) colnames(x@targets))
    if(by == "identical")
    {
        if(any(table(unlist(colNamesTargets)) != length(summarizedDataList)))
            stop(paste("Target information is not the same for all summarizedData-objects.\n",
                       "Consider using argument 'by = overlap'!"))
    }
    else if(by == "overlap")
    {
        if(any(table(unlist(colNamesTargets)) != length(summarizedDataList)))
        {
            cols <- colNamesTargets[[1]]
            for(i in 2:length(colNamesTargets))
                cols <- intersect(cols, colNamesTargets[[i]])
            for(i in 1:length(colNamesTargets))
            {
                targets <- summarizedDataList[[i]]@targets
                summarizedDataList[[i]]@targets <- targets[, match(cols, colnames(targets))]
            }
        }
    }

    ##add summarizedData-object name to targets info
    for(i in 1:length(summarizedDataList))
        summarizedDataList[[i]]@targets$summarizedDataName <- names(summarizedDataList)[i]

    ##combine, generate plot data and return
    summarizedDataList <- reduce(summarizedDataList)
    summarizedDataList@plotdata <- prepareData(summarizedDataList)
    summarizedDataList
}

##' Concatenates two summarizedData objects into one object
##'
##'
##' @title concatenates two summarizedData objects into one object
##' @param x summarizedData-object
##' @param y summarizedData-object
##' @param by argument indicating how the targets information should be combined
##' @return one summarizedData object
##' @importFrom BiocGenerics combine
##' @export
##' @docType methods
##' @examples
##' data(exampleData)
##' combine(exampleData, exampleData)
setMethod("combine", "summarizedData",
          function(x, y, by=c("identical", "overlap")) {
              .combine(x, y)
          })

##' Generate background data from a summarizedData-object
##'
##' Generates a background dataset can be used in the filter plots
##' @title generate background data
##' @param object summarizedData-object
##' @return list with background data for the filter plots
##' @author mvaniterson
##' @docType methods
setGeneric("as.background",
           function(object)
               standardGeneric("as.background")
           )

##' @rdname as.background
setMethod("as.background", "summarizedData",
          function(object) {
              ##MU
              MU <- t(object@MU)
              MU <- log2(na.omit(MU))
              x <- 0.5*(MU[,1] + MU[,2])
              y <- MU[,1] - MU[,2]
              bgMU <- data.frame(x, y)

              ##NP
              data <- object@plotdata
              d <- data[grepl(qcProbes["NP"], data$Type),]
              dGrn <- d[d$ExtendedType %in% c("NP (C)", "NP (G)"), c(1:5,7)]
              x <- tapply(dGrn$IntGrn, dGrn$Samples, mean)
              dRed <- d[d$ExtendedType %in% c("NP (A)", "NP (T)"), c(1:6)]
              y <- tapply(dRed$IntRed, dRed$Samples, mean)
              bgNP <- rotateData(data.frame(x=x, y=y), columns=c("x", "y"))

              ##BSI
              data <- object@plotdata
              d <- data[grepl(qcProbes["BSI"], data$Type),]
              dGrn <- d[grepl("C1|C2|C3", d$ExtendedType), c(1:5,7)]
              x <- tapply(dGrn$IntGrn, dGrn$Samples, mean)
              dRed <- d[grepl("C4|C5|C6", d$ExtendedType), c(1:6)]
              y <- tapply(dRed$IntRed, dRed$Samples, mean)
              bgBSI <- rotateData(data.frame(x=x, y=y), columns=c("x", "y"))

              ##HYB
              data <- object@plotdata
              d <- data[grepl(qcProbes["HYB"], data$Type),]
              d <- d[order(d$Samples),]
              x <- 0.5*(d$IntGrn[grepl("High", d$ExtendedType)] + d$IntGrn[grepl("Low", d$ExtendedType)])
              y <- d$IntGrn[grepl("High", d$ExtendedType)] - d$IntGrn[grepl("Low", d$ExtendedType)]
              bgHYB <- data.frame(x, y)

              list(MU = bgMU, NP = bgNP, BSI = bgBSI, HYB = bgHYB)
          })

setGeneric("updateObject", 
           function(object)
               standardGeneric("updateObject")
           )

setMethod("updateObject", "summarizedData",
          function(object) {
              old <- c("Color_Channel", "Name")
              new <- c("Color", "ExtendedType")
              if(!is.null(object)) {
                  if(sum(colnames(object@plotdata) %in% old) == 2) {
                      colnames(object@plotdata)[colnames(object@plotdata) %in% old] <- new
                  }
              }
              object
          })

setMethod("updateObject", "ANY",
          function(object) {
              object
          })
