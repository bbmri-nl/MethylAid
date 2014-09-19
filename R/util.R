qcProbes=list(
  BSI="^BISULFITE CONVERSION I$",
  BSII="^BISULFITE CONVERSION II$",
  EC="^EXTENSION$",
  SPI="^SPECIFICITY I$",
  HYB= "^HYBRIDIZATION$",
  NP="^NON-POLYMORPHIC$",
  SPII="^SPECIFICITY II$",
  TR="^TARGET REMOVAL$",
  SC="^STAINING$",
  NC="^NEGATIVE$") ## we don't use the normalization controls NORM_A, NORM_G, NORM_C or NORM_T

qcplot <- function(object, plotType, col,
                   type=c("boxplot", "sample", "scatter"),
                   location=NULL, threshold=NULL, outliers)
  {

    type <- match.arg(type)
    
    p <- if(plotType == "MU")
      plotMU(object, col, location, threshold, outliers)
    else if(plotType == "OP")
      plotOP(object, col, location, threshold, outliers)
    else if(plotType == "BS")
      plotBS(object, col, location, threshold, outliers)
    else if(plotType == "HC")
      plotHC(object, col, location, threshold, outliers)
    else if(plotType == "DP")
      plotDP(object, col, location, threshold, outliers)
    else ##if "BSI", "BSII", "HYB", "NP", "EC", "NC", "SC", "TR", "SPI", "SPII"
      switch(type,
             scatter=qcscatterplot(object, plotType, outliers),
             sample=qcsampleplot(object, plotType, outliers),
             boxplot=qcboxplot(object, plotType, outliers))

    if(any(class(p) %in% "ggplot"))
      return(invisible(print(p)))
    else
      return(invisible(p))
  }

setHighlight <- function(x, y, location)
  {
    ##scale x and y range
    location$x <- (location$x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
    location$y <- (location$y - mean(y, na.rm=TRUE))/sd(y, na.rm=TRUE)
    x <- (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)
    y <- (y - mean(y, na.rm=TRUE))/sd(y, na.rm=TRUE)

    d <- sqrt((x - location$x)^2 + (y - location$y)^2)
    ##clicked in empty space remove highlighted
    if(min(d, na.rm=TRUE) >
       0.05*sqrt(diff(range(x, na.rm=TRUE))^2 + diff(range(y, na.rm=TRUE))^2))

      {
        if(exists("highlight", envir=globalenv()))
          rm(list="highlight", envir=globalenv())
        
      }
    else
      {
        id <- which.min(d)
        highlight <- names(x)[id]
        assign("highlight", highlight, envir=globalenv())        
      }
  }

getHighLightIndex <- function()
  {
    get("highlight", envir=globalenv())
  }

setOutliers <- function(outliers, type)
{
  out <- get("outliers", envir=globalenv())
  out[, type] <- FALSE ##reset
  out[, type] <- rownames(out) %in% outliers
  assign("outliers", out, envir=globalenv())
}

getOutliers <- function(sampleIds)
  {
    if(exists("outliers", envir=globalenv()))
      {
        outliers <- get("outliers", envir=globalenv())
        outliers <- rownames(outliers[rowSums(outliers) > 0,, drop=FALSE])
        return(sampleIds %in% outliers)
      }
    else
      return(FALSE)
  }

prepareData <- function(object)
  {
    ##TODO add logarithm as plot option
    R <- log2(object@Rcontrols)
    G <- log2(object@Gcontrols)

    hm450.controls <- object@controls[!(object@controls$Type %in%
                                        c("NORM_A", "NORM_G", "NORM_C", "NORM_T")), ] ##not used yet!

    data <- data.frame(Address=rep(rownames(R), ncol(R)),
                       Samples=rep(colnames(R), each=nrow(R)),
                       IntRed=as.vector(R),
                       IntGrn=as.vector(G))

    merge(hm450.controls, data)
  }

##Taken from minfi
##Added: ellipsis to allow for argument na.rm
##       as.matrix in case the RGset contains only one sample
detectionP <- function (rgSet, type="m+u", ...)
  {
    locusNames <- getManifestInfo(rgSet, "locusNames")
    detP <- matrix(NA_real_, ncol=ncol(rgSet), nrow=length(locusNames),
                   dimnames=list(locusNames, sampleNames(rgSet)))
    controlIdx <- getControlAddress(rgSet, controlType="NEGATIVE")
    r <- getRed(rgSet)
    rBg <- r[controlIdx, ]
    rMu <- colMedians(as.matrix(rBg), ...)
    rSd <- colMads(as.matrix(rBg), ...)
    g <- getGreen(rgSet)
    gBg <- g[controlIdx, ]
    gMu <- colMedians(as.matrix(gBg), ...)
    gSd <- colMads(as.matrix(gBg), ...)
    TypeII <- getProbeInfo(rgSet, type="II")
    TypeI.Red <- getProbeInfo(rgSet, type="I-Red")
    TypeI.Green <- getProbeInfo(rgSet, type="I-Green")
    for (i in 1:ncol(rgSet)) {
      intensity <- r[TypeI.Red$AddressA, i] + r[TypeI.Red$AddressB, i]
      detP[TypeI.Red$Name, i] <- 1 - pnorm(intensity,
                                           mean=rMu[i] * 2,
                                           sd=rSd[i] * 2)
      intensity <- g[TypeI.Green$AddressA, i] + g[TypeI.Green$AddressB, i]
      detP[TypeI.Green$Name, i] <- 1 - pnorm(intensity,
                                             mean=gMu[i] *2,
                                             sd=gSd[i] * 2)
      intensity <- r[TypeII$AddressA, i] + g[TypeII$AddressA, i]
      detP[TypeII$Name, i] <- 1 - pnorm(intensity,
                                        mean=rMu[i] + gMu[i],
                                        sd=rSd[i] + gSd[i])
    }
    detP
  }
