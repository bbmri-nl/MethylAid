qcscatterplot <- function(object, plotType, showOutliers)
  {
    data <- object@plotdata
    data <- data[grepl(qcProbes[plotType], data$Type),]
    data <- droplevels(data)

    gp <- ggplot(data, aes_string(x="IntRed", y="IntGrn", colour = "ExtendedType"))
    gp <- gp + geom_point()
    gp <- gp + ggtitle(unique(data$Type))
    gp <- gp + xlab(expression(paste(log[2], "Probe Intensity (Red)")))
    gp <- gp + ylab(expression(paste(log[2], "Probe Intensity (Green)")))

    if(exists("highlight", envir=globalenv()))
      {
        highlight <- get("highlight", envir=globalenv())
        id <- as.character(data$Samples) == highlight
        gp <- gp + geom_point(data = data[id,],
                              aes_string(x="IntRed", y="IntGrn"),
                              colour="black", size = 4, shape = 4)
      }

    if(showOutliers)
      {
        outliers <- getOutliers(as.character(data$Samples))
        if(any(outliers))
          gp <- gp + geom_point(data = data[outliers,],
                                aes_string(x="IntRed", y="IntGrn"),
                                colour="black", size = 3, shape = 8)
      }

    gp
  }

qcsampleplot <- function(object, plotType, showOutliers)
  {
    data <- object@plotdata
    d <- data[grepl(qcProbes[plotType], data$Type),]

    dGrn <- d[,c(1:5, 7)]
    dGrn[,"Channel"] <- "Grn"
    dRed <- d[, c(1:6)]
    dRed[,"Channel"] <- "Red"
    colnames(dGrn)[6] <- colnames(dRed)[6]  <- "Intensity"
    data <- rbind(dGrn, dRed)

    data$Samples <- as.character(data$Samples)
    sampleNames <- data$Samples

    ##a la GenomeStudio when the number of samples less then 100
    if(length(unique(sampleNames)) > 100)
      data$Samples <- 1:nrow(data)

    gp <- ggplot(data, aes_string(x="Samples", y="Intensity", colour = "ExtendedType"))
    gp <- gp + geom_point()
    gp <- gp + ggtitle(unique(data$Type))
    gp <- gp + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    gp <- gp + facet_grid(.~Channel, scales="free")
    gp <- gp + ylab(expression(paste(log[2], "Probe Intensity")))
    gp <- gp + xlab("")

    if(exists("highlight", envir=globalenv()))
      {
        highlight <- get("highlight", envir=globalenv())
        id <- sampleNames == highlight
        gp <- gp + geom_point(data = data[id,],
                              aes_string(x = "Samples", y = "Intensity"),
                              shape=4, colour="black", size = 4)
      }

    if(showOutliers)
      {
        outliers <- getOutliers(sampleNames)
        if(any(outliers))
          gp <- gp + geom_point(data = data[outliers,],
                                aes_string(x = "Samples", y = "Intensity"),
                                shape = 8, colour="black", size = 3)
      }

    gp
  }

qcboxplot <- function(object, plotType, showOutliers)
  {
    data <- object@plotdata
    d <- data[grepl(qcProbes[plotType], data$Type),]
    dGrn <- d[,c(1:5, 7)]
    dGrn[,"Channel"] <- "Grn"
    dRed <- d[, c(1:6)]
    dRed[,"Channel"] <- "Red"
    colnames(dGrn)[6] <- colnames(dRed)[6]  <- "Intensity"
    data <- rbind(dGrn, dRed)

    gp <- ggplot(data,
                 aes_string(x = "Channel", y = "Intensity", colour = "ExtendedType"))
    gp <- gp + geom_boxplot()
    gp <- gp + ggtitle(unique(data$Type))
    gp <- gp + ylab(expression(paste(log[2], "Probe Intensity")))

    if(exists("highlight", envir=globalenv()))
      {
        highlight <- get("highlight", envir=globalenv())
        id <- as.character(data$Samples) == highlight
        gp <- gp + geom_point(data = data[id,],
                              aes_string(x = "Channel", y = "Intensity"),
                              colour="black", size = 4.5, shape = 4)
      }

    if(showOutliers)
      {
        outliers <- getOutliers(as.character(data$Samples))
        if(any(outliers))
          gp <- gp + geom_point(data = data[outliers,],
                                aes_string(x = "Channel", y = "Intensity"),
                                colour="black", size = 3, shape = 8)
      }

    gp
  }
