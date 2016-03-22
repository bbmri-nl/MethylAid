ggplot2base <- function(x, y, bg, ...) {
    op <- par(mar=c(5, 5, 3, 0), bg="white")
    plot(x, y, xaxt="n", yaxt="n", pch=16, bty="n", type="p",...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col="grey90",
         border=NA)

    ##add smooth background
    if(!is.null(bg)) {

            ## smoothScatter(bg$x, bg$y, colramp = colorRampPalette(c("grey90", "blue")),
            ##                nrpoints=0, nbin=256, add=TRUE,
            ##                xaxt="n", yaxt="n", postPlotHook=NULL, bty="n", ...)

            h <- hexbin(bg$x, bg$y)
            vps <- baseViewports()
            pushViewport(vps$inner, vps$figure, vps$plot)
            grid.hexagons(h, colramp = colorRampPalette(blues9[-9]))
            popViewport()

        }

    xax <- axTicks(1)
    axis(1, at=xax, lwd=0, lwd.ticks=1,
         col.axis="darkgrey", col.ticks="darkgrey",
         cex=0.5)

    yax <- axTicks(2)
    axis(2, at=yax, lwd=0, lwd.ticks=1,
         col.axis="darkgrey",
         col.ticks="darkgrey",
         las=2, cex=0.5)

    abline(h=yax, col="white", lwd=1.3)
    abline(h=yax[-length(yax)] + diff(yax)[1]/2, col="white", lwd=0.5)
    abline(v=xax, col="white", lwd=1.3)
    abline(v=xax[-length(xax)] + diff(xax)[1]/2, col="white", lwd=0.5)

    points(x, y, pch=1, ...)
    par(op)
}

gcscatterplot <- function(data, x, y, bg=NULL, col="None",
                          xlab=NULL, ylab=NULL,
                          threshold, thrtype=c("x", "y"),
                          showOutliers=TRUE,
                          xlim, ylim, main) {

    colors <- colorRampPalette(brewer.pal(9,"Blues"))

    ##because of merge can introduce "colnames.y"
    cols <- fcols <- factor(data[, 2+pmatch(col, colnames(data)[-c(1:2)])])

    if((nlevels(cols) == length(cols) | nlevels(cols) == 1) & length(cols) > 25) 
        cols <- 1            
    else if(nlevels(cols) < 10) ##qualitative colors minimal is three
        levels(cols) <- brewer.pal(max(3, nlevels(cols)),"Set1")[1:nlevels(cols)]
    else
        levels(cols) <- colors(nlevels(cols))

    if(thrtype == "x") {
            xlim <- range(c(data[,x], threshold, bg$x), na.rm=TRUE)
            ylim <- range(c(data[,y], bg$y), na.rm=TRUE)
        }
    else {
            xlim <- range(data[,x], na.rm=TRUE)
            ylim <- range(c(data[,y], threshold), na.rm=TRUE)
        }

    ggplot2base(data[,x], data[,y],
                bg,
                xlab=xlab, ylab=ylab,
                col=as.character(cols),
                xlim=xlim, ylim=ylim,
                main=main, cex.lab=2)

    if(thrtype == "x")
        abline(v=threshold, col=1, lty=2)
    else
        abline(h=threshold, col=1, lty=2)

    if(nlevels(fcols) > 1 & nlevels(fcols) < 25 )
        legend("topleft", levels(fcols), ncol=nlevels(fcols)%/%15 + 1, pch=1,
               text.col=levels(cols), col=levels(cols), title=col,
               title.col=1, bty="n")

    if(exists("highlight", envir=globalenv())) {
            highlight <- get("highlight", envir=globalenv())
            id <- data$Row.names == highlight
            points(data[id, x], data[id, y], cex=2, col=2, pch=4)
            dx <- 0.025*(par("usr")[2]-par("usr")[1])
            dy <- 0.025*(par("usr")[4]-par("usr")[3])
            text(data[id, x]+dx, data[id, y]+dy, data$Row.names[id], col=2)
        }

    if(showOutliers) {
            outliers <- getOutliers(data$Row.names)
            if(any(outliers))
                points(data[outliers, x], data[outliers, y], cex=2, col=2, pch=8)
        }
}

##MA like plot
rotateData <- function(data, columns) {
    data[,columns] <- c(0.5*(data[,columns[1]] + data[,columns[2]]),
                        data[,columns[1]] - data[,columns[2]])
    data
}

plotMU <- function(object, col, threshold, showOutliers=FALSE, background=FALSE) {

    MU <- log2(t(object@MU))
    targets <- object@targets
    data <- merge(MU, targets, by="row.names")
    data <- rotateData(data, columns=c("Methylated", "Unmethylated"))

    if(background)
        bg <- get("background", envir=globalenv())[["MU"]]
    else
        bg <- NULL

    if(exists("location", envir=globalenv())) {
            x <- data$Methylated
            y <- data$Unmethylated
            names(x) <- data$Row.names
            setHighlight(x, y)
        }

    outliers <- data$Row.names[data$Methylated <= threshold]
    setOutliers(outliers, type="MU")

    ##plot
    gcscatterplot(data, x="Methylated", y="Unmethylated",
                  bg,
                  col=col, threshold=threshold, thrtype="x",
                  showOutliers=showOutliers,
                  xlab=expression(paste(log[2], sqrt(M%*%U))),
                  ylab=expression(paste(log[2],"(", M/U, ")")),
                  main="rotated MU plot")
}

plotOP <- function(object, col, threshold, showOutliers=FALSE, background=FALSE) {

    data <- object@plotdata
    
    d <- data[grepl(qcProbes["NP"], data$Type),]

    dGrn <- d[d$ExtendedType %in% c("NP (C)", "NP (G)"), c(1:5,7)]        
    x <- tapply(dGrn$IntGrn, dGrn$Samples, mean)

    dRed <- d[d$ExtendedType %in% c("NP (A)", "NP (T)"), c(1:6)]    
    y <- tapply(dRed$IntRed, dRed$Samples, mean)

    data <- data.frame(x, y)
    targets <- object@targets
    data <- merge(data, targets, by="row.names", suffixes=c("", ".y")) ##as we expect x and y

    data <- rotateData(data, columns=c("x", "y"))

    if(background)
        bg <- get("background", envir=globalenv())[["NP"]]
    else
        bg <- NULL


    if(exists("location", envir=globalenv())) {
        x <- data$x
        y <- data$y
        names(x) <- data$Row.names
        setHighlight(x, y)
    }

    outliers <- data$Row.names[data$x <= threshold]
    setOutliers(outliers, type="OP")

    p <- gcscatterplot(data, x="x", y="y",
                       bg,
                       col=col, threshold=threshold, thrtype="x",
                       showOutliers=showOutliers,
                       ylab=expression(paste(log[2],"(", R/G, ")")),
                       xlab=expression(paste(log[2], sqrt(R%*%G))),
                       main="Sample-dependent overall quality control (NP)")
}

plotBS <- function(object, col, threshold, showOutliers=FALSE, background=FALSE) {
    data <- object@plotdata
    d <- data[grepl(qcProbes["BSI"], data$Type),]

    dGrn <- d[grepl("C1|C2|C3", d$ExtendedType), c(1:5,7)]
    x <- tapply(dGrn$IntGrn, dGrn$Samples, mean)

    dRed <- d[grepl("C4|C5|C6", d$ExtendedType), c(1:6)]
    y <- tapply(dRed$IntRed, dRed$Samples, mean)

    data <- data.frame(x, y)
    targets <- object@targets
    data <- merge(data, targets, by="row.names", suffixes=c("", ".y")) ##as we expect x and y

    data <- rotateData(data, columns=c("x", "y"))

    if(background)
        bg <- get("background", envir=globalenv())[["BSI"]]
    else
        bg <- NULL

    if(exists("location", envir=globalenv())) {
        x <- data$x
        y <- data$y
        names(x) <- data$Row.names
        setHighlight(x, y)
    }

    outliers <- data$Row.names[data$x <= threshold]
    setOutliers(outliers, type="BS")

    p <- gcscatterplot(data, x="x", y="y",
                       bg,
                       col=col, threshold=threshold, thrtype="x",
                       showOutliers=showOutliers,
                       ylab=expression(paste(log[2],"(", R/G, ")")),
                       xlab=expression(paste(log[2], sqrt(R%*%G))),
                       main="Bisulfite Conversion I quality control")
}

plotHC <- function(object, col, threshold, showOutliers=FALSE, background=FALSE) {
    data <- object@plotdata
    d <- data[grepl(qcProbes["HYB"], data$Type),]
    d <- d[order(d$Samples),]
    x <- 0.5*(d$IntGrn[grepl("High", d$ExtendedType)] + d$IntGrn[grepl("Low", d$ExtendedType)])
    y <- d$IntGrn[grepl("High", d$ExtendedType)] - d$IntGrn[grepl("Low", d$ExtendedType)]

    data <- data.frame(x, y, row.names=d$Samples[grepl("High", d$ExtendedType)])
    targets <- object@targets
    data <- merge(data, targets, by="row.names", suffixes=c("", ".y")) ##as we expect x and y

    if(background)
        bg <- get("background", envir=globalenv())[["HYB"]]
    else
        bg <- NULL

    if(exists("location", envir=globalenv())) {
        x <- data$x
        y <- data$y
        names(x) <- data$Row.names
        setHighlight(x, y)
    }

    outliers <- data$Row.names[data$x <= threshold]
    setOutliers(outliers, type="HC")

    p <- gcscatterplot(data, x="x", y="y",
                       bg,
                       col=col, threshold=threshold, thrtype="x",
                       showOutliers=showOutliers,
                       ylab=expression(paste(log[2],"(", H/L, ")")),
                       xlab=expression(paste(log[2], sqrt(H%*%L))),
                       main="Sample-independent overall quality control (Hyb)")

}

plotDP <- function(object, col, threshold, showOutliers=FALSE, background=FALSE) {
    y <- object@DPfreq
    x <- 1:length(y)
    data <- data.frame(x, y, row.names=names(y))
    targets <- object@targets
    data <- merge(data, targets, by="row.names", suffixes=c("", ".y")) ##as we expect x and y

    if(exists("location", envir=globalenv())) {
        x <- data$x
        y <- data$y
        names(x) <- data$Row.names
        setHighlight(x, y)
    }

    outliers <- data$Row.names[data$y <= threshold]
    setOutliers(outliers, type="DP")

    p <- gcscatterplot(data, x="x", y="y",
                       col=col, threshold=threshold, thrtype="y",
                       bg = NULL, ##do not show a background for the detection p-values
                       showOutliers=showOutliers,
                       xlab="Samples",
                       ylab="#probes P-value < 0.01",
                       main="Detection p-value based on negative control probes")

}
