##' summarize is the main function when called all samples in the targets file
##' will be summarized
##'
##' By default the summarization is performed on all data at once. Optionally
##' the data can be summarized
##' in batches using the batchSize option. Summarization of data can be
##' performed in parallel as well see
##' the MethylAid vignette for examples.
##' @title summarization of Illumina Human DNA Methylation array data
##' @param targets valid minfi targets file
##' @param batchSize the size of each the batch
##' @param BPPARAM see bpparam()
##' @param rp.zero Default TRUE replaces zero intensity values with NA's
##' @param verbose default is TRUE
##' @param file if given summarized data is stored as RData object
##' @export
##' @import minfi 
##' BiocParallel
##' @importFrom matrixStats colMedians colMads
##' @importFrom Biobase AnnotatedDataFrame
##' @importFrom SummarizedExperiment colData
##' @return summarized data is saved optionally returned
##' @author mvaniterson
##' @examples
##' library(minfiData)
##' baseDir <- system.file("extdata", package="minfiData")
##' targets <- read.metharray.sheet(baseDir)
##' data <- summarize(targets)
summarize <- function(targets, batchSize=-1, BPPARAM=NULL, rp.zero=TRUE,
                      verbose=TRUE, file=NULL) {
    if(verbose)
        message(paste("Start summarization ..."))

    ##add column None default coloring
    targets$None <- 1

    if(is.null(BPPARAM))
        {
            if(batchSize == -1)
                sData <- summarizeWholeBunch(targets, rp.zero, verbose)
            else
                sData <- summarizePerBatch(targets, batchSize, rp.zero, verbose)
        }
    else
        sData <- summarizeParallel(targets, batchSize, BPPARAM, rp.zero, verbose)

    ##prepare data for plotting  which speeds up the plotting in the shiny app
    if(verbose)
        message("Prepare data for plotting ...")
    sData@plotdata <- prepareData(sData)

    if(!is.null(file))
        {
            if(verbose)
                message("Saving results ...")

            ##maybe check if there is no extention
            assign(basename(file), sData)
            save(list=basename(file), file=paste0(file, ".RData"))
            message(paste("Summarized data stored: ", paste0(file, ".RData")))
        }

    if(verbose)
        message(paste("... Finished summarization."))

    invisible(sData)
}

summarizeWholeBunch <- function(targets, rp.zero, verbose)  {
    if(verbose)
        message("Summarize data in one go...")

    RGset <- read.metharray.exp(targets=targets)

    ##Set 0.0 to NA
    if(rp.zero)
        RGset <- replaceZero(RGset)

    ##calculate detection p-value and frequency of probe passing threshold
    DP <- detectionP(RGset, na.rm=TRUE)
    DPfreq <- colSums(DP < 0.01, na.rm=TRUE)/nrow(DP)

    ##summarize R and G channels control probes
    RG <- summarizeControls(RGset)

    ##summarize M and U values
    MU <- summarizeMUvalues(RGset)

    ##convert all columns to factors this is convenient for plotting
    if(nrow(targets) > 1)
        targets <- data.frame(apply(targets, 2, function(x)
            factor(as.character(x))),
                              row.names=row.names(targets))

    ##add row names
    rownames(targets) <- colnames(MU)

    sData <- new("summarizedData",
                 targets=targets,
                 controls=RG$TypeControl,
                 Rcontrols=as.matrix(RG$R),
                 Gcontrols=as.matrix(RG$G),
                 DPfreq=DPfreq,
                 MU=MU)
    sData
}

summarizePerBatch <- function(targets, batchSize, rp.zero, verbose){
    if(verbose)
        message("Summarize data in batches...")

    R <- G <- MU <- DPfreq <- c()
    tg <- targets
    while(nrow(tg) > 0)
        {
            ss <- 1:min(batchSize, nrow(tg))

            if(verbose)
                message(paste("Summarizing", length(ss), "samples..."))

            RGset <- read.metharray.exp(targets=tg[ss,])         

            ##Set 0.0 to NA
            if(rp.zero)
                RGset <- replaceZero(RGset)

            ##calculate detection p-value and frequency of probe passing threshold
            DP <- detectionP(RGset, na.rm=TRUE)
            DPfreq <- c(DPfreq, colSums(DP < 0.01, na.rm=TRUE)/nrow(DP))

            ##summarize R and G channels control probes
            RG <- summarizeControls(RGset)
            R <- cbind(R, RG$R)
            G <- cbind(G, RG$G)

            ##summarize M and U values
            MU <- cbind(MU, summarizeMUvalues(RGset))

            tg <- tg[-ss,]
        }

    ##convert all columns to factors this is convenient for plotting
    if(nrow(targets) > 1)
        targets <- data.frame(apply(targets, 2, function(x)
            factor(as.character(x))),
                              row.names=row.names(targets))

    ##add row names
    rownames(targets) <- colnames(R) <- colnames(G) <- colnames(MU)##!!!

    sData <- new("summarizedData",
                 targets=targets,
                 controls=RG$TypeControl,
                 Rcontrols=R,
                 Gcontrols=G,
                 DPfreq=DPfreq,
                 MU=MU)
    sData
}

summarizeParallel <- function(targets, batchSize,  BPPARAM, rp.zero, verbose) {
    if(verbose)
        message("Summarize data in parallel...")

    nworkers <- bpworkers(BPPARAM)

    y <- rep(1:nworkers, nrow(targets))
    y <- y[1:nrow(targets)]
    jobs <- split(targets, y)

    if(batchSize == -1)
        sumParallel <- function(x)
            summarizeWholeBunch(x, rp.zero=rp.zero, verbose=verbose)
    else
        sumParallel <- function(x)
            summarizePerBatch(x, batchSize=batchSize, rp.zero=rp.zero, verbose=verbose)

    ##using BiocParallel optionally can be run using batch jobs schedulers
    res <- bplapply(jobs, FUN=sumParallel, BPPARAM=BPPARAM)

    sData <- reduce(res)

    sData
}
