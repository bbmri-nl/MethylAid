summarizeControls <- function(RGset, array)  {
    
    ##control probe information
    TypeControl <- getProbeInfo(RGset, type = "Control")
    TypeControl <- as.data.frame(TypeControl)
    colnames(TypeControl)
    
    R <- getRed(RGset)
    G <- getGreen(RGset)

    ##maybe notify when controls are not on the array
    id <- intersect(TypeControl$Address, rownames(R))
    R <- R[rownames(R) %in% id,]
    G <- G[rownames(G) %in% id,]
    TypeControl <- TypeControl[TypeControl$Address %in% id,]
    TypeControl <- TypeControl[order(TypeControl$Address), ]

    list(TypeControl=TypeControl, R=R, G=G)
}

summarizeMUvalues <- function(RGset)
    {
        MU <- matrix(0.0, nrow=2, ncol=ncol(RGset))
        MUset <- preprocessRaw(RGset)
        M <- getMeth(MUset)
        MU[1,] <- colMedians(M, na.rm=TRUE)
        U <- getUnmeth(MUset)
        MU[2,] <- colMedians(U, na.rm=TRUE)
        colnames(MU) <- colnames(RGset)
        rownames(MU) <- c("Methylated", "Unmethylated")
        MU
    }
