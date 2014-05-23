summarizeControls <- function(RGset, array)
  {
    ##control probe information
    data(hm450.controls, package="FDb.InfiniumMethylation.hg19")

    R <- getRed(RGset)
    G <- getGreen(RGset)

    ##maybe notify when controls are not on the array
    id <- intersect(hm450.controls$Address, rownames(R))
    R <- R[rownames(R) %in% id,]
    G <- G[rownames(G) %in% id,]
    hm450.controls <- hm450.controls[hm450.controls$Address %in% id,]
    hm450.controls <- hm450.controls[order(hm450.controls$Address), ]

    list(hm450.controls=hm450.controls, R=R, G=G)
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
