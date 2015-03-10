replaceZero <- function(RGset)
  {
    ##replace zero intensity values of all probes in the Red and Green
    ##channels with NA's
    Red <- getRed(RGset)
    Green <- getGreen(RGset)

    ##replace the zeros
    Red[which(Red == 0)] <- NA
    Green[which(Green == 0)] <- NA

    ##construct new RGChannelSet
    pd <- pData(RGset)
    meta <- varMetadata(RGset)
    RGset <- RGChannelSet(Green = Green, Red = Red,
                          pheno = AnnotatedDataFrame(pd, meta))
    RGset@annotation =  "IlluminaHumanMethylation450k"

    ##return new RGChannelSet
    RGset
  }

replaceZeroInfo <- function(RGset)
  {
    types <-  c("I", "II", "Control", "I-Green", "I-Red", "SnpI", "SnpII")
    channels <- c("Red", "Green")
    NumberOfNAs <- matrix(0, nrow=7, ncol=2)

    for(i in 1:length(types))
      {
        ##get address of probes
        info <- getProbeInfo(RGset, type=types[i])
        address <- grep("Address", colnames(info))

        address <- ifelse(length(address) == 1, info[,address],
                          unlist(info[, address]@listData)) ##for Type I probes

        address <- as.integer(address)

        for(j in 1:length(channels))
          {
            intensity <- ifelse(channels[j] == "Green",
                                getGreen(RGset),
                                getRed(RGset))

            ##which rows contain NAs
            RowsWithNAs <- apply(intensity, 1, anyNA)

            ##which columns
            ##ColsWithNAs <- apply(intensity[RowsWithNAs,], 1, function(x)
            ## paste(which(is.na(x)), sep=":"))
            ##concatenate multiple sample using :
            ##summarize probe type
            ##sumprobes <- paste(names(ColsWithNAs), ColsWithNAs,
            ##sep="-", collapse=", ")
            ##print(sumprobes)

            ##count the NAs overlapping channel address of probes
            NumberOfNAs[i,j] <- sum(names(which(RowsWithNAs)) %in% address)
          }

        rownames(NumberOfNAs) <- types
        colnames(NumberOfNAs) <- channels
      }
    NumberOfNAs
  }
