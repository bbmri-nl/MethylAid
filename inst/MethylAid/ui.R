
## ##construct container for the outliers
## outliers <- matrix(FALSE, nrow=nrow(exampleData@targets), ncol=5,
##                    dimnames=list(row.names(exampleData@targets),
##                      c("MU", "BS", "OP", "HC", "DP")))

## assign("outliers", outliers, envir=globalenv())

MethylAid:::ui450k(exampleData)
