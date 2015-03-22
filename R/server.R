##initializer
initialize <- function(object, output, thresholds, background)
  {
    ##cat("initialize...\n")

    ##construct container for the outliers
    outliers <- matrix(FALSE, nrow=nrow(object@targets), ncol=5,
                       dimnames=list(row.names(object@targets),
                         names(thresholds)))

    assign("outliers", outliers, envir = globalenv())

    args <- list()
    args$object <- object
    args$col <- "None"
    args$location <- list(x=NULL, y=NULL)
    args$outliers <- FALSE
    args$type <- NULL

    ##detect outliers
    tmp <- tempfile("plot", fileext = ".pdf")
    pdf(tmp)
    for(plotType in names(thresholds))
      {
        args$plotType <- plotType
        args$threshold <- thresholds[[plotType]]
        plotType <- paste0("plot", plotType)
        do.call(qcplot, args)
      }
    dev.off()
    file.remove(tmp)

    ##generate background data and make it accesible for the plotting functions
    if(!is.null(background))
      assign("background", as.background(background), envir = globalenv())

  }

##finalizer
finalize <- function(object)
  {
    ##cat("finalize...\n")

    ##get outliers
    outliers <- get("outliers", envir = globalenv())

    ##clear global envirnoment
    for(obj in c("outliers", "highlight", "background"))
      {
        if(exists(obj, envir = globalenv()))
          rm(list=obj, envir = globalenv())
      }

    ##return outliers with information from targets file
    targets <- object@targets
    outliers <- targets[rownames(targets) %in%
                        rownames(outliers[rowSums(outliers) > 0,,
                                          drop=FALSE]),]

    return(outliers)
  }

server450k <- function(object, thresholds, background)
  {

    function(input, output, session)
      {
        ##initialize to get all outliers detected do this only once
        initialize(object, output, thresholds, background)

        getTabName <- function()
          {
            switch(input$pnlMain,
                   `FC`= input$pnlFC,
                   `SDC`=input$pnlSDC,
                   `SIC`=input$pnlSIC,
                   `Outliers`="",
                   `Help`="")
          }

        getLocation <- function()
          {
            if(is.null(input$clickIdMU) & is.null(input$clickIdOP) &
               is.null(input$clickIdBS) & is.null(input$clickIdHC) &
               is.null(input$clickIdDP))

              location <- list(x=NULL, y=NULL)

            location <- switch(getTabName(),
                               MU=list(x=input$clickIdMU$x, y=input$clickIdMU$y),
                               OP=list(x=input$clickIdOP$x, y=input$clickIdOP$y),
                               BS=list(x=input$clickIdBS$x, y=input$clickIdBS$y),
                               HC=list(x=input$clickIdHC$x, y=input$clickIdHC$y),
                               DP=list(x=input$clickIdDP$x, y=input$clickIdDP$y))

            return(location)
          }

        getPlotType <- function()
          {
            switch(input$pnlMain,                   
                   `SDC`= input$plotType,
                   `SIC`= input$plotType,
                   "scatter")
          }

        getBackground <- function()
          {
            if(exists("background", envir=globalenv()))
              return(input$background)
            else
              return(FALSE)
          }

        getPlotArguments <- function()
          {
            ##construct plotting arguments
            args <- list()
            args$object <- object
            args$col <- input$colorby ##reactive on metadata
            args$location <- getLocation() ##reactive of mouse clicking
            args$threshold <- thresholds[[getTabName()]]
            args$outliers <- input$outliers ##reactive on outliers checkbox
            args$type <- getPlotType() ##reactive on quality control display type
            args$plotType <- getTabName() ##reactive on tab panel switching
            args$background <- getBackground()
            args
          }

        ##create plot
        observe({

          args <- getPlotArguments()
          plotType <- paste0("plot", args$plotType)

          ##optionally save plot
          output$save <- downloadHandler(
            filename=function() {
              paste0(plotType, ".pdf")
            },
            content=function(file) {
              message(paste("Saving ..."))
              ##this doesn't work
              ##ggsave(filename=file,
              ##plot=do.call(qcplot, args),
              ##type="cairo-png")
              pdf(file, width=7, height = 7/2)
              message(do.call(qcplot, args))
              dev.off()
            },
            ##contentType='image/png'
            )

          ##do the plotting
          output[[plotType]] <- renderPlot({ do.call(qcplot, args) })
        })

        ##show outliers
        output$Outliers <- renderDataTable({

          dt <- get("outliers", envir = globalenv())
          if(sum(rowSums(dt) > 0) == 0)
            return(NULL)

          dt <- dt[rowSums(dt) > 0,,drop=FALSE] ##filter
          return(cbind(ID=rownames(dt), dt))
        })

        ##Stop App
        observe({
          if (input$exit == 0)
            return(NULL)
          stopApp(returnValue=finalize(object))
        })

      }
  }
