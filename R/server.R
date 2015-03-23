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
    args$showOutliers <- FALSE
    args$plotType <- NULL

    ##detect outliers
    tmp <- tempfile("plot", fileext = ".pdf")
    pdf(tmp)
    for(plotName in names(thresholds))
      {
        args$plotName <- plotName
        args$threshold <- thresholds[[plotName]]
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

        getTabName <- reactive({
          switch(input$pnlMain,
                 `FC`= input$pnlFC,
                 `SDC`=input$pnlSDC,
                 `SIC`=input$pnlSIC,
                 `Outliers`="",
                 `About`="")
        })

        clickIdMU <- reactive({
          location <- list(x=input$clickIdMU$x, y=input$clickIdMU$y)
          assign("location", location, envir=globalenv())
        })

        clickIdOP <- reactive({
          location <- list(x=input$clickIdOP$x, y=input$clickIdOP$y)
          assign("location", location, envir=globalenv())
        })

        clickIdBS <- reactive({
          location <- list(x=input$clickIdBS$x, y=input$clickIdBS$y)
          assign("location", location, envir=globalenv())
        })

        clickIdHC <- reactive({
          location <- list(x=input$clickIdHC$x, y=input$clickIdHC$y)
          assign("location", location, envir=globalenv())
        })

        clickIdDP <- reactive({
          location <- list(x=input$clickIdDP$x, y=input$clickIdDP$y)
          assign("location", location, envir=globalenv())
        })

        getPlotType <- reactive({
          switch(input$pnlMain,
                 `SDC`= input$plotType,
                 `SIC`= input$plotType,
                 "scatter")
        })

        getBackground <- reactive({
          if(exists("background", envir=globalenv()))
            return(input$background)
          else
            return(FALSE)
        })

        getPlotArguments <- function()
          {

            clickIdMU()
            clickIdOP()
            clickIdBS()
            clickIdHC()
            clickIdDP()

            ##construct plotting arguments
            args <- list()
            args$object <- object
            args$col <- input$colorby ##reactive on metadata
            args$threshold <- thresholds[[getTabName()]]
            args$showOutliers <- input$showOutliers ##reactive on outliers checkbox
            args$plotType <- getPlotType() ##reactive on quality control display type
            args$plotName <- getTabName() ##reactive on tab panel switching
            args$background <- getBackground()
            args
          }

        ##create plot
        observe({

          args <- getPlotArguments()
          plotName <- paste0("plot", args$plotName)

          ##optionally save plot
          output$save <- downloadHandler(
            filename=function() {
              paste0(plotName, ".pdf")
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
          output[[plotName]] <- renderPlot({ do.call(qcplot, args) })
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
