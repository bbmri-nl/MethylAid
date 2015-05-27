##initializer
initialize <- function(object, output, thresholds, background, ...)
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

    ##generate background data and make it accessible for the plotting functions
    if(!is.null(background)) {
      assign("background", as.background(background), envir = globalenv())
      output$showBackground <- renderUI({
        selectInput("showBackground", "Show background:", c("Off", "On"), selected="On")
      })
    }

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

server450k <- function(object, thresholds, background, ...)
  {
    function(input, output, session)
      {
        ##initialize to get all outliers detected do this only once
        initialize(object, output, thresholds, background, ...)

        ##on exit call finalize
        session$onSessionEnded(function() { stopApp(returnValue=finalize(object)) })

        getTabName <- reactive({
          switch(input$pnlMain,
                 `FC`= input$pnlFC,
                 `SDC`=input$pnlSDC,
                 `SIC`=input$pnlSIC,
                 `OT`="",
                 `About`="")
        })

        clickMU <- reactive({
          location <- list(x=input$clickMU$x, y=input$clickMU$y)
          assign("location", location, envir=globalenv())
        })

        clickOP <- reactive({
          location <- list(x=input$clickOP$x, y=input$clickOP$y)
          assign("location", location, envir=globalenv())
        })

        clickBS <- reactive({
          location <- list(x=input$clickBS$x, y=input$clickBS$y)
          assign("location", location, envir=globalenv())
        })

        clickHC <- reactive({
          location <- list(x=input$clickHC$x, y=input$clickHC$y)
          assign("location", location, envir=globalenv())
        })

        clickDP <- reactive({
          location <- list(x=input$clickDP$x, y=input$clickDP$y)
          assign("location", location, envir=globalenv())
        })

        getPlotType <- reactive({
          switch(input$pnlMain,
                 `SDC`= input$plotType,
                 `SIC`= input$plotType,
                 "scatter")
        })

        getBackground <- reactive({
          showBackground <- FALSE
          if(exists("background", envir=globalenv()) & !is.null(input$showBackground)) {
            showBackground <- switch(input$showBackground,
                                     "On" = TRUE,
                                     "Off" = FALSE)
          }
          return(showBackground)
        })

        showOutliers <- reactive({
          return(switch(input$showOutliers, "On" = TRUE, "Off" = FALSE))
        })

        getThreshold <- reactive({
            tabName <- getTabName()
            if(any(tabName %in% names(thresholds)))
              return(thresholds[[tabName]])
            return(NULL)
          })

        getPlotArguments <- function()
          {

            clickMU()
            clickOP()
            clickBS()
            clickHC()
            clickDP()

            ##construct plotting arguments
            args <- list()
            args$object <- object
            args$col <- input$colorby ##reactive on metadata
            args$threshold <- getThreshold()
            args$showOutliers <- showOutliers() ##reactive on outliers checkbox
            args$plotType <- getPlotType() ##reactive on quality control display type
            args$plotName <- getTabName() ##reactive on tab panel switching
            args$background <- getBackground()
            args
          }

        ##create plot
        observe({
          output[[paste0("plot", getTabName())]] <- renderPlot({ do.call(qcplot, getPlotArguments()) })
        })

        getOutliers <- function(){
          dt <- get("outliers", envir = globalenv())
          if(sum(rowSums(dt) > 0) == 0)
            dt <- NULL
          else
            {
              dt <- dt[rowSums(dt) > 0,,drop=FALSE] ##filter
              dt <- cbind(ID=rownames(dt), dt)
            }
          dt
        }

        output$OutlierTable <- renderDataTable({ getOutliers() })

        output$downloadPlot <- downloadHandler(
          filename=function() {
            plotName <- paste0("plot", getTabName())
            paste0(plotName, ".pdf")
          },
          content=function(file) {
            args <- getPlotArguments()
            pdf(file, width=7, height = 7/2)
            do.call(qcplot, args)
            dev.off()
          })

        output$downloadData <- downloadHandler(
          filename=function() { "MethylAid_outliers.csv" },
          content=function(file) {
            dt <- getOutliers()
            if(!is.null(dt))
              write.csv(getOutliers(), file)
          })

        ##generate report
        ## output$downloadReport <- downloadHandler(
        ##                                          filename = "MethylAid-report.pdf",
        ##                                          content = function(file) {
        ##                                            src <- file.path(path.package("MethylAid"), "Report")
        ##                                            ## temporarily switch to the temp dir, in case you do not have write
        ##                                            ## permission to the current working directory
        ##                                            dir.create(file.path(tempdir(), basename(src)))
        ##                                            file.copy(src, tempdir(), recursive=TRUE)
        ##                                            owd <- setwd(file.path(tempdir(), basename(src)))
        ##                                            on.exit(setwd(owd))
        ##                                            out <- knit("report.Rnw")
        ##                                            tools::texi2pdf(out)
        ##                                            file.rename("report.pdf", file)
        ##                                          })

      }
  }
