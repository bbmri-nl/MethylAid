
##initializer
initialize <- function(object, output)
  {
    ##cat("initialize...\n")
    
    ##construct container for the outliers
    outliers <- matrix(FALSE, nrow=nrow(object@targets), ncol=5,
                       dimnames=list(row.names(object@targets),
                         names(threshold.defaults)))   
    
    assign("outliers", outliers, envir = globalenv())     
    
    args <- list()
    args$object <- object
    args$col <- "None"
    args$location <- list(x=NULL, y=NULL)
    args$outliers <- FALSE
    args$type <- NULL
    
    for(plotType in names(threshold.defaults))
      {
        args$plotType <- plotType
        args$threshold <- threshold.defaults[[plotType]]
        plotType <- paste0("plot", plotType)
        output[[plotType]] <- renderPlot({ do.call(qcplot, args) })
      }   
  }

##finalizer
finalize <- function(object)
  {
    ##cat("finalize...\n") 

    ##get outliers 
    outliers <- get("outliers", envir = globalenv())
    
    ##clear global envirnoment
    for(obj in c("outliers", "highlight"))
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

server450k <- function(object)
  {

    function(input, output, session)
      {       
        ##initialize to get all outliers detected do this only once
        initialize(object, output)     
                      
        getTabName <- function()
          {
            switch(input$mainPanel,
                   `Filter controls`= switch(input$fcPanel,
                     `rotated M vs U plot`="MU",
                     `Overall sample-dependent control plot`="OP",
                     `Bisulfite conversion control plot`="BS",
                     `Overall sample-independent control plot`="HC",
                     `Detection P-value plot`="DP"),
                   `Sample-dependent controls`=input$sdcPanel,
                   `Sample-independent controls`=input$sicPanel,
                   `Outliers`="",
                   `Help`="")
          }

        getLocation <- function()
          {
            if(is.null(input$clickIdMU) & is.null(input$clickIdOP) &
               is.null(input$clickIdBS) & is.null(input$clickIdHC) &
               is.null(input$clickIdDP))
              return(list(x=NULL, y=NULL))

            location <- switch(getTabName(),
                               MU=list(x=input$clickIdMU$x, y=input$clickIdMU$y),
                               OP=list(x=input$clickIdOP$x, y=input$clickIdOP$y),
                               BS=list(x=input$clickIdBS$x, y=input$clickIdBS$y),
                               HC=list(x=input$clickIdHC$x, y=input$clickIdHC$y),
                               DP=list(x=input$clickIdDP$x, y=input$clickIdDP$y))
            return(location)
          }

        getThreshold <- function(tabName)
          {
            if(missing(tabName))
              tabName <- getTabName()
            if(is.null(tabName))
              return(NULL)

            threshold <- switch(tabName,
                                MU=input$thresholdMU,
                                OP=input$thresholdOP,
                                BS=input$thresholdBS,
                                HC=input$thresholdHC,
                                DP=input$thresholdDP,
                                NULL)
            return(threshold)
          }

        getPlotArguments <- function()
          {
            ##construct plotting arguments
            args <- list()
            args$object <- object
            args$col <- input$colorby ##reactive on metadata
            args$location <- getLocation() ##reactive of mouse clicking
            args$threshold <- getThreshold() ##reactive on threshold changes
            args$outliers <- input$outliers ##reactive on outliers checkbox
            args$type <- input$type ##reactive on quality control display type
            args$plotType <- getTabName() ##reactive on tab panel switching
            args
          }      
        
        ##create plot
        observe({
          
          args <- getPlotArguments()
          plotType <- args$plotType

          ##optionally save plot
          output$save <- downloadHandler(
                                         filename=function() {
                                           paste0("plot", plotType, ".pdf")
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
          plotType <- paste0("plot", plotType)
          output[[plotType]] <- renderPlot({ do.call(qcplot, args) })
        })

        ##show outliers
        output$Outliers <- renderDataTable({
          
          thrs <- getThreshold() ##reactive on threshold changes
          
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
