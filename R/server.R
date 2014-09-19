
server450k <- function(object)
  {  
    
    function(input, output, session)
      {

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

        ##initialize to get all outliers detected
        initialize <- function()
          {            
            for(plotType in c("MU", "OP", "BS", "HC", "DP"))
              {
                args <- getPlotArguments()
                args$plotType <- plotType
                args$threshold <- getThreshold(plotType)
                plotType <- paste0("plot", plotType)
                output[[plotType]] <- renderPlot({ do.call(qcplot, args) })
              }
          }
        
        ##create plot
        observe({ 
         
          ##initialize to get all outliers detected do this only once
          if(!exists("initialized", envir=globalenv()))
            {
              initialize()
              assign("initialized", TRUE, envir=globalenv())
            }

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
          ##message("Update data table") ##just for debugging
          dt <- get("outliers", envir=globalenv())

          if(sum(rowSums(dt) > 0) == 0)
            return(NULL)

          dt <- dt[rowSums(dt) > 0,,drop=FALSE] ##filter
          return(cbind(ID=rownames(dt), dt))
        })

        ##Stop App
        observe({
          if (input$exit == 0)
            return()
          stopApp(input$cutoff)
        })
      }
  }
