threshold.defaults <- list(MU = 10.50,
                           OP = 11.75,
                           BS = 12.75,
                           HC = 13.25,
                           DP = 0.95)

ui450k <- function(object)
  {
    width = "100%"
    height = "400px"

    start = "<br> <p style=\"width:800px;text-align:justify\"><span style=\"color:#000000;font-size:16px\">"
      end = "</span></p><br><br>"

    pageWithSidebar(headerPanel(paste("MethylAid(", packageVersion("MethylAid"),"): interactive visualization of Illumina 450k Methylation array data", sep="")),

                    sidebarPanel(conditionalPanel(condition = "input.mainPanel == 'Filter controls' && (input.fcPanel == 'rotated M vs U plot' || input.fcPanel == 'Overall sample-dependent control plot' || input.fcPanel == 'Bisulfite conversion control plot' || input.fcPanel == 'Overall sample-independent control plot' || input.fcPanel == 'Detection P-value plot')",
                                                  helpText("These controls are used to detect bad quality samples.",
                                                           "The default thresholds are based on our experience with",
                                                           "450k data that we have seen so far."),
                                                  br(), br(),
                                                  selectInput(inputId = "colorby", label = "Highlight metadata column:", choices = colnames(object@targets), selected = "None")
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' && input.fcPanel == 'rotated M vs U plot'",
                                                  sliderInput("thresholdMU", "threshold:",
                                                              min = threshold.defaults[["MU"]]-2, max = threshold.defaults[["MU"]]+2, value = threshold.defaults[["MU"]], step = 0.25)
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' && input.fcPanel == 'Overall sample-dependent control plot'",
                                                  sliderInput("thresholdOP", "threshold:",
                                                              min = threshold.defaults[["OP"]]-2, max = threshold.defaults[["OP"]]+2, value = threshold.defaults[["OP"]], step = 0.25)
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' && input.fcPanel == 'Bisulfite conversion control plot'",
                                                  sliderInput("thresholdBS", "threshold:",
                                                              min = threshold.defaults[["BS"]]-2, max = threshold.defaults[["BS"]]+2, value = threshold.defaults[["BS"]], step = 0.25)
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' && input.fcPanel == 'Overall sample-independent control plot'",
                                                  sliderInput("thresholdHC", "threshold:",
                                                              min = threshold.defaults[["HC"]]-2, max = threshold.defaults[["HC"]]+2, value = threshold.defaults[["HC"]], step = 0.25)

                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' && input.fcPanel == 'Detection P-value plot'",
                                                  sliderInput("thresholdDP", "threshold:",
                                                              min = 0.8, max = 1, value = threshold.defaults[["DP"]], step = 0.01)
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel ==  'Sample-dependent controls' && (input.sdcPanel == 'BSI' || input.sdcPanel == 'BSII' || input.sdcPanel == 'SPI' || input.sdcPanel == 'SPII' || input.sdcPanel == 'NP')",
                                                  helpText("The sample-dependent controls can be used to evaluate",
                                                           "performance across samples. These control oligos are",
                                                           "designed for bisulfite-converted human genomic DNA",
                                                           "sequences. Because target sequences do not contain CpG",
                                                           "dinucleotides, the performance of the control oligos does",
                                                           "not depend on the methylation status of the template DNA."),
                                                  br(), br(),
                                                  selectInput(inputId = "type", label = "plot type:", choices = c("boxplot", "sample", "scatter"), selected = "sample")
                                                  ),


                                 conditionalPanel(condition = "input.mainPanel == 'Sample-independent controls' && (input.sicPanel == 'SC' || input.sicPanel == 'TR' || input.sicPanel == 'EC' || input.sicPanel == 'HYB')",
                                                  helpText("Sample-independent controls evaluate the performance of",
                                                           "specific steps in the process flow."),
                                                  br(), br(),
                                                  selectInput(inputId = "type", label = "plot type:", choices = c("boxplot", "sample", "scatter"), selected = "sample")
                                                  ),

                                 conditionalPanel(condition = "input.mainPanel == 'Filter controls' || input.mainPanel ==  'Sample-dependent controls' || input.mainPanel == 'Sample-independent controls'",

                                                  checkboxInput("outliers", "Show outliers", value = FALSE),
                                                  downloadLink("save", "Save Plot")
                                                  ),


                                 br(), br(),

                                 actionButton("exit", "Exit")
                                 ),

                    mainPanel(
                              tabsetPanel(id = "mainPanel",
                                          tabPanel("Filter controls",
                                                   tabsetPanel(id = "fcPanel",
                                                               tabPanel(title = "rotated M vs U plot",
                                                                        ##HTML(paste0(start, headers["MU"], end)),
                                                                        plotOutput("plotMU", width=width, height=height, clickId="clickIdMU")),
                                                               tabPanel(title = "Overall sample-dependent control plot",
                                                                        ##HTML(paste0(start, headers["OP"], end)),
                                                                        plotOutput("plotOP", width=width, height=height, clickId="clickIdOP")),
                                                               tabPanel(title = "Bisulfite conversion control plot",
                                                                        ##HTML(paste0(start, headers["BS"], end)),
                                                                        plotOutput("plotBS", width=width, height=height, clickId="clickIdBS")),
                                                               tabPanel(title = "Overall sample-independent control plot",
                                                                        ##HTML(paste0(start, headers["HC"], end)),
                                                                        plotOutput("plotHC", width=width, height=height, clickId="clickIdHC")),
                                                               tabPanel(title = "Detection P-value plot",
                                                                        ##HTML(paste0(start, headers["DP"], end)),
                                                                        plotOutput("plotDP", width=width, height=height, clickId="clickIdDP"))
                                                               )
                                                   ),

                                          tabPanel("Sample-dependent controls",
                                                   tabsetPanel(id = "sdcPanel",
                                                               tabPanel(title = "BSI", HTML(paste0(start, headers["BSI"], end)), plotOutput("plotBSI", width=width, height=height)),
                                                               tabPanel(title = "BSII", HTML(paste0(start, headers["BSII"], end)), plotOutput("plotBSII", width=width, height=height)),
                                                               tabPanel(title = "SPI", HTML(paste0(start, headers["SPI"], end)), plotOutput("plotSPI", width=width, height=height)),
                                                               tabPanel(title = "SPII", HTML(paste0(start, headers["SPII"], end)), plotOutput("plotSPII", width=width, height=height)),
                                                               tabPanel(title = "NP", HTML(paste0(start, headers["NP"], end)), plotOutput("plotNP", width=width, height=height))
                                                               )
                                                   ),

                                          tabPanel("Sample-independent controls",
                                                   tabsetPanel(id = "sicPanel",
                                                               tabPanel(title = "SC", HTML(paste0(start, headers["SC"], end)), plotOutput("plotSC", width=width, height=height)),
                                                               tabPanel(title = "TR", HTML(paste0(start, headers["TR"], end)), plotOutput("plotTR", width=width, height=height)),
                                                               tabPanel(title = "EC", HTML(paste0(start, headers["EC"], end)), plotOutput("plotEC", width=width, height=height)),
                                                               tabPanel(title = "HYB", HTML(paste0(start, headers["HYB"], end)), plotOutput("plotHYB", width=width, height=height))
                                                               )
                                                   ),

                                          tabPanel(title = "Outliers",
                                                   dataTableOutput('Outliers')
                                                   ),
                                          tabPanel(title = "Help",
                                                   includeHTML(file.path(path.package("MethylAid"), "www", "Help.html"))
                                                   )

                                          ) ##tabsetPanel
                              )##mainPanel
                    )##pageWithSidebar
  }
