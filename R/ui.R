ui450k <- function(object) {

    object <- updateObject(object)

    condFC <- "%spnlMain == 'FC' && (%spnlFC == 'MU' || %spnlFC == 'OP' || %spnlFC == 'BS' || %spnlFC == 'HC' || %spnlFC == 'DP')"
    condSDC <- "%spnlMain ==  'SDC' && (%spnlSDC == 'BSI' || %spnlSDC == 'BSII' || %spnlSDC == 'SPI' || %spnlSDC == 'SPII' || %spnlSDC == 'NP')"
    condSIC <- "%spnlMain == 'SIC' && (%spnlSIC == 'SC' || %spnlSIC == 'TR' || %spnlSIC == 'EC' || %spnlSIC == 'HYB')"
    condMain <- "%spnlMain == 'FC' || %spnlMain == 'SDC' || %spnlMain == 'SIC'"

    fluidPage(

        headerPanel(HTML(hdrUI), windowTitle="MethylAid"),

        fluidRow(
            column(2,
                   br(), br(), br(), br(), br(), br(), br(),


                   conditionalPanel(condition = gsub("%s", "input.", condMain),
                                    wellPanel(
                                        selectInput("showOutliers", "Show outliers:", c("Off", "On"), selected="On"),

                                        conditionalPanel(condition = gsub("%s", "input.", condFC),
                                                         selectInput("colorby", "Color by:", rev(colnames(object@targets)), "None"), br(),
                                                         uiOutput("showBackground"),
                                                         helpText(disclaimer)
                                                         ),

                                        conditionalPanel(condition = gsub("%s", "input.", condSDC),
                                                         selectInput("plotType", "Plot type:", c("boxplot", "sample", "scatter"), "sample"), br(),
                                                         helpText(htSDC)
                                                         ),

                                        conditionalPanel(condition = gsub("%s", "input.", condSIC),
                                                         selectInput("plotType", "Plot type:", c("boxplot", "sample", "scatter"), "sample"), br(),
                                                         helpText(htSIC)
                                                         ),
                                        br(), br(),

                                        downloadLink("downloadPlot", "Save current plot")

                                        ## downloadButton("downloadReport", "Generate report")
                                    )
                                    )
                   ),

            column(10,

                   tabsetPanel(id="pnlMain",
                               tabPanel(title = "Filter control plots", value="FC",
                                        tabsetPanel(id = "pnlFC",

                                                    tabPanel(title = names(hdrFC)[1],
                                                             HTML(paste0(start, hdrFC[1], end)),
                                                             plotOutput(paste0("plot", names(hdrFC)[1]), width=width, height=height,
                                                                        click=paste0("click", names(hdrFC[1])))
                                                             ),

                                                    tabPanel(title = names(hdrFC)[2],
                                                             HTML(paste0(start, hdrFC[2], end)),
                                                             plotOutput(paste0("plot", names(hdrFC)[2]), width=width, height=height,
                                                                        click=paste0("click", names(hdrFC[2])))
                                                             ),

                                                    tabPanel(title = names(hdrFC)[3],
                                                             HTML(paste0(start, hdrFC[3], end)),
                                                             plotOutput(paste0("plot", names(hdrFC)[3]), width=width, height=height,
                                                                        click=paste0("click", names(hdrFC[3])))
                                                             ),

                                                    tabPanel(title = names(hdrFC)[4],
                                                             HTML(paste0(start, hdrFC[4], end)),
                                                             plotOutput(paste0("plot", names(hdrFC)[4]), width=width, height=height,
                                                                        click=paste0("click", names(hdrFC[4])))

                                                             ),

                                                    tabPanel(title = names(hdrFC)[5],
                                                             HTML(paste0(start, hdrFC[5], end)),
                                                             plotOutput(paste0("plot", names(hdrFC)[5]), width=width, height=height,
                                                                        click=paste0("click", names(hdrFC[5])))

                                                             )
                                                    )

                                        ),

                               tabPanel(title = "Sample-dependent controls", value="SDC",
                                        tabsetPanel(id = "pnlSDC",

                                                    tabPanel(title = names(hdrSDC)[1],
                                                             HTML(paste0(start, hdrSDC[1], end)),
                                                             plotOutput(paste0("plot", names(hdrSDC)[1]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSDC[1])))),

                                                    tabPanel(title = names(hdrSDC)[2],
                                                             HTML(paste0(start, hdrSDC[2], end)),
                                                             plotOutput(paste0("plot", names(hdrSDC)[2]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSDC[2])))),

                                                    tabPanel(title = names(hdrSDC)[3],
                                                             HTML(paste0(start, hdrSDC[3], end)),
                                                             plotOutput(paste0("plot", names(hdrSDC)[3]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSDC[3])))),

                                                    tabPanel(title = names(hdrSDC)[4],
                                                             HTML(paste0(start, hdrSDC[4], end)),
                                                             plotOutput(paste0("plot", names(hdrSDC)[4]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSDC[4])))),

                                                    tabPanel(title = names(hdrSDC)[5],
                                                             HTML(paste0(start, hdrSDC[5], end)),
                                                             plotOutput(paste0("plot", names(hdrSDC)[5]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSDC[5]))))
                                                    )
                                        ),

                               tabPanel(title = "Sample-independent controls", value="SIC",
                                        tabsetPanel(id = "pnlSIC",
                                                    tabPanel(title = names(hdrSIC)[1],
                                                             HTML(paste0(start, hdrSIC[1], end)),
                                                             plotOutput(paste0("plot", names(hdrSIC)[1]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSIC[1])))),

                                                    tabPanel(title = names(hdrSIC)[2],
                                                             HTML(paste0(start, hdrSIC[2], end)),
                                                             plotOutput(paste0("plot", names(hdrSIC)[2]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSIC[2])))),

                                                    tabPanel(title = names(hdrSIC)[3],
                                                             HTML(paste0(start, hdrSIC[3], end)),
                                                             plotOutput(paste0("plot", names(hdrSIC)[3]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSIC[3])))),

                                                    tabPanel(title = names(hdrSIC)[4],
                                                             HTML(paste0(start, hdrSIC[4], end)),
                                                             plotOutput(paste0("plot", names(hdrSIC)[4]), width=width, height=height,
                                                                        click=paste0("click", names(hdrSIC[4]))))
                                                    )
                                        ),

                               tabPanel(title = "Outlier Table", value = "OT",
                                        dataTableOutput('OutlierTable')
                                        ),

                               tabPanel(title = "About",
                                        includeHTML(file.path(path.package("MethylAid"), "www", "About.html"))
                                        ),

                               conditionalPanel(condition = "input.pnlMain == 'OT'",
                                                downloadLink("downloadData", "Save outlier table")
                                                )

                               )
                   )
        )
    )
}
