tabPanel("SSANOVA",
         div(style="display: inline-block;vertical-align:top; width: 140px;",
             uiOutput('spkrSSANOVA')),
         div(style="display: inline-block;vertical-align:top; width: 140px;",
             uiOutput('comp1SSANOVA')),
         div(style="display: inline-block;vertical-align:top; width: 140px;",
             uiOutput('comp2SSANOVA')),
         tags$br(),
         div(style="display: inline-block;vertical-align:top; width: 180px;",
             checkboxInput('polarPlot', 'Polar coordinates', 
                           value = FALSE)),
         div(style="display: inline-block;vertical-align:top; width: 140px;",
             checkboxInput('barePlot', 'Bare Plot', value = FALSE)),
         #actionButton('ssbtn', 'ssbtn'),
         tags$hr(),
         fluidRow(
           tabBox(
             title = "SSANOVA Plots",
             id = "tabset1", width = 12,
             tabPanel('Individual Contours',
                      plotOutput('plotSSANOVAIndividual'),
                      downloadButton("downloadPlot1", "Download Plot"),
                      downloadButton("downloadData1", "Download Data")),
             tabPanel('Overall Comparison',
                      plotOutput('plotSSANOVAComparison'),
                      downloadButton("downloadPlot2", "Download Plot"),
                      downloadButton("downloadData2", "Download Data")),
             tabPanel('Confidence Intervals',
                      plotOutput('plotSSANOVAInteraction'),
                      downloadButton("downloadPlot3", "Download Plot"),
                      downloadButton("downloadData3", "Download Data"))
           )
         )
)