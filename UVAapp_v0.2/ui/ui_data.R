tabPanel("Data",
         titlePanel(NULL),
         fluidPage(
           uiOutput('smr'),
           DT::dataTableOutput('summary')
         ))