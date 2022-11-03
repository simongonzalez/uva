tabPanel("Dynamic",
         sidebarLayout(
           sidebarPanel(
             uiOutput('basis_selection_ui'),
             uiOutput('firstComparison_selection_ui'),
             uiOutput('secondComparison_selection_ui'),
             uiOutput('gridLine_selection_ui')
           ),
           mainPanel(
             plotOutput('plotDynamic',
                        click = 'dyn_click_main',
                        dblclick = 'dyn_dblclick_main', 
                        hover = 'dyn_hover_main', 
                        brush = 'dyn_brush_main'),
             #plotlyOutput('plotDynamicComparison'),
             highchartOutput('plotDynamicComparison'),
             highchartOutput("plotGridline")
             #amChartsOutput(outputId = 'plotGridline')
           )))
