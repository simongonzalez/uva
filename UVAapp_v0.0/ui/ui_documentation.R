tabPanel("Documentation",
         sidebarLayout(
           sidebarPanel(
             actionButton('app_info', 'The App', width = '150'),
             tags$hr(),
             actionButton('data_info', 'Data', width = '150'),
             tags$hr(),
             actionButton('visualization_info', 'Visualization', width = '150'),
             tags$hr(),
             actionButton('in_context_info', 'In Context', width = '150'),
             tags$hr(),
             h1('Graphics'),
             actionButton('contours_info', 'Contours', width = '150'),
             tags$hr(),
             actionButton('palate_info', 'Palate', width = '150'),
             tags$hr(),
             actionButton('graphics_window_info', 'Graphics Window', width = '150')),
           mainPanel(
             uiOutput('documentation')
           )
         )
         
)