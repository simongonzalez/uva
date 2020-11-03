tabPanel("Visualisation",
         sidebarLayout(
           sidebarPanel(
             radioButtons("radio_measures", label = h5("Plot in"),
                          choices = list("Millimeters" = 1, "Pixels" = 2),
                          selected = 1, inline = T),
             uiOutput('speaker'),
             radioButtons("segment_selection", label = h5("Segment Selection"),
                          choices = list("Single" = 1, "Multiple" = 2, "ALL" = 3),
                          selected = 1, inline = T),
             uiOutput('segment'),

             uiOutput("repetition_radioUI"),
             uiOutput('repetition'),
             uiOutput("radio_framesUI"),
             uiOutput('frame'),

             uiOutput('inContext'),
             tags$hr()

           ),
           mainPanel(
             div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput("palate_plot", label = "Palate Trace", value = T)),
             div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput("smooth_contour", label = "Smooth", value = T)),

             plotOutput('plotStatic',
                        click = 'lm_click_main',
                        dblclick = 'lm_dblclick_main', 
                        hover = 'lm_hover_main', 
                        brush = 'lm_brush_main'), #, width = 750, height = 750)
             div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('prevBttn')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('follBttn')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 uiOutput('plot_lines_UI_main')),
             tags$hr(),
             uiOutput('coordenate_info_ui'),
             tags$hr(),
             textInput3(inputId="main_manual_origin_x", label="x-origin", value = 0, class="input-mini"),
             textInput3(inputId="main_manual_secondPoint_x", label="x-second", value = 0, class="input-mini"),
             tags$br(),
             textInput3(inputId="main_manual_origin_y", label="y-origin", value = 0, class="input-mini"),
             textInput3(inputId="main_manual_secondPoint_y", label="y-second", value = 0, class="input-mini"),
             tags$hr(),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 checkboxInput("xy_point_main", label = "XYPoint", value = T)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 checkboxInput("line_point_main", label = "Line Point", value = T)),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 uiOutput('intersection_labels_UI_main')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 uiOutput('intersection_labelsRepel_UI_main')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 uiOutput('intersection_labelsExtreme_UI_main'))
           )))
