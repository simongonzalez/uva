tabPanel("GridLines",
         sidebarLayout(
           sidebarPanel(
             uiOutput('radio_measures_GLUI'),
             uiOutput('speaker_GL'),
             uiOutput('segment_GL'),
             radioButtons("define_origin_GL", label = h5("Define Origin Point"),
                          choices = c('Manual', 'Narrow', 'Wide'),
                          inline = T),
             uiOutput('include_palate_GL_UI'),
             uiOutput('define_fieldview_GL_UI'),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 uiOutput('leftlineangle_fieldview_GL_UI')),
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 uiOutput('rightlineangle_fieldview_GL_UI')),
             
             uiOutput('number_of_gridlines_radio_ui'),
             uiOutput('number_of_gridlines_input_ui'),
             uiOutput('setGridlinesBttnUI'),
             tags$br(),
             downloadButton(outputId = 'gridlinesPlot', label = 'Save Plot')
           ),
           mainPanel(
             
             bsCollapse(id = "collapseExampleGL", multiple = T, open = "Plot",
                        bsCollapsePanel('Plot',
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                            uiOutput("palate_plot_GL_UI")),
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                            checkboxInput("smooth_contour_GL", label = "Smooth", value = T)),
                                        tags$hr(),
                                        uiOutput('coordenate_info_ui_GL'),
                                        tags$hr(),
                                        textInput3(inputId="gridlines_manual_origin_x", label="x-origin", 
                                                   value = 0, class="input-mini"),
                                        textInput3(inputId="gridlines_manual_firstPoint_x", label="x-first", 
                                                   value = 0, class="input-mini"),
                                        textInput3(inputId="gridlines_manual_secondPoint_x", label="x-second", 
                                                   value = 0, class="input-mini"),
                                        tags$br(),
                                        textInput3(inputId="gridlines_manual_origin_y", label="y-origin", 
                                                   value = 0, class="input-mini"),
                                        textInput3(inputId="gridlines_manual_firstPoint_y", label="y-first", 
                                                   value = 0, class="input-mini"),
                                        textInput3(inputId="gridlines_manual_secondPoint_y", label="y-second", 
                                                   value = 0, class="input-mini"),
                                        tags$hr(),
                                        jqui_draggable(jqui_resizable(plotOutput('plotStatic_GL',
                                                                                 click = 'gl_click',
                                                                                 dblclick = 'gl_dblclick', 
                                                                                 hover = 'gl_hover', 
                                                                                 brush = 'gl_brush'))),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('manual_originPoint_btn_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('automatic_originPoint_btn_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('manual_fieldviewPoint_btn_UI')),
                                        
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('set_fieldviewPoint_btn_UI')),
                                        
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('clear_fieldviewPoint_btn_UI')),
                                        
                                        tags$hr(),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            checkboxInput("xy_point_GL", label = "XYPoint", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            checkboxInput("line_point_GL", label = "Line Point", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('GL_intersection_labels_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('GL_intersection_labelsRepel_UI')),
                                        tags$hr(),
                                        lapply(1:20, function(i) {
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                              uiOutput(paste0('inGL_inContext', i)))
                                        }),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('calculate_intersections_btn_ui')),
                                        style = "primary"
                        ),
                        bsCollapsePanel("TableGL",
                                        hr(),
                                        h4('Table'),
                                        DT::dataTableOutput('gridline_table'),
                                        style = "success"
                        )
             )
             
           )
           
         ))