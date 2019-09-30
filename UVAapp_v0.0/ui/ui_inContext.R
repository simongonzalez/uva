tabPanel("In context",
         sidebarLayout(
           sidebarPanel(
             radioButtons("radio_measures_LM", label = h5("Plot in"),
                          choices = list("Millimeters" = 1, "Pixels" = 2),
                          selected = 1, inline = T),
             uiOutput('speaker_LM'),
             uiOutput('segment_LM'),
             uiOutput('repetition_LM'),
             uiOutput('frame_LM'),
             tags$hr(),
             radioButtons("extra_frames_LM", label = h5("Plot surrounding frames"),
                          choices = list("None" = 1, "Both" = 2, "Before" = 3, "After" = 4), inline = TRUE),
             uiOutput('extra_frames_number_LMUI'), 
             uiOutput('hide_middle_frame_LMUI'),
             tags$hr()
           ),
           mainPanel(
             
             bsCollapse(id = "collapseExample", multiple = T, open = "Plot",
                        bsCollapsePanel('Plot',
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                            checkboxInput("palate_plot_LM", label = "Palate Trace", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                            checkboxInput("smooth_contour_LM", label = "Smooth", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 150px;",
                                            uiOutput('colour_contiguous_frames_LMUI')),
                                        plotOutput('plotStatic_LM',
                                                   click = 'lm_click',
                                                   dblclick = 'lm_dblclick', 
                                                   hover = 'lm_hover', 
                                                   brush = 'lm_brush'),
                                        verbatimTextOutput("vals"),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('prevBttn_LM')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('follBttn_LM')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('plot_lines_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            tags$br()),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('see_classification_UI')),
                                        tags$hr(),
                                        uiOutput('coordenate_info_ui_LM'),
                                        tags$hr(),
                                        textInput3(inputId="incontext_manual_origin_x", label="x-origin", value = 0, class="input-mini"),
                                        textInput3(inputId="incontext_manual_secondPoint_x", label="x-second", value = 0, class="input-mini"),
                                        tags$br(),
                                        textInput3(inputId="incontext_manual_origin_y", label="y-origin", value = 0, class="input-mini"),
                                        textInput3(inputId="incontext_manual_secondPoint_y", label="y-second", value = 0, class="input-mini"),
                                        tags$hr(),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            checkboxInput("xy_point", label = "XYPoint", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            checkboxInput("line_point", label = "Line Point", value = T)),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('intersection_labels_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('intersection_labelsRepel_UI')),
                                        div(style="display: inline-block;vertical-align:top; width: 100px;",
                                            uiOutput('intersection_labelsExtreme_UI')),
                                        tags$hr(),
                                        lapply(1:20, function(i) {
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                              uiOutput(paste0('inLM_inContext', i)))
                                        }),
                                        style = "primary"
                        ),
                        bsCollapsePanel("Table",
                                        hr(),
                                        h4('Table'),
                                        DT::dataTableOutput('landmark_table'),
                                        style = "success"
                        )
             )
             
           )
           
         ))