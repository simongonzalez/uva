library(shiny)
library(DT)
library(pryr)
library(shinyjs)
library(shinydashboard)
library(colourpicker)
library(ggrepel)
library(shinyBS)
library(RColorBrewer)
library(wesanderson)
library(data.table)
library(ggplot2)
library(tidyr)
library(plotly)
library(rAmCharts)
library(highcharter)
library(raster)
library(sp) 
library(rgdal)
library(rhandsontable)
library(shinyjqui)

#shiny::runApp(display.mode="showcase")

options(shiny.maxRequestSize = 100*1024^2)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
}

textInput2<-function (inputId, label, value = "",...) 
{
  tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
                                                         type = "text", value = value,...))
}

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value,...))
}

shinyUI(fluidPage(theme = "bootstrap.css",
                  navbarPage("Ultrasound Visualisation & Analysis",
                             tabPanel("Home",
                                      h2(style="text-align:center", 'Welcome'),
                                      tags$hr(),
                                      HTML('<center><img src="uva.png" height = 300 width="600"></center>'),
                                      div(style="text-align:center",
                                          h1('Ultrasound Visualization & Analysis'),
                                          h4('Visualise and analyse tongue contours for speech research'),
                                          tags$hr(),
                                          HTML('<li>'),
                                          a('Author: Simon Gonzalez (PhD in Linguistics)', href="https://au.linkedin.com/in/gonzalezsimon", target="_blank"),
                                          HTML('<li>'),
                                          a('Email: simon.gonzalez@anu.edu.au', 
                                            href="https://researchers.anu.edu.au/researchers/gonzalez-ochoa-s",
                                            target="_blank"),
                                          HTML('<li>'),
                                          a('Citation: Gonzalez, S. (2018, September 25). UVA. Retrieved from osf.io/z5hyj'),
                                          HTML('<li>'),
                                          a('Online CV', href="https://www.visualcv.com/simongonzalez", target="_blank"),
                                          HTML('<li>'),
                                          a('Thesis: Place oppositions in English coronal obstruents: an ultrasound study
                                            - (University of Newcastle, Australia)', 
                                            href="http://hdl.handle.net/1959.13/1310302", target="_blank")
                                      )
                                      
                             ),
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
                                      
                             ),
                             tabPanel("Load",
                                      titlePanel("Uploading Files"),
                                      fluidPage(
                                        checkboxInput("main_invert_y", label = "DefaultY", value = F),
                                        fileInput('file1', 'Choose file to upload',
                                                  accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    'text/tab-separated-values',
                                                    'text/plain',
                                                    '.csv',
                                                    '.tsv'
                                                  )
                                        ),
                                        tags$hr(),
                                        checkboxInput('header', 'Header', TRUE),
                                        radioButtons('sep', 'Separator',
                                                     c(Comma=',',
                                                       Semicolon=';',
                                                       Tab='\t'),
                                                     ','),
                                        radioButtons('quote', 'Quote',
                                                     c(None='',
                                                       'Double Quote'='"',
                                                       'Single Quote'="'"),
                                                     '"'),
                                        tags$hr()
                                      )),
                             tabPanel("Data",
                                      titlePanel(NULL),
                                      fluidPage(
                                        uiOutput('smr'),
                                        DT::dataTableOutput('summary')
                                      )),
                             tabPanel("Manage Data",
                                      titlePanel(NULL),
                                      fluidPage(
                                        box(width=12,
                                            h3(strong("Manage Data"),align="center"),
                                            hr(),
                                            column(12,
                                                   HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                                                   bsButton('Del_row_head', label = 'Delete selected rows', icon("chevron-circle-left"), style = "primary"),
                                                   bsButton('Compare_row_head', label = 'Compare selected rows', icon("chevron-circle-left"), style = "primary"),
                                                   HTML('</div>')
                                            ),
                                            
                                            column(12,DT::dataTableOutput("Main_table")),
                                            tags$script(HTML('$(document).on("click", "input", function () {
                                                             var checkboxes = document.getElementsByName("row_selected");
                                                             var checkboxesChecked = [];
                                                             for (var i=0; i<checkboxes.length; i++) {
                                                             
                                                             if (checkboxes[i].checked) {
                                                             checkboxesChecked.push(checkboxes[i].value);
                                                             }
                                                             }
                                                             Shiny.onInputChange("checked_rows",checkboxesChecked);
                                                             })')),
                                            tags$script("$(document).on('click', '#Main_table button', function () {
                                                        Shiny.onInputChange('lastClickId',this.id);
                                                        Shiny.onInputChange('lastClick', Math.random())
                                                        });")
                                            
                                        )
                                        )),
                             tabPanel("Data Inspection",
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
                                          tags$hr(),
                                          downloadButton(outputId = 'visualisationPlot', label = 'Save Plot')
                                          
                                        ),
                                        mainPanel(
                                          div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput("palate_plot", label = "Palate Trace", value = F)),
                                          div(style="display: inline-block;vertical-align:top; width: 150px;",checkboxInput("smooth_contour", label = "Smooth", value = T)),
                                          
                                          jqui_draggable(jqui_resizable(plotOutput('plotStatic',
                                                     click = 'lm_click_main',
                                                     dblclick = 'lm_dblclick_main', 
                                                     hover = 'lm_hover_main', 
                                                     brush = 'lm_brush_main'))), #, width = 750, height = 750)
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
                                          # div(style="display: inline-block;vertical-align:top; width: 100px;",
                                          #     checkboxInput("main_invert_y", label = "InvertY", value = T)),
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
                                        ))),
                             tabPanel("LandMarks",
                                      sidebarLayout(
                                        sidebarPanel(
                                          uiOutput('landMark_numbers_ui'),
                                          bsButton("create_landmarks", label = "Create", icon("chevron-circle-left"), style = "primary")
                                        ),
                                        mainPanel(
                                          lapply(1:20, function(i) {
                                            uiOutput(paste0('inLM', i))
                                          }),
                                          
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('set_LMs_UI')),
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('add_LMs_UI')),
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('delete_LMs_UI'))
                                        ))),
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
                                          tags$hr(),
                                          downloadButton(outputId = 'landmarksPlot', label = 'Save Plot')
                                        ),
                                        mainPanel(
                                          
                                          bsCollapse(id = "collapseExample", multiple = T, open = "Plot",
                                                     bsCollapsePanel('Plot',
                                                                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                                         checkboxInput("palate_plot_LM", label = "Palate Trace", value = F)),
                                                                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                                         checkboxInput("smooth_contour_LM", label = "Smooth", value = T)),
                                                                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                                         uiOutput('colour_contiguous_frames_LMUI')),
                                                                     jqui_draggable(jqui_resizable(plotOutput('plotStatic_LM',
                                                                                click = 'lm_click',
                                                                                dblclick = 'lm_dblclick', 
                                                                                hover = 'lm_hover', 
                                                                                brush = 'lm_brush'))),
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
                                        
                                      )),
                             navbarMenu("Graphics",
                                        tabPanel("Contours",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     radioButtons("tongue_colour_set_radio", label = h5('Colour Set (Only for multiple frames of a single segment)'),
                                                                  choices = list("Mono" = 1, "Double" = 2, "Multiple" = 3, "Spectrum" = 4),
                                                                  selected = 1, inline = T),
                                                     uiOutput("tongue_colour_setUI"),
                                                     #uiOutput("tongue_colour_nmbr_sliderUI"),
                                                     uiOutput("invert_coloursUI"),
                                                     #alpha value
                                                     sliderInput("tongue_alpha_slider",  label = "Contours Colour Alpha Value", min = 0.0, max = 1.0, value = 1, step = 0.1, animate = T),
                                                     #width value
                                                     sliderInput("tongue_width_slider",  label = "Contours Line Width", min = 1, max = 5, value = 2, step = 0.5, animate = T),
                                                     #smooth value
                                                     sliderInput("tongue_smooth_slider",  label = "Contours Smooth Spline Value", min = 0.1, max = 1.0, value = 0.3, step = 0.1, animate = T),
                                                     selectInput("line_type_tongue", label = "Line Type", choices = list("Solid -" = 1, "Dashed ----" = 2, "Dotted ...." = 3, "Dotdash .-.-" = 4), selected = 1)
                                                   ),
                                                   mainPanel(
                                                     jqui_draggable(jqui_resizable(plotOutput('plotStaticContours')))
                                                   ))),
                                        tabPanel("Palate",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     #Colour
                                                     #useShinyjs(),
                                                     colourInput("palate_colour", label = "Palate Trace Colour", palette = "limited",
                                                                 allowedCols = c("#000000", "#03a9f4", "#f44336", "#009688", "#ff9800", "#9c27b0", "#3f51b5",
                                                                                 "#795548", "#607d8b", "#00bcd4", "#e91e63", "#cddc39", "#ffeb3b"), value = "#03a9f4"),
                                                     #alpha value
                                                     sliderInput("palate_alpha_slider",  label = "Palate Trace Colour Alpha Value", min = 0.0, max = 1.0, value = 1, step = 0.1, animate = T),
                                                     #width value
                                                     sliderInput("palate_width_slider",  label = "Palate Trace Line Width", min = 1, max = 5, value = 3, step = 0.5, animate = T),
                                                     #smooth value
                                                     sliderInput("palate_smooth_slider",  label = "Palate Trace Smooth Spline Value", min = 0.1, max = 1.0, value = 0.3, step = 0.1, animate = T),
                                                     selectInput("line_type_palate", label = "Line Type", choices = list("Solid -" = 1, "Dashed ----" = 2, "Dotted ...." = 3, "Dotdash .-.-" = 4), selected = 1)
                                                   ),
                                                   mainPanel(
                                                     jqui_draggable(jqui_resizable(plotOutput('plotStaticPalate')))
                                                   ))),
                                        tabPanel("Graphics Window",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectInput("font_type", "Choose Font",
                                                                 choices = c("Courier","Helvetica","Palatino","Times"), selected = "Times"),
                                                     #--------------------------------------
                                                     colourInput("text_color", label = "Text Colour", palette = "limited",
                                                                 allowedCols = c("#000000", "#03a9f4", "#f44336", "#009688", "#ff9800", "#9c27b0", "#3f51b5",
                                                                                 "#795548", "#607d8b", "#e91e63"), value = "#000000"),
                                                     #Size
                                                     #--------------------------------------
                                                     #Size
                                                     sliderInput("axis_size", label = "Axis Labels Size", min = 0, max = 30, value = 20, step = 2, animate = T),
                                                     sliderInput("legend_size", label = "Legend Title Size", min = 0, max = 20, value = 14, step = 2, animate = T),
                                                     #--------------------------------------
                                                     #Size
                                                     sliderInput("ticks_size", label = "Axis Ticks Size", min = 0, max = 20, value = 12, step = 2, animate = T)
                                                     #uiOutput("text_main"),
                                                     #textInput("txt_ttl", label = "Title", value = 'ttl'),
                                                     #checkboxInput("include_speaker", label = "Include Speaker", value = T),
                                                     #checkboxInput("include_segment", label = "Include Segment", value = T),
                                                     #checkboxInput("include_repetition", label = "Include Repetition", value = T),
                                                     #checkboxInput("include_frame", label = "Include Frame", value = T)
                                                   ),
                                                   mainPanel(
                                                     jqui_draggable(jqui_resizable(plotOutput('plotStaticGraphics')))
                                                     #plotOutput('plotStatic')
                                                   )))),
                             tabPanel("GridLines",
                                      sidebarLayout(
                                        sidebarPanel(
                                          radioButtons("radio_measures_GL", label = h5("Plot in"),
                                                       choices = list("Millimeters" = 1, "Pixels" = 2),
                                                       selected = 1, inline = T),
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
                                          tags$br(),
                                          downloadButton(outputId = 'gridlinesPlot', label = 'Save Plot')
                                          # div(style="display: inline-block;vertical-align:top; width: 150px;",
                                          #     uiOutput('distance_leftlineangle_fieldview_GL_UI')),
                                          # div(style="display: inline-block;vertical-align:top; width: 150px;",
                                          #     uiOutput('distance_rightlineangle_fieldview_GL_UI'))
                                        ),
                                        mainPanel(
                                          
                                          bsCollapse(id = "collapseExampleGL", multiple = T, open = "Plot",
                                                     bsCollapsePanel('Plot',
                                                                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                                         checkboxInput("palate_plot_GL", label = "Palate Trace", value = F)),
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
                                        
                                      )),
                             tabPanel("Analysis",
                                      sidebarLayout(
                                        sidebarPanel(
                                          numericInput('frameRate', label = 'Frame Rate', value = 0.033),
                                          uiOutput('dynamicAnalysisSingle_landmark_selection_ui'),
                                          uiOutput('dynamicAnalysisSingle_speaker_selection_ui'),
                                          uiOutput('dynamicAnalysisSingle_segment_selection_ui'),
                                          uiOutput('dynamicAnalysisSingle_repetition_selection_ui'),
                                          uiOutput('dynamicAnalysisSingle_originPoint_ui'),
                                          uiOutput('dynamicAnalysisSingle_fontsize_ui'),
                                          sliderInput("dynamicAnalysisSingle_alpha", label = "Alpha", min = 0, 
                                                      max = 1, value = 0.7, step = 0.1),
                                          radioButtons(inputId = 'dynamicType', label = 'Dynamic Analysis', 
                                                       choices = c('Displacement', 'Distance', 'Velocity', 'Acceleration'), selected = 'Displacement', inline = T),
                                          downloadButton('dynamicAnalysisSingle_downloadPlot','Download Plot')
                                        ),
                                        mainPanel(
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                          radioButtons(inputId = 'dynamicPlot_type', label = NULL, 
                                                       choices = c('Contours', 'Heatmap'), selected = 'Contours', inline = T)),
                                          
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                              checkboxInput("dynamicAnalysisSingle_gridlines", label = "Plot Gridlines", value = F)),
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                              checkboxInput("dynamicAnalysisSingle_tokens", label = "Tokens", value = F)),
                                          div(style="display: inline-block;vertical-align:top; width: 100px;",
                                              checkboxInput("dynamicAnalysisSingle_ciplot", label = "Confidence Intervals", value = F)),

                                          jqui_draggable(jqui_resizable(plotlyOutput('dynamicAnalysisSingle_plot'))),
                                          jqui_draggable(jqui_resizable(highchartOutput('dynamicAnalysisSingle_displacements')))
                                          
                                        )))
                  )))
