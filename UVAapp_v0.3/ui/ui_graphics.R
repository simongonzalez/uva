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
                        radioButtons('smoothMethod', 'Smooth Method',
                                     choices = c('auto', 'gam', 'loess'),
                                     selected = 'auto'),
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
                        colourpicker::colourInput("palate_colour", label = "Palate Trace Colour", palette = "limited",
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
                        colourpicker::colourInput("text_color", label = "Text Colour", palette = "limited",
                                                  allowedCols = c("#000000", "#03a9f4", "#f44336", "#009688", "#ff9800", "#9c27b0", "#3f51b5",
                                                                  "#795548", "#607d8b", "#e91e63"), value = "#000000"),
                        #Size
                        #--------------------------------------
                        #Size
                        sliderInput("axis_size", label = "Axis Labels Size", min = 0, max = 30, value = 20, step = 2, animate = T),
                        sliderInput("legend_size", label = "Legend Title Size", min = 0, max = 20, value = 14, step = 2, animate = T),
                        #--------------------------------------
                        #Format
                        selectInput('saveFormat', label = 'Image Format',
                                    choices = c('bmp','jpeg','pdf',
                                                'png','svg','tiff'),
                                    selected = 'png'
                        ),
                        # radioButtons("saveFormat", label = 'Image Format',
                        #              choices = c(), selected = 1),
                        #--------------------------------------
                        #Size
                        sliderInput("ticks_size", label = "Axis Ticks Size", min = 0, max = 20, value = 12, step = 2, animate = T),
                        sliderInput("widthGraphics", label = 'Image Output Width (cm)', min = 10, 
                                    max = 100, value = 40),
                        sliderInput("heightGraphics", label = 'Image Output Height (cm)', min = 10, 
                                    max = 100, value = 20)
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
                      ))))