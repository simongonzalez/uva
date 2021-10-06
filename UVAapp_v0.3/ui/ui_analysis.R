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
             downloadButton('dynamicAnalysisSingle_downloadPlot','Download Plot'),

             selectInput("comparison_theme", "Comparison Theme", list(
               "ggplot" = c('theme_minimal()', 'theme_gray()', 'theme_bw()', 'theme_linedraw()', 'theme_light()',
                            'theme_dark()', 'theme_classic()', 'theme_void()', 'theme_test()'),
               "ggthemes" = c('theme_wsj()', 'theme_tufte()', 'theme_stata()', 'theme_solid()', 'theme_solarized()',
                              'theme_map()', 'theme_igray()', 'theme_hc()', 'theme_gdocs()', 'theme_fivethirtyeight()',
                              'theme_few()', 'theme_excel()', 'theme_economist()', 'theme_calc()'),
               "ggpubr" = c('theme_pubr()', 'theme_pubclean()', 'theme_classic2()', 'theme_transparent()')
             )),
             
             selectInput('analysis_theme', 'Dynamic Analysis Theme', 
                         choices = unlist(strsplit('smpl,db,538,alone,bloom,chalk,darkunica,economist,elementary,ffx,flat,flatdark,ft,ggplot2,google,gridlight,handdrawn,hcrt,monokai,null,sandsignika,superheroes,tufte', ',')),
                         selected = 'hcrt')
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
