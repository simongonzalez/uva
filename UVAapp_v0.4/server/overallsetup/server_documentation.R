#======================================================================================
#======================================================================================
# DOCUMENTATION------------------------------------------------------------------------
#======================================================================================
#======================================================================================

doc_current = reactiveValues(data = 'app_info')

observeEvent(input$app_info, {
  doc_current$data = 'app_info'
})
observeEvent(input$data_info, {
  doc_current$data = 'data_info'
})
observeEvent(input$visualization_info, {
  doc_current$data = 'visualization_info'
})
observeEvent(input$in_context_info, {
  doc_current$data = 'in_context_info'
})
observeEvent(input$contours_info, {
  doc_current$data = 'contours_info'
})
observeEvent(input$palate_info, {
  doc_current$data = 'palate_info'
})
observeEvent(input$graphics_window_info, {
  doc_current$data = 'graphics_window_info'
})

output$documentation = renderUI({
  if(doc_current$data == 'app_info'){
    
    tags$div(style="text-align:justify",
             tags$p(h3('What is the UVA app?'),
                    'The visualization and analysis of tongue contours can be a time consuming process to set up. 
                      This requires a solid knowledge of compuer skills, with computer programming at the top of the list.
                      As a linguist with strong computer skills, I developped an online application using the shiny plataform by RStudio*. 
                      UVA allows any user 
                      (student, researcher, speech professional) visualise and analyse tongue contours with just a few clicks, 
                      with little or no knowledge on computer programming.'
             ),
             #HTML('<li>'),
             tags$p(h3('What data can it import?'),
                    'Tongue contours data created in EdgeTrak (Li, Kambhamettu, and Stone, 2005)**.
                      The app imports the raw data and saves it in CSV file.
                      Future versions will import data from other programs.'
             ),
             tags$p(h3('What makes UVA app stand out?'),
                    h5('The first strength of the app is that it allows user visualize/compare their data in seconds straight from EdgeTrak
                         without the need to know programming. Everything is done as using any other online application.
                         This can save long hours of setup and experimentation. Another advantage is that the app does not require
                         any installation of special plug-ins or software. The drawback is that it cannot be used offline.
                         Future versions will also include data analysis capabilities.'),
                    h5('The second strength is that it allows the user control all graphic parameters of figures.
                         This is a big plus in reasearch output since these figures can be used for display (talks, papers, reports)')
             ),
             tags$p(h3('What will future versions include?'),
                    'This is the first version of the application. 
                      It only includes visualization and comparison of tongue contours.
                      Future versions will include the analysis options.
                      Most of the analysis types are based on my doctoral thesis measurements (see link in welcome page).
                      However, further measurements will be included. Next versions will also have saving capabiblites
                      and version control.'
             ),
             tags$hr(),
             tags$p(h5('*RStudio Team (2015). RStudio: Integrated Development for R. RStudio,
                         Inc., Boston, MA URL http://www.rstudio.com/.)'),
                    h5('**Li, M., Kambhamettu, C., and Stone, M. (2005) 
                         Automatic contour tracking in ultrasound images. 
                         Clinical Linguistics and Phonetics 19(6-7); 545-554.')
             )
    )
  }else if(doc_current$data == 'data_info'){
    tags$div(style="text-align:justify",
             tags$p(h3('Data Summary'),
                    'This section shows a summary of the data. Summary is shown in four columns:',
                    #tags$li(),
                    h4(style="text-align:center", 'Speaker - Segment - Repetition - Frame'),
                    #tags$li(),
                    'The table has searching capabilities and columns can be arranged in ascending and descending order.
                      This allows the user to quickly check the number of repetitions/frames available in the data.'
             ))
  }else if(doc_current$data == 'visualization_info'){
    tags$div(style="text-align:justify",
             tags$p(h2('Data Visualization'),
                    'This section is the core of the applications. It is divided in the following sections:',
                    tags$hr(),
                    h3(style="text-align:left", 'Plot in'),
                    'The user has the option to see the plot in millimiters or pixels.',
                    'The default values exported from EdgeTrak are in pixels.',
                    'The program algorithm converts these values to millimiters depending on the',
                    'exporting settings the user selected.',
                    tags$hr(),
                    h3(style="text-align:left", 'Speaker'),
                    'Selection of Speakers in the dataset',
                    tags$hr(),
                    h3(style="text-align:left", 'Segment selection'),
                    HTML('<li>'),h5(style="text-align:left", 'Single: Only one segment is selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Multiple: One or more segments are selected'),
                    HTML('<li>'),h5(style="text-align:left", 'All: All segments in the dataset are selected'),
                    tags$hr(),
                    h3(style="text-align:left", 'Repetition selection'),
                    'This is a reactive selection. It means that the options vary depending on the number of segments',
                    h4(style="text-align:left", 'If only one segment is selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Single: Only one repetition is selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Range: One or more repetitions are selected'),
                    HTML('<li>'),h5(style="text-align:left", 'All: All repetitions from the specified segment are selected'),
                    h1(""),
                    h4(style="text-align:left", 'If more than one segment is selected'),
                    h5('NOTE: What is the difference between Exclusive and Inclusive options?',
                       'One important property in this program is that it deals with uneven data.',
                       'If the data is even (i.e. all segments have the same number of repetitions and number of frames),',
                       'there is no difference between Exclusive and Inclusive.'),
                    h5('If multiple segments are selected and they have different number of repetitions,',
                       'the user has two options. The Exclusive option gives ONLY the number of repetions common to all segments selected.',
                       'For example, if segment A has four repetitions and segment B has three,',
                       'only three repetitions are shown and the fourth one in segment A is excluded.',
                       'On the other hand, the Inclusive options shows all available repetitions in both segments.',
                       'This distinction is also applicable for frame selection.'),
                    HTML('<li>'),h5(style="text-align:left", 'Single (Exclusive): Only one repetition is selected (Non-common repetitions are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'Single (Inclusive): Only one repetition is selected (All repetitions are included)'),
                    HTML('<li>'),h5(style="text-align:left", 'Range (Exclusive): One or more repetitions are selected (Non-common repetitions are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'Range (Inclusive): One or more repetitions are selected (All repetitions are included)'),
                    HTML('<li>'),h5(style="text-align:left", 'All (Exclusive): All repetitions from the specified segments are selected (Non-common repetitions are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'All (Inclusive): All repetitions from the specified segments are selected (All repetitions are included)'),
                    tags$hr(),
                    h3(style="text-align:left", 'Frame selection'),
                    'This is a reactive selection. It means that the options vary depending on the number of segments and number of repetitions',
                    h4(style="text-align:left", 'If only one segment and one repetition are selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Single: Only one frame is selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Range: One or more frames are selected'),
                    HTML('<li>'),h5(style="text-align:left", 'All: All frames from the specified segment and repetition are selected'),
                    h1(""),
                    h4(style="text-align:left", 'If more than one segment is selected and/or more than one repetition is selected'),
                    HTML('<li>'),h5(style="text-align:left", 'Single (Exclusive): Only one frame is selected (Non-common frames are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'Single (Inclusive): Only one frame is selected (All frames are included)'),
                    HTML('<li>'),h5(style="text-align:left", 'Range (Exclusive): One or more frames are selected (Non-common frames are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'Range (Inclusive): One or more frames are selected (All frames are included)'),
                    HTML('<li>'),h5(style="text-align:left", 'All (Exclusive): All frames from the specified segments and repetitions are selected (Non-common frames are excluded)'),
                    HTML('<li>'),h5(style="text-align:left", 'All (Inclusive): All frames from the specified segments and repetitions are selected (All frames are included)'),
                    tags$hr(),
                    h3(style="text-align:left", 'Go to contiguous contours'),
                    'Select whether buttons to contiguous contours are shown. The options are Previous and Next contours.',
                    tags$hr(),
                    h3(style="text-align:left", 'Plot with Palate Trace'),
                    'Select whether the palate trace is shown in the plot. The palate trace has to be given in the input dataset.',
                    tags$hr(),
                    h3(style="text-align:left", 'Smooth Contours'),
                    'Select whether contours are smoothed or shown as raw data.'
             ))
  }else if(doc_current$data == 'in_context_info'){
    tags$div(style="text-align:justify",
             tags$p(h2('Data Visualization in Context'),
                    'This section helps identify articulatory landmakrs by visualizing contours in relation to the complete sequence. It is divided in the following sections:',
                    tags$hr(),
                    h3(style="text-align:left", 'Plot in'),
                    'The user has the option to see the plot in millimiters or pixels.',
                    'The default values exported from EdgeTrak are in pixels.',
                    'The program algorithm converts these values to millimiters depending on the',
                    'exporting settings the user selected.',
                    tags$hr(),
                    h3(style="text-align:left", 'Speaker'),
                    'Selection of Speakers in the dataset',
                    tags$hr(),
                    h3(style="text-align:left", 'Segment selection'),
                    'Selection of segment',
                    tags$hr(),
                    h3(style="text-align:left", 'Repetition selection'),
                    'Selection of repetition',
                    tags$hr(),
                    h3(style="text-align:left", 'Frame selection'),
                    'Selection of frame',
                    tags$hr(),
                    h3(style="text-align:left", 'Plot surrounding frames'),
                    'Select whether contiguous contours are plotted with the current contour.',
                    h4('None: No contiguous contours'),
                    h4('Both: Previous and following contours are plotted'),
                    h4('Before: Only previous contours are plotted'),
                    h4('After: Only following contours are plotted'),
                    h4('Colour: Select whether contours are plotted in colour or in B&W'),
                    'If contiguous contours are selected:',
                    h4('Number of extra frames: Number of frames plotted to the current frame'),
                    h4('Hide Middle Frame: Choose to hide the current frame (Middle Frame)'),
                    tags$hr(),
                    h3(style="text-align:left", 'Go to contiguous contours'),
                    'Select whether buttons to contiguous contours are shown. The options are Previous and Next contours.',
                    tags$hr(),
                    h3(style="text-align:left", 'Plot with Palate Trace'),
                    'Select whether the palate trace is shown in the plot. The palate trace has to be given in the input dataset.',
                    tags$hr(),
                    h3(style="text-align:left", 'Smooth Contours'),
                    'Select whether contours are smoothed or shown as raw data.'
             ))
  }else if(doc_current$data == 'contours_info'){
    tags$div(style="text-align:justify",
             tags$p(h2('Controlling Graphics - Tongue Contours'),
                    'This section gives users the option to modify the graphic parameters of tongue contours. It is divided in the following sections:',
                    tags$hr(),
                    h3(style="text-align:left", 'Colour Set'),
                    h5('This applies mainly when a multiple selection of frames from a single segment has been selected.'),
                    h5('If only one frame is selected, the contour colour is light or blank (Mono) because the colour set is based on a continuum.'),
                    h5('If more than one segment is selected, the colour set selection will not affect the colours.'),
                    tags$hr(),
                    h3(style="text-align:left", 'Contours Colour Alpha Value'),
                    'Selection of the alpha value. Close to 0 is invisible. Close to 1 is darker.',
                    tags$hr(),
                    h3(style="text-align:left", 'Contours Line Width'),
                    'Width of line',
                    tags$hr(),
                    h3(style="text-align:left", 'Contours Smooth Spline Value'),
                    'Smoothing parameter value. Closer to 0 is less smooth.',
                    tags$hr(),
                    h3(style="text-align:left", 'Line Type'),
                    'Type of line for the contour'
             ))
  }else if(doc_current$data == 'palate_info'){
    tags$div(style="text-align:justify",
             tags$p(h2('Controlling Graphics - Palate Trace'),
                    'This section gives users the option to modify the graphic parameters of the palate trace. It is divided in the following sections:',
                    tags$hr(),
                    h3(style="text-align:left", 'Palate Trace Colour'),
                    'Colour of the Palate Trace',
                    tags$hr(),
                    h3(style="text-align:left", 'Palate Trace Colour Alpha Value'),
                    'Selection of the alpha value. Close to 0 is invisible. Close to 1 is darker.',
                    tags$hr(),
                    h3(style="text-align:left", 'Palate Trace Line Width'),
                    'Width of line',
                    tags$hr(),
                    h3(style="text-align:left", 'Palate Trace Smooth Spline Value'),
                    'Smoothing parameter value. Closer to 0 is less smooth.',
                    tags$hr(),
                    h3(style="text-align:left", 'Line Type'),
                    'Type of line for the palate trace'
             ))
  }else if(doc_current$data == 'graphics_window_info'){
    tags$div(style="text-align:justify",
             tags$p(h2('Controlling Graphics - Graphics Window'),
                    'This section gives users the option to modify the graphic parameters of text and legens. It is divided in the following sections:',
                    tags$hr(),
                    h3(style="text-align:left", 'Choose Font'),
                    'Font type for all text labels',
                    tags$hr(),
                    h3(style="text-align:left", 'Text Colour'),
                    'Colour for all text labels',
                    tags$hr(),
                    h3(style="text-align:left", 'Axis Labels Size'),
                    'Size for labels in axes',
                    tags$hr(),
                    h3(style="text-align:left", 'Legend Title Size'),
                    'Size for legend title',
                    tags$hr(),
                    h3(style="text-align:left", 'Axis Ticks Size'),
                    'Size of axis tick marks'
             ))
  }
})