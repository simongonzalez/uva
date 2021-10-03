#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#creates the speaker selection widget
output$speaker_LM <- renderUI({
  #if there is no input file
  if (is.null(importFiles))
    return()
  
  #imports the input file
  #selects the dataframe with values
  #gets the unique labels of the speakers
  spkrs <- importFiles()[[2]] %>%
    .$speaker %>% sort() %>% unique()
  
  #creates the speaker widget
  selectInput('spkr_LM', 'Speaker', spkrs)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This is part of the selection section
#Controls the SEGMENTS input from the user.
#creates the segment selection widget
output$segment_LM <- renderUI({
  #if there is no speaker widget
  if (is.null(input$spkr_LM))
    return()
  
  #gets speaker label
  speaker_label <- input$spkr_LM
  
  #imports the input file
  #selects the dataframe with values
  #deletes palate trace from the subset
  #gets segment label
  segs <- importFiles()[[2]] %>%
    filter(speaker == speaker_label) %>%
    filter(segment != "pal") %>%
    .$segment %>% sort() %>% unique()
  
  #creates the segment widget
  selectInput('sgmnt_LM', label = 'Segment', c(segs), selected = segs[1], multiple = F)
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This is part of the selection section
#Controls the REPETITIONS input from the user.
#creates the repetition selection widget
output$repetition_LM <- renderUI({
  #if there is no segment widget
  if (is.null(input$sgmnt_LM))
    return()
  
  #gets speaker label
  speaker_label <- input$spkr_LM
  
  #gets segment label
  seg <- input$sgmnt_LM
  
  #imports the input file
  #selects the dataframe with values
  #unique values of repetitions
  reps <- importFiles()[[2]] %>%
    filter(speaker == speaker_label) %>%
    filter(segment == seg) %>%
    .$repetition %>% sort() %>% unique()
  
  #creates the repetition selection based on input
  selectInput('rpttn_LM', label = "Repetition", c(reps), selected = 1)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This is part of the selection section
#Controls the FRAMES input from the user.

#initiates a variable to store the maximun number of frames for plotting
max_frames_LM <- reactiveValues(n=1)

#creates the frame selection widget
output$frame_LM <- renderUI({
  if(is.null(input$spkr))
    return()
  if(is.null(input$sgmnt_LM))
    return()
  if(is.null(input$rpttn_LM))
    return()
  
  #gets speaker label
  speaker_label <- input$spkr
  #gets segment label
  seg <- input$sgmnt_LM
  #gets repetition label
  rep <- input$rpttn_LM
  
  #imports the input file
  #selects the dataframe with values
  #subsets the data to the speaker and omits the palata trace data
  #subsets dataframe to the segment input
  #Gets frame values
  frames <- importFiles()[[2]] %>%
    filter(speaker == speaker_label) %>%
    filter(segment != "pal") %>%
    filter(segment %in% seg) %>%
    filter(repetition %in% rep) %>%
    .$frame %>% sort() %>% unique()
  
  max_frames_LM$n <- frames
  
  #creates widget selection input
  if(!is.null(cntxt_plt_LM$n)){
    tmp_selected <- cntxt_plt_LM$n
  }else{
    tmp_selected <- 1
  }
  
  selectInput('frm_LM', 'Frame', c(1:frames), selected = tmp_selected)
  
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section controls Previous and Following segments
#this reactive value controls the number of extra frames to be plotted
output$extra_frames_number_LMUI <- renderUI({
  if(input$extra_frames_LM > 1){
    sliderInput("extra_frames_number_LM", label = "Number of extra frames", 
                min = 0, max = 7, value = 1, step = 1, animate = T)
  }else{
    return()
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value creates the Previous (button) icon
output$prevBttn_LM <- renderUI({
  bsButton("previousButton_LM", label = "Previous", icon("chevron-circle-left"), style = "primary")
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value creates the next (button) icon
output$follBttn_LM <- renderUI({
  bsButton("nextButton_LM", label = "Next", icon("chevron-circle-right"), style = "primary")
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#reactive value to store the number of the current working contour
cntxt_plt_LM <- reactiveValues(n=NULL)
#this section observes the behaviour of the next button
observeEvent(input$nextButton_LM, {
  cntxt_plt_LM$n <- as.numeric(input$frm_LM) + 1
  if(cntxt_plt_LM$n > max_frames_LM$n){
    cntxt_plt_LM$n <- max_frames_LM$n
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this section observes the behaviour of the previous button
observeEvent(input$previousButton_LM, {
  cntxt_plt_LM$n <- as.numeric(input$frm_LM) - 1
  if(cntxt_plt_LM$n < 1){
    cntxt_plt_LM$n <- 1
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section controls the colours of contours
#this reactive value controls the text of the plot title
output$text_main <- renderUI({
  ttl <- 'get_plot_title()'
  
  return(NULL)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value controls the tongue contour
output$tongue_colour_setUI <- renderUI({
  if(input$tongue_colour_set_radio == 1){  
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices <- c("Default", "Grey", "Blue", "Green", "Red", "Purple", "Orange"), selected = "Default")
  }else if(input$tongue_colour_set_radio == 2){
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices <- c("Red-Grey", "Red-Blue", "Purple-Orange", "Purple-Green", "Brown-Green"))
  }else if(input$tongue_colour_set_radio == 3){
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices <- c("RedYellowGreen", "RedYellowBlue", "YellowGreenBlue"))
  }else if(input$tongue_colour_set_radio == 4){
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices <- c("Spectral", "Rainbow", "Heat", "Terrain", "Topo", "Random"))
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value inverts the colors of the tongue contours
output$invert_coloursUI <- renderUI({
  fl <- plotStatic_data()
  fl <- fl[[1]]
  frms <- unique(fl$frame)
  frms_nmbr <- length(frms)
  
  if(frms_nmbr > 1 && input$tongue_colour_set!='Default'){
    checkboxInput("invert_colours", label = "Invert Colour Order", value = F)
  }else{
    return()
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value controls the hiding of the middle frame in the plot
output$hide_middle_frame_LMUI <- renderUI({
  if(input$extra_frames_LM > 1){
    checkboxInput("hide_middle_frame_UI", label = "Hide Middle Frame", value = F)
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this reactive value controls the colours of the contours, whether black and white or in colour
output$colour_contiguous_frames_LMUI <- renderUI({
  radioButtons("colour_contiguous_frames_LM", label = NULL,
               choices = list("Colour" = 1, "B&W" = 2),
               selected = 1, inline = T)
})
