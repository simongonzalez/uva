library(shiny)
library(DT)
library(pryr)
library(shinyjs)
library(ggplot2)
library(colourpicker)
library(ggrepel)
library(sp)
library(rgeos)

options(shiny.maxRequestSize = 100*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  autoInvalidate <- reactiveTimer(2000)
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - I FUNCTIONS
  #functions--------------------------------------------------------------
  #function to find the angle between the origin point and the intersection point
  #....................................................................................
  find_angle = function(origin, point){
    zero_point.x = 10
    zero_point.y = 0
    
    measure_point.x = point[1] - origin[1]
    measure_point.y = point[2] - origin[2]
    
    angle = atan2(measure_point.y, measure_point.x) - atan2(zero_point.y, zero_point.x)
    
    #covert from radians to degrees
    angle = angle * 360 / (2*pi)
    
    if (angle < 0){
      angle = angle + 360
    }
    left_angle = angle
    right_angle = 180 - angle
    
    return(list(left_angle, right_angle))
  }
  
  
  #function to give the xy point given the origin point, a line length and an angle
  #returns the xy of the other end of the line
  #....................................................................................
  lines_fn <- function(x0, y0, length_ln, angle_ln){
    
    angle_ln = angle_ln * pi / 180
    
    ab <- cos(angle_ln) * length_ln
    bc <- sign(sin(angle_ln)) * sqrt(length_ln^2 - ab^2)
    
    x1 <- x0 + ab
    y1 <- y0 + bc
    
    vals = c(x1,y1)
    
    return(vals)
  }
  
  lines_fn <- function(x0, y0, length_ln, angle_ln){
    
    angle_ln = angle_ln * pi / 180
    
    ab <- cos(angle_ln) * length_ln
    bc <- sign(sin(angle_ln)) * sqrt(length_ln^2 - ab^2)
    
    x1 <- x0 + ab
    y1 <- y0 + bc
    
    vals = c(x1,y1)
    
    return(vals)
  }
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - II Input files
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #thi section inputs the file uploaded
  importFiles <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    #input file from upload
    inFile <- input$file1
    
    #if a file has been input
    if (is.null(inFile))
      return(NULL)
    
    
    #shows the progress of the data upload
    withProgress(message = 'Importing Data - Please wait', value = 0.1,{
      
      #reads in the csv file
      file <- read.csv(inFile$datapath, header = input$header,
                       sep = input$sep, quote = input$quote)
      
      file$X <- NULL
      #make sure there are 100 points per tongue contour......................
      ##..............................................
      ###...............................................
      nameOfUnit <- names(file)[ncol(file)]

      dat <- file
      # xvalues <- dat[dat$coord == 'x',nameOfUnit]
      # yvalues <- dat[dat$coord == 'y',nameOfUnit]
      # 
      # dat <- dat[dat$coord == 'x',]
      # dat$coord <- NULL
      # dat[[nameOfUnit]] <- NULL
      # 
      # dat$X <- xvalues
      # dat$Y <- yvalues
      # 
      # tmpxpoint <- max(dat$X)
      # tmpypoint <- min(dat$Y) - (1/abs(min(dat$Y) - max(dat$Y)))
      # 
      # dat$X <- abs(tmpxpoint - dat$X)
      # dat$Y <- abs(tmpypoint - dat$Y)
      # 
      # dat$X <- sqrt((dat$X^2) + (dat$Y^2))#radius
      # dat$Y <- atan2(dat$Y, dat$X)#theta
      # 
      # xvalues <- dat$X
      # yvalues <- dat$Y
      # 
      # dat$X <- NULL
      # dat$Y <- NULL
      # 
      # dat <- dat[rep(seq_len(nrow(dat)), each = 2), ]
      # 
      # dat$coord <- c('x', 'y')
      # 
      # dat[[nameOfUnit]] <- c(rbind(xvalues,yvalues))

      #..............................................
      ##..............................................
      ###..............................................
      ###
      file <- dat

      df <- file
      
      cntr <- 1
      
      for(spi in sort(unique(df$speaker))){
        dfspi <- df[df$speaker == spi,]
        
        for(segi in sort(unique(dfspi$segment))){
          dfsegi <- dfspi[dfspi$segment == segi,]
          
          for(repi in sort(unique(dfsegi$repetition))){
            dfrepi <- dfsegi[dfsegi$repetition == repi,]
            
            for(fri in sort(unique(dfrepi$frame))){
              dffri <- dfrepi[dfrepi$frame == fri,]
              
              xvalues <- dffri[dffri$coord == 'x',nameOfUnit]
              yvalues <- dffri[dffri$coord == 'y',nameOfUnit]
              
              xvalues <- approx(xvalues, n = 100)$y
              yvalues <- approx(yvalues, n = 100)$y
              
              newvalues <- as.vector(rbind(xvalues,yvalues))
              
              tmpdat <- data.frame(speaker = spi, segment = segi, repetition = repi, 
                                   frame = fri, point = rep(1:100, each = 2), 
                                   coord = rep(c('x', 'y')))
              tmpdat[[nameOfUnit]] <- newvalues
              
              if(cntr == 1){
                newdat <- tmpdat
              }else{
                newdat <- rbind(newdat, tmpdat)
              }
              
              cntr <- cntr + 1
              
            }
          }
        }
      }
      #..............................................
      ##..............................................
      ###..............................................
      file <- newdat
      
      file$speaker <- as.character(file$speaker)
      
      #gest the label of the speaker
      speakers_lbl <- unique(file$speaker)
      #gets number of speakers
      speakers_nmbr <- length(speakers_lbl)
      
      #creates an empty dataframe
      df0 <- data.frame(matrix(ncol = 4, nrow = 0))
      #creates the names of the dataframe
      names(df0) <- c("speaker", "segment", "repetition", "frame")
      #creates a dataframe to store running values
      old_df <- df0
      
      #iterates for the total number of speakers
      for(i in 1:speakers_nmbr)
      {
        #gets the label of the segment
        segments_lbl <- unique(file[file$speaker == speakers_lbl[i], 'segment'])
        #gets the number of segments
        segments_nmbr <- length(segments_lbl)
        
        #iterates for the total number of segments
        for(j in 1:segments_nmbr)
        {
          #gets the labels of the rpetitions for the iterated segment
          repetitions_lbl <- unique(file[file$speaker == speakers_lbl[i] & 
                                           file$segment == segments_lbl[j], 'repetition'])
          #gets the number of repetitions for the iterated segment
          repetitions_nmbr <- length(repetitions_lbl)
          
          #iterates for the total number of repetitions in the iterated segment
          for(k in 1:repetitions_nmbr)
          {
            #gets the labels of the frames for each repetition
            frames_lbl <- unique(file[file$speaker == speakers_lbl[i] & file$segment == segments_lbl[j] & 
                                        file$repetition == repetitions_lbl[k], 'frame'])
            #gets the number of frames for each repetition
            frames_nmbr <- length(frames_lbl)
            
            #creates a temporal dataframe to store the iterated values
            df1 <- data.frame(matrix(ncol = 4, nrow = 1))
            #creates names of the uterated dataframe
            names(df1) <- c("speaker", "segment", "repetition", "frame")
            
            #gets the segment labels
            tmp_seg_lbl <- levels(droplevels(segments_lbl[j]))
            
            #stores speaker label
            df1[1,1] <- speakers_lbl[i]
            #stores segment label
            df1[1,2] <- tmp_seg_lbl
            #stores the repetition label
            df1[1,3] <- repetitions_lbl[k]
            #stores the frame label
            df1[1,4] <- frames_nmbr
            
            #adds the new line to the working dataframe
            current_df <- rbind(old_df, df1)
            
            #adds the working dataframe to the final dataframe
            old_df <- current_df
          }
        }
      }
      #names the columns of the dataframe
      names(current_df) <- c("speaker", "segment", "repetition", "frame")
    })
    #returns the values from the reactive value
    #there are two outputs:
    #1. The csv file and the current dataframe with the specified values
    return(list(file, current_df, nameOfUnit))
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #data inspection
  output$radio_measuresUI <- renderUI({
    if(is.null(importFiles()))
      return()
    
    unitsOption <- importFiles()[[3]]
    
    if(unitsOption == 'mm'){
      radioButtons("radio_measures", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 1, inline = T)
    }else{
      radioButtons("radio_measures", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 2, inline = T)
    }
  })
  
  output$radio_measures_LMUI <- renderUI({
    if(is.null(importFiles()))
      return()
    
    unitsOption <- importFiles()[[3]]
    
    if(unitsOption == 'mm'){
      radioButtons("radio_measures_LM", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 1, inline = T)
    }else{
      radioButtons("radio_measures_LM", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 2, inline = T)
    }
  })
  
  output$radio_measures_GLUI <- renderUI({
    if(is.null(importFiles()))
      return()
    
    unitsOption <- importFiles()[[3]]
    
    if(unitsOption == 'mm'){
      radioButtons("radio_measures_GL", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 1, inline = T)
    }else{
      radioButtons("radio_measures_GL", label = h5("Plot in"),
                   choices = list("Millimeters" = 1, "Pixels" = 2),
                   selected = 2, inline = T)
    }
  })
  
  


  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - III DATA Summary
  
  #This section outputs the summary of the data input
  #================================================================================
  #================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the reactive object - DT table with output
  output$summary <- DT::renderDataTable({
    #if a file has not been input
    if (is.null(input$file1))
      return()
    
    #imports the input file
    file <- importFiles()
    #selects the dataframe with values
    file <- file[[2]]
    
    #print('im here')
    
    #creates the DT table output
    DT::datatable(file, style = "bootstrap", selection="none")
    
    #DT::datatable(file, options = list(searching = T), style = "bootstrap")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - IV Basic visualisation
  
  #This is part of the selection section
  #Controls the SPEAKERS input from the user.
  #============================================================================================
  #============================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the speaker selection widget
  output$speaker <- renderUI({
    #if there is no input file
    if (is.null(input$file1))
      return()
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    #gets the unique labels of the speakers
    spkrs <- unique(fileIn$speaker)
    
    #creates the speaker widget
    selectInput(inputId = 'spkr', label = 'Speaker', choices = spkrs, multiple = FALSE)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  #This is part of the selection section
  #Controls the SEGMENTS input from the user.
  #============================================================================================
  #============================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the segment selection widget
  output$segment <- renderUI({
    #if there is no speaker widget
    if (is.null(input$spkr))
      return()
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    #deletes palate trace from the subset
    fileIn <- fileIn[fileIn$segment != "pal",]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp <- input$spkr
    #gets segment labels----------------------------------------------------------------------------
    segs <- unique(fileIn[fileIn$speaker == sp, 'segment'])
    
    #creates the segment widget
    #if there is only one segment selected........................................................
    if(input$segment_selection == 1)
    {
      #single selection
      selectInput(inputId = 'sgmnt', label = NULL, choices = c(segs), selected = segs[1], multiple = F)
    }else if(input$segment_selection == 2){
      #if there are two segments selected........................................................
      #multiple selection
      selectInput(inputId = 'sgmnt', label = NULL, choices = c(segs), selected = segs[1], multiple = T)
    }else{
      #if all segments are selected........................................................
      #all segments
      return()
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the radio buttons for repetition selection
  output$repetition_radioUI = renderUI({
    
    if(length(input$sgmnt) == 1 && input$segment_selection != 3){
      #if there is only one segment selected and this is not equal to All
      radioButtons("repetition_radio", label = h5("Repetition Selection"),
                   choices = list("Single" = 1,
                                  "Range" = 2,
                                  "All" = 3),
                   selected = 1, inline = T)
    }else if(length(input$sgmnt) > 1 || input$segment_selection == 3){
      #if multiple segments are selected, including All segments
      radioButtons("repetition_radio", label = h5("Repetition Selection"),
                   choices = list("Single (Exclusive)" = 1, "Single (Inclusive)" = 2,
                                  "Range (Exclusive)" = 3,  "Range (Inclusive)" = 4,
                                  "All (Exclusive)" = 5, "All (Inclusive)" = 6),
                   selected = 1, inline = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  #This is part of the selection section
  #Controls the REPETITIONS input from the user.
  #============================================================================================
  #============================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the repetition selection widget
  output$repetition <- renderUI({
    #if there is no segment widget
    if (is.null(input$sgmnt))
      return()
    
    #imports the input file
    file1 <- importFiles()
    #selects the dataframe with values
    file1 <- file1[[2]]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp = input$spkr
    
    #gets segment label-----------------------------------------------------------------------------
    if(input$segment_selection == 3)
    {
      #if All segments are selected
      seg <- "ALL"
    }else{
      #if NOT All segments are selected (it can be a single segment(1), or multiple segments(2))
      seg <- input$sgmnt
    }
    
    #===========================================================================================
    #subsets file to the selected sehgments and omits the palate trace from the data frame
    fileIn <- file1[file1$speaker == sp & file1$segment != "pal",]
    
    if(length(seg) == 1 && input$segment_selection != 3){
      #if only one segment is selected and this is NOT All segments
      
      #subsets data to the specified speaker, segment(s) and extracts the repetition numbers
      all_reps <- fileIn[fileIn$speaker == sp & fileIn$segment %in% seg, 'repetition']
      #unique values of repetitions
      unique_reps <- unique(all_reps)
      #maximum repetition number
      reps <- max(unique_reps)
      
    }else if (length(seg) > 1 || input$segment_selection == 3){
      #if more than one segment is selected or All segments are selected
      
      if(seg == "ALL"){
        #if all segments are selected
        
        #get all the segments for the speaker
        segs <- unique(fileIn[fileIn$speaker == sp, 'segment'])
      }else{
        #if NOT All segments are selected
        
        #subsets data to the specified segment(s)
        segs <- unique(fileIn[fileIn$speaker == sp & fileIn$segment == seg, 'segment'])
      }
      
      #initialises an empty array to store the maximum nuber of repetitions
      tmp_data <- c()
      
      #iterates for each segment
      for(i in 1:length(segs))
      {
        #gets the number of repetitions for the segment iterated
        reps <- unique(fileIn[fileIn$speaker == sp & fileIn$segment == segs[i], 'repetition'])
        #stores the maximum number in the array
        tmp_data[i] <- max(reps)
      }
      
      #checks whether the repetions are INCLUSIVE or EXCLUSIVE
      if(input$repetition_radio == 2 || input$repetition_radio == 4 || input$repetition_radio == 6){
        #INCLUSIVE = gets all the repetitions
        reps <- max(tmp_data)
      }else if(input$repetition_radio == 1 || input$repetition_radio == 3 || input$repetition_radio == 5){
        #EXCLUSIVE = gets only the repetitions that are common across all segments
        reps <- min(tmp_data)
      }
    }
    
    #===========================================================================================
    #creates the repetion selection based on input
    
    if(length(seg) == 1 && input$segment_selection != 3){
      #if only one segment has been selected and it is NOT the All option
      if(input$repetition_radio == 1){
        #if a SINGLE repetition is selected
        selectInput(inputId = 'rpttn', label = NULL, choices = c(1:reps))
      }else if(input$repetition_radio == 2){
        #if a repetition RANGE is selected
        sliderInput(inputId = 'rpttn', label = NULL,
                    min = 1, max = reps, value = c(1,1), step = 1, animate = T)
      }else if(input$repetition_radio == 3){
        #if All repetitions are selected
        return()
      }
    }else if(length(seg) > 1 || input$segment_selection == 3){
      #if more than one segment is selected including All segments
      if(input$repetition_radio == 1 || input$repetition_radio == 2){
        #if a SINGLE repetition is selected
        selectInput(inputId = 'rpttn', label = NULL, choices = c(1:reps))
      }else if(input$repetition_radio == 3 || input$repetition_radio == 4){
        #if a repetition RANGE is selected
        sliderInput(inputId = 'rpttn', label = NULL,
                    min = 1, max = reps, value = c(1,1), step = 1, animate = T)
      }else if(input$repetition_radio == 5 || input$repetition_radio == 6){
        #if All repetitions are selected
        return()
      }
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the radio buttons for frame selection
  output$radio_framesUI = renderUI({
    
    #repetition input
    rep = input$rpttn
    
    #first section has the options but having the same result: one segment and one repetition 
    #1. one segment in single selection and one repetition in single selection
    #2. one segment in multiple selection and one repetition in single selection
    #3. one segment in single selection and one repetition in multiple selection
    #4. one segment in multiple selection and one repetition in multiple selection
    
    if((input$segment_selection == 1 && input$repetition_radio == 1) || 
       (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 1) ||
       (input$segment_selection == 1 && input$repetition_radio == 2 && length(rep) > 1 && rep[1] == rep[2]) || 
       (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 2 &&
        length(rep) > 1 && rep[1] == rep[2])){
      radioButtons("radio_frames", label = h5("Frame Selection"),
                   choices = list("Single" = 1, "Range" = 2, "ALL" = 3),
                   selected = 1, inline = T)
    }else{
      
      radioButtons("radio_frames", label = h5("Frame Selection"),
                   choices = list("Single (Exclusive)" = 1, "Single (Inclusive)" = 2,
                                  "Range (Exclusive)" = 3,  "Range (Inclusive)" = 4,
                                  "All  (Exclusive)" = 5, "All (Inclusive)" = 6),
                   selected = 1, inline = T)
      
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This is part of the selection section
  #Controls the FRAMES input from the user.
  #============================================================================================
  #============================================================================================
  
  #initiates a variable to store the maximun number of frames for plotting
  
  max_frames <- reactiveValues(n=1)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the frame selection widget
  output$frame <- renderUI({
    
    #imports the input file
    file1 <- importFiles()
    #selects the dataframe with values
    file1 <- file1[[2]]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp <- input$spkr
    
    #subsets the data to the speaker and omits the palata trace data
    fileIn <- file1[file1$speaker == sp & file1$segment != "pal",]
    
    #segments-----------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------
    #Segment Selection
    if(input$segment_selection == 3){
      #if All segments is selected
      seg <- "ALL"
      
      #selects all segments for this speaker
      segs <- unique(fileIn[fileIn$speaker == sp, 'segment',])
    }else{
      #if NOT All segments are selected
      
      #gets the segment input
      seg <- input$sgmnt
      
      #gets the unique values of the segments
      segs <- unique(fileIn[fileIn$speaker == sp & fileIn$segment == seg, 'segment',])
    }
    
    #===========================================================================================
    #set repetitions
    
    if(seg == "ALL"){
      #if All segments are selected
      
      #Repetition Selection not including ALL options
      if(input$repetition_radio < 5){
        rep <- input$rpttn
      }else{
        #Repetition Selection including ALL options
        rep <- "ALL"
      }
    }else{
      #Segment SelectInput != ALL (Single and Multiple)
      
      #SelectInput is only one segment
      if(input$segment_selection == 1 || length(seg) == 1){
        
        #Repetition Selection is not ALL (Single and Range)
        if(input$repetition_radio < 3){
          rep <- input$rpttn
        }else{
          #Repetition Selection is ALL
          rep <- "ALL"
        }
        
        #SelectInput is more than one segment
      }else if(input$segment_selection == 2 && length(seg) > 1){
        #Repetition Selection is not ALL (Single (inclusive and exclusive) and Range (inclusive and exclusive))
        if(input$repetition_radio < 5){
          rep <- input$rpttn
        }else{
          #Repetition Selection is ALL (inclusive and exclusive)
          rep <- "ALL"
        }
      }
    }
    
    #Gets repetion values-----------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------------
    #Repetion and Repetition Selection
    if (rep == "ALL"){
      #if All repetitions is selected
      
      #determines the number of repetitions based on the selection
      
      #inititates an empty array to store the repetition values
      tmp_rep_vals <- c()
      
      #iterates for each segment
      #finds the repetition value for each segment
      for(i in 1:length(segs)){
        #gets the repetition values for the segment(s) selected
        all_reps <- fileIn[fileIn$speaker == sp & fileIn$segment == segs[i], 'repetition']
        #gets the unique values of the repetitions
        reps_labels <- unique(all_reps)
        #gets the length of the labels
        reps <- length(reps_labels)
        
        #stores the repetitions in the array
        tmp_rep_vals[i] <- reps
      }
      
      #if All segments are selected or multiple segments are selected
      if(input$segment_selection == 3 || length(seg) > 1){
        #if All (Exclusive) is selected
        if(input$repetition_radio == 5){
          #creates a repetition cutoff based on the minimum repetition value
          rep_cutoff <- min(tmp_rep_vals)
        }
      }
      
      #===========================================================================================
      #sets the repetition values, initial and final
      tmp_data_reps <- c()
      
      #iterates for each segment
      for(i in 1:length(segs)){
        #gets repetitions per segment
        all_reps <- fileIn[fileIn$speaker == sp & fileIn$segment == segs[i], 'repetition']
        #gets unique labels
        reps_labels <- unique(all_reps)
        #gets length of repetition labels
        reps <- length(reps_labels)
        
        #if All segments are selected or multiple segments are selected
        if(input$segment_selection == 3 || length(seg) > 1){
          #if All (Exclusive) is selected
          if(input$repetition_radio == 5){
            #if the repetition length is larger than the repetition cutoff
            if(reps > rep_cutoff){
              #repetitions length is the same as the repetition cutoff
              reps <- rep_cutoff
              #changes labels for the corresponding repetition labels
              reps_labels <- reps_labels[1:rep_cutoff]
            }
          }
        }
        
        #If only one repetition is selected
        if(reps == 1){
          #the initial repetition label is the same as the final repetition label
          #sets the initial repetition label
          init_rep <- reps_labels
          #sets the final repetition label
          final_rep <- reps_labels
        }else{
          #If more than one repetition is selected
          
          #sets the initial repetition label
          init_rep <- reps_labels[1]
          #sets the final repetition label
          final_rep <- reps_labels[reps]
        }
        
        #===========================================================================================
        #initiates an empty vector for storing the frame values for plotting
        tmp_data_frames <- c()
        
        #if only one repetition is selected
        if(reps == 1){
          #gets the frames for one segment and one repetition
          tmp_data_frames <- fileIn[fileIn$speaker == sp & fileIn$segment == segs[i] & 
                                      fileIn$repetition == init_rep, 'frame']
        }else{
          #if multiple repetitions are selected
          
          #iterates from initial to final repetition labels
          for(j in init_rep:final_rep){
            #gets the frames for one segment and multiple repetitions
            tmp_data_frames[j] <- fileIn[fileIn$speaker == sp & fileIn$segment == segs[i] & 
                                           fileIn$repetition == j, 'frame']
          }
        }
        
        #===========================================================================================
        #adds value to the array storing repetition values for plotting
        
        #if ALL segments are selected
        if(seg == "ALL"){
          
          #adds all repetition labels to the array
          tmp_data_reps <- append(tmp_data_reps, tmp_data_frames)
          
        }else{
          #if NOT ALL segments are selected
          
          #If only one segment is selected
          if(input$segment_selection == 1 || length(seg) == 1){
            # #adds the maximum repetition label to the array
            # tmp_data_reps[i] = min(tmp_data_frames)
            
            #Exclusive selections (Single, Range and ALL)
            if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
              tmp_data_reps[i] <- min(tmp_data_frames)
              #Inclusive selections (Single, Range and ALL)
            }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
              tmp_data_reps[i] <- max(tmp_data_frames)
            }
            
            #If more than one segment is selected
          }else if(input$segment_selection == 2 && length(seg) > 1){
            
            #adds all repetition labels to the array
            tmp_data_reps <- append(tmp_data_reps, tmp_data_frames)
          }
        }
      }
      #tmp_data_reps = append(tmp_data_reps, tmp_data_frames)
      
      #frames-------------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------------------
      #If ALL segments are selected
      if(seg == "ALL"){
        #ALL (Exclisve)
        if(input$repetition_radio == 5){
          #Exclusive selections (Single, Range and ALL)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            frames <- min(tmp_data_reps)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
          #ALL (Inclisve)
        }else if(input$repetition_radio == 6){
          #Exclusive selections (Single, Range and ALL)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            frames <- min(tmp_data_reps)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
        }
      }else{
        #If NOT ALL segments are selected
        
        #If only one segment is selected
        if(input$segment_selection == 1 || length(seg) == 1){
          #frames = max(tmp_data_reps)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            #print('here2')
            frames <- min(tmp_data_reps)
            #print(frames)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
          
          
          #If more than one segment is selected
        }else if(input$segment_selection == 2 && length(seg) > 1){
          
          #ALL (Exclisve)
          if(input$repetition_radio == 5){
            #Exclusive selections (Single, Range and ALL)
            if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
              frames <- min(tmp_data_reps)
              #Inclusive selections (Single, Range and ALL)
            }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
              frames <- max(tmp_data_reps)
            }
            
            #ALL (Inclisve)
          }else if(input$repetition_radio == 6){
            #Exclusive selections (Single, Range and ALL)
            if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
              frames <- min(tmp_data_reps)
              #Inclusive selections (Single, Range and ALL)
            }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
              frames <- max(tmp_data_reps)
            }
            
          }
        }
      }
      #--------------------------------------------------------------------------------------------------------
    }else{
      #if not all repetitions are selected
      
      tmp_data_reps <- c()
      
      empty_frame <- 0
      empty_frame_array <- 0
      
      #iterates each segment
      for(i in 1:length(segs)){
        #if only one repetition is selected
        if(length(rep) == 1){
          reps_labels <- rep
          reps <- 1
        }else{
          #if multiple repetitions are selected
          reps_labels <- c(rep[1]:rep[2])
          reps <- length(reps_labels)
        }
        
        #If only one repetition is selected
        if(reps == 1)
        {
          init_rep <- reps_labels
          final_rep <- reps_labels
        }else{
          #If more than one repetition is selected
          init_rep <- reps_labels[1]
          final_rep <- reps_labels[reps]
        }
        
        tmp_data_frames <- c()
        
        #stores the frames in an array
        if(reps == 1){
          frames = fileIn[fileIn$speaker == sp & fileIn$segment == segs[i] & fileIn$repetition == init_rep, 'frame']
          if(length(frames) == 0){
            empty_frame <- 1
          }else{
            empty_frame <- 0
            tmp_data_frames <- frames
          }
        }else{
          tmp_frm_loop_val <- 1
          for(j in init_rep:final_rep){
            frames <- fileIn[fileIn$speaker == sp & fileIn$segment == segs[i] & fileIn$repetition == j, 'frame']
            if(length(frames) == 0){
              empty_frame <- 1
            }else{
              empty_frame <- 0
              tmp_data_frames[tmp_frm_loop_val] <- frames
              tmp_frm_loop_val <- tmp_frm_loop_val + 1
            }
          }
        }
        
        if(length(tmp_data_frames) != 0){
          #if ALL segments are selected
          if(seg == "ALL"){
            #if ALL (exclusive) repetitions are selected
            if(input$radio_frames == 1 || input$radio_frames == 3){
              tmp_data_reps[i] <- min(tmp_data_frames)
              #if ALL (inclusive) repetitions are selected
            }else if(input$radio_frames == 2 || input$radio_frames == 4){
              tmp_data_reps[i] <- max(tmp_data_frames)
            }
          }else{
            #if NOT ALL segments are selected
            
            #If only one segment is selected
            if(input$segment_selection == 1 || length(seg) == 1){
              
              if(input$repetition_radio == 1){
                tmp_data_reps[i] <- max(tmp_data_frames)
              }else if(input$repetition_radio == 2){
                
                if(length(rep) > 1 && rep[1] != rep[2]){
                  if(input$radio_frames == 1 || input$radio_frames == 3){
                    tmp_data_reps[i] <- min(tmp_data_frames)
                  }else if(input$radio_frames == 2 || input$radio_frames == 4){
                    tmp_data_reps[i] <- max(tmp_data_frames)
                  }
                }else{
                  if(input$radio_frames == 1){
                    tmp_data_reps[i] <- min(tmp_data_frames)
                  }else if(input$radio_frames == 2){
                    tmp_data_reps[i] <- max(tmp_data_frames)
                  }
                }
              }
              
              #If more than one segment is selected
            }else if(input$segment_selection == 2 && length(seg) > 1){
              if(input$repetition_radio == 1 || input$repetition_radio == 3){
                #if ALL (exclusive) repetitions are selected
                tmp_data_reps[i] <- min(tmp_data_frames)
              }else if(input$repetition_radio == 2 || input$repetition_radio == 4){
                #if ALL (inclusive) repetitions are selected
                tmp_data_reps[i] <- max(tmp_data_frames)
              }
            }
          }
        }
      }
      
      
      #frames-------------------------------------------------------
      #-------------------------------------------------------------
      #-------------------------------------------------------------
      #If ALL segments are selected
      if(seg == "ALL"){
        #ALL (Exclisve)
        if(input$repetition_radio == 1 || input$repetition_radio == 3){
          #Exclusive selections (Single, Range and ALL)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            frames <- min(tmp_data_reps)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
          #ALL (Inclisve)
        }else if(input$repetition_radio == 2 || input$repetition_radio == 4){
          #Exclusive selections (Single, Range and ALL)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            frames <- min(tmp_data_reps)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
        }
      }else{
        #If NOT ALL segments are selected
        
        #If only one segment is selected
        if(input$segment_selection == 1 || length(seg) == 1){
          frames <- max(tmp_data_reps)
          
          #If more than one segment is selected
        }else if(input$segment_selection == 2 && length(seg) > 1){
          
          #ALL (Exclisve)
          if(input$repetition_radio == 1 || input$repetition_radio == 3){
            #Exclusive selections (Single, Range and ALL)
            if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
              frames <- min(tmp_data_reps)
              #Inclusive selections (Single, Range and ALL)
            }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
              frames <- max(tmp_data_reps)
            }
            
            #ALL (Inclisve)
          }else if(input$repetition_radio == 2 || input$repetition_radio == 4){
            #Exclusive selections (Single, Range and ALL)
            if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
              frames <- min(tmp_data_reps)
              #Inclusive selections (Single, Range and ALL)
            }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
              frames <- max(tmp_data_reps)
            }
          }
        }
      }
      
    }
    
    max_frames$n <- frames
    
    #if(input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 3){
    if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 3) ||
       ((input$segment_selection == 2 && length(input$sgmnt) == 1) && 
        input$repetition_radio == 1 && input$radio_frames == 3) ||
       (input$segment_selection == 1 && input$repetition_radio != 1 && input$radio_frames > 4) ||
       ((input$segment_selection == 2 && length(input$sgmnt) == 1) && 
        input$repetition_radio != 1 && input$radio_frames > 4) ||
       (input$segment_selection == 3 && input$radio_frames > 4) ||
       (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$radio_frames > 4)){
      #&& input$repetition_radio > 4
      #if(input$radio_frames == 3){
      
      selectInput(inputId = 'frm', label = 'Frame', choices = frames)
      
    }else if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 1) ||
             (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
              input$repetition_radio == 1 && input$radio_frames == 1) ||
             (input$segment_selection == 1 && input$repetition_radio == 2 && input$radio_frames < 3) ||
             (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
              input$repetition_radio == 2 && input$radio_frames < 3) ||
             (input$segment_selection == 1 && input$repetition_radio == 3 && input$radio_frames < 3) ||
             (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
              input$repetition_radio == 3 && input$radio_frames < 3) ||
             (input$segment_selection == 3 && input$radio_frames < 3) ||
             (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$radio_frames < 3)){
      
      if((input$segment_selection == 1 && input$repetition_radio == 2 && length(rep) > 1 && 
          rep[1] == rep[2] && input$radio_frames == 2) ||
         (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 2 &&
          length(rep) > 1 && rep[1] == rep[2] && input$radio_frames == 2)){
        
        sliderInput(inputId = 'frm', label = 'Frames',
                    min = 1, max = frames, value = c(1,1), step= 1, animate = T)
        
      }else{
        
        
        if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 1) && 
           !is.null(cntxt_plt$n) ||
           (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 1 && 
            input$radio_frames == 1)){
          selectInput(inputId = 'frm', label = 'Frame', choices = c(1:frames), selected = cntxt_plt$n)
        }else{
          selectInput(inputId = 'frm', label = 'Frame', choices = c(1:frames), selected = 1)
        }
      }
      
    }else if((input$segment_selection == 1 && input$repetition_radio == 2 && 
              length(rep) > 1 && rep[1] == rep[2] && input$radio_frames == 3) ||
             (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 2 &&
              length(rep) > 1 && rep[1] == rep[2] && input$radio_frames == 3)){
      
      return(NULL)
    }else{
      
      sliderInput(inputId = 'frm', label = 'Frames',
                  min = 1, max = frames, value = c(1,1), step= 1, animate = T)
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #This section controls the previous and following buttons creation and behaviours
  
  # output$inContext = renderUI({
  #   if(input$segment_selection != 3 && length(input$sgmnt) == 1 && input$repetition_radio == 1 && input$radio_frames == 1){
  #     checkboxInput("in_Cntxt", label = "Go to contiguous contours", value = T)
  #   }else{
  #     return()
  #   }
  # })
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$prevBttn = renderUI({
    if(
      (input$segment_selection == 1 & input$repetition_radio == 1 & input$radio_frames == 1) ||
      (input$segment_selection == 2 & length(input$sgmnt) == 1 & input$repetition_radio == 1 & input$radio_frames == 1)){
      bsButton("previousButton", label = "Previous", icon("chevron-circle-left"), style = "primary")
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$follBttn = renderUI({
    if(
      (input$segment_selection == 1 & input$repetition_radio == 1 & input$radio_frames == 1) ||
      (input$segment_selection == 2 & length(input$sgmnt) == 1 & input$repetition_radio == 1 & input$radio_frames == 1)){
      bsButton("nextButton", label = "Next", icon("chevron-circle-right"), style = "primary")
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  cntxt_plt <- reactiveValues(n=NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  observeEvent(input$nextButton, {
    cntxt_plt$n = as.numeric(input$frm) + 1
    if(cntxt_plt$n > max_frames$n){
      cntxt_plt$n = max_frames$n
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  observeEvent(input$previousButton, {
    cntxt_plt$n = as.numeric(input$frm) - 1
    if(cntxt_plt$n < 1){
      cntxt_plt$n = 1
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the dataframe for plotting based on users input
  plotStatic_data <- reactive({
    #returns NULL is the number of frames is NULL
    if (is.null(input$frm))
      return()
    
    #number of points per second
    frm_nmbr_per_contour <- 100
    
    #imports the plot data
    fileIn <- importFiles()
    #gets the values of segment contours
    fileIn <- fileIn[[1]]
    d <- fileIn
    
    #gets speaker input labels
    sp <- input$spkr
    
    #gets input measurement unit
    ms <- as.character(input$radio_measures)
    commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
    #Measurement*********************************************
    if (ms == 1){
      #measurements in millimiters
      #d <- subset(d, select = -c(pixel))
      d <- d[c(commonNamesInFile, 'mm')]
      measure_val <- "mm"
    }else if (ms == 2){
      #measurements in pixels
      #d <- subset(d, select = -c(mm))
      d <- d[c(commonNamesInFile, 'pixel')]
      measure_val <- "pixel"
    }
    
    #SPEAKERS*********************************************
    fl <- d[d$speaker %in% sp, ]
    
    #Sets extrema points for the plot (xy limits)
    x_xtrm <- as.numeric(unique(fl[fl$coord == "x", measure_val]))
    x_xtrm_min <- min(x_xtrm) - 2
    x_xtrm_max <- max(x_xtrm) + 2
    
    y_xtrm <- as.numeric(unique(fl[fl$coord == "y", measure_val]))
    y_xtrm_min <- min(y_xtrm) - 2
    y_xtrm_max <- max(y_xtrm) + 2
    
    extrema <- c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
    
    #Palate*********************************************
    #sets the palata trace plot
    if(input$palate_plot == T){
      pal_fl_x <- approx(as.numeric(fl[fl$segment == 'pal' & fl$coord == 'x', measure_val]), 
                         n = frm_nmbr_per_contour)$y
      pal_fl_y <- approx(as.numeric(fl[fl$segment == 'pal' & fl$coord == 'y', measure_val]), 
                         n = frm_nmbr_per_contour)$y
      
      pal_vals <- data.frame(segment = rep("palate", length(pal_fl_x)), 
                             repetition = rep(1, length(pal_fl_x)), 
                             frame = rep(1, length(pal_fl_x)), 
                             point =  1:length(pal_fl_x), 
                             x = pal_fl_x, 
                             y = pal_fl_y)
      
    }else if(input$palate_plot == F){
      pal_vals <- data.frame(matrix(nrow = 0, ncol = 0))
    }
    
    fl <- fl[fl$speaker == sp & fl$segment != "pal",]
    
    #SEGMENTS*********************************************
    seg <- input$sgmnt
    if(input$segment_selection == 3)
    {
      segment_lbl <- unique(fl[,"segment"])
      nmbr_segment <- length(segment_lbl)
    }else{
      fl <- fl[fl$segment %in% seg, ]
      segment_lbl <- unique(fl[,"segment"])
      nmbr_segment <- length(segment_lbl)
    }
    
    #REPETITIONS*********************************************
    rep = input$rpttn
    if((input$segment_selection == 3 && input$repetition_radio == 6) ||
       (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 6) ||
       (input$segment_selection == 1 && input$repetition_radio == 3) ||
       (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 3)){
      
      repetition_lbl <- unique(fl[,"repetition"])
      nmbr_repetition <- length(repetition_lbl)
      
    }else if((input$segment_selection == 3 && input$repetition_radio == 5) ||
             (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 5)){
      
      #gets the highest common repetition
      tmp_max_frm_array <- c()
      for(tmp_seg in 1:nmbr_segment){
        tmp_fr <- unique(fl[fl$segment == segment_lbl[tmp_seg], 'repetition'])
        tmp_max_frm_array[tmp_seg] <- max(tmp_fr)
      }
      
      reps <- c(1:min(tmp_max_frm_array))
      fl <- fl[fl$repetition %in% reps, ]
      repetition_lbl <- unique(fl[,"repetition"])
      nmbr_repetition <- length(repetition_lbl)
      
    }else{
      if((input$segment_selection == 1 && input$repetition_radio == 1) ||
         (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 1) ||
         (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio < 3) ||
         (input$segment_selection == 3 && input$repetition_radio < 3)){
        
        fl <- fl[fl$repetition %in% rep, ]
        repetition_lbl <- unique(fl[,"repetition"])
        nmbr_repetition <- length(repetition_lbl)
        
      }else{
        if(rep[1] == rep[2]){
          fl <- fl[fl$repetition %in% rep, ]
          repetition_lbl <- unique(fl[,"repetition"])
          nmbr_repetition <- length(repetition_lbl)
          
        }else{
          reps <- c(rep[1]:rep[2])
          
          fl <- fl[fl$repetition %in% reps, ]
          repetition_lbl <- unique(fl[,"repetition"])
          nmbr_repetition <- length(repetition_lbl)
        }
      }
    }
    
    #carries out calculation
    #get frames for each segment, based on the repetitions
    #c = structure(list(a = a, b = b))
    
    tmp_rep_array <- list()
    
    #conditions
    #1. All segments - All (inclusive) repetition
    #2. Multiple segments - All (inclusive) repetition
    #3. One segment - Range (exclusive) repetition
    #4. One segment - Range (exclusive) repetition
    
    if((input$segment_selection == 3 && input$repetition_radio == 6) ||
       (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 6) ||
       (input$segment_selection == 1 && input$repetition_radio == 3) ||
       (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 3)){
      
      empty_condition <- 1
    }else{
      #label of repetition
      tmp_rep_nmbr <- repetition_lbl
      #number of repetitions to be plotted
      nmbr_tmp_rep_nmbr <- nmbr_repetition
    }
    
    #iterates for each segment
    
    #conditions
    #1. All segments - All (inclusive) repetition
    #2. Multiple segments - All (inclusive) repetition
    #3. One segment - Range (exclusive) repetition
    #4. One segment - Range (exclusive) repetition
    
    for(tmp_seg in 1:nmbr_segment){
      if((input$segment_selection == 3 && input$repetition_radio == 6) ||
         (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 6) ||
         (input$segment_selection == 1 && input$repetition_radio == 3) ||
         (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 3)){
        
        #subsets the repetitions of the iterated segment
        tmp_rep_nmbr <- unique(fl[fl$segment == segment_lbl[tmp_seg], 'repetition'])
        #number of repetitions for the iterated segment
        nmbr_tmp_rep_nmbr <- length(tmp_rep_nmbr)
      }
      
      #creates a matrix to store the frame labels for each repetition
      #it has two rows, one for the segment label and the second for the number of repetition
      tmp_frm_array <- matrix(ncol = (nmbr_tmp_rep_nmbr + 1), nrow = 2)
      #the first cell stores the number of repetitions to be stored
      tmp_frm_array[1,1] <- nmbr_tmp_rep_nmbr
      tmp_frm_array[2,1] <- 0
      #counter to store the numer of repetitions. It starts with 2 cince it will start storing in [1,2]
      tmp_fr_loop_cntr <- 2
      
      loop_cntr_inside <- 1
      
      #loop that gets the frame number of the selected repetition labels
      for(tmp_frm_cntr in tmp_rep_nmbr[1]:tmp_rep_nmbr[nmbr_tmp_rep_nmbr]){
        #subsets to the number of frames for the iterated segment and iterated repetition
        
        tmp_fr <- fl[fl$segment == segment_lbl[tmp_seg] & fl$repetition == tmp_frm_cntr, 'frame']
        
        #if there is at least one row in the subseted dataframe
        if(length(tmp_fr) > 0){
          
          #get the unique labels of the frames
          tmp_fr <- unique(tmp_fr)
          #number of frames for the iterated repetition
          nmbr_tmp_fr <- length(tmp_fr)
          
          #stores the values in the frames matrix
          #stores the label of the repetition number
          tmp_frm_array[1,tmp_fr_loop_cntr] <- tmp_frm_cntr
          #stores the number of frames for the given repetition
          tmp_frm_array[2,tmp_fr_loop_cntr] <- nmbr_tmp_fr
          
          #updates the counter for the frames matrix
          tmp_fr_loop_cntr <- tmp_fr_loop_cntr + 1
        }else{
          #if there are not repetitions of the iterated number for the iterated segment
          
          if((input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 2) ||
             (input$segment_selection == 3 && input$repetition_radio == 2)){
            
            #conditions
            #1. Multiple segments - Single (inclusive) repetition
            #2. All segments - Single (inclusive) repetition
            
            #all values are taken to 0, which prevents plotting for the iterated segment and repetition
            tmp_frm_array[1,1] <- 0
            tmp_frm_array[1,tmp_fr_loop_cntr] <- 0
            tmp_frm_array[2,tmp_fr_loop_cntr] <- 0
          }else if((input$segment_selection == 2 && length(input$sgmnt) > 1 && input$repetition_radio == 4) ||
                   (input$segment_selection == 3 && input$repetition_radio == 4)){
            
            #conditions
            #1. Multiple segments - Range (inclusive) repetition
            #2. All segments - Range (inclusive) repetition
            
            if(loop_cntr_inside == 1){
              tmp_frm_array <- matrix(ncol = 1, nrow = 2)
              tmp_frm_array[1,1] <- 0
              tmp_frm_array[2,1] <- 0
            }else{
              tmp_frm_array <- tmp_frm_array[,-c(tmp_fr_loop_cntr)]
              tmp_m_dim <- dim(tmp_frm_array)
              
              if(is.null(tmp_m_dim)){
                tmp_frm_array <- matrix(c(0,0), ncol = 1, nrow = 2)
              }else{
                tmp_frm_array[1,1] <- tmp_m_dim[2]-1
              }
              
            }
          }
        }
        loop_cntr_inside <- loop_cntr_inside + 1
      }
      tmp_rep_array[[tmp_seg]] <- tmp_frm_array
    }
    
    names(tmp_rep_array) <- segment_lbl
    
    #sets the frame range based on user input
    #Single inclusive
    #tmp_frm_array = list()
    prep_plot_df <- data.frame(matrix(nrow = 0, ncol = 6))
    names(prep_plot_df) <- c("segment", "repetition", "frame", "point", "x", "y")
    
    #iterates for each segment
    for(tmp_seg in 1:nmbr_segment){
      #gets the frames per repetition
      tmp_fl <- tmp_rep_array[[tmp_seg]]
      
      #number of repetitions to be plotted
      tmp_rep_it <- tmp_fl[1,1]
      
      tmp_rep_it_dim <- dim(tmp_fl)
      reps_in_loop <- tmp_rep_it_dim[2]
      
      if(tmp_rep_it > 0){
        
        repetition_lbl_init <- tmp_fl[1,2]
        repetition_lbl_fin <- tmp_fl[1,reps_in_loop]
        
        rep_inside_cntr <- 1
        for(tmp_rep_loop in repetition_lbl_init:repetition_lbl_fin){
          
          if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 1) ||
             (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
              input$repetition_radio == 1 && input$radio_frames == 1)){
            #one segement - one repetition - one frame
            init_tmp_fr_id <- input$frm
            tmp_fr_id <- input$frm
          }else if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 2) ||
                   (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
                    input$repetition_radio == 1 && input$radio_frames == 2)){
            #one segment - one repetition - RANGE of frames
            tmp_frms <- input$frm
            init_tmp_fr_id <- tmp_frms[1]
            tmp_fr_id <- tmp_frms[2]
          }else if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 3) ||
                   (input$segment_selection == 2 && length(input$sgmnt) == 1 && 
                    input$repetition_radio == 1 && input$radio_frames == 3)){
            #one segment - one repetition - All frames
            tmp_frms <- input$frm
            init_tmp_fr_id <- 1
            tmp_fr_id <- tmp_frms
          }else if((input$segment_selection == 1 && input$repetition_radio == 2) ||
                   (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 2)){
            #one segment - RANGE of repetitions
            
            if(rep[1] == rep[2]){
              #Both repetitions are the same
              if(input$radio_frames == 1){
                init_tmp_fr_id <- input$frm
                tmp_fr_id <- input$frm
              }else if(input$radio_frames == 2){
                tmp_input_frm <- input$frm
                init_tmp_fr_id <- tmp_input_frm[1]
                tmp_fr_id <- tmp_input_frm[2]
              }else if(input$radio_frames == 3){
                init_tmp_fr_id <- 1
                tmp_fr_id <- tmp_fl[2,rep_inside_cntr+1]
              }
            }else if(rep[1] != rep[2]){
              #repetitions are NOT the same
              if(input$radio_frames == 1 || input$radio_frames == 2){
                init_tmp_fr_id <- input$frm
                tmp_fr_id <- input$frm
              }else if(input$radio_frames == 3 || input$radio_frames == 4){
                tmp_frms <- input$frm
                init_tmp_fr_id <- tmp_frms[1]
                tmp_fr_id <- tmp_frms[2]
              }else if(input$radio_frames == 5 || input$radio_frames == 6){
                if(rep_inside_cntr == 1){
                  if(input$radio_frames == 5){
                    max_frm_val <- min(tmp_fl[2,2:reps_in_loop])
                  }else if(input$radio_frames == 6){
                    max_frm_val <- max(tmp_fl[2,2:reps_in_loop])
                  }
                }
                init_tmp_fr_id <- 1
                if(max_frm_val > tmp_fl[2,rep_inside_cntr+1]){
                  tmp_fr_id <- tmp_fl[2,rep_inside_cntr+1]
                }else{
                  tmp_fr_id <- max_frm_val
                }
              }
            }
          }else if((input$segment_selection == 1 && input$repetition_radio == 3) ||
                   (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 3)){
            #one segment - All repetitions
            if(input$radio_frames == 1 || input$radio_frames == 2){
              init_tmp_fr_id <- input$frm
              tmp_fr_id <- input$frm
            }else if(input$radio_frames == 3 || input$radio_frames == 4){
              tmp_frms <- input$frm
              init_tmp_fr_id <- tmp_frms[1]
              tmp_fr_id <- tmp_frms[2]
            }else if(input$radio_frames == 5 || input$radio_frames == 6){
              if(rep_inside_cntr == 1){
                if(input$radio_frames == 5){
                  max_frm_val <- min(tmp_fl[2,2:reps_in_loop])
                }else if(input$radio_frames == 6){
                  max_frm_val <- max(tmp_fl[2,2:reps_in_loop])
                }
              }
              init_tmp_fr_id <- 1
              if(max_frm_val > tmp_fl[2,rep_inside_cntr+1]){
                tmp_fr_id <- tmp_fl[2,rep_inside_cntr+1]
              }else{
                tmp_fr_id <- max_frm_val
              }
            }
          }else if(input$segment_selection > 1){
            #multiple segments
            
            if(input$radio_frames == 1 || input$radio_frames == 2){
              #one frame
              init_tmp_fr_id <- input$frm
              tmp_fr_id <- input$frm
            }else if(input$radio_frames == 3 || input$radio_frames == 4){
              #multiple frames
              tmp_frms <- input$frm
              init_tmp_fr_id <- tmp_frms[1]
              tmp_fr_id <- tmp_frms[2]
            }else if(input$radio_frames == 5 || input$radio_frames == 6){
              #All frames
              if(rep_inside_cntr == 1){
                tmp_array_frm_values_all <- c()
                for(tmp_list_i in 1:length(tmp_rep_array)){
                  tmp_array_frm_values_all <- append(tmp_array_frm_values_all, tmp_rep_array[[tmp_list_i]][2,])
                }
                
                tmp_array_frm_values_all <- tmp_array_frm_values_all[tmp_array_frm_values_all != 0]
                
                if(input$radio_frames == 5){
                  max_frm_val <- min(tmp_array_frm_values_all)
                }else if(input$radio_frames == 6){
                  max_frm_val <- max(tmp_array_frm_values_all)
                }
              }
              init_tmp_fr_id <- 1
              if(max_frm_val > tmp_fl[2,rep_inside_cntr+1]){
                tmp_fr_id <- tmp_fl[2,rep_inside_cntr+1]
              }else{
                tmp_fr_id <- max_frm_val
              }
            }
          }else{
            if(rep_inside_cntr == 1){
              
              tmp_array_frm_values_all <- c()
              for(tmp_list_i in 1:length(tmp_rep_array)){
                tmp_array_frm_values_all <- append(tmp_array_frm_values_all, tmp_rep_array[[tmp_list_i]][2,])
              }
              
              tmp_array_frm_values_all <- tmp_array_frm_values_all[tmp_array_frm_values_all != 0]
              
              if(input$radio_frames == 5){
                max_frm_val <- min(tmp_array_frm_values_all)
              }else if(input$radio_frames == 6){
                max_frm_val <- max(tmp_array_frm_values_all)
              }
            }
            
            init_tmp_fr_id <- 1
            if(max_frm_val > tmp_fl[2,rep_inside_cntr+1]){
              tmp_fr_id <- tmp_fl[2,rep_inside_cntr+1]
            }else{
              tmp_fr_id <- max_frm_val
            }
          }
          
          
          for(tmp_fr_id_loop in init_tmp_fr_id:tmp_fr_id){
            tmp_frm_array_x <- as.numeric(fl[fl$segment == segment_lbl[tmp_seg] 
                                             & fl$repetition == tmp_rep_loop & fl$frame == tmp_fr_id_loop
                                             & fl$coord == "x", measure_val])
            
            tmp_frm_array_y <- as.numeric(fl[fl$segment == segment_lbl[tmp_seg] 
                                             & fl$repetition == tmp_rep_loop & fl$frame == tmp_fr_id_loop
                                             & fl$coord == "y", measure_val])
            
            if(length(tmp_frm_array_x) > 0){
              tmp_seg_loop <- rep(segment_lbl[tmp_seg], frm_nmbr_per_contour)
              tmp_rpttn_loop <- rep(repetition_lbl[rep_inside_cntr], frm_nmbr_per_contour)
              tmp_fr_loop <- rep(tmp_fr_id_loop, frm_nmbr_per_contour)
              tmp_pnt_loop <- 1:frm_nmbr_per_contour
              
              tmp_df_loop <- data.frame(segment = rep(segment_lbl[tmp_seg], frm_nmbr_per_contour), 
                                        repetition = rep(repetition_lbl[rep_inside_cntr], frm_nmbr_per_contour), 
                                        frame = rep(tmp_fr_id_loop, frm_nmbr_per_contour), 
                                        point = 1:frm_nmbr_per_contour,
                                        x = tmp_frm_array_x, 
                                        y = tmp_frm_array_y)
              
              prep_plot_df <- rbind(prep_plot_df, tmp_df_loop)
            }
            
          }
          
          rep_inside_cntr <- rep_inside_cntr + 1
        }
      }
    }
    
    #prep_plot_df = rbind(prep_plot_df, pal_vals)
    
    return(list(prep_plot_df, tmp_rep_array, extrema, pal_vals))
    #return(list(repetition_lbl, nmbr_repetition))
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the plot object
  output$plotStatic = output$plotStaticContours = output$plotStaticPalate = output$plotStaticGraphics = renderPlot({
    #if the plot data is empty
    if(is.null(plotStatic_data()))
      return()
    
    #imports the plot data
    fl <- plotStatic_data()
    #imports the axis limits
    extrema <- fl[[3]]
    #imports the values of the palate trace
    pal <- fl[[4]]
    #gets the values of segment contours
    fl <- fl[[1]]
    
    #gets the title of the plot
    ttl <- 'get_plot_title()'
    
    #gets the colour for the palate
    pal_colour <- input$palate_colour
    #gets the line type for the palate
    pal_line_type <- as.numeric(input$line_type_palate)
    #gets the line type for the segment contours
    tongue_line_type <- as.numeric(input$line_type_tongue)
    
    #gets segment labels--------------------------------------------------------
    segs <- unique(fl$segment)
    #gets number of segments
    segs_nmbr <- length(segs)
    
    #gets repetition labels-----------------------------------------------------
    reps <- unique(fl$repetition)
    #gets number of repetitions
    reps_nmbr <- length(reps)
    
    #gets frame labels----------------------------------------------------------
    frms <- unique(fl$frame)
    #gets number of frames
    frms_nmbr <- length(frms)
    
    #if a colour set for the tongue contour is selected.........................
    if(!is.null(input$tongue_colour_set)){
      if(input$tongue_colour_set_radio == 1)
      {
        #one colour range
        if(input$tongue_colour_set=='Default'){
          pallete_colour <- c()
        }else if(input$tongue_colour_set=='Grey'){
          # colourCount = length(unique(mtcars$hp))
          # getPalette = colorRampPalette(brewer.pal(frms_nmbr, "Set1"))
          pallete_colour <- brewer.pal(frms_nmbr,"Greys")
        }else if(input$tongue_colour_set=='Blue'){
          pallete_colour <- brewer.pal(frms_nmbr,"Blues")
        }else if(input$tongue_colour_set=='Green'){
          pallete_colour <- brewer.pal(frms_nmbr,"Greens")
        }else if(input$tongue_colour_set=='Red'){
          pallete_colour <- brewer.pal(frms_nmbr,"Reds")
        }else if(input$tongue_colour_set=='Purple'){
          pallete_colour <- brewer.pal(frms_nmbr,"Purples")
        }else if(input$tongue_colour_set=='Orange'){
          pallete_colour <- brewer.pal(frms_nmbr,"Oranges")
        }
      }else if(input$tongue_colour_set_radio == 2)
      {
        #two colour range
        if(input$tongue_colour_set=='Red-Grey'){
          pallete_colour <- brewer.pal(frms_nmbr,"RdGy")
        }else if(input$tongue_colour_set=='Red-Blue'){
          pallete_colour <- brewer.pal(frms_nmbr,"RdBu")
        }else if(input$tongue_colour_set=='Purple-Orange'){
          pallete_colour <- brewer.pal(frms_nmbr,"PuOr")
        }else if(input$tongue_colour_set=='Purple-Green'){
          pallete_colour <- brewer.pal(frms_nmbr,"PRGn")
        }else if(input$tongue_colour_set=='Brown-Green'){
          pallete_colour <- brewer.pal(frms_nmbr,"BrBG")
        }
      }else if(input$tongue_colour_set_radio == 3)
      {
        #three colour range
        if(input$tongue_colour_set=='RedYellowGreen'){
          pallete_colour <- brewer.pal(frms_nmbr,"RdYlGn")
        }else if(input$tongue_colour_set=='RedYellowBlue'){
          pallete_colour <- brewer.pal(frms_nmbr,"RdYlBu")
        }else if(input$tongue_colour_set=='YellowGreenBlue'){
          pallete_colour <- brewer.pal(frms_nmbr,"YlGnBu")
        }
      }else if(input$tongue_colour_set_radio == 4)
      {
        #multi colour range
        if(input$tongue_colour_set=='Spectral'){
          pallete_colour <- brewer.pal(frms_nmbr,"Spectral")
        }else if(input$tongue_colour_set=='Rainbow'){
          pallete_colour <- rainbow(frms_nmbr)
        }else if(input$tongue_colour_set=='Heat'){
          pallete_colour <- heat.colors(frms_nmbr)
        }else if(input$tongue_colour_set=='Terrain'){
          pallete_colour <- terrain.colors(frms_nmbr)
        }else if(input$tongue_colour_set=='Topo'){
          pallete_colour <- topo.colors(frms_nmbr)
        }else if(input$tongue_colour_set=='Random'){
          pallete_colour <- distinctColorPalette(frms_nmbr)
        }
      }
    }else{
      #if no colour set is selected for the palate
      pallete_colour <- c()
    }
    
    #if colours for the palate trace are inverted
    if(!is.null(input$invert_colours) && input$invert_colours == T)
    {
      pallete_colour <- rev(pallete_colour)
    }
    
    #Plotting-------------------------------------------------------------------------------------------
    ##creates the basic setting of the plot
    
    p <- ggplot() + theme_bw() +
      theme(title = element_text(family = input$font_type, colour = input$text_color, size = input$title_size),
            axis.title.x = element_text(family = input$font_type, colour = input$text_color, size = input$axis_size),
            axis.title.y = element_text(family = input$font_type, colour = input$text_color, size = input$axis_size),
            legend.title = element_text(family = input$font_type, colour = input$text_color, size = input$legend_size),
            axis.text = element_text(size = input$ticks_size))
    
    #if contour smoothing is selected-------------------------------------------------------------------
    if(input$smooth_contour == T){
      if(segs_nmbr > 1){
        #if more than one segment is plotted............................................................
        p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(segment, repetition, frame), 
                                          colour = segment),
                           stat="smooth", method = "auto",linetype = tongue_line_type, 
                           alpha = input$tongue_alpha_slider,
                           se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
      }else{
        #if only one segment is plotted..................................................................
        if(reps_nmbr > 1){
          #if more than one repetition is selected.......................................................
          p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                            colour = repetition),
                             stat="smooth", method = "auto",linetype = tongue_line_type, 
                             alpha = input$tongue_alpha_slider,
                             se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
        }else{
          #if only one repetition is selected............................................................
          p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                            colour = frame),
                             stat="smooth", method = "auto",linetype = tongue_line_type, 
                             alpha = input$tongue_alpha_slider,
                             se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
        }
      }
    }else{
      #if not contour smoothing is selected--------------------------------------------------------------
      if(segs_nmbr > 1){
        #if more than one segment is plotted.............................................................
        p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(segment, repetition, frame), 
                                          colour = segment),
                           size = input$tongue_width_slider, linetype = tongue_line_type, 
                           alpha = input$tongue_alpha_slider)
      }else{
        #if only one segment is plotted..................................................................
        if(reps_nmbr > 1){
          #if more than one repetition is selected.......................................................
          p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                            colour = repetition),
                             size = input$tongue_width_slider, linetype = tongue_line_type, 
                             alpha = input$tongue_alpha_slider)
        }else{
          #if only one repetition is selected............................................................
          p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                            colour = frame),
                             size = input$tongue_width_slider, linetype = tongue_line_type, 
                             alpha = input$tongue_alpha_slider)
        }
      }
    }
    
    #plot palate trace
    if(input$palate_plot == T){
      #if plotting the palate has been selected
      if(input$smooth_contour == T){
        #if contours are smoothed
        p <- p + geom_line(data = pal, aes(x = x, y = y),
                           stat="smooth", method = "auto",linetype = pal_line_type, alpha = input$palate_alpha_slider,
                           se = F, size = input$palate_width_slider, span = input$palate_smooth_slider, 
                           colour = pal_colour)
      }else{
        #if no smoothing is selected
        p <- p + geom_line(data = pal, aes(x = x, y = y), 
                           size = input$palate_width_slider, linetype = pal_line_type, 
                           alpha = input$palate_alpha_slider, colour = pal_colour)
      }
    }
    
    #if a colour is selected for the palate contour
    if(!is.null(pallete_colour)){
      p <- p + scale_colour_gradientn(colours=pallete_colour)
    }
    
    #adds the limits to axes and inverts values for the Y axis
    
    if(input$main_invert_y == T){
      p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) + 
        scale_y_continuous(limits = c(extrema[3], extrema[4])) + 
        labs(x ='Tongue Advancement', y = 'Tongue Height')
    }else{
      p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) + 
        scale_y_reverse(limits = c(extrema[4], extrema[3])) + 
        labs(x ='Tongue Advancement', y = 'Tongue Height')
    }
    
    
    #add annotations----------------------------------------------------------------------------------------------------
    #adds single click information
    if(input$xy_point_main == T){
      #if plotting points is selected
      if(!is.null(clicked_reactive_main$n)){
        #if clicking information is not empty
        
        #gets the x value of click information
        x <- clicked_reactive_main$n$x
        #gets the y value of click information
        y <- clicked_reactive_main$n$y
        
        p <- p + geom_label_repel(data = data.frame(x = x, y = y, text = paste0('x=', round(x), '\n', 'y=', round(y))),
                                  aes(x = x, y = y, label = text, fill = '#FF6A6A'), size = 5,
                                  colour = "white", fontface = "bold", show.legend = FALSE)
        
        p <- p + geom_point(data = data.frame(x = x, y = y),
                            aes(x = x, y = y), size = 5,
                            colour = "#FF6A6A", show.legend = FALSE)
      }
    }
    
    #adds line point information (and/or double clicking)
    if(input$line_point_main == T){
      #if plotting lines is selected
      if(!is.null(dbl_clicked_reactive_main$n)){
        #if the double click information is not empty
        
        if(line_point_counter_main$n == 0){
          #if no point has been previously plotted, i.e. the user has only double-clicked the origin point
          
          #defines the x value............................................
          if(input$main_manual_origin_x != 0){
            #if the x value of the origin point has been manually input
            x <- input$main_manual_origin_x
          }else{
            #if the x value of the origin point comes from the double-click
            x <- dbl_clicked_reactive_main$n$x
          }
          
          #defines the y value............................................
          if(input$main_manual_origin_y != 0){
            #if the y value of the origin point has been manually input
            y <- input$main_manual_origin_y
          }else{
            #if the y value of the origin point comes from the double-click
            y <- dbl_clicked_reactive_main$n$y
          }
          
          #plots the origin point............................................
          p <- p + geom_point(data = data.frame(x = x, y = y),
                              aes(x = x, y = y), size = 7,
                              colour = "#FF6A6A", show.legend = FALSE)
          
        }else if(line_point_counter_main$n == 1){
          #if the origin point has already been plotted
          
          #defines the x value............................................
          if(input$main_manual_origin_x != 0){
            #if the x value of the origin point has been manually input
            x <- input$main_manual_origin_x
          }else{
            #if the x value of the origin point comes from the double-click
            x <- line_fill_cntr_main$x1
          }
          
          #defines the y value............................................
          if(input$main_manual_origin_y != 0){
            #if the y value of the origin point has been manually input
            y <- input$main_manual_origin_y
          }else{
            #if the y value of the origin point comes from the double-click
            y <- line_fill_cntr_main$y1
          }
          
          #plots the origin point............................................
          p <- p + geom_point(data = data.frame(x = x, y = y),
                              aes(x = x, y = y), size = 7,
                              colour = "#3CB371", show.legend = FALSE)
          
          #plot second point and line============================================================
          if(first_point_main$x != x & first_point_main$y != y){
            #checks if the values of the second point are not the same as the origin point
            
            #defines the x value............................................
            if(input$main_manual_secondPoint_x != 0){
              #if the x value of the origin point has been manually input
              x2 <- input$main_manual_secondPoint_x
            }else{
              #if the x value of the origin point comes from the double-click
              x2 <- first_point_main$x
            }
            
            #defines the y value............................................
            if(input$main_manual_secondPoint_y != 0){
              #if the y value of the origin point has been manually input
              y2 <- input$main_manual_secondPoint_y
            }else{
              #if the y value of the origin point comes from the double-click
              y2 <- first_point_main$y
            }
            
            #find angles of the line
            #the angle to the left of the line and the angles to the right of the line
            tmp_angle <- find_angle(c(x2,y2), c(x,y))
            #gets the angle to the left
            tmp_angle_left <- round(tmp_angle[[1]])
            #gets the angle to the right
            tmp_angle_right <- round(tmp_angle[[2]])
            
            #creates a value to seperate the label from the exact xy point
            if(input$radio_measures == 1){
              #if the plotting measures is in millimiters
              symbol_sep_measure <- 5
            }else if(input$radio_measures == 2){
              #if the plotting measures is in pixels
              symbol_sep_measure <- 20
            }
            
            #adds line to the plot
            p <- p + geom_line(data = data.frame(x = c(x, x2), y = c(y, y2)),
                               aes(x = x, y = y),
                               colour = "#3CB371", show.legend = FALSE) + 
              annotate("text", x=x-symbol_sep_measure, y=y, label= paste0(tmp_angle_left, '°')) +
              annotate("text", x=x+symbol_sep_measure, y=y, label= paste0(tmp_angle_right, '°'))
            
            #adds points to the plot
            p <- p + geom_point(data = data.frame(x = x2, y = y2),
                                aes(x = x, y = y), size = 7,
                                colour = "#FF6A6A", show.legend = FALSE)
            
            #Plots the Contours---------------------------------------------------------------------------------
            #create label_plotting_df to store all the plotting information per contour
            label_plotting_df <- data.frame(matrix(ncol = 6, nrow = 0))
            #adds the column names
            names(label_plotting_df) <- c('segment', 'repetition', 'frame', 'x', 'y', 'label')
            
            #iterates through all segments in the data
            for(seg_i in unique(fl$segment)){
              #creates a temporary dataframe with the iterated segment
              fl_seg <- fl[fl$segment == seg_i, ]
              
              #iterates through all repetitions in the data
              for(rep_i in unique(fl_seg$repetition)[1]:unique(fl_seg$repetition)[length(unique(fl_seg$repetition))]){
                #creates atemporary dataframe with the iterated repetition
                fl_i <- fl_seg[fl_seg$repetition == rep_i, ]
                
                #iterates through all frames in the data
                for(frame_i in unique(fl_i$frame)[1]:unique(fl_i$frame)[length(unique(fl_i$frame))]){
                  #creates a temporary dataframe with the iterated frame
                  cnt_tmp <- fl_i[fl_i$frame == frame_i,]
                  
                  #the four following lines find an intersection between the line and a tongue contour
                  #creates a spatial line with the iterated contour frame
                  cnt_line <- SpatialLines(list(Lines(list(Line(cbind(cnt_tmp$x, cnt_tmp$y))), 1)))
                  #creates a spatial line with the line, from the origin point to selected second point
                  basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
                  
                  #if there is an intersection point between the line and the contour
                  if(!is.null(gIntersection(cnt_line, basis_line)$x)){
                    
                    #gets the x value of the intersection
                    int_x <- gIntersection(cnt_line, basis_line)$x
                    #gets the y value of the intersection
                    int_y <- gIntersection(cnt_line, basis_line)$y
                    
                    #calculate distance
                    tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                    
                    #creates an intersection label with the xy values of the intersection
                    #new editing
                    #int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                    int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                    
                    
                    #creates a temporary dataframe with the iterated values
                    tmp_df <- data.frame(segment = seg_i, repetition = rep_i, frame = frame_i, 
                                         x = int_x, y = int_y, label = int_label)
                    
                    #the iterated dataframe is added to the main plotting dataframe
                    label_plotting_df <- rbind(label_plotting_df, tmp_df)
                    View(label_plotting_df)
                  }
                }
              }
            }
            
            #Adds circle(s) and label(s) to the plot--------------------------------------------------
            #first, it starts by plotting the origin point
            p <- p + geom_point(data = label_plotting_df,
                                aes(x = x, y = y), shape=1, size = 10,
                                colour = "darkolivegreen", show.legend = FALSE)
            
            #Plotting ONLY extreme intersections, so as to not overpopulate the plot
            if(input$intersection_labels_main == T){
              #if plotting intersection labels is selected
              if(input$intersection_labelsExtreme_main == T){
                #if plotting ONLY extreme intersections is selected
                if(nrow(label_plotting_df) > 1){
                  #if more than one intersection point is to be plotted
                  
                  #gets the maximum x value
                  tmp_max_x <- label_plotting_df[label_plotting_df$x == max(label_plotting_df$x),]
                  #gets the minimum x value
                  tmp_min_x <- label_plotting_df[label_plotting_df$x == min(label_plotting_df$x),]
                  
                  #creates a dataframe with ONLY the values of these two extreme intersections
                  label_plotting_df <- rbind(tmp_max_x, tmp_min_x)
                }
              }
            }
            
            #Plotting all intersections..........................................................
            if(input$intersection_labels_main == T){
              #if plotting intersection labels is selected
              #colour = repetition
              if(segs_nmbr > 1){
                #if more than one segment is plotted
                if(input$intersection_labelsRepel_main == T){
                  #if the labels are to be repelled, i.e. clearer plotting
                  p <- p + geom_label_repel(data = label_plotting_df, aes(x = x, y = y, label = label, fill = segment),
                                            size = 3.5, fontface = "bold", show.legend = FALSE)
                }else{
                  #if the labels are not repelled, i.e. messier plotting
                  p <- p + geom_label(data = label_plotting_df, aes(x = x, y = y, label = label, fill = segment), 
                                      size = 3.5, fontface = "bold", show.legend = FALSE)
                }
                
                
              }else{
                #if only one segment is plotted
                if(reps_nmbr > 1){
                  #if more than one repetition is plotted
                  if(input$intersection_labelsRepel_main == T){
                    #if the labels are to be repelled, i.e. clearer plotting
                    p <- p + geom_label_repel(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                            fill = repetition), 
                                              size = 3.5, fontface = "bold", show.legend = FALSE)
                  }else{
                    #if the labels are not repelled, i.e. messier plotting
                    p <- p + geom_label(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                      fill = repetition), 
                                        size = 3.5, fontface = "bold", show.legend = FALSE)
                  }
                }else{
                  #if only one repetition is plotted
                  if(input$intersection_labelsRepel_main == T){
                    #if the labels are to be repelled, i.e. clearer plotting
                    p <- p + geom_label_repel(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                            fill = frame),
                                              # fill = unique(fl$frame)[frame_i]), 
                                              size = 3.5,fontface = "bold", show.legend = FALSE)
                  }else{
                    #if the labels are not repelled, i.e. messier plotting
                    p <- p + geom_label(data = label_plotting_df, aes(x = x, y = y, label = label, fill = frame),
                                        # fill = unique(fl$frame)[frame_i]), 
                                        size = 3.5,fontface = "bold", show.legend = FALSE)
                  }
                }
              }
            }
            
            #Plotting the Palate Trace----------------------------------------------------------------
            if(input$palate_plot == T){
              #if plotting the palate trace is selected
              
              #the four following lines find an intersection between the line and the palate contour
              #creates a spatial line with the palate trace
              palate_line <- SpatialLines(list(Lines(list(Line(cbind(pal$x, pal$y))), 1)))
              #creates a spatial line with the line, from the origin point to selected second point
              basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
              
              #if there is an intersection point between the line and the contour
              if(!is.null(gIntersection(palate_line, basis_line)$x)){
                
                #gets the x value of the intersection
                int_x <- gIntersection(palate_line, basis_line)$x
                #gets the y value of the intersection
                int_y <- gIntersection(palate_line, basis_line)$y
                #creates a temporary dataframe with the intersection values
                plot_data_pal <- data.frame(x = int_x, y = int_y)
                
                #new editing
                tmpInternalDistancePal <- sqrt((x-int_x)^2+(y-int_y)^2)
                
                
                #creates an intersection label with the xy values of the intersection
                #plot_data_pal$label <- paste0('x=', round(plot_data_pal$x), ', ', 'y=', round(plot_data_pal$y))
                
                plot_data_pal$label <- format(round(tmpInternalDistancePal, 2), nsmall = 2)
                
                
                
                
                #adds circle and label to the plot.......................................................
                p <- p + geom_point(data = plot_data_pal,
                                    aes(x = x, y = y), shape=1, size = 10,
                                    colour = "deepskyblue4", show.legend = FALSE)
                
                if(input$intersection_labels_main == T){
                  #if plotting intersection labels is selected
                  p <- p + geom_label(data = plot_data_pal, aes(x = x, y = y, label = label), 
                                      size = 3.5, fontface = "bold", show.legend = FALSE)
                }
              }
            }
          }
        }
        
      }
    }
    #plots the final figure
    p
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  source(file.path("server/savePlots", "saveVisualisation.R"),  local = TRUE)$values
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - I DOCUMENTATION
  #This section shows all the documentation of the program
  #================================================================================
  #================================================================================
  
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
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  
  #This section controls the graphics parameters
  #Text
  #colours
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$text_main = renderUI({
    ttl = 'get_plot_title()'
    
    return(NULL)
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$tongue_colour_setUI = renderUI({
    if(input$tongue_colour_set_radio == 1)
    {  
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices = c("Default", "Grey", "Blue", "Green", "Red", "Purple", "Orange"), selected = "Default")
    }else if(input$tongue_colour_set_radio == 2)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices = c("Red-Grey", "Red-Blue", "Purple-Orange", "Purple-Green", "Brown-Green"))
    }else if(input$tongue_colour_set_radio == 3)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices = c("RedYellowGreen", "RedYellowBlue", "YellowGreenBlue"))
    }else if(input$tongue_colour_set_radio == 4)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices = c("Spectral", "Rainbow", "Heat", "Terrain", "Topo", "Random"))
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$invert_coloursUI = renderUI({
    fl = plotStatic_data()
    fl = fl[[1]]
    frms = unique(fl$frame)
    frms_nmbr = length(frms)
    
    if(frms_nmbr > 1 && input$tongue_colour_set!='Default')
    {
      checkboxInput("invert_colours", label = "Invert Colour Order", value = F)
    }else{
      return()
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  
  #This section controls the visualisation of the xy information of the point clicked
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #UI to show the xy values of the point clicked
  output$coordenate_info_ui <- renderUI({
    verbatimTextOutput("coordenate_info")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #shows the xy values of the point clicked
  output$coordenate_info <- renderText({
    
    #if the double clicked button is not NULL
    if(is.null(dbl_clicked_reactive_main$n))
      return()
    
    #if the plotting of line is selected
    if(input$line_point_main == T){
      
      #if the double clicked is not empty
      if(!is.null(dbl_clicked_reactive_main$n)){
        
        #if no point of the line has been previously clicked
        if(line_point_counter_main$n == 0){
          
          #if the x value of the origin point has been manually selected
          if(input$main_manual_origin_x != 0){
            #sets the x value from the manual input
            x <- input$main_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x <- dbl_clicked_reactive_main$n$x
          }
          
          #if the y value of the origin point has been manually selected
          if(input$main_manual_origin_y != 0){
            #sets the y value from the manual input
            y <- input$main_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y <- dbl_clicked_reactive_main$n$y
          }
          
          #prints the xy values of the point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
        }else if(line_point_counter_main$n == 1){
          #if one point of the line has been previously clicked
          
          #if the x value of the origin point has been manually selected
          if(input$main_manual_origin_x != 0){
            #sets the x value from the manual input
            x <- input$main_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x <- line_fill_cntr_main$x1
          }
          
          #if the y value of the origin point has been manually selected
          if(input$main_manual_origin_y != 0){
            #sets the y value from the manual input
            y <- input$main_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y <- line_fill_cntr_main$y1
          }
          
          #prints the xy values of the origin point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
          #plot second point and line
          ##if the second point is not the same as the first point
          if(first_point_main$x != x & first_point_main$y != y){
            #if the x value of the origin point has been manually selected
            if(input$main_manual_secondPoint_x != 0){
              #sets the x value from the manual input
              x2 <- input$main_manual_secondPoint_x
            }else{
              #if the x value of the origin point is created from clicking
              #sets the x value from the clicking
              x2 <- first_point_main$x
            }
            
            #if the y value of the origin point has been manually selected
            if(input$main_manual_secondPoint_y != 0){
              #sets the y value from the manual input
              y2 <- input$main_manual_secondPoint_y
            }else{
              #if the y value of the origin point is created from clicking
              #sets the y value from the clicking
              y2 <- first_point_main$y
            }
            
            #prints the xy values of the second point
            print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1), 
                         ' - Second Point: ', format(round(x2, 1), nsmall = 1), ',', format(round(y2, 1), nsmall = 1)))
          }
          
        }
        
      }
      
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #this section controls checkboxes and button to plot/display lines and labels in the plot object
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #checkbox to show the intersection labels
  output$intersection_labels_UI_main = renderUI({
    if(input$line_point_main == T){
      checkboxInput("intersection_labels_main", label = "Labels", value = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #checkbox to repel labels in the plot object
  output$intersection_labelsRepel_UI_main = renderUI({
    if(input$line_point_main == T){
      checkboxInput("intersection_labelsRepel_main", label = "Repel", value = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #checkbox to choose showing only the extreme labels in the intersections
  output$intersection_labelsExtreme_UI_main = renderUI({
    if(input$line_point_main == T){
      checkboxInput("intersection_labelsExtreme_main", label = "Extreme", value = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates button widget to plot lines
  output$plot_lines_UI_main = renderUI({
    if(input$line_point_main == F)
      return()
    
    if(line_point_counter_main$n == 0){
      bsButton("plot_lines_main", label = "Set anchor", icon("anchor"), style = "primary")
    }else{
      bsButton("plot_lines_main", label = "Clear lines", icon("anchor"), style = "danger")
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  ##THIS SECTION OBSERVES THE CLICKING ACTIVITY IN THE VISUALISATION TAB
  ##======================================================================
  ##======================================================================
  ##======================================================================
  
  #stores the xy values of the line plotted, both the begin and end points
  line_fill_cntr_main <- reactiveValues(x1 = 0, y1 = 0, x2 = 0, y2 = 0)
  
  #stores the xy values of the origin point
  origin_point_set_main <- reactiveValues(x = 0, y = 0)
  #stores the xy values of the second point of the line
  second_point_set_main <- reactiveValues(x = 0, y = 0)
  
  #stores the xy values of the double click
  dbl_clicked_reactive_main <- reactiveValues(n = NULL)
  #counts the number of points for the line
  line_point_counter_main <- reactiveValues(n = 0)
  
  #stores the xy values of the first point of the line
  first_point_main <- reactiveValues(x = 0, y = 0)
  
  #----------------------------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #observes and the stores the double clicking activity
  observe({
    #if the double clicking of the plot object is not NULL
    if (!is.null(input$lm_dblclick_main)) {
      #stores the double clicking information
      dbl_clicked_reactive_main$n <- input$lm_dblclick_main
      
      #passes the x value to the first point
      first_point_main$x <- input$lm_dblclick_main$x
      #passes the y value to the first point
      first_point_main$y <- input$lm_dblclick_main$y
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #counts the number of clicks for the line
  plot_line_click_counter_main <- reactiveValues(n = 0)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reacts to the clicking of the button to plot lines and stores the xy values
  observeEvent(input$plot_lines_main,{   
    #if the plot lines button is clicked, then the counter is updated
    plot_line_click_counter_main$n <- plot_line_click_counter_main$n + 1
    
    #runs the actions if the double clicked button has been previously clicked
    if (!is.null(dbl_clicked_reactive_main$n)) {
      #if no clicking information has been stored, i.e. no points in the plot object
      if(sum(line_fill_cntr_main$x1, line_fill_cntr_main$x2, 
             line_fill_cntr_main$y1, line_fill_cntr_main$y2) == 0){
        
        ##the xy information is the same for both points of the line but only the x1y1 is plotted
        line_fill_cntr_main$x1 <- dbl_clicked_reactive_main$n$x
        line_fill_cntr_main$y1 <- dbl_clicked_reactive_main$n$y
        line_fill_cntr_main$x2 <- dbl_clicked_reactive_main$n$x
        line_fill_cntr_main$y2 <- dbl_clicked_reactive_main$n$y
        
        #updates the number of points in the line
        line_point_counter_main$n <- 1
      }else{
        #if previous clicking information has been stored
        if((line_fill_cntr_main$x1 == line_fill_cntr_main$x2) & 
           (line_fill_cntr_main$y1 == line_fill_cntr_main$y2)){
          
          #stores the new xy information for the second point
          line_fill_cntr_main$x2 = dbl_clicked_reactive_main$n$x
          line_fill_cntr_main$y2 = dbl_clicked_reactive_main$n$y
          
          #updates the number of points in the line
          line_point_counter_main$n <- 1
        }
      }
      
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reacts to the clicking of the button to plot lines and stores the xy values
  observeEvent(input$plot_lines_main,{
    #if there are two points already stored
    if(plot_line_click_counter_main$n == 2){
      
      #then the counting is reset to zero
      line_fill_cntr_main$x1 <- 0
      line_fill_cntr_main$y1 <- 0
      line_fill_cntr_main$x2 <- 0
      line_fill_cntr_main$y2 <- 0
      
      line_point_counter_main$n <- 0
      dbl_clicked_reactive_main$n <- NULL
      plot_line_click_counter_main$n <- 0
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #stores the single clicking information
  clicked_reactive_main <- reactiveValues(n = NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #observes the activity of the single clicking
  observe({
    #if the single clicking information is not NULL
    if (!is.null(input$lm_click_main)) {
      #stores the single clicking information
      clicked_reactive_main$n <- input$lm_click_main       
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #PROGRAMPART - V Landmarks
  #This is part of the selection section
  #Controls the SPEAKERS input from the user.
  #============================================================================================
  #============================================================================================
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the speaker selection widget
  output$speaker_LM <- renderUI({
    #if there is no input file
    if (is.null(importFiles))
      return()
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    #gets the unique labels of the speakers
    spkrs <- unique(fileIn$speaker)
    
    #creates the speaker widget
    selectInput('spkr_LM', 'Speaker', spkrs)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #This is part of the selection section
  #Controls the SEGMENTS input from the user.
  #============================================================================================
  #============================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the segment selection widget
  output$segment_LM <- renderUI({
    #if there is no speaker widget
    if (is.null(input$spkr_LM))
      return()
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    #deletes palate trace from the subset
    fileIn <- fileIn[fileIn$segment != "pal",]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp <- input$spkr_LM
    #gets segment label-----------------------------------------------------------------------------
    segs <- unique(fileIn[fileIn$speaker == sp, 'segment'])
    
    #creates the segment widget
    selectInput('sgmnt_LM', label = 'Segment', c(segs), selected = segs[1], multiple = F)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This is part of the selection section
  #Controls the REPETITIONS input from the user.
  #============================================================================================
  #============================================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the repetition selection widget
  output$repetition_LM <- renderUI({
    #if there is no segment widget
    if (is.null(input$sgmnt_LM))
      return()
    
    #imports the input file
    file1 <- importFiles()
    #selects the dataframe with values
    fileIn <- file1[[2]]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp <- input$spkr_LM
    
    #gets segment label-----------------------------------------------------------------------------
    seg <- input$sgmnt_LM
    
    #unique values of repetitions
    reps <- unique(fileIn[fileIn$speaker == sp & fileIn$segment == seg, 'repetition'])
    
    #creates the repetion selection based on input
    selectInput('rpttn_LM', label = "Repetition", c(reps), selected = 1)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This is part of the selection section
  #Controls the FRAMES input from the user.
  #============================================================================================
  #============================================================================================
  
  #initiates a variable to store the maximun number of frames for plotting
  max_frames_LM <- reactiveValues(n=1)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the frame selection widget
  output$frame_LM <- renderUI({
    
    #imports the input file
    file1 <- importFiles()
    #selects the dataframe with values
    file1 <- file1[[2]]
    
    #gets speaker label-----------------------------------------------------------------------------
    sp <- input$spkr
    
    #subsets the data to the speaker and omits the palata trace data
    fileIn <- file1[file1$speaker == sp & file1$segment != "pal",]
    
    #segments-----------------------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    #gets segment label
    seg <- input$sgmnt_LM
    
    #subsets dataframe to the segment input
    segs <- unique(fileIn[fileIn$speaker == sp & fileIn$segment == seg, 'segment'])
    
    #Gets repetion values-----------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    #gets repetition label
    rep <- input$rpttn_LM
    
    #
    #Gets frame values--------------------------------------------
    #-------------------------------------------------------------
    #-------------------------------------------------------------
    frames <- subset(fileIn, speaker == sp & segment == segs & repetition == rep, select = frame)
    frames <- unique(frames$frame)
    
    max_frames_LM$n <- frames
    
    #creates widget selection input
    if(!is.null(cntxt_plt_LM$n)){
      selectInput('frm_LM', 'Frame', c(1:frames), selected = cntxt_plt_LM$n)
    }else{
      selectInput('frm_LM', 'Frame', c(1:frames), selected = 1)
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This section controls Previous and Following segments
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$prevBttn_LM <- renderUI({
    bsButton("previousButton_LM", label = "Previous", icon("chevron-circle-left"), style = "primary")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value creates the next (button) icon
  output$follBttn_LM <- renderUI({
    bsButton("nextButton_LM", label = "Next", icon("chevron-circle-right"), style = "primary")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #reactive value to store the number of the current working contour
  cntxt_plt_LM <- reactiveValues(n=NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this section observes the behaviour of the next button
  observeEvent(input$nextButton_LM, {
    cntxt_plt_LM$n <- as.numeric(input$frm_LM) + 1
    if(cntxt_plt_LM$n > max_frames_LM$n){
      cntxt_plt_LM$n <- max_frames_LM$n
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value controls the text of the plot title
  output$text_main <- renderUI({
    ttl <- 'get_plot_title()'
    
    return(NULL)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value controls the tongue contour
  output$tongue_colour_setUI <- renderUI({
    if(input$tongue_colour_set_radio == 1)
    {  
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices <- c("Default", "Grey", "Blue", "Green", "Red", "Purple", "Orange"), selected = "Default")
    }else if(input$tongue_colour_set_radio == 2)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices <- c("Red-Grey", "Red-Blue", "Purple-Orange", "Purple-Green", "Brown-Green"))
    }else if(input$tongue_colour_set_radio == 3)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices <- c("RedYellowGreen", "RedYellowBlue", "YellowGreenBlue"))
    }else if(input$tongue_colour_set_radio == 4)
    {
      selectInput("tongue_colour_set", label = "Select Colour", 
                  choices <- c("Spectral", "Rainbow", "Heat", "Terrain", "Topo", "Random"))
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value inverts the colors of the tongue contours
  output$invert_coloursUI <- renderUI({
    fl <- plotStatic_data()
    fl <- fl[[1]]
    frms <- unique(fl$frame)
    frms_nmbr <- length(frms)
    
    if(frms_nmbr > 1 && input$tongue_colour_set!='Default')
    {
      checkboxInput("invert_colours", label = "Invert Colour Order", value = F)
    }else{
      return()
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value controls the hiding of the middle frame in the plot
  output$hide_middle_frame_LMUI <- renderUI({
    if(input$extra_frames_LM > 1){
      checkboxInput("hide_middle_frame_UI", label = "Hide Middle Frame", value = F)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #this reactive value controls the colours of the contours, whether black and white or in colour
  output$colour_contiguous_frames_LMUI <- renderUI({
    radioButtons("colour_contiguous_frames_LM", label = NULL,
                 choices = list("Colour" = 1, "B&W" = 2),
                 selected = 1, inline = T)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the dataframe for plotting based on users input
  plotStatic_data_LM <- reactive({
    #returns NULL is the number of frames is NULL
    if (is.null(input$frm_LM))
      return()
    
    #number of points per second
    frm_nmbr_per_contour = 100
    
    #imports the plot data
    fileIn <- importFiles()
    #gets the values of segment contours
    fileIn <- fileIn[[1]]
    d <- fileIn
    
    #gets speaker input labels
    sp <- input$spkr_LM
    
    #gets input measurement unit
    ms <- as.character(input$radio_measures_LM)
    
    commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
    #Measurement*********************************************
    if (ms == 1){
      #measurements in millimiters
      #d <- subset(d, select = -c(pixel))
      d <- d[c(commonNamesInFile, 'mm')]
      measure_val <- "mm"
    }else if (ms == 2){
      #measurements in pixels
      #d <- subset(d, select = -c(mm))
      d <- d[c(commonNamesInFile, 'pixel')]
      measure_val <- "pixel"
    }
    
    #SPEAKERS*********************************************
    fl <- d[d$speaker %in% sp, ]
    
    #Sets extrema points for the plot (xy limits)
    x_xtrm <- subset(fl, coord == "x", select = measure_val)
    x_xtrm <- unique(x_xtrm[[measure_val]])
    x_xtrm_min <- min(x_xtrm) - 2
    x_xtrm_max <- max(x_xtrm) + 2
    
    y_xtrm <- subset(fl, coord == "y", select = measure_val)
    y_xtrm <- unique(y_xtrm[[measure_val]])
    y_xtrm_min <- min(y_xtrm) - 2
    y_xtrm_max <- max(y_xtrm) + 2
    
    extrema <- c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
    
    #Palate*********************************************
    #sets the palata trace plot
    if(input$palate_plot_LM == T){
      pal_fl <- subset(fl, segment == "pal")
      
      pal_fl_x <- subset(pal_fl, coord == "x", select = measure_val)
      pal_fl_x <- pal_fl_x[,1]
      pal_fl_x <- as.numeric(pal_fl_x)
      pal_fl_x <- approx(pal_fl_x, n = frm_nmbr_per_contour)
      pal_fl_x <- pal_fl_x$y
      
      pal_fl_y <- subset(pal_fl, coord == "y", select = measure_val)
      pal_fl_y <- pal_fl_y[,1]
      pal_fl_y <- as.numeric(pal_fl_y)
      pal_fl_y <- approx(pal_fl_y, n = frm_nmbr_per_contour)
      pal_fl_y <- pal_fl_y$y
      
      tmp_seg_pal <- rep("palate", length(pal_fl_x))
      tmp_rep_pal <- rep(1, length(pal_fl_x))
      tmp_frm_pal <- rep(1, length(pal_fl_x))
      tmp_pnt_pal <- 1:length(pal_fl_x)
      
      pal_vals <- data.frame(tmp_seg_pal, tmp_rep_pal, tmp_frm_pal, tmp_pnt_pal, pal_fl_x, pal_fl_y)
      names(pal_vals) <- c("segment", "repetition", "frame", "point", "x", "y")
      
    }else{
      pal_vals <- data.frame(matrix(nrow = 0, ncol = 0))
    }
    
    fl <- subset(fl, speaker == sp & segment != "pal")
    
    #SEGMENTS*********************************************
    seg <- input$sgmnt_LM
    fl <- fl[fl$segment %in% seg, ]
    
    #REPETITIONS*********************************************
    rep <- input$rpttn_LM
    fl <- fl[fl$repetition %in% rep, ]
    
    frm <- as.numeric(input$frm_LM)
    
    #get maximun frame number
    
    max_frm_nmbr <- subset(fl, segment == seg & repetition == rep, select = frame)
    max_frm_nmbr <- unique(max_frm_nmbr$frame)
    max_frm_nmbr <- max(max_frm_nmbr)
    
    if(input$extra_frames_LM == 2){
      tmp_frm_diff <- as.numeric(input$extra_frames_number_LM)
      
      if(frm - tmp_frm_diff < 1){
        
        if(frm == 1){
          bef_frm <- 0
        }else{
          bef_frm <- 1
        }
        
      }else{
        bef_frm <- frm - tmp_frm_diff
      }
      if(frm + tmp_frm_diff > max_frm_nmbr){
        
        if(frm == max_frm_nmbr){
          aft_frm <- 0
        }else{
          aft_frm <- max_frm_nmbr
        }
        
      }else{
        aft_frm <- frm + tmp_frm_diff
      }
      
    }else  if(input$extra_frames_LM == 3){
      tmp_frm_diff <- as.numeric(input$extra_frames_number_LM)
      if(frm - tmp_frm_diff < 1){
        if(frm == 1){
          bef_frm <- 0
        }else{
          bef_frm <- 1
        }
      }else{
        bef_frm <- frm - tmp_frm_diff
      }
      aft_frm <- 0
    }else  if(input$extra_frames_LM == 4){
      tmp_frm_diff <- as.numeric(input$extra_frames_number_LM)
      bef_frm <- 0
      if(frm + tmp_frm_diff > max_frm_nmbr){
        if(frm == max_frm_nmbr){
          aft_frm <- 0
        }else{
          aft_frm <- max_frm_nmbr
        }
      }else{
        aft_frm <- frm + tmp_frm_diff
      }
    }else{
      bef_frm <- 0
      aft_frm <- 0
      max_extra_frame <- 1
    }
    
    frames_context_plotting <- c(bef_frm, frm, aft_frm)
    
    max_extra_frame_ids <- c(bef_frm, frm, aft_frm)
    max_extra_frame_ids <- max_extra_frame_ids[max_extra_frame_ids != 0]
    init_tmp_fr_id <- min(max_extra_frame_ids)
    tmp_fr_id <- max(max_extra_frame_ids)
    
    max_extra_frame <- bef_frm + aft_frm + 1
    
    tmp_frm_array_y <- c()
    
    prep_plot_df <- data.frame(matrix(ncol = 6, nrow = 0))
    names(prep_plot_df) <- c("segment", "repetition", "frame", "point", "x", "y")
    
    for(tmp_fr_id_loop in init_tmp_fr_id:tmp_fr_id){
      tmp_frm_array_x <- subset(fl, segment == seg 
                                & repetition == rep & frame == tmp_fr_id_loop
                                & coord == "x", select = measure_val)
      tmp_frm_array_x <- as.numeric(tmp_frm_array_x[[measure_val]])
      
      tmp_frm_array_y <- subset(fl, segment == seg 
                                & repetition == rep & frame == tmp_fr_id_loop
                                & coord == "y", select = measure_val)
      tmp_frm_array_y <- as.numeric(tmp_frm_array_y[[measure_val]])
      
      tmp_seg_loop <- rep(seg, frm_nmbr_per_contour)
      tmp_rpttn_loop <- rep(rep, frm_nmbr_per_contour)
      tmp_fr_loop <- rep(tmp_fr_id_loop, frm_nmbr_per_contour)
      tmp_pnt_loop <- 1:frm_nmbr_per_contour
      
      tmp_df_loop <- data.frame(tmp_seg_loop, tmp_rpttn_loop, tmp_fr_loop, tmp_pnt_loop,
                                tmp_frm_array_x, tmp_frm_array_y)
      names(tmp_df_loop) <- c("segment", "repetition", "frame", "point", "x", "y")
      prep_plot_df <- rbind(prep_plot_df, tmp_df_loop)
    }
    
    return(list(prep_plot_df, extrema, pal_vals, frames_context_plotting))
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This section controls the plotting in the plot object
  
  #this function finds the angle from the origin point to a given point
  #==========================================================================================
  #==========================================================================================
  #==========================================================================================
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  find_angle <- function(origin, point){
    #xy points for reference
    zero_point.x <- 10
    zero_point.y <- 0
    
    #differences between xy points
    measure_point.x <- point[1] - origin[1]
    measure_point.y <- point[2] - origin[2]
    
    #angle in radians between the two points
    angle <- atan2(measure_point.y, measure_point.x) - atan2(zero_point.y, zero_point.x)
    
    #covert from radians to degrees
    angle <- angle * 360 / (2*pi)
    
    #convert angles to inverted y axis (for pixels)
    if (angle < 0){
      angle <- angle + 360
    }
    left_angle <- angle
    right_angle <- 180 - angle
    
    return(list(left_angle, right_angle))
  }
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #creates a plot object output
  #==========================================================================================
  #==========================================================================================
  #==========================================================================================
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$plotStatic_LM <- renderPlot({
    
    #if the plot data is null
    if(is.null(plotStatic_data_LM()))
      return()
    
    #imports the plot data
    fl <- plotStatic_data_LM()
    #imports information on contiguous contexts
    frames_context_plotting <- fl[[4]]
    #the first index stores the number of previous frames
    #the second index stores the number of the middle frame
    #the third index stores the number of following frames
    
    #imports limits information for plotting
    extrema <- fl[[2]]
    #imports palate trace information
    pal <- fl[[3]]
    #imports tongue contours information
    fl <- fl[[1]]
    
    #gets information of previous contours
    if(frames_context_plotting[1] == 0){
      #if there are no previous contours
      prev_cnt <- NULL
    }else{
      #if there are previous contours
      #subsets data to the previous frames
      prev_cnt <- subset(fl, frame == c(frames_context_plotting[1]:(frames_context_plotting[2]-1)))
    }
    
    middl_cnt <- subset(fl, frame == frames_context_plotting[2])
    
    #gets information of following contours
    if(frames_context_plotting[3] == 0)
    {
      #if there are no following contours
      foll_cnt <- NULL
    }else{
      #if there are following contours
      #subsets data to the following frames
      foll_cnt <- subset(fl, frame == c((frames_context_plotting[2]+1):frames_context_plotting[3]))
    }
    
    #gets the title of the plot
    ttl <- 'get_plot_title()'
    
    #gets the colour of the palate trace
    pal_colour <- input$palate_colour
    #gets the line type of the palate trace
    pal_line_type <- as.numeric(input$line_type_palate)
    #gets the line type of the tongue contour
    tongue_line_type <- as.numeric(input$line_type_tongue)
    
    #gets the segment label(s)
    segs <- unique(fl$segment)
    #gets the number of segments
    segs_nmbr <- length(segs)
    
    #gets the repetition label(s)
    reps <- unique(fl$repetition)
    #gets the number of repetitions
    reps_nmbr <- length(reps)
    
    #gets the frame label(s)
    frms <- unique(fl$frame)
    #gets the number of frames
    frms_nmbr <- length(frms)
    
    #initialises the pallete colour for tongue contours
    pallete_colour <- c()
    
    #tongue contour colouring options
    if(input$extra_frames_LM > 1 && input$colour_contiguous_frames_LM == 1){
      #if there are extra frames and tongue contours are plotted in colour
      middl_cnt_colour <- "dodgerblue4"
      prev_cnt_colour <- "darkolivegreen"
      foll_cnt_colour <- "firebrick"
      palate_cnt_colour <- "deepskyblue4"
    }else if(input$extra_frames_LM > 1 && input$colour_contiguous_frames_LM != 1){
      #if there are extra frames and tongue contours are plotted in black and white
      middl_cnt_colour <- "gray12"
      prev_cnt_colour <- "gray16"
      foll_cnt_colour <- "gray16"
      palate_cnt_colour <- "gray8"
    }else{
      #if there are no extra frames
      middl_cnt_colour <- "dodgerblue4"
      palate_cnt_colour <- "deepskyblue4"
    }
    
    #creates the basic plot
    #=================================================================================================
    p <- ggplot() + theme_bw() +
      theme(title = element_text(family = input$font_type, colour = input$text_color, 
                                 size = input$title_size),
            axis.title.x = element_text(family = input$font_type, colour = input$text_color, 
                                        size = input$axis_size),
            axis.title.y = element_text(family = input$font_type, colour = input$text_color, 
                                        size = input$axis_size),
            legend.title = element_text(family = input$font_type, 
                                        colour = input$text_color, size = input$legend_size),
            axis.text = element_text(size = input$ticks_size))
    
    #plots the tongue contours
    #=================================================================================================
    #if there is one extra frame or if user chooses not to hide the middle frame
    if(input$extra_frames_LM == 1 || input$hide_middle_frame_UI == F){
      if(input$smooth_contour_LM == T){
        #if contour is smoothed
        p <- p + geom_line(data = middl_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                 colour = "Middle", linetype = "Middle"),
                           stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                           se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
      }else{
        #if contour is not smoothed
        p <- p + geom_line(data = middl_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                 colour = "Middle", linetype = "Middle"),
                           size = input$tongue_width_slider, alpha = input$tongue_alpha_slider)
      }
    }
    
    #if there are previous contours
    #..................................................................................................
    if(!is.null(prev_cnt)){
      if(input$tongue_alpha_slider > 0.2){
        #if the alpha value of the contour is larger than 0.2
        if(input$smooth_contour_LM == T){
          #if the contour is smoothed
          p <- p + geom_line(data = prev_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Previous", linetype = "Previous"),
                             stat="smooth", method = "auto", alpha = as.numeric(input$tongue_alpha_slider)-0.1,
                             se = F, size = as.numeric(input$tongue_width_slider)-0.5, 
                             span = input$tongue_smooth_slider)
        }else{
          #if the contour is not smoothed
          p <- p + geom_line(data = prev_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Previous", linetype = "Previous"),
                             size = as.numeric(input$tongue_width_slider)-0.5, 
                             alpha = as.numeric(input$tongue_alpha_slider)-0.1)
        }
      }else{
        #if the alpha value of the contour is less than 0.2
        if(input$smooth_contour_LM == T){
          #if the contour is smoothed
          p <- p + geom_line(data = prev_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Previous", linetype = "Previous"),
                             stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                             se = F, size = as.numeric(input$tongue_width_slider)-0.5, 
                             span = input$tongue_smooth_slider)
        }else{
          #if the contour is not smoothed
          p <- p + geom_line(data = prev_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Previous", linetype = "Previous"),
                             size = as.numeric(input$tongue_width_slider)-0.5, alpha = input$tongue_alpha_slider)
        }
      }
    }
    
    #if there are following contours
    #..................................................................................................
    if(!is.null(foll_cnt)){
      if(input$tongue_alpha_slider > 0.2){
        #if the alpha value of the contour is larger than 0.2
        if(input$smooth_contour_LM == T){
          #if the contour is smoothed
          p <- p + geom_line(data = foll_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Following", linetype = "Following"),
                             stat="smooth", method = "auto", alpha = as.numeric(input$tongue_alpha_slider)-0.1,
                             se = F, size = as.numeric(input$tongue_width_slider)-0.5, 
                             span = input$tongue_smooth_slider)
        }else{
          #if the contour is not smoothed
          p <- p + geom_line(data = foll_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Following", linetype = "Following"),
                             size = as.numeric(input$tongue_width_slider)-0.5, 
                             alpha = as.numeric(input$tongue_alpha_slider)-0.1)
        }
      }else{
        #if the alpha value of the contour is less than 0.2
        if(input$smooth_contour_LM == T){
          #if the contour is smoothed
          p <- p + geom_line(data = foll_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Following", linetype = "Following"),
                             stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                             se = F, size = as.numeric(input$tongue_width_slider)-0.5, 
                             span = input$tongue_smooth_slider)
        }else{
          #if the contour is not smoothed
          p <- p + geom_line(data = foll_cnt, aes(x = x, y = y, group = interaction(repetition, frame), 
                                                  colour = "Following", linetype = "Following"),
                             size = as.numeric(input$tongue_width_slider)-0.5, alpha = input$tongue_alpha_slider)
        }
        
      }
    }
    
    #plots the palate trace
    #=================================================================================================
    if(input$palate_plot_LM == T){
      if(input$smooth_contour_LM == T){
        #if the contour is smoothed
        p <- p + geom_line(data = pal, aes(x = x, y = y),
                           stat="smooth", method = "auto",linetype = pal_line_type, alpha = input$palate_alpha_slider,
                           se = F, size = input$palate_width_slider, 
                           span = input$palate_smooth_slider, colour = pal_colour)
      }else{
        #if the contour is not smoothed
        p <- p + geom_line(aes(x = x, y = y), size = input$palate_width_slider, linetype = pal_line_type, 
                           alpha = input$palate_alpha_slider, colour = pal_colour, pal)
      }
    }
    
    #defines line types
    #=================================================================================================
    #if there is more than one extra frame
    if(input$extra_frames_LM > 1){
      if(input$hide_middle_frame_UI == F){
        #if user chooses not to hide the middle frame
        if(is.null(prev_cnt) && !is.null(foll_cnt)){
          #if there is no previous frame but there are following frames
          p <- p + scale_linetype_manual(name = "Frames", values=c("Middle" = "solid", "Following" = "dashed"))
        }else if(is.null(foll_cnt) && !is.null(prev_cnt)){
          #if there is no following frame but there are previous frames
          p <- p + scale_linetype_manual(name = "Frames", values=c("Middle" = "solid", "Previous" = "dotted"))
        }else if(is.null(prev_cnt) && is.null(foll_cnt)){
          #if there is no following frame nor previous frame
          p <- p + scale_linetype_manual(name = "Frames", values=c("Middle" = "solid"))
        }else{
          #if there are both previous and following frames
          p <- p + scale_linetype_manual(name = "Frames", 
                                         values=c("Middle" = "solid", "Previous" = "dotted", "Following" = "dashed"))
        }
      }else{
        #if user chooses to hide the middle frame
        if(is.null(prev_cnt) && !is.null(foll_cnt)){
          #if there is no previous frame but there are following frames
          p <- p + scale_linetype_manual(name = "Frames", values=c("Following" = "dashed"))
        }else if(is.null(foll_cnt) && !is.null(prev_cnt)){
          #if there is no following frame but there are previous frames
          p <- p + scale_linetype_manual(name = "Frames", values=c("Previous" = "dotted"))
        }else if(is.null(prev_cnt) && is.null(foll_cnt)){
          #if there is no following frame nor previous frame
          p <- p + scale_linetype_manual(name = "Frames", values=c("Middle" = "solid"))
        }else{
          #if there are both previous and following frames
          p <- p + scale_linetype_manual(name = "Frames", values=c("Previous" = "dotted", "Following" = "dashed"))
        }
      }
    }else{
      #if there are no extra frames
      p <- p + scale_linetype_manual(name = "Frames", values=c("Middle" = "solid"))
    }
    
    #defines colours
    #=================================================================================================
    #if there is more than one extra frame
    if(input$extra_frames_LM > 1){
      #if there is more than one extra frame
      if(input$hide_middle_frame_UI == F){
        #if user chooses not to hide the middle frame
        if(is.null(prev_cnt) && !is.null(foll_cnt)){
          #if there is no previous frame but there are following frames
          p <- p + scale_colour_manual(name = "Frames", values=c("firebrick", "dodgerblue4"))
        }else if(is.null(foll_cnt) && !is.null(prev_cnt)){
          #if there is no following frame but there are previous frames
          p <- p + scale_colour_manual(name = "Frames", values=c("dodgerblue4", "darkolivegreen"))
        }else if(is.null(prev_cnt) && is.null(foll_cnt)){
          #if there is no following frame nor previous frame
          p <- p + scale_colour_manual(name = "Frames", values=c("dodgerblue4"))
        }else{
          #if there are both previous and following frames
          p <- p + scale_colour_manual(name = "Frames", values=c("firebrick", "dodgerblue4", "darkolivegreen"))
        }
      }else{
        #if user chooses to hide the middle frame
        if(is.null(prev_cnt) && !is.null(foll_cnt)){
          #if there is no previous frame but there are following frames
          p <- p + scale_colour_manual(name = "Frames", values=c("firebrick"))
        }else if(is.null(foll_cnt) && !is.null(prev_cnt)){
          #if there is no following frame but there are previous frames
          p <- p + scale_colour_manual(name = "Frames", values=c("darkolivegreen"))
        }else if(is.null(prev_cnt) && is.null(foll_cnt)){
          #if there is no following frame nor previous frame
          p <- p + scale_colour_manual(name = "Frames", values=c("dodgerblue4"))
        }else{
          #if there are both previous and following frames
          p <- p + scale_colour_manual(name = "Frames", values=c("firebrick", "darkolivegreen"))
        }
      }
    }else{
      #if there are no extra frames
      p <- p + scale_colour_manual(name = "Frames", values=c("dodgerblue4"))
    }
    
    #adds legend to plot
    #=================================================================================================
    p <- p + guides(linetype = guide_legend(override.aes = list(size=1)))
    
    #if user chooses, colours are changed to black and white
    #=================================================================================================
    if(input$colour_contiguous_frames_LM == 2){
      p <- p + scale_colour_grey()
    }
    
    #adds limits to axes, reverses y axis, and ads xy axes labels
    #=================================================================================================
    
    if(input$main_invert_y){
      p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) +
        scale_y_continuous(limits = c(extrema[3], extrema[4])) +
        labs(x ='Tongue Advancement', y = 'Tongue Height')
    }else{
      p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) +
        scale_y_reverse(limits = c(extrema[4], extrema[3])) +
        labs(x ='Tongue Advancement', y = 'Tongue Height')
    }
    
    
    
    #adds annotations
    #=================================================================================================
    #labels, lines and line points
    #adds labels--------------------------------------------------------------------------------------
    
    #if plotting xy point is selected
    if(input$xy_point == T){
      if(!is.null(clicked_reactive$n)){
        #if single clicking is active
        
        #gets x value
        x <- clicked_reactive$n$x
        #gets y value
        y <- clicked_reactive$n$y
        
        #plots intersection labels
        p <- p + geom_label_repel(data = data.frame(x = x, y = y, 
                                                    text = paste0('x=', round(x), '\n', 'y=', round(y))),
                                  aes(x = x, y = y, label = text, fill = '#FF6A6A'), size = 5,
                                  colour = "white", fontface = "bold", show.legend = FALSE)
        
        #plots the point of intersections
        p <- p + geom_point(data = data.frame(x = x, y = y),
                            aes(x = x, y = y), size = 5,
                            colour = "#FF6A6A", show.legend = FALSE)
      }
    }
    
    #adds line points-----------------------------------------------------------------------------------
    
    #if plotting the lines is selected
    if(input$line_point == T){
      #if double clicking information is not empty
      if(!is.null(dbl_clicked_reactive$n)){
        
        #if the first point has not been plotted
        if(line_point_counter$n == 0){
          
          if(input$incontext_manual_origin_x != 0){
            #if the origin point has been defined
            #the x value is defined by the manual input
            x <- input$incontext_manual_origin_x
          }else{
            #if origin point has not been defined
            #the x value is defined by the double clicking
            x <- dbl_clicked_reactive$n$x
          }
          
          if(input$incontext_manual_origin_y != 0){
            #if the origin point has been defined
            #the y value is defined by the manual input
            y <- input$incontext_manual_origin_y
          }else{
            #if origin point has not been defined
            #the y value is defined by the double clicking
            y <- dbl_clicked_reactive$n$y
          }
          
          #adds the first point
          p <- p + geom_point(data = data.frame(x = x, y = y),
                              aes(x = x, y = y), size = 7,
                              colour = "#FF6A6A", show.legend = FALSE)
          
        }else if(line_point_counter$n == 1){
          #if the first point has been plotted
          
          if(input$incontext_manual_origin_x != 0){
            #if the origin point has been defined
            #the x value is defined by the manual input
            x <- input$incontext_manual_origin_x
          }else{
            #if origin point has not been defined
            #the x value is defined by the double clicking
            x <- line_fill_cntr$x1
          }
          
          if(input$incontext_manual_origin_y != 0){
            #if the origin point has been defined
            #the y value is defined by the manual input
            y <- input$incontext_manual_origin_y
          }else{
            #if origin point has not been defined
            #the y value is defined by the double clicking
            y <- line_fill_cntr$y1
          }
          
          #adds the first point
          p <- p + geom_point(data = data.frame(x = x, y = y),
                              aes(x = x, y = y), size = 7,
                              colour = "#3CB371", show.legend = FALSE)
          
          #plots second point and line....................................................
          #if the first point is not the same as the previous point
          if(first_point$x != x & first_point$y != y){
            
            if(input$incontext_manual_secondPoint_x != 0){
              #if the second point has been defined
              #the x value is defined by the manual input
              x2 <- input$incontext_manual_secondPoint_x
            }else{
              #if the second point has not been defined
              #the x value is defined by the double clicking
              x2 <- first_point$x
            }
            
            if(input$incontext_manual_secondPoint_y != 0){
              #if the second point has been defined
              #the y value is defined by the manual input
              y2 <- input$incontext_manual_secondPoint_y
            }else{
              #if the second point has not been defined
              #the y value is defined by the double clicking
              y2 <- first_point$y
            }
            
            #find angles of lines
            #=================================================================================================
            tmp_angle <- find_angle(c(x2,y2), c(x,y))
            #angle to the left of the line
            tmp_angle_left <- round(tmp_angle[[1]])
            #angle to the right of the line
            tmp_angle_right <- round(tmp_angle[[2]])
            
            #adds lines
            #=================================================================================================
            #creates separation from point to label
            if(input$radio_measures_LM == 1){
              #if input measurements are in millimiters
              symbol_sep_measure <- 5
            }else if(input$radio_measures_LM == 2){
              #if input measurements are in pixels
              symbol_sep_measure <- 20
            }
            
            #adds lines to the plot with respective angles at the point of origin
            p <- p + geom_line(data = data.frame(x = c(x, x2), y = c(y, y2)),
                               aes(x = x, y = y),
                               colour = "#3CB371", show.legend = FALSE) + 
              annotate("text", x=x-symbol_sep_measure, y=y, label= paste0(tmp_angle_left, '°')) +
              annotate("text", x=x+symbol_sep_measure, y=y, label= paste0(tmp_angle_right, '°'))
            
            #adds points to the plot
            p <- p + geom_point(data = data.frame(x = x2, y = y2),
                                aes(x = x, y = y), size = 7,
                                colour = "#FF6A6A", show.legend = FALSE)
            
            #finds itersections between lines and contours
            #=================================================================================================
            
            #creates label_plotting_df
            #creates a dataframe to store information on the segment, xy points, the corresponding label and colour
            label_plotting_df_LM <- data.frame(matrix(ncol = 5, nrow = 0))
            #adds column names to the dataframe
            names(label_plotting_df_LM) <- c('segment', 'x', 'y', 'label', 'color_fill')
            
            #Adds Middle contour------------------------------------------------------------------------------
            if(is.null(input$hide_middle_frame_UI)){
              #plots only the middle frame without contiguous contours
              
              #creates spatial lines with the tongue contour
              middle_line <- SpatialLines(list(Lines(list(Line(cbind(middl_cnt$x, middl_cnt$y))), 1)))
              #creates a spatial line from point of origin to corresponding points
              basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
              
              #if there is an intersection between line and middle contour
              if(!is.null(gIntersection(middle_line, basis_line)$x)){
                
                #gets the x value
                int_x <- gIntersection(middle_line, basis_line)$x
                #gets the y value
                int_y <- gIntersection(middle_line, basis_line)$y
                
                #new editing
                tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                
                
                #creates the intersection label
                #int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                
                #creates a temporary dataframe with the information
                tmp_df <- data.frame(segment = 'Middle', x = int_x, y = int_y, 
                                     label = int_label, color_fill = 'dodgerblue4')
                #adds the temporary dataframe to the main label plotting
                label_plotting_df_LM <- rbind(label_plotting_df_LM, tmp_df)
              }
            }else{
              #plots the middle frame with contiguous contours
              if(input$hide_middle_frame_UI == F){
                #if user does not choose to hide the middle frame
                
                #creates spatial lines with the tongue contour
                middle_line <- SpatialLines(list(Lines(list(Line(cbind(middl_cnt$x, middl_cnt$y))), 1)))
                #creates a spatial line from point of origin to corresponding points
                basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
                
                #if there is an intersection between line and middle contour
                if(!is.null(gIntersection(middle_line, basis_line)$x)){
                  
                  #gets the x value
                  int_x <- gIntersection(middle_line, basis_line)$x
                  #gets the y value
                  int_y <- gIntersection(middle_line, basis_line)$y
                  
                  #new editing
                  tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                  
                  
                  #creates the intersection label
                  #int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                  int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                  
                  #creates a temporary dataframe with the information
                  tmp_df <- data.frame(segment = 'Middle', x = int_x, y = int_y, label = int_label, 
                                       color_fill = 'dodgerblue4')
                  #adds the temporary dataframe to the main label plotting
                  label_plotting_df_LM <- rbind(label_plotting_df_LM, tmp_df)
                }
              }
            }
            
            #Plots Previous contour------------------------------------------------------------------------------
            if(!is.null(prev_cnt)){
              #if there are previous contours
              
              #iterates through all previous contours
              for(frame_i in unique(prev_cnt$frame)[1]:unique(prev_cnt$frame)[length(unique(prev_cnt$frame))]){
                
                #subsets to the iterated contour
                prev_cnt_tmp <- prev_cnt[prev_cnt$frame == frame_i,]
                
                #spatial line with the previous contour
                previous_line <- SpatialLines(list(Lines(list(Line(cbind(prev_cnt_tmp$x, prev_cnt_tmp$y))), 1)))
                #spatial line from the point of origin to corresponding points
                basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
                
                #if there is an intersection between point and contour
                if(!is.null(gIntersection(previous_line, basis_line)$x)){
                  
                  #gets the x value
                  int_x <- gIntersection(previous_line, basis_line)$x
                  #gets the y value
                  int_y <- gIntersection(previous_line, basis_line)$y
                  
                  #new editing
                  tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                  
                  
                  #creates an intersection label
                  #int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                  int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                  
                  #creates a temporary dataframe with the information
                  tmp_df <- data.frame(segment = 'Previous', x = int_x, y = int_y, label = int_label, 
                                       color_fill = 'darkolivegreen')
                  #adds the temporary dataframe to the main label plotting
                  label_plotting_df_LM <- rbind(label_plotting_df_LM, tmp_df)
                }
              }
            }
            
            #Plots Following contour-----------------------------------------------------------------------------
            if(!is.null(foll_cnt)){
              #if there are following contours
              
              #iterates through all following contours
              for(frame_i in unique(foll_cnt$frame)[1]:unique(foll_cnt$frame)[length(unique(foll_cnt$frame))]){
                
                #subsets to the iterated contour
                foll_cnt_tmp <- foll_cnt[foll_cnt$frame == frame_i,]
                
                #spatial line with the previous contour
                following_line <- SpatialLines(list(Lines(list(Line(cbind(foll_cnt_tmp$x, foll_cnt_tmp$y))), 1)))
                #spatial line from the point of origin to corresponding points
                basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
                
                #if there is an intersection between point and contour
                if(!is.null(gIntersection(following_line, basis_line)$x)){
                  
                  #gets the x value
                  int_x <- gIntersection(following_line, basis_line)$x
                  #gets the y value
                  int_y <- gIntersection(following_line, basis_line)$y
                  
                  #new editing
                  tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                  
                  
                  #creates an intersection label
                  #int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                  int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                  
                  #creates a temporary dataframe with the information
                  tmp_df <- data.frame(segment = 'Following', x = int_x, y = int_y, label = int_label, 
                                       color_fill = 'firebrick')
                  #adds the temporary dataframe to the main label plotting
                  label_plotting_df_LM <- rbind(label_plotting_df_LM, tmp_df)
                }
              }
            }
            
            #adds circle and label of intersections-----------------------------------------------------------
            #adds the intersection point(s)
            p <- p + geom_point(data = label_plotting_df_LM,
                                aes(x = x, y = y), shape=1, size = 10,
                                colour = "firebrick", show.legend = FALSE)
            
            #adds the intersection label(s)
            
            #if plotting intersection labels is selected
            if(input$intersection_labels == T){
              #if plotting extreme values is selected
              if(input$intersection_labelsExtreme == T){
                #if there is more than one intersection label
                if(nrow(label_plotting_df_LM) > 1){
                  tmp_max_x <- label_plotting_df_LM[label_plotting_df_LM$x == max(label_plotting_df_LM$x),]
                  tmp_min_x <- label_plotting_df_LM[label_plotting_df_LM$x == min(label_plotting_df_LM$x),]
                  label_plotting_df_LM <- rbind(tmp_max_x, tmp_min_x)
                }
              }
            }
            
            #if plotting intersection labels is selected
            if(input$intersection_labels == T){
              
              #creates a setNames variable to map between segments and corresponding colours
              manual_fill_names <- setNames(as.character(label_plotting_df_LM$color_fill), 
                                            as.character(label_plotting_df_LM$segment))
              
              #if user selects repelling labels, which creates a more readable plot without label overlap
              if(input$intersection_labelsRepel == T){
                p <- p + geom_label_repel(data = label_plotting_df_LM, aes(x = x, y = y, label = label, 
                                                                           fill = factor(segment)), size = 3.5,
                                          color = 'white', fontface = "bold", show.legend = FALSE) +
                  scale_fill_manual(values=manual_fill_names)
              }else{
                #if user does not select labe repelling, which results in labels overlap
                p <- p + geom_label(data = label_plotting_df_LM, aes(x = x, y = y, label = label, 
                                                                     fill = factor(segment)), size = 3.5,
                                    color = 'white', fontface = "bold", show.legend = FALSE) +
                  scale_fill_manual(values=manual_fill_names)
              }
            }
            
            #Plots Palate contour---------------------------------------------------------------------------------
            #if plotting the palate trace is selected
            if(input$palate_plot_LM == T){
              
              #spatial line with thepalate contour
              palate_line <- SpatialLines(list(Lines(list(Line(cbind(pal$x, pal$y))), 1)))
              #spatial line from the point of origin to corresponding points
              basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(x, x2), c(y, y2)))), 1)))
              
              #if there is an intersection between line and palate contour
              if(!is.null(gIntersection(palate_line, basis_line)$x)){
                
                #gets the x value
                int_x <- gIntersection(palate_line, basis_line)$x
                #gets the y value
                int_y <- gIntersection(palate_line, basis_line)$y
                #creates a temporary dataframe with the information
                plot_data_pal <- data.frame(x = int_x, y = int_y)
                
                #new editing
                tmpInternalDistance <- sqrt((x-int_x)^2+(y-int_y)^2)
                
                
                #creates the intersection label
                #plot_data_pal$label <- paste0('x=', round(plot_data_pal$x), ', ', 'y=', round(plot_data_pal$y))
                plot_data_pal$label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                
                #add circle and label for the palate intersection
                #adds intersection point
                p <- p + geom_point(data = plot_data_pal,
                                    aes(x = x, y = y), shape=1, size = 10,
                                    colour = "deepskyblue4", show.legend = FALSE)
                
                #if plotting intersection label is selected
                #plots intersection label
                if(input$intersection_labels == T){
                  p <- p + geom_label(data = plot_data_pal, aes(x = x, y = y, label = label), 
                                      fill = "deepskyblue4", size = 3.5,
                                      colour = "white", fontface = "bold", show.legend = FALSE)
                }
              }
            }
            
          }
        }
        
      }
    }
    
    p
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  source(file.path("server/savePlots", "saveLandmarks.R"),  local = TRUE)$values
  
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #This section controls the visualisation of the xy information of the point clicked
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #UI to show the xy values of the point clicked
  output$coordenate_info_ui_LM <- renderUI({
    verbatimTextOutput("coordenate_info_LM")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #shows the xy values of the point clicked
  output$coordenate_info_LM <- renderText({
    
    #if the double clicked button is not NULL
    if(is.null(dbl_clicked_reactive$n))
      return()
    
    #if the plotting of line is selected
    if(input$line_point == T){
      
      #if the double clicked is not empty
      if(!is.null(dbl_clicked_reactive$n)){
        
        #if no point of the line has been previously clicked
        if(line_point_counter$n == 0){
          
          
          #if the x value of the origin point has been manually selected
          if(input$incontext_manual_origin_x != 0){
            #sets the x value from the manual input
            x <- input$incontext_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x <- dbl_clicked_reactive$n$x
          }
          
          #if the y value of the origin point has been manually selected
          if(input$incontext_manual_origin_y != 0){
            #sets the y value from the manual input
            y <- input$incontext_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y <- dbl_clicked_reactive$n$y
          }
          
          #prints the xy values of the point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
        }else if(line_point_counter$n == 1){
          #if one point of the line has been previously clicked
          
          #if the x value of the origin point has been manually selected
          if(input$incontext_manual_origin_x != 0){
            #sets the x value from the manual input
            x <- input$incontext_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x <- line_fill_cntr$x1
          }
          #if the y value of the origin point has been manually selected
          if(input$incontext_manual_origin_y != 0){
            #sets the y value from the manual input
            y <- input$incontext_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y <- line_fill_cntr$y1
          }
          
          #prints the xy values of the origin point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
          #plot second point and line
          ###if the second point is not the same as the first point
          if(first_point$x != x & first_point$y != y){
            #if the x value of the origin point has been manually selected
            if(input$incontext_manual_secondPoint_x != 0){
              #sets the x value from the manual input
              x2 <- input$incontext_manual_secondPoint_x
            }else{
              #if the x value of the origin point is created from clicking
              #sets the x value from the clicking
              x2 <- first_point$x
            }
            
            #if the y value of the origin point has been manually selected
            if(input$incontext_manual_secondPoint_y != 0){
              #sets the y value from the manual input
              y2 <- input$incontext_manual_secondPoint_y
            }else{
              #if the y value of the origin point is created from clicking
              #sets the y value from the clicking
              y2 <- first_point$y
            }
            
            #prints the xy values of the second point
            print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1), 
                         ' - Second Point: ', format(round(x2, 1), nsmall = 1), ',', format(round(y2, 1), nsmall = 1)))
          }
          
        }
        
      }
      
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  ##THIS SECTION OBSERVES THE CLICKING ACTIVITY IN THE LANDMARKS TAB
  ##======================================================================
  ##======================================================================
  ##======================================================================
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #optional output to dynamically display the x value of single click
  output$vals <- renderText({
    #print(input$lm_click$x)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #stores the xy values of the line plotted, both the begin and end points
  line_fill_cntr <- reactiveValues(x1 = 0, y1 = 0, x2 = 0, y2 = 0)
  
  #stores the xy values of the double click
  dbl_clicked_reactive <- reactiveValues(n = NULL)
  #counts the number of points for the line
  line_point_counter <- reactiveValues(n = 0)
  
  #stores the xy values of the first point of the line
  first_point <- reactiveValues(x = 0, y = 0)
  
  #----------------------------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #observes and the stores the double clicking activity
  observe({
    #if the double clicking of the plot object is not NULL
    if (!is.null(input$lm_dblclick)) {
      dbl_clicked_reactive$n <- input$lm_dblclick 
      
      #passes the x value to the first point
      first_point$x <- input$lm_dblclick$x
      #passes the y value to the first point
      first_point$y <- input$lm_dblclick$y
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #counts the number of clicks for the line
  plot_line_click_counter <- reactiveValues(n = 0)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reacts to the clicking of the button to plot lines and stores the xy values
  observeEvent(input$plot_lines,{   
    #if the plot lines button is clicked, then the counter is updated
    plot_line_click_counter$n <- plot_line_click_counter$n + 1
    
    #runs the actions if the double clicked button has been previously clicked
    if (!is.null(dbl_clicked_reactive$n)) {
      #if no clicking information has been stored, i.e. no points in the plot object
      if(sum(line_fill_cntr$x1, line_fill_cntr$x2, line_fill_cntr$y1, line_fill_cntr$y2) == 0){
        
        ##the xy information is the same for both points of the line but only the x1y1 is plotted
        line_fill_cntr$x1 <- dbl_clicked_reactive$n$x
        line_fill_cntr$y1 <- dbl_clicked_reactive$n$y
        line_fill_cntr$x2 <- dbl_clicked_reactive$n$x
        line_fill_cntr$y2 <- dbl_clicked_reactive$n$y
        
        #updates the number of points in the line
        line_point_counter$n <- 1
      }else{
        #if previous clicking information has been stored
        if((line_fill_cntr$x1 == line_fill_cntr$x2) & (line_fill_cntr$y1 == line_fill_cntr$y2)){
          
          #stores the new xy information for the second point
          line_fill_cntr$x2 <- dbl_clicked_reactive$n$x
          line_fill_cntr$y2 <- dbl_clicked_reactive$n$y
          
          #updates the number of points in the line
          line_point_counter$n <- 1
        }
      }
      
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reacts to the clicking of the button to plot lines and stores the xy values
  observeEvent(input$plot_lines,{
    #if there are two points already stored
    if(plot_line_click_counter$n == 2){
      
      #then the counting is reset to zero
      line_fill_cntr$x1 <- 0
      line_fill_cntr$y1 <- 0
      line_fill_cntr$x2 <- 0
      line_fill_cntr$y2 <- 0
      
      line_point_counter$n <- 0
      dbl_clicked_reactive$n <- NULL
      plot_line_click_counter$n <- 0
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #----------------------------------------------------------------------
  #stores the single clicking information
  clicked_reactive = reactiveValues(n = NULL)
  
  #observes the activity of the single clicking
  observe({
    #if the single clicking information is not NULL
    if (!is.null(input$lm_click)) {
      #stores the single clicking information
      clicked_reactive$n <- input$lm_click   
    }
  })
  
  
  #this section controls checkboxes and button to plot/display lines and labels in the plot object
  
  #checkbox to show the intersection labels
  output$intersection_labels_UI <- renderUI({
    if(input$line_point == T){
      checkboxInput("intersection_labels", label = "Labels", value = T)
    }
  })
  
  #checkbox to repel labels in the plot object
  output$intersection_labelsRepel_UI <- renderUI({
    if(input$line_point == T){
      checkboxInput("intersection_labelsRepel", label = "Repel", value = T)
    }
  })
  
  #checkbox to choose showing only the extreme labels in the intersections
  output$intersection_labelsExtreme_UI <- renderUI({
    if(input$line_point == T){
      checkboxInput("intersection_labelsExtreme", label = "Extreme", value = T)
    }
  })
  
  #creates button widget to plot lines
  output$plot_lines_UI <- renderUI({
    if(input$line_point == F)
      return()
    
    if(line_point_counter$n == 0){
      bsButton("plot_lines", label = "Set anchor", icon("anchor"), style = "primary")
    }else{
      bsButton("plot_lines", label = "Clear lines", icon("anchor"), style = "danger")
    }
  })
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #This section controls the definition and classification of landmarks
  #==========================================================================================
  #==========================================================================================
  
  #this reactive value creates the classification buton seen in every tongue contour
  #if no landmarks have been defined, the button show the label "Undefined"
  #if at least one landmark has been created, then the label is "Unclassified"
  #if the tongue contour has been classified, then the label shows its classification
  output$see_classification_UI <- renderUI({
    #==============================
    #get the speaker
    sp <- input$spkr_LM
    #get the segment
    seg <- input$sgmnt_LM
    #get the repetition
    rep <- input$rpttn_LM
    #get the frame
    frm <- input$frm_LM
    
    inside_df <- all_plot_data$d
    
    tmp_line <- inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                            inside_df$repetition == rep & inside_df$frame == frm, ]
    
    #if no extra labels have been added
    if(length(colnames(tmp_line)) == 9){
      bsButton('see_classification', label = 'Undefined', icon("exclamation"), style = "warning")
    }else{
      tmp_vector <- tmp_line[1,10:length(colnames(tmp_line))]
      names(tmp_vector) <- NULL
      tmp_vector <- unlist(c(tmp_vector))
      
      #if extra labels have been added but the frame has not been assigned
      if(length(which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))) == 0){
        bsButton('see_classification', label = 'Unclassified', icon("question-circle-o"), style = "success")
      }else{
        tmp_landmark_label <- tmp_vector[which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))]
        bsButton('see_classification', label = tmp_landmark_label, icon("close"), style = "danger")
      }
    }
  })
  #=========================================================================================================
  #This section oberves the acitivity of the see classification button.
  observeEvent(input$see_classification,{
    #==============================
    #get the speaker
    sp <- input$spkr_LM
    #get the segment
    seg <- input$sgmnt_LM
    #get the repetition
    rep <- input$rpttn_LM
    #get the frame
    frm <- input$frm_LM
    
    inside_df <- all_plot_data$d
    
    tmp_line <- inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                            inside_df$repetition == rep & inside_df$frame == frm, ]
    
    #if no extra labels have been added
    if(length(colnames(tmp_line)) > 9){
      tmp_vector <- tmp_line[1,10:length(colnames(tmp_line))]
      names(tmp_vector) <- NULL
      tmp_vector <- unlist(c(tmp_vector))
      
      #if extra labels have been added but the frame has not been assigned
      if(length(which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))) != 0){
        tmp_landmark_label <- tmp_vector[which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))]
        
        inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                    inside_df$repetition == rep & inside_df$frame == frm, tmp_landmark_label] <- ""
        
        all_plot_data$d <- inside_df
        
      }
    }
  })
  
  #============================================================================================================
  #clicking controls
  #counts the number of times user clicks the SET button
  set_landmark_count <- reactiveValues(n = 0)
  #counts the number of times user clicks the EDIT button
  edit_landmark_count <- reactiveValues(n = 0)
  #counts the number of times user clicks the DONE button
  done_landmark_count <- reactiveValues(n = 0)
  
  existing_landmark_buttons_cntr <- reactiveValues(n = NULL)
  existing_landmark_buttons_cntr_ADDBTN <- reactiveValues(n = NULL)
  
  incoming_labels <- reactiveValues(val = NULL)
  edit_textInput <- reactiveValues(val = NULL)
  edit_textInput_clicked <- reactiveValues(n = 0)
  initial_textInput_blank <- reactiveValues(val = 0)
  
  #controls the ADD button behaviour
  delete_confirmation <- reactiveValues(val = 0)
  
  #define data======================================================================================================
  all_plot_data <- reactiveValues(d = NULL)
  button_column_correlation <- reactiveValues(d = NULL)
  landmark_labels <- reactiveValues(val = NULL)
  
  inContextButtonBehaviour <- reactiveValues(n = NULL)
  
  #============================================================================================================
  #define Land Marks numbers
  output$landMark_numbers_ui <- renderUI({
    #if the number of landmakrs has been modified internally after pressing the DONE button
    if(!is.null(existing_landmark_buttons_cntr$n)){
      #number of current active buttons
      tmp_selection <- length(existing_landmark_buttons_cntr$n)
      selectInput("landMark_numbers", label = h5("Number of Landmarks"), 
                  choices = c(1:20), selected = tmp_selection)
    }else{
      selectInput("landMark_numbers", label = h5("Number of Landmarks"), 
                  choices = c(1:20))
    }
  })
  
  #------------------------------------------------------------------------------------------------------------
  #creates the input text fields for the labels of the landmarks based on the number entered by the user
  observeEvent(input$create_landmarks,{
    #setting this variable to 0 indicates that the textfields are those of the initial run of the program
    set_landmark_count$n <- 0
    input_numbers <- input$landMark_numbers
    lapply(1:input_numbers, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        textInput(paste0("inLM_text", i), label = h5(paste0("Landmark ", i)), value = "")
      })
    })
  })
  
  #------------------------------------------------------------------------------------------------------------
  #creates the button to control setting and Editting of landmarks (labels, deleting, adding)
  observeEvent(input$create_landmarks,{
    output$set_LMs_UI <- renderUI({
      #if the first textinout is null, it is the initial run of the program
      if(is.null(input[[paste0("inLM_text", 1)]])){
        bsButton("set_LMs", label = "Set", icon("chevron-circle-left"), style = "primary")
      }else{
        #if a landmark has already been created and is edited or requested to be edited
        if(set_landmark_count$n == 1){
          #checks whether a landmark has been edited or request to be edited
          if(edit_landmark_count$n == 1){
            #if a landmakr has been edited
            bsButton("done_LMs", label = "Done", icon("check-circle"), style = "warning")
          }else{
            #if a landmark is requested to be edited
            bsButton("edit_LMs", label = "Edit", icon("pencil"), style = "danger")
          }
        }else if(set_landmark_count$n == 0){
          #if a landmark has already existed and all have been deleted
          bsButton("set_LMs", label = "Set", icon("chevron-circle-left"), style = "primary")
        }
      }
    })
  })
  
  #------------------------------------------------------------------------------------------------------------
  #creates the landmark buttons and checks whether no landmark label has been entered
  observeEvent(input$set_LMs,{
    #input number of Landmarks
    input_numbers <- input$landMark_numbers
    
    #stores the active Landmark labels
    incoming_labels$val <- NULL
    
    #empty conditions
    #if the user presses Set and no input has been made, check whether all inputs are blank
    initial_textInput_blank$val <- 0
    
    lapply(1:input_numbers, function(i) {
      tmp_label <- input[[paste0("inLM_text", i)]]
      
      if(tmp_label != ""){
        initial_textInput_blank$val <- isolate(initial_textInput_blank$val) + 1
        incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
      }
    })
    
    #if not input has been made, clear all the text inputs and the Set button
    if(initial_textInput_blank$val == 0){
      lapply(1:input_numbers, function(i) {
        output[[paste0('inLM', i)]] <- renderUI({
          return()
        })
      })
      #sets SET button to null
      output$set_LMs_UI <- renderUI({
        return()
      })
      
      #sets the buttons in context in the InContext window to NULL
      lapply(1:input_numbers, function(i) {
        output[[paste0('inLM_inContext', i)]] <- renderUI({
          return()
        })
      })
      
      landmark_labels$val <- NULL
      button_column_correlation$d$btn_id <- NULL
      button_column_correlation$d$column_label <- NULL
    }else{
      landmark_labels$val <- incoming_labels$val
      button_column_correlation$d$btn_id <- paste0("inLM_inContext_btn", 1:length(landmark_labels$val))
      button_column_correlation$d$column_label <- landmark_labels$val
      
      #if some input has been made
      set_landmark_count$n <- 1
      
      lapply(1:initial_textInput_blank$val, function(i) {
        output[[paste0('inLM', i)]] <- renderUI({
          tmp_label <- incoming_labels$val[i]
          
          if(tmp_label != ''){
            bsButton(paste("set_LMs_btn", i), label = tmp_label, icon("flag-o"), style = "warning", block = T)
          }
        })
      })
      
      #sets the InContext buttons
      #plots the buttons in the inContext window
      lapply(1:initial_textInput_blank$val, function(i) {
        output[[paste0('inLM_inContext', i)]] <- renderUI({
          tmp_label <- incoming_labels$val[i]
          
          if(tmp_label != ''){
            
            bsButton(paste0("inLM_inContext_btn", i), label = tmp_label, icon("flag-o"), style = "warning")
            
          }
        })
      })
      
      #adds counter to all the in context buttons
      for(add_counter_i in 1:length(landmark_labels$val)){
        inContextButtonBehaviour$n[add_counter_i] <- 0
      }
      
      #print(inContextButtonBehaviour$n)
      
      #creates the dataframe
      if(!is.null(landmark_labels$val)){
        tmp_df <- importFiles()
        tmp_df <- tmp_df[[1]]
        
        input_numbers <- length(landmark_labels$val)
        
        for(i_add_col in landmark_labels$val){
          tmp_df[[i_add_col]] <- NULL
        }
        
        all_plot_data$d <- tmp_df
        
        #View(all_plot_data$d)
      }
      
      edit_textInput_clicked$n <- 0
    }
  })
  
  #adds the values to the data shown in inContext table
  observe({
    lapply(1:length(landmark_labels$val), function(i) {
      observeEvent(input[[paste0("inLM_inContext_btn", i)]],{
        
        tmp_display_df <- data.frame(button_id = button_column_correlation$d$btn_id, 
                                     column_label = button_column_correlation$d$column_label)
        
        #get the Landmark column ID
        tmp_column_name <- as.character(tmp_display_df[tmp_display_df$button_id == 
                                                         paste0("inLM_inContext_btn", i), 'column_label'])
        
        #set the landmark in the file
        #==============================
        #get the speaker
        sp <- input$spkr_LM
        
        #get the segment
        seg <- input$sgmnt_LM
        
        #get the repetition
        rep <- input$rpttn_LM
        
        #get the frame
        frm <- input$frm_LM
        
        #store the value
        inside_df <- all_plot_data$d
        
        #deletes the current labels of the frame or sets it to ""
        for(delete_i in incoming_labels$val){
          if(length(which(delete_i %in% colnames(all_plot_data$d))) != 0){
            inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                        inside_df$repetition == rep & inside_df$frame == frm, delete_i] <- ""
          }
        }
        
        #find the row with the specified information
        inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                    inside_df$repetition == rep & inside_df$frame == frm, tmp_column_name] = tmp_column_name
        
        all_plot_data$d <- inside_df
        #View(all_plot_data$d)
      })
    })
  })
  
  #============================================================================================================
  #creates the edit button landmarks
  #displays as text
  observeEvent(input$edit_LMs,{
    edit_landmark_count$n <- 1
    input_numbers <- input$landMark_numbers
    lapply(1:input_numbers, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        if(edit_textInput_clicked$n  == 1){
          tmp_label <- incoming_labels$val[i]
        }else{
          tmp_label <- input[[paste0("inLM_text", i)]]
        }
        textInput(paste0("inLM_text", i), label = NULL, value = tmp_label)
      })
    })
  })
  #------------------------------------------------------------------------------------------------------------
  #add button
  observeEvent(input$edit_LMs,{
    output$add_LMs_UI <- renderUI({
      bsButton("add_LMs_btn", label = NULL, icon("plus-circle"), style = "warning", block = T)
    })
    output$delete_LMs_UI <- renderUI({
      if(delete_confirmation$val == 0){
        bsButton("delete_LMs_btn", label = 'All', icon("times-circle"), style = "danger", block = T)
      }else{
        bsButton("delete_LMs_btn", label = 'Sure', icon("question-circle"), style = "danger", block = T)
      }
    })
  })
  #------------------------------------------------------------------------------------------------------------
  #creates the input text fields for the labels of the landmarks when the ADD button is pressed
  observeEvent(input$add_LMs_btn,{
    set_landmark_count$n <- 1
    
    existing_landmark_buttons_cntr$n <- as.numeric(input$landMark_numbers) + 1
    existing_landmark_buttons_cntr_ADDBTN$n <- existing_landmark_buttons_cntr$n
    input_numbers <- as.numeric(input$landMark_numbers)
    incoming_labels$val <- NULL
    
    #find and redefine the number id of the textfields
    lapply(1:input_numbers, function(i) {
      tmp_label <- input[[paste0("inLM_text", i)]]
      
      if(tmp_label != ""){
        existing_landmark_buttons_cntr_ADDBTN$n <- append(isolate(existing_landmark_buttons_cntr_ADDBTN$n), i)
        incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
      }
    })
    
    incoming_labels$val <- append(isolate(incoming_labels$val), '')
    
    lapply(1:(existing_landmark_buttons_cntr_ADDBTN$n), function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        textInput(paste0("inLM_text", i), label = NULL, value = incoming_labels$val[i])
      })
    })
  })
  
  #------------------------------------------------------------------------------------------------------------
  #controls for the delete button
  observeEvent(input$delete_LMs_btn,{
    
    if(delete_confirmation$val == 1){
      #deletes the columns from the dataframe
      tmp_df <- all_plot_data$d
      tmp_current_column_labels <- names(tmp_df)[-c(1:9)]
      for(delete_i in tmp_current_column_labels){
        tmp_df[[delete_i]] <- NULL
      }
      
      all_plot_data$d <- tmp_df
      
      #View(all_plot_data$d)
      
      #deletes all inputs
      lapply(1:20, function(i) {
        output[[paste0('inLM', i)]] <- renderUI({
          return()
        })
      })
      
      #deletes in context buttons UIs
      lapply(1:20, function(i) {
        output[[paste0('inLM_inContext', i)]] <- renderUI({
          return()
        })
      })
      
      #deletes set button
      output$set_LMs_UI <- renderUI({
        return()
      })
      
      #deletes add button
      output$add_LMs_UI <- renderUI({
        return()
      })
      
      #deletes delete button
      output$delete_LMs_UI <- renderUI({
        return()
      })
    }
    
    #updates the value for asking the user
    if(delete_confirmation$val == 0){
      delete_confirmation$val <- 1
      landmark_labels$val <- NULL
      button_column_correlation$d$btn_id <- NULL
      button_column_correlation$d$column_label <- NULL
    }else{
      delete_confirmation$val <- 0
      set_landmark_count$n <- 1
      edit_landmark_count$n <- 0
    }
  })
  
  #------------------------------------------------------------------------------------------------------------
  #when editing is finished
  observeEvent(input$done_LMs,{
    edit_landmark_count$n <- 0
    done_landmark_count$n <- 1
    existing_landmark_buttons_cntr$n <- NULL
    
    #checks if a new textinput field has been added by pressing the ADD button
    if(is.null(existing_landmark_buttons_cntr_ADDBTN$n)){
      input_numbers <- input$landMark_numbers
    }else{
      input_numbers <- length(existing_landmark_buttons_cntr_ADDBTN$n)
    }
    
    existing_landmark_buttons_cntr_ADDBTN$n <- NULL
    
    #gets the labels of the textinput fields
    incoming_labels$val <- NULL
    
    lapply(1:input_numbers, function(i) {
      tmp_label <- input[[paste0("inLM_text", i)]]
      
      if(tmp_label != ""){
        existing_landmark_buttons_cntr$n <- append(isolate(existing_landmark_buttons_cntr$n), i)
        incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
      }
    })
    
    #deletes all textinput fields
    lapply(1:20, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        return()
      })
    })
    
    lapply(1:20, function(i) {
      updateTextInput(session, paste0("inLM_text", i), label = h5(paste0("Landmark ", i)), value = NULL)
    })
    
    if(!is.null(existing_landmark_buttons_cntr$n)){
      for(change_value in 1:length(existing_landmark_buttons_cntr$n)){
        existing_landmark_buttons_cntr$n[change_value] <- change_value
      }
    }else{
      output$set_LMs_UI <- renderUI({
        return()
      })
    }
    
    if(is.null(incoming_labels$val)){
      
      landmark_labels$val <- NULL
      button_column_correlation$d$btn_id <- NULL
      button_column_correlation$d$column_label <- NULL
      
      lapply(1:20, function(i) {
        output[[paste0('inLM', i)]] <- renderUI({
          return()
        })
      })
    }else{
      
      landmark_labels$val <- incoming_labels$val
      button_column_correlation$d$btn_id <- paste0("inLM_inContext_btn", 1:length(landmark_labels$val))
      button_column_correlation$d$column_label <- landmark_labels$val
      
      #creates the landmark buttons
      set_landmark_count$n <- 1
      lapply(existing_landmark_buttons_cntr$n, function(i) {
        output[[paste0('inLM', i)]] <- renderUI({
          tmp_label <- incoming_labels$val[i]
          bsButton(paste("set_LMs_btn", i), label = tmp_label, icon("flag-o"), style = "warning", block = T)
        })
      })
      
      #creates the InContext landmark buttons
      lapply(existing_landmark_buttons_cntr$n, function(i) {
        output[[paste0('inLM_inContext', i)]] <- renderUI({
          tmp_label <- incoming_labels$val[i]
          
          if(tmp_label != ''){
            bsButton(paste0("inLM_inContext_btn", i), label = tmp_label, icon("flag-o"), style = "warning")
          }
        })
      })
      
      #controls the columns editing
      tmp_df <- all_plot_data$d
      
      input_labels_number <- length(landmark_labels$val)
      
      #check whether changes have been made
      tmp_current_column_labels <- names(tmp_df)[-c(1:9)]
      
      #if a change has been made
      if((length(landmark_labels$val) == length(tmp_current_column_labels) &
          length(setdiff(landmark_labels$val, tmp_current_column_labels)) != 0) ||
         (length(landmark_labels$val) != length(tmp_current_column_labels))){
        #if a label has been added or removed
        if(length(landmark_labels$val) != length(tmp_current_column_labels)){
          #if a label has been added
          if(length(tmp_current_column_labels) < input_labels_number){
            #gets the new label
            tmp_new_label <- setdiff(landmark_labels$val, tmp_current_column_labels)
            #adds labels
            for(i_new_add_column in tmp_new_label){
              tmp_df[[i_new_add_column]] <- NULL
            }
          }else if(length(tmp_current_column_labels) > input_labels_number){
            #if a label has been deleted
            #gets the new label
            tmp_delete_label <- setdiff(tmp_current_column_labels, landmark_labels$val)
            for(i_delete_column in tmp_delete_label){
              tmp_df[[i_delete_column]] <- NULL
            }
          }
        }else{
          if(length(setdiff(landmark_labels$val, tmp_current_column_labels)) != 0){
            current_label <- setdiff(tmp_current_column_labels, landmark_labels$val)
            new_label <- setdiff(landmark_labels$val, tmp_current_column_labels)
            
            for(i_change_label in 1:length(current_label)){
              tmp_df[[new_label[i_change_label]]] <- NULL
              tmp_df[[current_label[i_change_label]]] <- NULL
            }
          }
        }
      }
      
      all_plot_data$d <- tmp_df
      
      #View(all_plot_data$d)
      
      #adds the clicked Landmark to the dataset
      if(!is.null(landmark_labels$val)){
        lapply(1:input_numbers, function(i) {
          observeEvent(input[[paste0("inLM_inContext_btn", i)]],{
            
            tmp_display_df <- data.frame(button_id = button_column_correlation$d$btn_id, 
                                         column_label = button_column_correlation$d$column_label)
            
            #get the Landmark column ID
            tmp_column_name <- as.character(tmp_display_df[tmp_display_df$button_id == 
                                                             paste0("inLM_inContext_btn", i), 'column_label'])
            #set the landmark in the file
            #==============================
            #get the speaker
            sp <- input$spkr_LM
            
            #get the segment
            seg <- input$sgmnt_LM
            
            #get the repetition
            rep <- input$rpttn_LM
            
            #get the frame
            frm <- input$frm_LM
            
            #store the value
            inside_df <- all_plot_data$d
            
            #find the row with the specified information
            inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                        inside_df$repetition == rep & inside_df$frame == frm, tmp_column_name] <- tmp_column_name
            
            all_plot_data$d <- inside_df
            #View(all_plot_data$d)
          })
        })
      }
      
      edit_textInput_clicked$n <- 1
    }
    
    output$add_LMs_UI <- renderUI({
      return()
    })
    
    output$delete_LMs_UI <- renderUI({
      return()
    })
  })
  
  
  #This section prepares and displays the landmark table
  #============================================================================================
  #============================================================================================
  
  #creates the reactive value to create the landmark table
  prep_landmark_table <- reactive({
    #if plot data is null
    if(is.null(all_plot_data$d))
      return()
    
    #imports the plot data
    df <- all_plot_data$d
    
    #subsets data to the first point
    df <- df[df$point == 1,]
    #subsets data to the x points
    df <- df[df$coord == 'x',]
    
    #creates a vector with the names of the columns to be dropped
    drops <- c('point', 'coord', 'pixel', 'mm')
    #drops the columns in the vector
    df <- df[ , !(names(df) %in% drops)]
    
    write.csv(df, paste0('./workingFiles/df_landmark.csv'), row.names = F)
    
    #returns the dataframe
    return(df)
  })
  
  #creates the landmark table display
  output$landmark_table = output$gridline_table = DT::renderDataTable(
    prep_landmark_table(), 
    options = list(lengthChange = T, dom = 'tip', scrollX = TRUE),
    rownames = FALSE, style = "bootstrap"
  )
  
  
  #This section controls the data management
  
  #sets the dataframe and their values
  vals <- reactiveValues()
  observe({
    if(!is.null(importFiles()[[1]])){
      dat <- as.data.table(importFiles()[[1]])
      dat_short <- dat[dat$point == 1 & dat$coord == 'x',]
      dat_short <- dat_short[, c('X', 'coord', 'point', 'pixel', 'mm'):=NULL]
      vals$Data <- dat_short
    }
  })
  
  output$Main_table <- renderDataTable({
    DT <- vals$Data
    DT[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
    
    DT[["Actions"]] <-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
             <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Modify</button>
             </div>
             
             ')
    datatable(DT,
              escape=F, style = "bootstrap")}
  )
  
  observeEvent(input$Del_row_head,{
    #gets the numeric value of the row
    row_to_del <- as.numeric(gsub("Row","",input$checked_rows))
    #deletes the row
    vals$Data <- vals$Data[-row_to_del]}
  )
  
  ###Brand visualisation 
  observeEvent(input$Compare_row_head,{
    #gets the index of the rows
    row_to_del <- as.numeric(gsub("Row","",input$checked_rows))
    #gets the total number of rows to be compared
    number_brands <- length(row_to_del)
    
    #gets the plotting values
    
    #get the index of the rows
    tmp_dat_short <- vals$Data
    tmp_dat_short <- tmp_dat_short[row_to_del]
    plot_dat <- data.frame(matrix(nrow = 0, ncol = ncol(importFiles()[[1]])))
    names(plot_dat) <- names(importFiles()[[1]])
    
    tpm_incoming_data <- as.data.table(importFiles()[[1]])
    
    for(speaker_i in unique(tmp_dat_short$speaker)){
      speaker_df <- tmp_dat_short[tmp_dat_short$speaker == speaker_i,]
      for(segment_i in unique(speaker_df$segment)){
        segment_df <- speaker_df[speaker_df$segment == segment_i,]
        for(repetition_i in unique(segment_df$repetition)){
          repetition_df <- segment_df[segment_df$repetition == repetition_i,]
          for(frame_i in unique(repetition_df$frame)){
            iterated_df <- tpm_incoming_data[tpm_incoming_data$speaker == speaker_i & 
                                               tpm_incoming_data$segment == segment_i &
                                               tpm_incoming_data$repetition == repetition_i & 
                                               tpm_incoming_data$frame == frame_i,]
            
            names(iterated_df) <- names(plot_dat)
            
            plot_dat <- rbind(plot_dat, iterated_df) 
          }
        }
      }
    }
    
    #creates fake sales values
    vals$plotvalues <- as.data.table(plot_dat)
    
    #makes the Brads as factor
    vals$plotvalues[,speaker:=as.factor(speaker)]
    vals$plotvalues[,segment:=as.factor(segment)]
    vals$plotvalues[,repetition:=as.factor(repetition)]
    vals$plotvalues[,frame:=as.factor(frame)]
    #shows the plot in a UI modal window
    showModal(plotvalues_modal)
  }
  )
  
  #creates a modal dialog with the plot
  plotvalues_modal<-modalDialog(
    fluidPage(
      h3(strong("Contours for selected tokens"),align="center"),
      plotOutput('sales_plot')
    ),
    #size of the modal window, large in this case
    size <- "l"
  )
  
  #creates the plot
  output$sales_plot <- renderPlot({
    require(ggplot2)
    #line ggplot of the fake sales
    
    if(length(unique(vals$plotvalues$speaker)) == 1){
      #if only one speaker is selected
      if(length(unique(vals$plotvalues$segment)) == 1){
        #if only one segment number is selected
        if(length(unique(vals$plotvalues$repetition)) == 1){
          #if only one repetition is selected
          if(length(unique(vals$plotvalues$frame)) == 1){
            #if only one frame is selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = frame, colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = frame, colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }else{
            #if multiple frames are selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(repetition,frame), 
                                               colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(repetition,frame), 
                                               colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
          }
        }else{
          #if multiple repetitions are selected
          if(length(unique(vals$plotvalues$frame)) == 1){
            #if only one frame is selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = repetition, colour = repetition)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = repetition, colour = repetition)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }else{
            #if multiple frames are selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(repetition,frame), 
                                               colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(repetition,frame), 
                                               colour = frame)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }
        }
      }else{
        #if multiple segment numbers are selected
        if(length(unique(vals$plotvalues$repetition)) == 1){
          #if only one repetition is selected
          if(length(unique(vals$plotvalues$frame)) == 1){
            #if only one frame is selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = segment, colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = segment, colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }else{
            #if multiple frames are selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,frame), colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,frame), colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }
        }else{
          #if multiple repetitions are selected
          if(length(unique(vals$plotvalues$frame)) == 1){
            #if only one frame is selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,repetition), 
                                               colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,repetition), 
                                               colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
            
          }else{
            #if multiple frames are selected
            
            if(input$main_invert_y){
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,repetition), 
                                               colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }else{
              p <- ggplot(vals$plotvalues, aes(x = point, y = mm, group = interaction(segment,repetition), 
                                               colour = segment)) + 
                geom_line(stat = 'smooth', method = 'auto') +
                scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
            }
            
            
          }
        }
      }
    }
    #print('here')
    print(p)
    
  })
  
  ##Managing in row deletion / modification
  modal_modify <- modalDialog(
    fluidPage(
      h3(strong("Row modification"),align="center"),
      hr(),
      #row selected
      dataTableOutput('row_modif'),
      #action button to save the changes
      actionButton("save_changes","Save changes"),
      
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       
                       Shiny.onInputChange('newValue', list_value)
});"))
    ),
    size="l"
  )
  
  #checks the last clicked button
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   #if the user clicks the delete button
                   #gets the row index
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   #deletes it from the dataset
                   vals$Data=vals$Data[-row_to_del]
                 }
                 else if (input$lastClickId%like%"modify")
                 {
                   #if the user clicks the modify button
                   #open a modal window
                   showModal(modal_modify)
                 }
               }
  )
  
  #modifying the dataset
  output$row_modif <- renderDataTable({
    #seltected row to modify
    selected_row <- as.numeric(gsub("modify_","",input$lastClickId))
    #gets the old row values
    old_row <- vals$Data[selected_row]
    #creates a list to store the new values
    row_change <- list()
    
    #iterates through all the columns
    for (i in colnames(old_row))
    {
      if (is.numeric(vals$Data[[i]]))
      {
        #if the column value is numeric
        row_change[[i]] <- paste0('<input class="new_input" type="number" id=new_',i,'><br>')
      }
      else
        #if the column value is a character
        row_change[[i]] <- paste0('<input class="new_input" type="text" id=new_',i,'><br>')
    }
    
    #converts the list values to a datatable class
    row_change <- as.data.table(row_change)
    #sets the names to the column names of the original data
    setnames(row_change,colnames(old_row))
    #adds the row to the original data
    DT <- rbind(old_row,row_change)
    rownames(DT) <- c("Current values","New values")
    DT
    
  },escape=F,options=list(dom='t',ordering=F)
  )
  
  #if new values are entered
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF <- data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF) <- colnames(vals$Data)
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickId))] <- DF
               }
  )
  
  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  ##PROGRAMPART - GRIDLINES 
  #This section controls the visualisation of the xy information of the point clicked
  
  #UI to show the xy values of the point clicked
  output$coordenate_info_ui_GL <- renderUI({
    verbatimTextOutput("coordenate_info_GL")
  })
  
  #shows the xy values of the point clicked
  #prints the coordenate information to the user
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$coordenate_info_GL <- renderText({
    
    #if the double clicked button is not NULL
    #if the double clik is NULL, i.e. no double click has been clicked
    if(is.null(dbl_clicked_reactive_GL$n))
      return()
    
    #if the plotting of line is selected
    #if points are to be shown
    if(input$line_point_GL == T){
      
      #if the double clicked is not empty
      if(!is.null(dbl_clicked_reactive_GL$n)){
        
        #if no point of the line has been previously clicked
        if(line_point_counter_GL$n == 0){
          
          
          #if the x value of the origin point has been manually selected
          if(input$gridlines_manual_origin_x != 0){
            #sets the x value from the manual input
            x = input$gridlines_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x = dbl_clicked_reactive_GL$n$x
          }
          
          #if the y value of the origin point has been manually selected
          if(input$gridlines_manual_origin_y != 0){
            #sets the y value from the manual input
            y = input$gridlines_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y = dbl_clicked_reactive_GL$n$y
          }
          
          #prints the xy values of the point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
        }else if(line_point_counter_GL$n == 1){
          #if one point of the line has been previously clicked
          
          #if the x value of the origin point has been manually selected
          if(input$gridlines_manual_origin_x != 0){
            #sets the x value from the manual input
            x = input$gridlines_manual_origin_x
          }else{
            #if the x value of the origin point is created from clicking
            #sets the x value from the clicking
            x = line_fill_cntr_GL$x1
          }
          
          #if the y value of the origin point has been manually selected
          if(input$gridlines_manual_origin_y != 0){
            #sets the y value from the manual input
            y = input$gridlines_manual_origin_y
          }else{
            #if the y value of the origin point is created from clicking
            #sets the y value from the clicking
            y = line_fill_cntr_GL$y1
          }
          
          #prints the xy values of the origin point
          print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1)))
          
          #plot second point and line
          ##if the second point is not the same as the first point
          if(first_point$x != x & first_point$y != y){
            #if the x value of the origin point has been manually selected
            if(input$gridlines_manual_secondPoint_x != 0){
              #sets the x value from the manual input
              x2 = input$gridlines_manual_secondPoint_x
            }else{
              #if the x value of the origin point is created from clicking
              #sets the x value from the clicking
              x2 = first_point_GL$x
            }
            
            #if the y value of the origin point has been manually selected
            if(input$gridlines_manual_secondPoint_y != 0){
              #sets the y value from the manual input
              y2 = input$gridlines_manual_secondPoint_y
            }else{
              #if the y value of the origin point is created from clicking
              #sets the y value from the clicking
              y2 = first_point_GL$y
            }
            
            #prints the xy values of the second point
            print(paste0('origin: ', format(round(x, 1), nsmall = 1), ',', format(round(y, 1), nsmall = 1), 
                         ' - Second Point: ', format(round(x2, 1), nsmall = 1), ',', format(round(y2, 1), nsmall = 1)))
          }
          
        }
        
      }
      
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$speaker_GL = renderUI({
    if (is.null(importFiles))
      return()
    #file
    file = importFiles()
    file = file[[2]]
    
    spkrs = unique(file$speaker)
    selectInput('spkr_GL', 'Speaker', spkrs)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$segment_GL = renderUI({
    if (is.null(input$spkr_GL))
      return()
    #file
    file = importFiles()
    file = file[[2]]
    file = file[file$segment != "pal",]
    #speaker
    sp = input$spkr_GL
    #segments
    segs = unique(file[file$speaker == sp, 'segment'])
    
    selectInput('sgmnt_GL', label = 'Segment', c('All', segs), selected = segs[1], multiple = T)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  plotStatic_data_GL <- reactive({
    
    if (is.null(input$sgmnt_GL))
      return()
    
    frm_nmbr_per_contour = 100
    file = importFiles()
    file = file[[1]]
    d = file
    
    sp = input$spkr_GL
    
    ms = as.character(input$radio_measures_GL)
    commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
    #Measurement*********************************************
    if (ms == 1){
      #measurements in millimiters
      #d <- subset(d, select = -c(pixel))
      d <- d[c(commonNamesInFile, 'mm')]
      measure_val <- "mm"
    }else if (ms == 2){
      #measurements in pixels
      #d <- subset(d, select = -c(mm))
      d <- d[c(commonNamesInFile, 'pixel')]
      measure_val <- "pixel"
    }
    
    #SPEAKERS*********************************************
    fl = d[d$speaker %in% sp, ]
    
    #Sets extrema
    x_xtrm = subset(fl, coord == "x", select = measure_val)
    x_xtrm = unique(x_xtrm[[measure_val]])
    x_xtrm_min = min(x_xtrm) - 2
    x_xtrm_max = max(x_xtrm) + 2
    
    y_xtrm = subset(fl, coord == "y", select = measure_val)
    y_xtrm = unique(y_xtrm[[measure_val]])
    y_xtrm_min = min(y_xtrm) - 2
    y_xtrm_max = max(y_xtrm) + 2
    
    extrema = c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
    
    #Palate*********************************************
    if(input$palate_plot_GL == T)
    {
      pal_fl = subset(fl, segment == "pal")
      
      pal_fl_x = subset(pal_fl, coord == "x", select = measure_val)
      pal_fl_x = pal_fl_x[,1]
      pal_fl_x = as.numeric(pal_fl_x)
      pal_fl_x = approx(pal_fl_x, n = frm_nmbr_per_contour)
      pal_fl_x = pal_fl_x$y
      
      pal_fl_y = subset(pal_fl, coord == "y", select = measure_val)
      pal_fl_y = pal_fl_y[,1]
      pal_fl_y = as.numeric(pal_fl_y)
      pal_fl_y = approx(pal_fl_y, n = frm_nmbr_per_contour)
      pal_fl_y = pal_fl_y$y
      
      tmp_seg_pal = rep("palate", length(pal_fl_x))
      tmp_rep_pal = rep(1, length(pal_fl_x))
      tmp_frm_pal = rep(1, length(pal_fl_x))
      tmp_pnt_pal = 1:length(pal_fl_x)
      
      pal_vals = data.frame(tmp_seg_pal, tmp_rep_pal, tmp_frm_pal, tmp_pnt_pal, pal_fl_x, pal_fl_y)
      names(pal_vals) = c("segment", "repetition", "frame", "point", "x", "y")
      
    }else{
      pal_vals = data.frame(matrix(nrow = 0, ncol = 0))
    }
    
    fl =fl[fl$speaker == sp & fl$segment != 'pal',]
    
    #SEGMENTS*********************************************
    if(input$sgmnt_GL != 'All'){
      seg = input$sgmnt_GL
      fl = fl[fl$segment %in% seg, ]
    }
    
    prep_plot_df = data.frame(matrix(ncol = 6, nrow = 0))
    names(prep_plot_df) = c("segment", "repetition", "frame", "point", "x", "y")
    
    tmp_df = fl[fl$coord == 'x',]
    colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
    tmp_df$y = fl[fl$coord == 'y', ncol(tmp_df)]
    tmp_df = tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
    
    return(list(tmp_df, extrema, pal_vals))
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #stores the largest distane from the point of origin and the intersection for each line
  store_larger_distance = reactiveValues(n= 0)
  #stores all the data frame information
  grid_lines_data_frame = reactiveValues(d = NULL)
  grid_lines_data_frame2 = reactiveValues(d = NULL)
  
  #plots the gridlines
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$plotStatic_GL = renderPlot({
    fl = plotStatic_data_GL()
    extrema = fl[[2]]
    pal = fl[[3]]
    fl = fl[[1]]
    
    ttl = 'get_plot_title()'
    
    pal_colour = input$palate_colour
    pal_line_type = as.numeric(input$line_type_palate)
    tongue_line_type = as.numeric(input$line_type_tongue)
    
    segs = unique(fl$segment)
    segs_nmbr = length(segs)
    
    pallete_colour = c()
    
    #creates basic plot
    #....................................................................................
    #This section creates the basic plot
    #....................................................................................
    #creates the basic plot
    p <- ggplot() + theme_bw() +
      theme(title = element_text(family = input$font_type, colour = input$text_color, 
                                 size = input$title_size),
            axis.title.x = element_text(family = input$font_type, colour = input$text_color, 
                                        size = input$axis_size),
            axis.title.y = element_text(family = input$font_type, colour = input$text_color, 
                                        size = input$axis_size),
            legend.title = element_text(family = input$font_type, colour = input$text_color, 
                                        size = input$legend_size),
            axis.text = element_text(size = input$ticks_size))
    
    #if the contour is smoothed
    #---------------------------------------------------------------------------------------------
    if(input$smooth_contour_GL == T){
      #if all segments are plotted
      if(input$sgmnt_GL == 'All'){
        
        p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                          group = interaction(segment, repetition, frame), 
                                          color = segment),
                           stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                           se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
      }else{
        #if NOT all segments are plotted
        if(segs_nmbr == 1){
          #if only one segment is plotted
          p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                            group = interaction(repetition, frame), 
                                            color = frame),
                             stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                             se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
        }else{
          #if multiple segments are plotted
          p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                            group = interaction(segment, repetition, frame), 
                                            color = segment),
                             stat="smooth", method = "auto", alpha = input$tongue_alpha_slider,
                             se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
        }
      }
    }else{
      #if the contour is NOT smoothed
      #if all segments are plotted
      if(input$sgmnt_GL == 'All'){
        
        p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                          group = interaction(segment, repetition, frame), 
                                          color = segment),
                           size = input$tongue_width_slider, alpha = input$tongue_alpha_slider)
      }else{
        #if NOT all segments are plotted
        if(segs_nmbr == 1){
          #if only one segment is plotted
          p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                            group = interaction(repetition, frame), color = frame),
                             size = input$tongue_width_slider, alpha = input$tongue_alpha_slider)
        }else{
          #if multiple segments are plotted
          p <- p + geom_line(data = fl, aes(x = x, y = y, 
                                            group = interaction(segment, repetition, frame), 
                                            color = segment),
                             size = input$tongue_width_slider, alpha = input$tongue_alpha_slider)
        }
      }
    }
    
    #plot palate trace
    #------------------------------------------------------------------------------------------
    if(input$palate_plot_GL== T){
      if(input$smooth_contour_GL == T){
        #if the contour is smoothed
        p <- p + geom_line(data = pal, aes(x = x, y = y),
                           stat="smooth", method = "auto",linetype = pal_line_type, 
                           alpha = input$palate_alpha_slider,
                           se = F, size = input$palate_width_slider, 
                           span = input$palate_smooth_slider, colour = pal_colour)
      }else{
        #if the palate contour is not smoothed
        p <- p + geom_line(aes(x = x, y = y), size = input$palate_width_slider, 
                           linetype = pal_line_type, 
                           alpha = input$palate_alpha_slider, colour = pal_colour, pal)
      }
    }
    
    #if the y value of the origin point is automatically set
    #set this y value as one of the limits for the y axis
    if(!is.null(origin_point_automatic_GL$y)){
      extrema[4] <- origin_point_automatic_GL$y
    }
    
    #add annotations
    if(input$line_point == T){
      
      #add origin points------------------------------------------------------------------------
      #This section adds the origin point to the plot
      #add origin point------------------------------------------------------------------------
      #if the selection of the origin point is NOT manual but NARROW or WIDE
      if(input$define_origin_GL != 'Manual'){
        #if the origin point is not NULL
        if(!is.null(origin_point_automatic_GL$x)){
          #gets the x value of the automatic origin point
          x <- origin_point_automatic_GL$x
          #gets the y value of the automatic origin point
          y <- origin_point_automatic_GL$y
          
          #plots the origin point
          p <- p + geom_point(data = data.frame(x = x, y = y),
                              aes(x = x, y = y), size = 7,
                              colour = "#3CB371", show.legend = FALSE)
        }
      }else{
        #add origin point------------------------------------------------------------------------
        #if the selection of the origin point is manual and not NARROW or WIDE
        #manual origin input
        #if the double click values are not NULL or if the origin point values are not NULL
        if(!is.null(dbl_clicked_reactive_GL$n) || !is.null(origin_point_GL$x)){
          #if this is the first time the double click has been clicked
          if(line_point_counter_GL$n == 0){
            
            #if the gridlines manual origin has already been established
            if(input$gridlines_manual_origin_x != 0){
              x <- input$gridlines_manual_origin_x
            }else{
              #if the origin has not been manually established
              x <- dbl_clicked_reactive_GL$n$x
            }
            
            #if the gridlines manual origin has already been established
            if(input$gridlines_manual_origin_y != 0){
              y <- input$gridlines_manual_origin_y
            }else{
              #if the origin has not been manually established
              y <- dbl_clicked_reactive_GL$n$y
            }
            
            #plots the origin point
            p <- p + geom_point(data = data.frame(x = x, y = y),
                                aes(x = x, y = y), size = 7,
                                colour = "#FF6A6A", show.legend = FALSE)
          }else if(line_point_counter_GL$n == 1){
            #final defined point
            
            #if the gridlines manual origin has already been established
            if(input$gridlines_manual_origin_x != 0){
              x <- input$gridlines_manual_origin_x
            }else{
              #if the origin has not been manually established
              x <- origin_point_GL$x
            }
            
            #if the gridlines manual origin has already been established
            if(input$gridlines_manual_origin_y != 0){
              y <- input$gridlines_manual_origin_y
            }else{
              #if the origin has not been manually established
              y <- origin_point_GL$y
            }
            
            #plots the origin point
            p <- p + geom_point(data = data.frame(x = x, y = y),
                                aes(x = x, y = y), size = 7,
                                colour = "#3CB371", show.legend = FALSE)
          }
        }
      }
      
      #add fieldview points------------------------------------------------------------------------
      #if the manual origin point is not NULL OR the automatic origin point is not NULL
      if(!is.null(origin_point_GL$x) || !is.null(origin_point_automatic_GL$x)){
        #if the selection of the fieldview is NOT manual
        if(input$define_fieldview_GL != 'Manual'){
          #angle defined fieldview
          
          #checks whether the palate trace is included in the plotting
          if(input$include_palate_GL){
            all_contours <- rbind(fl, pal)
          }else{
            all_contours <- fl
          }
          
          tmp_line_origin_x <- round(x, digits = 2)
          tmp_line_origin_y <- round(y, digits = 2)
          tmp_default_distance <- 1000
          
          tmp_origin_point_x = round(origin_point_automatic_GL$x, digits = 2)
          tmp_origin_point_y = round(origin_point_automatic_GL$y, digits = 2)
          
          #calculate left line=================================================================
          #if the left line is defined by an angle
          #--------------------------------------------------------------------------------------
          if(input$define_fieldview_GL != 'Angle'){
            #create label_plotting_df
            label_plotting_df_GL_names <- c('segment', 'left_x', 'left_y', 'left_label', 
                                            'right_x', 'right_y', 'right_label', 'left_angle', 'right_angle', 'distance')
            label_plotting_df_GL = data.frame(matrix(ncol = length(label_plotting_df_GL_names), nrow = 1))
            names(label_plotting_df_GL) = label_plotting_df_GL_names
            
            #calculate left line===============================================================================
            #This section calculates the left line of the fieldview
            #within server_plotstatic
            
            #calculate left line===========================================================================
            #initial common angle
            tmp_initial_angle <- 179
            #final common angle
            tmp_final_angle <- 91
            #sequence from inital to final angle
            tmp_final_angle_all <- seq(tmp_initial_angle, tmp_final_angle)
            #length of angles from initial to final
            tmp_fieldview_angles <- length(tmp_final_angle_all)
            #angle counter
            tmp_final_angle_all_cntr <- 1
            
            #while counter to check the number of iterated contours
            complete_intersections <- 0
            
            #checks whether the angle is defined by a Narrow or a Wide setting
            if(input$define_fieldview_GL == 'Narrow'){
              #if the angle is defined as narrow, 
              #the algorithm takes into account ONLY the internal common areas
              tmp_total_iterated_contours <- nrow(all_contours[all_contours$point == 1,])
            }else if(input$define_fieldview_GL == 'Wide'){
              #if the angle is defined as wide
              #the algorithm takes into account all areas
              tmp_total_iterated_contours <- 1
            }
            
            #Finds the left angle.............................................................................
            withProgress(message = 'Finding left angle', value = 0, {
              # Number of times we'll go through the loop
              n <- tmp_fieldview_angles
              
              #finds the interaction in all lines for all segments
              while(complete_intersections < tmp_total_iterated_contours){
                #creates a temporary line from angle i
                tmp_leftline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                       tmp_default_distance, 
                                                       tmp_final_angle_all[tmp_final_angle_all_cntr]*-1)
                tmp_leftline_end_x <- tmp_leftline_from_function[1]
                tmp_leftline_end_y <- tmp_leftline_from_function[2]
                
                #find angles
                tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                        c(tmp_leftline_end_x, tmp_leftline_end_y))
                tmp_angle_left_first <- tmp_angle[[1]]
                tmp_angle_right_first <- tmp_angle[[2]]
                
                tmp_final_angle_all_cntr <- tmp_final_angle_all_cntr + 1
                
                #find itersections
                #create label_plotting_df
                tmp_label_plotting_df_GL_names <- c('angle', 'segment', 'repetition', 'frame', 
                                                    'left_x', 'left_y', 'left_label', 'distance')
                tmp_label_plotting_df_GL <- data.frame(matrix(ncol = length(tmp_label_plotting_df_GL_names), 
                                                              nrow = 0))
                names(tmp_label_plotting_df_GL) <- tmp_label_plotting_df_GL_names
                
                #iteration per segment
                for(segmenti in unique(all_contours$segment)){
                  tmp_df_intersection_segment <- all_contours[all_contours$segment == segmenti,]
                  for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                    tmp_df_intersection_repetition <- 
                      tmp_df_intersection_segment[tmp_df_intersection_segment$repetition == repetitioni,]
                    for(framei in unique(tmp_df_intersection_repetition$frame)){
                      tmp_df_intersection_i <- tmp_df_intersection_repetition[tmp_df_intersection_repetition$frame == framei,]
                      
                      iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                  tmp_df_intersection_i$y))), 
                                                                  1)))
                      
                      if(!is.null(tmp_leftline_from_function)){
                        tmp_df <- data.frame(segment = segmenti)
                        
                        if(is.na(tmp_leftline_end_x)){
                          seadfare <- 1
                        }
                        #View(tmp_label_plotting_df_GL)
                        #left angle----------------------------------------------------------------------
                        basis_line_left <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, 
                                                                                     tmp_leftline_end_x),
                                                                                   c(tmp_line_origin_y, 
                                                                                     tmp_leftline_end_y)))), 1)))
                        
                        #if the temporal line intersects with the tongue contour
                        if(!is.null(gIntersection(iterated_contour, basis_line_left)$x)){
                          
                          int_x <- round(gIntersection(iterated_contour, basis_line_left)$x, digits = 2)
                          int_y <- round(gIntersection(iterated_contour, basis_line_left)$y, digits = 2)
                          
                          # if(int_x != 0 && int_x < 50){
                          #   print(paste0(int_x, '_', int_y))
                          # }
                          
                          tmp_df$angle <- tmp_final_angle_all[tmp_final_angle_all_cntr]
                          tmp_df$segment <- segmenti
                          tmp_df$repetition <- repetitioni
                          tmp_df$frame <- framei
                          tmp_df$left_x <-  int_x
                          tmp_df$left_y <-  int_y
                          
                          #new editing
                          tmpInternalDistance <- sqrt((tmp_line_origin_x-int_x)^2+(tmp_line_origin_y-int_y)^2)
                          
                          #left_int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                          left_int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                          
                          tmp_df$left_label <- left_int_label
                          
                          #store the largest distance
                          tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                          
                          tmp_df$distance <- store_larger_distance$n
                          
                          if(tmp_store_distance > store_larger_distance$n){
                            
                            store_larger_distance$n <- tmp_store_distance
                          }
                        }else{
                          #if the temporal line does not intersect with the tongue contour
                          int_x <- 0
                          int_y <- 0
                          
                          # if(int_x != 0 && int_x < 50){
                          #   print(paste0(int_x, '_', int_y))
                          # }
                          
                          tmp_df$angle <- tmp_final_angle_all[tmp_final_angle_all_cntr]
                          tmp_df$segment <- segmenti
                          tmp_df$repetition <- repetitioni
                          tmp_df$frame <- framei
                          tmp_df$left_x <-  int_x
                          tmp_df$left_y <-  int_y
                          
                          left_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                          
                          tmp_df$left_label <- left_int_label
                          
                          #store the largest distance
                          #tmp_store_distance = sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                          tmp_store_distance <- 0
                          
                          tmp_df$distance <- store_larger_distance$n
                        }
                        
                        tmp_label_plotting_df_GL <- rbind(tmp_label_plotting_df_GL, tmp_df)
                        
                      }
                      
                    }
                  }
                }
                
                complete_intersections <- nrow(tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$left_x != 0,])
                incProgress(1/n, detail = paste("PLEASE WAIT"))
              }
              
            })
            
            tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$left_x != 0,]
            tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$left_x ==
                                                                   min(tmp_label_plotting_df_GL$left_x),]
            tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL[1,]
            
            label_plotting_df_GL$segment <- 'multiple'
            label_plotting_df_GL$left_x <- tmp_label_plotting_df_GL$left_x
            label_plotting_df_GL$left_y <- tmp_label_plotting_df_GL$left_y
            
            
            label_plotting_df_GL$left_label <- paste0('x=', round(tmp_label_plotting_df_GL$left_x), ', ', 
                                                      'y=', round(tmp_label_plotting_df_GL$left_y))
            label_plotting_df_GL$left_angle <- tmp_label_plotting_df_GL$angle
            label_plotting_df_GL$distance <- tmp_label_plotting_df_GL$distance 
            
            #calculate right line==============================================================================
            #This section calculates the left line of the fieldview
            #within server_plotstatic
            
            #calculate right line=================================================================================
            tmp_initial_angle <- 1
            tmp_final_angle <- 89
            tmp_final_angle_all <- seq(tmp_initial_angle, tmp_final_angle)
            tmp_fieldview_angles <- length(tmp_final_angle_all)
            tmp_final_angle_all_cntr <- 1
            
            complete_intersections <- 0
            
            if(input$define_fieldview_GL == 'Narrow'){
              tmp_total_iterated_contours <- nrow(all_contours[all_contours$point == 1,])
            }else if(input$define_fieldview_GL == 'Wide'){
              tmp_total_iterated_contours <- 1
            }
            
            withProgress(message = 'Finding right angle', value = 0, {
              # Number of times we'll go through the loop
              n <- tmp_fieldview_angles
              
              while(complete_intersections < tmp_total_iterated_contours){
                tmp_rightline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                        tmp_default_distance, 
                                                        tmp_final_angle_all[tmp_final_angle_all_cntr]*-1)
                tmp_rightline_end_x <- tmp_rightline_from_function[1]
                tmp_rightline_end_y <- tmp_rightline_from_function[2]
                
                #find angles
                tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                        c(tmp_rightline_end_x, tmp_rightline_end_y))
                tmp_angle_left_second <- tmp_angle[[1]]
                tmp_angle_right_second <- tmp_angle[[2]]
                
                tmp_final_angle_all_cntr <- tmp_final_angle_all_cntr + 1
                
                #find itersections
                #create label_plotting_df
                tmp_label_plotting_df_GL_names <- c('angle', 'segment', 'repetition', 'frame', 'right_x', 'right_y')
                tmp_label_plotting_df_GL <- data.frame(matrix(ncol = length(tmp_label_plotting_df_GL_names), nrow = 0))
                names(tmp_label_plotting_df_GL) <- tmp_label_plotting_df_GL_names
                
                #iteration per segment
                for(segmenti in unique(all_contours$segment)){
                  tmp_df_intersection_segment <- all_contours[all_contours$segment == segmenti,]
                  for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                    tmp_df_intersection_repetition <- 
                      tmp_df_intersection_segment[tmp_df_intersection_segment$repetition == repetitioni,]
                    for(framei in unique(tmp_df_intersection_repetition$frame)){
                      tmp_df_intersection_i <- tmp_df_intersection_repetition[tmp_df_intersection_repetition$frame == framei,]
                      
                      iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                  tmp_df_intersection_i$y))), 1)))
                      
                      if(!is.null(tmp_rightline_from_function)){
                        tmp_df <- data.frame(segment = segmenti)
                        
                        if(is.na(tmp_rightline_end_x)){
                          seadfare <- 1
                        }
                        #View(tmp_label_plotting_df_GL)
                        #right angle---------------------------------------------------------------------------------
                        basis_line_right <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, 
                                                                                      tmp_rightline_end_x),
                                                                                    c(tmp_line_origin_y, 
                                                                                      tmp_rightline_end_y)))), 1)))
                        
                        if(!is.null(gIntersection(iterated_contour, basis_line_right)$x)){
                          
                          int_x <- round(gIntersection(iterated_contour, basis_line_right)$x, digits = 2)
                          int_y <- round(gIntersection(iterated_contour, basis_line_right)$y, digits = 2)
                          
                          # if(int_x == 133.57 & int_y == 63.42){
                          #   browser()
                          # }
                          
                          tmp_df$angle <- tmp_final_angle_all[tmp_final_angle_all_cntr]
                          tmp_df$segment <- segmenti
                          tmp_df$repetition <- repetitioni
                          tmp_df$frame <- framei
                          tmp_df$right_x <- int_x
                          tmp_df$right_y <- int_y
                          
                          right_int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                          
                          tmp_df$right_label <- right_int_label
                          
                          #store the largest distance
                          tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                          
                          tmp_df$distance <- store_larger_distance$n
                          
                          if(tmp_store_distance > store_larger_distance$n){
                            store_larger_distance$n <- tmp_store_distance
                          }
                        }else{
                          int_x <- 0
                          int_y <- 0
                          
                          tmp_df$angle <- tmp_final_angle_all[tmp_final_angle_all_cntr]
                          tmp_df$segment <- segmenti
                          tmp_df$repetition <- repetitioni
                          tmp_df$frame <- framei
                          tmp_df$right_x <- int_x
                          tmp_df$right_y <- int_y
                          
                          right_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                          
                          tmp_df$right_label <- right_int_label
                          
                          #store the largest distance
                          #tmp_store_distance = sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                          tmp_store_distance <- 0
                          
                          tmp_df$distance <- store_larger_distance$n
                        }
                        
                        tmp_label_plotting_df_GL <- rbind(tmp_label_plotting_df_GL, tmp_df)
                        
                      }
                      
                    }
                  }
                }
                
                complete_intersections <- nrow(tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$right_x != 0,])
                incProgress(1/n, detail = paste("PLEASE WAIT"))
              }
              
            })
            
            tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$right_x != 0,]
            tmp_label_plotting_df_GL <- 
              tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$right_x == max(tmp_label_plotting_df_GL$right_x),]
            tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL[1,]
            
            label_plotting_df_GL$segment <- 'multiple'
            label_plotting_df_GL$right_x <- tmp_label_plotting_df_GL$right_x
            label_plotting_df_GL$right_y <- tmp_label_plotting_df_GL$right_y
            label_plotting_df_GL$right_label <- paste0('x=', round(tmp_label_plotting_df_GL$right_x), ', ', 
                                                       'y=', round(tmp_label_plotting_df_GL$right_y))
            label_plotting_df_GL$right_angle <- tmp_label_plotting_df_GL$angle
            label_plotting_df_GL$distance <- tmp_label_plotting_df_GL$distance 
            
            if(!is.null(tmp_angle_right_first) & !is.null(tmp_angle_right_second)){
              if(tmp_angle_right_first < 0){
                tmp_angle_right_first = tmp_angle_right_first * -1
              }
              
              if(tmp_angle_right_second < 0){
                tmp_angle_right_second = tmp_angle_right_second * -1
              }
              
              mid_angle = tmp_angle_right_second + ((tmp_angle_right_first - tmp_angle_right_second) / 2)
              
              #mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
            }
            
          }else{
            #calculate left right lines========================================================================
            #This section calculates both left and right line of the fieldview
            
            #calculate left right line====================================================================================
            tmp_leftline_from_function <- NULL
            #if the left angle input is not NULL
            if(!is.null(input$leftlineangle_fieldview_GL)){
              #if the left angle input is not == 0
              if(input$leftlineangle_fieldview_GL != 0){
                tmp_leftline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                       tmp_default_distance, input$leftlineangle_fieldview_GL*-1)
                tmp_leftline_end_x <- tmp_leftline_from_function[1]
                tmp_leftline_end_y <- tmp_leftline_from_function[2]
                
                #find angles
                tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                        c(tmp_leftline_end_x, tmp_leftline_end_y))
                tmp_angle_left_first <- tmp_angle[[1]]
                tmp_angle_right_first <- tmp_angle[[2]]
                
                #store the largest distance
                tmp_store_distance <- sqrt((tmp_origin_point_x-tmp_leftline_end_x)^2+
                                             (tmp_origin_point_y-tmp_leftline_end_y)^2)
                if(tmp_store_distance > store_larger_distance$n){
                  browser()
                  store_larger_distance$n <- tmp_store_distance
                }
              }
            }
            
            #calculate right line=========================================================================================
            tmp_rightline_from_function <- NULL
            
            if(!is.null(input$rightlineangle_fieldview_GL)){
              if(input$rightlineangle_fieldview_GL != 0){
                tmp_rightline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                        tmp_default_distance, input$rightlineangle_fieldview_GL*-1)
                tmp_rightline_end_x <- tmp_rightline_from_function[1]
                tmp_rightline_end_y <- tmp_rightline_from_function[2]
                
                #find angles
                tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                        c(tmp_rightline_end_x, tmp_rightline_end_y))
                tmp_angle_left_second <- tmp_angle[[1]]
                tmp_angle_right_second <- tmp_angle[[2]]
                
                #store the largest distance
                tmp_store_distance <- sqrt((tmp_origin_point_x-tmp_rightline_end_x)^2+
                                             (tmp_origin_point_y-tmp_rightline_end_y)^2)
                if(tmp_store_distance > store_larger_distance$n){
                  store_larger_distance$n <- tmp_store_distance
                }
              }
            }
            
            #====================================================================================
            
            if(!is.null(tmp_angle_right_first) & !is.null(tmp_angle_right_second)){
              if(tmp_angle_right_first < 0){
                tmp_angle_right_first <- tmp_angle_right_first * -1
              }
              
              if(tmp_angle_right_second < 0){
                tmp_angle_right_second <- tmp_angle_right_second * -1
              }
              
              #mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
              mid_angle <- tmp_angle_right_second + ((tmp_angle_right_first - tmp_angle_right_second) / 2)
            }
            
            #find itersections
            #create label_plotting_df
            label_plotting_df_GL_names <- c('segment', 'left_x', 'left_y', 'left_label', 
                                            'right_x', 'right_y', 'right_label', 'left_angle', 'right_angle', 'distance')
            label_plotting_df_GL <- data.frame(matrix(ncol = length(label_plotting_df_GL_names), nrow = 0))
            names(label_plotting_df_GL) <- label_plotting_df_GL_names
            
            #iteration per segment
            for(segmenti in unique(all_contours$segment)){
              tmp_df_intersection <- all_contours[all_contours$segment == segmenti,]
              #iteration per repetition
              for(repetitioni in unique(tmp_df_intersection$repetition)){
                tmp_df_intersection <- tmp_df_intersection[tmp_df_intersection$repetition == repetitioni,]
                #iteration per frame
                for(framei in unique(tmp_df_intersection$frame)){
                  
                  tmp_df_intersection_i <- tmp_df_intersection[tmp_df_intersection$frame == framei,]
                  
                  iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                              tmp_df_intersection_i$y))), 1)))
                  
                  if(!is.null(tmp_leftline_from_function) & !is.null(tmp_rightline_from_function)){
                    tmp_df <- data.frame(segment = segmenti)
                    
                    #left angle-----------------------------------------------------------------------------------------
                    basis_line_left <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, tmp_leftline_end_x), 
                                                                               c(tmp_line_origin_y, tmp_leftline_end_y)))), 1)))
                    
                    if(!is.null(gIntersection(iterated_contour, basis_line_left)$x)){
                      
                      int_x <- round(gIntersection(iterated_contour, basis_line_left)$x, digits = 2)
                      int_y <- round(gIntersection(iterated_contour, basis_line_left)$y, digits = 2)
                      
                      left_int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                      
                      tmp_df$left_x <- int_x
                      tmp_df$left_y <- int_y
                      tmp_df$left_label <- left_int_label
                      
                      #store the largest distance
                      tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                      if(tmp_store_distance > store_larger_distance$n){
                        store_larger_distance$n <- tmp_store_distance
                      }
                    }else{
                      int_x <- 0
                      int_y <- 0
                      left_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                      
                      tmp_df$left_x <- int_x
                      tmp_df$left_y <- int_y
                      tmp_df$left_label <- left_int_label
                      
                      #store the largest distance
                      tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                      if(tmp_store_distance > store_larger_distance$n){
                        store_larger_distance$n <- tmp_store_distance
                      }
                    }
                    
                    
                    #right angle-----------------------------------------------------------------------------------------
                    basis_line_right <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, tmp_rightline_end_x), 
                                                                                c(tmp_line_origin_y, tmp_rightline_end_y)))), 1)))
                    
                    if(!is.null(gIntersection(iterated_contour, basis_line_right)$x)){
                      
                      int_x <- gIntersection(iterated_contour, basis_line_right)$x
                      int_y <- gIntersection(iterated_contour, basis_line_right)$y
                      
                      right_int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                      
                      tmp_df$right_x <- int_x
                      tmp_df$right_y <- int_y
                      tmp_df$right_label <- right_int_label
                      
                      #store the largest distance
                      tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                      if(tmp_store_distance > store_larger_distance$n){
                        store_larger_distance$n <- tmp_store_distance
                      }
                    }else{
                      int_x <- 0
                      int_y <- 0
                      right_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                      
                      tmp_df$right_x <- int_x
                      tmp_df$right_y <- int_y
                      tmp_df$right_label <- right_int_label
                      
                      #store the largest distance
                      tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                      if(tmp_store_distance > store_larger_distance$n){
                        store_larger_distance$n <- tmp_store_distance
                      }
                    }
                    
                    label_plotting_df_GL <- rbind(label_plotting_df_GL , tmp_df)
                    
                  }
                  
                }
              }
            }
          }
          
          #plot non manual lines================================================================================
          #This section plots automatic lines of the fieldview
          
          #View(label_plotting_df_GL)
          if((length(unique(label_plotting_df_GL$left_x)) == 1 & unique(label_plotting_df_GL$left_x) == 0) && 
             (length(unique(label_plotting_df_GL$right_x)) == 1 & unique(label_plotting_df_GL$right_x) == 0)){
            showNotification("No intersection with the specified angles")
          }else{
            if(length(unique(label_plotting_df_GL$left_x)) == 1 & unique(label_plotting_df_GL$left_x) == 0){
              showNotification("No intersection with the specified LEFT angle")
            }else if(length(unique(label_plotting_df_GL$right_x)) == 1 & unique(label_plotting_df_GL$right_x) == 0){
              showNotification("No intersection with the specified RIGHT angle")
            }else{
              showNotification("READY")
              
              #set text xy
              if(input$radio_measures_GL == 1){
                #millimiters
                symbol_sep_measure = 5
              }else if(input$radio_measures_GL == 2){
                #pixels
                symbol_sep_measure = 20
              }
              
              #calculate the most extreme point
              #left angle
              label_plotting_df_GL_tmp <- label_plotting_df_GL[label_plotting_df_GL$left_x != 0,]
              min_x_left_angle <- label_plotting_df_GL_tmp[grepl(min(label_plotting_df_GL_tmp$left_x), 
                                                                 label_plotting_df_GL_tmp$left_x),]
              if(nrow(min_x_left_angle) > 1){
                min_x_left_angle <- min_x_left_angle[1,]
              }
              
              #right angle
              label_plotting_df_GL_tmp <- label_plotting_df_GL[label_plotting_df_GL$right_x != 0,]
              max_x_right_angle <- label_plotting_df_GL_tmp[grepl(max(label_plotting_df_GL_tmp$right_x), 
                                                                  label_plotting_df_GL_tmp$right_x),]
              if(nrow(max_x_right_angle) > 1){
                max_x_right_angle <- max_x_right_angle[1,]
              }
              
              #plot left line===============================================================
              angle_leftLine_annotation <- paste0('L: ', abs(180 - label_plotting_df_GL_tmp$left_angle), 
                                                  '°, R: ', label_plotting_df_GL_tmp$left_angle, '°')
              
              # angle_leftLine_annotation <- paste0('L: ', abs(180 - input$leftlineangle_fieldview_GL), 
              #                                     '°, R: ', input$leftlineangle_fieldview_GL, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_line_origin_x, min_x_left_angle$left_x), 
                                                  y = c(tmp_line_origin_y, min_x_left_angle$left_y)),
                                aes(x = x, y = y),
                                colour = "black", show.legend = FALSE) + 
                annotate("text", x=tmp_line_origin_x-(symbol_sep_measure*0.5), 
                         y=tmp_line_origin_y, label = angle_leftLine_annotation, hjust = 1)
              #------------------------------------------------------------------------------------------------
              
              
              #add point
              p = p + geom_point(data = data.frame(x = min_x_left_angle$left_x, 
                                                   y = min_x_left_angle$left_y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "black", show.legend = FALSE)
              
              
              #plot right line===============================================================
              angle_rightLine_annotation <- paste0('L: ', abs(180 - label_plotting_df_GL_tmp$right_angle), 
                                                   '°, R: ', label_plotting_df_GL_tmp$right_angle, '°')
              
              # angle_rightLine_annotation <- paste0('L: ', abs(180 - input$rightlineangle_fieldview_GL), 
              #                                      '°, R: ', input$rightlineangle_fieldview_GL, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_line_origin_x, max_x_right_angle$right_x), 
                                                  y = c(tmp_line_origin_y, max_x_right_angle$right_y)),
                                aes(x = x, y = y),
                                colour = "black", show.legend = FALSE) + 
                annotate("text", x=tmp_line_origin_x+(symbol_sep_measure*0.5), 
                         y=tmp_line_origin_y, label = angle_rightLine_annotation, hjust = 0)
              #---------------------------------------------------------------------------------------------------
              
              
              #add point
              p = p + geom_point(data = data.frame(x = max_x_right_angle$right_x, 
                                                   y = max_x_right_angle$right_y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "black", show.legend = FALSE)
              
            }
            
          } 
          
        }else{
          #if the selection of the origin point is manual
          #plots the first temporal fieldview point
          if(!is.null(dbl_clicked_reactive_GL$n)){
            
            if(input$define_origin_GL == 'Manual'){
              if(input$gridlines_manual_origin_x != 0){
                tmp_origin_point_x = round(input$gridlines_manual_origin_x, digits = 2)
              }else{
                tmp_origin_point_x = round(origin_point_GL$x, digits = 2)
              }
              
              if(input$gridlines_manual_origin_y != 0){
                tmp_origin_point_y = round(input$gridlines_manual_origin_y, digits = 2)
              }else{
                tmp_origin_point_y = round(origin_point_GL$y, digits = 2)
              }
              
            }else{
              
              tmp_origin_point_x = round(origin_point_automatic_GL$x, digits = 2)
              tmp_origin_point_y = round(origin_point_automatic_GL$y, digits = 2)
              
            }
            
            if(fieldview_points_cntr$n == 0){
              
              if(input$gridlines_manual_firstPoint_x != 0){
                x = round(input$gridlines_manual_firstPoint_x, digits = 2)
              }else{
                x = round(dbl_clicked_reactive_GL$n$x, digits = 2)
              }
              
              if(input$gridlines_manual_firstPoint_y != 0){
                y = round(input$gridlines_manual_firstPoint_y, digits = 2)
              }else{
                y = round(dbl_clicked_reactive_GL$n$y, digits = 2)
              }
              
              #------------------------------------------------------------------------------------------------
              #find angles
              tmp_angle = find_angle(c(x,y), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left = round(tmp_angle[[1]])
              tmp_angle_right = round(tmp_angle[[2]])
              
              #set text xy
              if(input$radio_measures_LM == 1){
                #millimiters
                symbol_sep_measure = 5
              }else if(input$radio_measures_LM == 2){
                #pixels
                symbol_sep_measure = 20
              }
              
              #add line
              angle_firstPoint_annotation <- paste0('L: ', tmp_angle_left, '°, R: ', tmp_angle_right, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x), y = c(tmp_origin_point_y, y)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x+(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_firstPoint_annotation, hjust = 0)
              #-----------------------------------------------------------------------------------------------
              
              #add point
              p = p + geom_point(data = data.frame(x = x, y = y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "#FF6A6A", show.legend = FALSE)
              
            }else if(fieldview_points_cntr$n == 1){
              
              if(input$gridlines_manual_secondPoint_x != 0){
                x = round(input$gridlines_manual_secondPoint_x, digits = 2)
              }else{
                x = round(dbl_clicked_reactive_GL$n$x, digits = 2)
              }
              
              if(input$gridlines_manual_secondPoint_y != 0){
                y = round(input$gridlines_manual_secondPoint_y, digits = 2)
              }else{
                y = round(dbl_clicked_reactive_GL$n$y, digits = 2)
              }
              
              #plots the first fieldview point and the temporal second fieldview
              if(input$gridlines_manual_firstPoint_x != 0){
                x1 = round(input$gridlines_manual_firstPoint_x, digits = 2)
              }else{
                x1 = round(fieldview_point_GL$x1, digits = 2)
              }
              
              if(input$gridlines_manual_firstPoint_y != 0){
                y1 = round(input$gridlines_manual_firstPoint_y, digits = 2)
              }else{
                y1 = round(fieldview_point_GL$y1, digits = 2)
              }
              #------------------------------------------------------------------------------------------------
              #find angles
              tmp_angle_first = find_angle(c(x1,y1), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_first = round(tmp_angle_first[[1]])
              tmp_angle_right_first = round(tmp_angle_first[[2]])
              
              #set text xy
              if(input$radio_measures_LM == 1){
                #millimiters
                symbol_sep_measure = 5
              }else if(input$radio_measures_LM == 2){
                #pixels
                symbol_sep_measure = 20
              }
              
              #add first line
              angle_firstPoint_annotation <- paste0('L: ', tmp_angle_left_first, 
                                                    '°, R: ', tmp_angle_right_first, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x1), y = c(tmp_origin_point_y, y1)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x+(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_firstPoint_annotation, hjust = 0)
              #================================================================================================
              #find angles
              tmp_angle_second = find_angle(c(x,y), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_second = round(tmp_angle_second[[1]])
              tmp_angle_right_second = round(tmp_angle_second[[2]])
              
              #add second line
              angle_secondPoint_annotation <- paste0('L: ', tmp_angle_left_second, 
                                                     '°, R: ', tmp_angle_right_second, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x), y = c(tmp_origin_point_y, y)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x-(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_secondPoint_annotation, hjust = 1)
              
              #................................................................................................
              #plot the middle angle
              mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
              
              p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                               label= paste0('M: ', mid_angle, '°'))
              #------------------------------------------------------------------------------------------------
              #plot the first point
              p = p + geom_point(data = data.frame(x = x1, y = y1),
                                 aes(x = x1, y = y1), size = 7,
                                 colour = "#3CB371", show.legend = FALSE)
              #plot the second point
              p = p + geom_point(data = data.frame(x = x, y = y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "#FF6A6A", show.legend = FALSE)
            }else if(fieldview_points_cntr$n == 2){
              #plots the first fieldview point and the temporal second fieldview
              #first point
              if(input$gridlines_manual_firstPoint_x != 0){
                x1 = round(input$gridlines_manual_firstPoint_x, digits = 2)
              }else{
                x1 = round(fieldview_point_GL$x1, digits = 2)
              }
              
              if(input$gridlines_manual_firstPoint_y != 0){
                y1 = round(input$gridlines_manual_firstPoint_y, digits = 2)
              }else{
                y1 = round(fieldview_point_GL$y1, digits = 2)
              }
              
              #second point
              if(input$gridlines_manual_secondPoint_x != 0){
                x = round(input$gridlines_secondPoint_origin_x, digits = 2)
              }else{
                x = round(fieldview_point_GL$x2, digits = 2)
              }
              
              if(input$gridlines_manual_secondPoint_y != 0){
                y = round(input$gridlines_manual_secondPoint_y, digits = 2)
              }else{
                y = round(fieldview_point_GL$y2, digits = 2)
              }
              
              #------------------------------------------------------------------------------------------------
              #find angles
              tmp_angle_first = find_angle(c(x1,y1), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_first = round(tmp_angle_first[[1]])
              tmp_angle_right_first = round(tmp_angle_first[[2]])
              
              #store the largest distance
              tmp_store_distance = sqrt((tmp_origin_point_x-x1)^2+(tmp_origin_point_y-y1)^2)
              
              if(tmp_store_distance > store_larger_distance$n){
                store_larger_distance$n = tmp_store_distance
                #print(paste0('inHere_', store_larger_distance$n))
              }
              
              #set text xy
              if(input$radio_measures_LM == 1){
                #millimiters
                symbol_sep_measure = 5
              }else if(input$radio_measures_LM == 2){
                #pixels
                symbol_sep_measure = 20
              }
              
              #add first line
              angle_firstPoint_annotation <- paste0('L: ', tmp_angle_left_first, 
                                                    '°, R: ', tmp_angle_right_first, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x1), y = c(tmp_origin_point_y, y1)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x+(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_firstPoint_annotation, hjust = 0)
              #================================================================================================
              #find angles
              tmp_angle_second = find_angle(c(x,y), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_second = round(tmp_angle_second[[1]])
              tmp_angle_right_second = round(tmp_angle_second[[2]])
              
              #store the largest distance
              tmp_store_distance = sqrt((tmp_origin_point_x-x)^2+(tmp_origin_point_y-y)^2)
              if(tmp_store_distance > store_larger_distance$n){
                store_larger_distance$n = tmp_store_distance
              }
              
              #add second line
              angle_secondPoint_annotation <- paste0('L: ', tmp_angle_left_second, 
                                                     '°, R: ', tmp_angle_right_second, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x), y = c(tmp_origin_point_y, y)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x-(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_secondPoint_annotation, hjust = 1)
              
              #.................................................................................................
              #plot the middle angle
              mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
              
              p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                               label= paste0('M: ', mid_angle, '°'))
              #------------------------------------------------------------------------------------------------
              
              
              p = p + geom_point(data = data.frame(x = x1, y = y1),
                                 aes(x = x1, y = y1), size = 7,
                                 colour = "#3CB371", show.legend = FALSE)
              
              p = p + geom_point(data = data.frame(x = x, y = y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "#3CB371", show.legend = FALSE)
            }else{
              #plots the first fieldview point and the temporal second fieldview
              
              #first point
              if(input$gridlines_manual_firstPoint_x != 0){
                x1 = round(input$gridlines_manual_firstPoint_x, digits = 2)
              }else{
                x1 = round(fieldview_point_1_vals$x, digits = 2)
              }
              
              if(input$gridlines_manual_firstPoint_y != 0){
                y1 = round(input$gridlines_manual_firstPoint_y, digits = 2)
              }else{
                y1 = round(fieldview_point_1_vals$y, digits = 2)
              }
              
              #second point
              if(input$gridlines_manual_secondPoint_x != 0){
                x = round(input$gridlines_secondPoint_origin_x, digits = 2)
              }else{
                x = round(fieldview_point_2_vals$x, digits = 2)
              }
              
              if(input$gridlines_manual_secondPoint_y != 0){
                y = round(input$gridlines_manual_secondPoint_y, digits = 2)
              }else{
                y = round(fieldview_point_2_vals$y, digits = 2)
              }
              #------------------------------------------------------------------------------------------------
              #find angles
              tmp_angle_first = find_angle(c(x1,y1), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_first = round(tmp_angle_first[[1]])
              tmp_angle_right_first = round(tmp_angle_first[[2]])
              
              #set text xy
              if(input$radio_measures_LM == 1){
                #millimiters
                symbol_sep_measure = 5
              }else if(input$radio_measures_LM == 2){
                #pixels
                symbol_sep_measure = 20
              }
              
              #add first line
              angle_firstPoint_annotation <- paste0('L: ', tmp_angle_left_first, 
                                                    '°, R: ', tmp_angle_right_first, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x1), y = c(tmp_origin_point_y, y1)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x+(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_firstPoint_annotation, hjust = 0)
              #=================================================================================================
              #find angles
              tmp_angle_second = find_angle(c(x,y), c(tmp_origin_point_x,tmp_origin_point_y))
              tmp_angle_left_second = round(tmp_angle_second[[1]])
              tmp_angle_right_second = round(tmp_angle_second[[2]])
              
              #add second line
              angle_secondPoint_annotation <- paste0('L: ', tmp_angle_left_second, 
                                                     '°, R: ', tmp_angle_right_second, '°')
              
              p = p + geom_line(data = data.frame(x = c(tmp_origin_point_x, x), y = c(tmp_origin_point_y, y)),
                                aes(x = x, y = y),
                                colour = "#3CB371", show.legend = FALSE) + 
                annotate("text", x=tmp_origin_point_x-(symbol_sep_measure*0.5), y=tmp_origin_point_y, 
                         label= angle_secondPoint_annotation, hjust = 1)
              
              #..................................................................................................
              #plot the middle angle
              mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
              
              p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                               label= paste0('M: ', mid_angle, '°'))
              #--------------------------------------------------------------------------------------------------
              
              
              p = p + geom_point(data = data.frame(x = x1, y = y1),
                                 aes(x = x1, y = y1), size = 7,
                                 colour = "#3CB371", show.legend = FALSE)
              
              p = p + geom_point(data = data.frame(x = x, y = y),
                                 aes(x = x, y = y), size = 7,
                                 colour = "#3CB371", show.legend = FALSE)
            }
          }
        }
        
        
      }
      
    }
    
    #add the grid lines
    #This section adds the grid lines to the plot
    #plots the gridlines after the number of gridlines has been entered in the GRID LINES BY
    
    #if input$number_of_gridlines_input is not NULL
    if(!is.null(input$number_of_gridlines_input)){
      #if input$number_of_gridlines_radio equals NUMBER
      #The location of gridlines is absed on number of Gridlines
      if(input$number_of_gridlines_radio == 'Number'){
        #if the number of gridlines is larger than 2 and lower than 101, i.e. from 3 to 100
        if(input$number_of_gridlines_input > 2 & input$number_of_gridlines_input < 101){
          #get the angles for each gridline
          
          #gets the numbre of gridlines from the input
          number_of_lines <- input$number_of_gridlines_input
          #gets the mind_angle, the angle between the right gridline and the left gridline
          mid_angle_value <-  mid_angle
          #creates the value of the angle step between all gridlines
          step_angle <- mid_angle_value/(input$number_of_gridlines_input - 1)
          
          #creates the column names of the dataframe to store the values of the gridlines
          grid_lines_data_frame_names <- unlist(strsplit('gridLine initial_angle final_angle mid_angle line_angle x y', ' '))
          #creates dataframe to store values of the gridlines
          grid_lines_data_frame <- data.frame(matrix(nrow = 0, ncol = length(grid_lines_data_frame_names)))
          #sets the names of the dataframe
          names(grid_lines_data_frame) <- grid_lines_data_frame_names
          #creates a sequence of angle values from right to lefty angle, separated by the step angles
          #print(label_plotting_df_GL)
          angles_seq <- seq(from = label_plotting_df_GL$right_angle, to = label_plotting_df_GL$left_angle, 
                            length.out = input$number_of_gridlines_input)
          
          # angles_seq = seq(from = tmp_angle_right_first, to = tmp_angle_right_second, 
          #                  length.out = input$number_of_gridlines_input)
          
          #calculates the intersection point between each gridline and each tongue contour
          for(number_of_lines_i in 1:number_of_lines){
            
            #calculates the intersection point
            angle_by_distance_i <- lines_fn(tmp_origin_point_x, tmp_origin_point_y, 
                                            store_larger_distance$n, angles_seq[number_of_lines_i]*-1)
            
            #stores the intersection point in the gridlines dataframe
            grid_lines_data_frame[nrow(grid_lines_data_frame) + 1, ] <- c(number_of_lines_i,
                                                                          tmp_angle_right_first,
                                                                          tmp_angle_right_second,
                                                                          mid_angle_value,
                                                                          angles_seq[number_of_lines_i],
                                                                          round(angle_by_distance_i[1], digits = 2),
                                                                          round(angle_by_distance_i[2], digits = 2))
          }
          
          #adds the speaker label to thegridlines dataframe
          grid_lines_data_frame$speaker <- as.character(input$spkr_GL)
          #adds the segment label to thegridlines dataframe
          grid_lines_data_frame$segment <- as.character(input$sgmnt_GL)
          
          #print all gridlines
          for(print_lines_i in 1:nrow(grid_lines_data_frame)){
            
            p <- p + geom_line(data = data.frame(x = c(tmp_origin_point_x,
                                                       grid_lines_data_frame[print_lines_i,'x']),
                                                 y = c(tmp_origin_point_y,
                                                       grid_lines_data_frame[print_lines_i,'y'])),
                               aes(x = x, y = y),
                               colour = "#3CB371", show.legend = FALSE)
          }
          
          #saves the gridlines dataframe to the temporal dataframe
          grid_lines_data_frame$d <- grid_lines_data_frame
          
          
        }
      }
    }
    
    #creates a duplucate dataframe to store the information of the gridlines
    grid_lines_data_frame2$d <- grid_lines_data_frame$d
    
    #visualises the gridlines dataframe
    #View(grid_lines_data_frame2$d)
    
    if(!is.null(grid_lines_data_frame$d)){
      
      if(input$main_invert_y){
        p = p + scale_x_continuous(limits = c(extrema[1]-10, extrema[2]+10)) +
          scale_y_continuous(limits = c(extrema[4]+10, min(grid_lines_data_frame$d$y)-10)) +
          labs(x ='Tongue Advancement', y = 'Tongue Height')
      }else{
        p = p + scale_x_continuous(limits = c(extrema[1]-10, extrema[2]+10)) +
          scale_y_reverse(limits = c(extrema[4]+10, min(grid_lines_data_frame$d$y)-10)) +
          labs(x ='Tongue Advancement', y = 'Tongue Height')
      }
      
      
      
    }else{
      
      if(input$main_invert_y){
        p = p + scale_x_continuous(limits = c(extrema[1]-10, extrema[2]+10)) +
          scale_y_continuous(limits = c(extrema[4]+10, extrema[3]-10)) +
          labs(x ='Tongue Advancement', y = 'Tongue Height')
      }else{
        p = p + scale_x_continuous(limits = c(extrema[1]-10, extrema[2]+10)) +
          scale_y_reverse(limits = c(extrema[4]+10, extrema[3]-10)) +
          labs(x ='Tongue Advancement', y = 'Tongue Height')
      }
      
      
      
    }
    
    p
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  source(file.path("server/savePlots", "saveGridlines.R"),  local = TRUE)$values
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$GL_intersection_labels_UI = renderUI({
    if(input$line_point_GL == T){
      checkboxInput("GL_intersection_labels", label = "Labels", value = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$GL_intersection_labelsRepel_UI = renderUI({
    if(input$line_point_GL == T){
      checkboxInput("GL_intersection_labelsRepel", label = "Repel", value = T)
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #stores the double click information
  dbl_clicked_reactive_GL = reactiveValues(n = NULL)
  #counter to check whether a line is to be plotted
  line_point_counter_GL  = reactiveValues(n = 0)
  
  #stores the values of the origin point
  origin_point_GL = reactiveValues(x = NULL, y = NULL)
  #stores the counter for the origin point. 0 = not defined, 1 = defined
  origin_point_cntr = reactiveValues(n = 0)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the button to manually set the origin point
  output$manual_originPoint_btn_UI = renderUI({
    if(input$define_origin_GL != 'Manual')
      #return()
      #bsButton("manual_originPoint_btn", label = "Set", icon("check-circle"), style = "primary")
      if(origin_point_cntr$n == 0){
        bsButton("manual_originPoint_btn", label = "Set", icon("check-circle"), style = "primary")
      }else{
        bsButton("manual_originPoint_btn", label = "Clear Origin Point", icon("times-circle"), style = "danger")
      }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #stores the input from the double clicking
  observe({                
    if (!is.null(input$gl_dblclick)) {
      dbl_clicked_reactive_GL$n <- input$gl_dblclick 
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #sets and resets the value of the counter for the origin point
  observeEvent(input$manual_originPoint_btn,{ 
    
    if(origin_point_cntr$n == 0){
      origin_point_cntr$n = origin_point_cntr$n + 1
      origin_point_GL$x = dbl_clicked_reactive_GL$n$x
      origin_point_GL$y = dbl_clicked_reactive_GL$n$y
      line_point_counter_GL$n = 1
      dbl_clicked_reactive_GL$n = NULL
    }else if(origin_point_cntr$n == 1){
      origin_point_cntr$n = 0
      origin_point_GL$x = NULL
      origin_point_GL$y = NULL
      line_point_counter_GL$n = 0
      
      fieldview_point_GL$x1 = NULL
      fieldview_point_GL$y1 = NULL
      fieldview_point_GL$x2 = NULL
      fieldview_point_GL$y2 = NULL
      
      fieldview_point_1_vals$x = NULL
      fieldview_point_1_vals$y = NULL
      fieldview_point_2_vals$x = NULL
      fieldview_point_2_vals$y = NULL
      
      fieldview_points_cntr$n = 0
      
      dbl_clicked_reactive_GL$n = NULL
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #stores the values of the origin point
  origin_point_automatic_GL = reactiveValues(x = NULL, y = NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$include_palate_GL_UI = renderUI({
    checkboxInput("include_palate_GL", label = "Include Palate Trace", value = TRUE)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  observe({
    if(!is.null(plotStatic_data_GL())){
      if(input$define_origin_GL != 'Manual'){
        fl = plotStatic_data_GL()
        extrema = fl[[2]]
        pal = fl[[3]]
        fl = fl[[1]]
        
        #point X------------------------------------------------------
        #define the extreme values of x
        if(input$define_origin_GL == 'Narrow'){
          if(input$include_palate_GL == T){
            min_x = max(as.numeric(  c(fl[fl$point == 1, 'x'],    pal$x[1])          ))
            max_x = min(as.numeric(  c(fl[fl$point == 100, 'x'],  pal$x[nrow(pal)])  ))
          }else{
            min_x = max(as.numeric(fl[fl$point == 1, 'x']))
            max_x = min(as.numeric(fl[fl$point == 100, 'x']))
          }
        }else if(input$define_origin_GL == 'Wide'){
          if(input$include_palate_GL){
            min_x = min(as.numeric(  c(fl[fl$point == 1, 'x'],    pal$x[1])          ))
            max_x = max(as.numeric(  c(fl[fl$point == 100, 'x'],  pal$x[nrow(pal)])  ))
          }else{
            min_x = min(as.numeric(fl[fl$point == 1, 'x']))
            max_x = max(as.numeric(fl[fl$point == 100, 'x']))
          }
        }
        
        #define the origin x point
        origin_x = min_x + ((max_x - min_x) / 2)
        
        #point Y------------------------------------------------------
        #define the extreme values of x
        max_y = min(as.numeric(fl$y))
        
        #define the lowest y point of the contours
        min_y = max(as.numeric(fl$y))
        
        #define the origin y point
        origin_y = min_y + ((min_y - max_y) / 1.5)
        
        #origin point
        origin_point_automatic_GL$x = origin_x
        origin_point_automatic_GL$y = origin_y
      }
    }
    
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the button to automatically clear the origin point
  output$automatic_originPoint_btn_UI = renderUI({
    if(is.null(origin_point_automatic_GL$x))
      return()
    
    bsButton("automatic_originPoint_btn", label = "Clear", icon("times-circle"), style = "danger")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  observeEvent(input$automatic_originPoint_btn,{ 
    origin_point_automatic_GL$x = NULL
    origin_point_automatic_GL$y = NULL
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #This section controls the setting of the fieldview
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the fieldview radio buttons
  output$define_fieldview_GL_UI = renderUI({
    if(   (input$define_origin_GL == 'Manual' & !is.null(origin_point_GL$x)) ||
          (input$define_origin_GL != 'Manual' & !is.null(origin_point_automatic_GL$x))   ){
      radioButtons("define_fieldview_GL", label = h5("Define Analysis Fan View"),
                   choices = c('Manual', 'Angle', 'Narrow', 'Wide'),
                   selected = 'Manual', inline = T)
    }else{
      return()
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #stores the counter for the number of fieldview points stored
  fieldview_points_cntr <- reactiveValues(n = 0)
  #stores the xy values of the fieldview lines, both initial and final
  fieldview_point_GL <- reactiveValues(x1 = NULL, y1 = NULL, x2 = NULL, y2 = NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the buttons for the manual input
  output$manual_fieldviewPoint_btn_UI <- renderUI({
    
    if(is.null(input$define_fieldview_GL))
      return()
    
    if(input$define_origin_GL == 'Manual' & origin_point_cntr$n == 0)
      return()
    
    if(input$define_fieldview_GL != 'Manual')
      return()
    
    #print('h')
    if(fieldview_points_cntr$n == 0){
      bsButton("manual_fieldviewPoint_btn", label = "Set first point", icon("check-circle"), style = "primary")
    }else if(fieldview_points_cntr$n == 1){
      bsButton("manual_fieldviewPoint_btn", label = "Set second point", icon("check-circle"), style = "warning")
    }else if(fieldview_points_cntr$n == 2){
      return()
      #bsButton("manual_fieldviewPoint_btn", label = "Clear fieldview points", icon("check-circle"), style = "danger")
    }else if(fieldview_points_cntr$n == 3){
      bsButton("manual_fieldviewPoint_btn", label = "Clear fieldview points", icon("check-circle"), style = "danger")
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates set button
  output$set_fieldviewPoint_btn_UI <- renderUI({
    if(is.null(input$define_fieldview_GL))
      return()
    if(input$define_fieldview_GL != 'Manual')
      return()
    if(fieldview_points_cntr$n != 2)
      return()
    
    bsButton("set_fieldviewPoint_btn", label = "Set", icon("check-circle"), style = "warning")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates clear button
  output$clear_fieldviewPoint_btn_UI <- renderUI({
    if(is.null(input$define_fieldview_GL))
      return()
    if(input$define_fieldview_GL != 'Manual')
      return()
    if(fieldview_points_cntr$n != 2)
      return()
    
    bsButton("clear_fieldviewPoint_btn", label = "Clear", icon("times-circle"), style = "danger")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  observe({                
    if (!is.null(input$gl_dblclick)) {
      dbl_clicked_reactive_GL$n <- input$gl_dblclick 
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #controls the behaviour of the manual fieldview button
  observeEvent(input$manual_fieldviewPoint_btn,{
    if(fieldview_points_cntr$n == 2){
      fieldview_point_GL$x1 <- NULL
      fieldview_point_GL$y1 <- NULL
      fieldview_point_GL$x2 <- NULL
      fieldview_point_GL$y2 <- NULL
      
      fieldview_point_1_vals$x <- NULL
      fieldview_point_1_vals$y <- NULL
      fieldview_point_2_vals$x <-  NULL
      fieldview_point_2_vals$y <- NULL
      
      fieldview_points_cntr$n <- 3
    }else if(fieldview_points_cntr$n == 0){
      fieldview_point_GL$x1 <- dbl_clicked_reactive_GL$n$x
      fieldview_point_GL$y1 <- dbl_clicked_reactive_GL$n$y
      fieldview_points_cntr$n <- fieldview_points_cntr$n + 1
    }else if(fieldview_points_cntr$n == 1){
      fieldview_point_GL$x2 <- dbl_clicked_reactive_GL$n$x
      fieldview_point_GL$y2 <- dbl_clicked_reactive_GL$n$y
      fieldview_points_cntr$n <- fieldview_points_cntr$n + 1
    }else if(fieldview_points_cntr$n == 3){
      fieldview_points_cntr$n <- 0
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #stores the final values of the fieldview points
  fieldview_point_1_vals <- reactiveValues(x = NULL, y = NULL)
  fieldview_point_2_vals <- reactiveValues(x = NULL, y = NULL)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #controls the behaviour of the SET button
  observeEvent(input$set_fieldviewPoint_btn,{ 
    
    if(input$define_origin_GL == 'Manual'){
      origin_point <- c(origin_point_GL$x,origin_point_GL$y)
    }else{
      origin_point <- c(origin_point_automatic_GL$x,origin_point_automatic_GL$y)
    }
    
    fieldview_point_1 <- c(fieldview_point_GL$x1, fieldview_point_GL$y1)
    fieldview_point_2 <- c(fieldview_point_GL$x2, fieldview_point_GL$y2)
    
    if(is.null(fieldview_point_GL$x1) || is.null(fieldview_point_GL$x2) ||
       length(setdiff(origin_point, fieldview_point_1)) == 0 ||
       length(setdiff(origin_point, fieldview_point_2)) == 0 ||
       length(setdiff(fieldview_point_1, fieldview_point_2)) == 0 ){
      fieldview_points_cntr$n <- 0
    }else{
      fieldview_points_cntr$n <- 3
      fieldview_point_1_vals$x <- fieldview_point_GL$x1
      fieldview_point_1_vals$y <- fieldview_point_GL$y1
      fieldview_point_2_vals$x <- fieldview_point_GL$x2
      fieldview_point_2_vals$y <- fieldview_point_GL$y2
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #controls the behaviour of the CLEAR button
  observeEvent(input$clear_fieldviewPoint_btn,{ 
    fieldview_point_GL$x1 <- NULL
    fieldview_point_GL$y1 <- NULL
    fieldview_point_GL$x2 <- NULL
    fieldview_point_GL$y2 <- NULL
    
    fieldview_point_1_vals$x <- NULL
    fieldview_point_1_vals$y <- NULL
    fieldview_point_2_vals$x <- NULL
    fieldview_point_2_vals$y <- NULL
    
    fieldview_points_cntr$n <- 0
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$leftlineangle_fieldview_GL_UI = renderUI({
    if(is.null(input$define_fieldview_GL))
      return()
    
    if(input$define_fieldview_GL != 'Angle')
      return()
    
    numericInput('leftlineangle_fieldview_GL', label = 'Left', value = 135, min = 1)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$rightlineangle_fieldview_GL_UI = renderUI({
    if(is.null(input$define_fieldview_GL))
      return()
    
    if(input$define_fieldview_GL != 'Angle')
      return()
    
    numericInput('rightlineangle_fieldview_GL', label = 'Right', value = 45, min = 1)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$number_of_gridlines_radio_ui <- renderUI({
    
    # if(input$define_origin_GL == 'Manual' & fieldview_points_cntr$n != 3)
    #   return()
    
    if(is.null(input$define_fieldview_GL))
      return()
    
    if(input$define_origin_GL == 'Manual' & origin_point_cntr$n == 0)
      return()
    
    # if(input$define_fieldview_GL != 'Manual')
    #   return()
    
    # if(fieldview_points_cntr$n != 3)
    #   return()
    
    if(input$define_origin_GL == 'Manual' & fieldview_points_cntr$n != 3)
      return()
    
    if(input$define_fieldview_GL == 'Manual' & fieldview_points_cntr$n != 3)
      return()
    
    
    radioButtons("number_of_gridlines_radio", label = h5("Gridlines by:"),
                 choices = c("Number", "Angle"), 
                 selected = "Number")
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  output$number_of_gridlines_input_ui <- renderUI({
    
    if(is.null(input$number_of_gridlines_radio))
      return()
    
    numericInput("number_of_gridlines_input", label = NULL,
                 value = 20, min = 2, max = 100)
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  ##THIS SECTION OBSERVES THE CLICKING ACTIVITY IN THE GRIDLINES TAB
  ##======================================================================
  ##======================================================================
  ##======================================================================
  
  #stores the xy values of the line plotted, both the begin and end points
  line_fill_cntr_GL <- reactiveValues(x1 = 0, y1 = 0, x2 = 0, y2 = 0)
  
  #stores the xy values of the double click
  dbl_clicked_reactive_GL <- reactiveValues(n = NULL)
  #counts the number of points for the line
  line_point_counter_GL <- reactiveValues(n = 0)
  
  #stores the values of the first point double clicked in the image
  first_point_GL <- reactiveValues(x = 0, y = 0)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #observes the activity in double click
  observe({
    #if double click has been entered
    if (!is.null(input$gl_dblclick)) {
      #stores the double click information
      dbl_clicked_reactive_GL$n <- input$gl_dblclick 
      
      #stores the x value of the double click
      first_point_GL$x <- input$gl_dblclick$x
      #stores the y value of the double click
      first_point_GL$y <- input$gl_dblclick$y
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  
  #counts the number of clicks for the line
  plot_line_click_counter_GL <- reactiveValues(n = 0)
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #observes the activity of the SET ANCHOR button
  observeEvent(input$manual_originPoint_btn,{
    #print('rgtdhsfahdhjka')
    
    #if the plot lines button is clicked, then the counter is updated
    plot_line_click_counter_GL$n <- plot_line_click_counter_GL$n + 1
    
    #runs the actions if the double clicked button has been previously clicked
    if (!is.null(dbl_clicked_reactive_GL$n)) {
      #if no clicking information has been stored, i.e. no points in the plot object
      if(sum(line_fill_cntr_GL$x1, line_fill_cntr_GL$x2, line_fill_cntr_GL$y1, line_fill_cntr_GL$y2) == 0){
        
        ##the xy information is the same for both points of the line but only the x1y1 is plotted
        line_fill_cntr_GL$x1 <- dbl_clicked_reactive_GL$n$x
        line_fill_cntr_GL$y1 <- dbl_clicked_reactive_GL$n$y
        line_fill_cntr_GL$x2 <- dbl_clicked_reactive_GL$n$x
        line_fill_cntr_GL$y2 <- dbl_clicked_reactive_GL$n$y
        
        #updates the number of points in the line
        line_point_counter_GL$n <- 1
      }else{
        #if previous clicking information has been stored
        
        if((line_fill_cntr_GL$x1 == line_fill_cntr_GL$x2) & (line_fill_cntr_GL$y1 == line_fill_cntr_GL$y2)){
          
          #stores the new xy information for the second point
          line_fill_cntr_GL$x2 <- dbl_clicked_reactive_GL$n$x
          line_fill_cntr_GL$y2 <- dbl_clicked_reactive_GL$n$y
          
          #updates the number of points in the line
          line_point_counter_GL$n <- 1
        }
      }
      
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reacts to the clicking of the button to plot lines and stores the xy values
  #user clicks on CLEAR LINES button
  observeEvent(input$manual_originPoint_btn,{
    if(plot_line_click_counter_GL$n == 2){
      
      #if there are two points already stored
      line_fill_cntr_GL$x1 <- 0
      line_fill_cntr_GL$y1 <- 0
      line_fill_cntr_GL$x2 <- 0
      line_fill_cntr_GL$y2 <- 0
      
      line_point_counter_GL$n <- 0
      dbl_clicked_reactive_GL$n <- NULL
      plot_line_click_counter_GL$n <- 0
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #stores the single clicking information
  clicked_reactive_GL <- reactiveValues(n = NULL)
  
  #observes the activity of the single clicking
  #observes the activity of click in GridLines plot
  observe({
    #if the single clicking information is not NULL
    if (!is.null(input$gl_click)) {
      #stores the single clicking information
      clicked_reactive_GL$n <- input$gl_click       
    }
  })
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  prepare_landmark_data <- reactive({
    
    landmarks <- read.csv('./workingFiles/df_landmark.csv', stringsAsFactors = F)
    landmarks$X <- NULL
    landmarks[is.na(landmarks)] <- ''
    
    landmarks$landmarks <- ''
    
    for(i in 5:(ncol(landmarks) - 1)){
      landmarks$landmarks <- paste0(landmarks$landmarks, landmarks[[i]])
    }
    
    landmarks$landmarks <- trim(landmarks$landmarks)
    landmarks <- landmarks[landmarks$landmarks != '',]
    
    landmarks <- landmarks[,c(1:4,ncol(landmarks))]
    
    #reads intersections file
    fls <- list.files('workingFiles', pattern = 'ints', full.names = T)
    
    fls <- list.files('workingFiles', pattern = 'ints', full.names = T)
    
    for(ii in fls){
      speaker_i <- read.csv(ii)
      speaker_i$X <- NULL
      speaker_i$speaker <- gsub('ints|\\.csv', '', basename(ii))
      
      if(ii == fls[1]){
        speaker_df <- speaker_i
      }else{
        speaker_df <- rbind(speaker_df, speaker_i)
      }
    }
    
    speaker_df <- merge(speaker_df, landmarks, by = c('segment', 'repetition', 'frame', 'speaker'), all = T)
    speaker_df$landmarks[is.na(speaker_df$landmarks)] <- 'UNCL'
    
    #merge gridlines
    flsGridlines <- list.files('workingFiles', pattern = 'gridLines_', full.names = T)
    
    for(ii in flsGridlines){
      gridlines_i <- read.csv(ii)
      gridlines_i$X <- NULL
      gridlines_i$speaker <- gsub('gridLines_|\\.csv', '', basename(ii))
      
      if(ii == flsGridlines[1]){
        gridlines_df <- gridlines_i
      }else{
        gridlines_df <- rbind(gridlines_df, gridlines_i)
      }
    }
    
    flsoriginPoints <- list.files('workingFiles', pattern = 'originPoint_', full.names = T)
    
    for(ii in flsoriginPoints){
      originPoints_i <- read.csv(ii)
      originPoints_i$X <- NULL
      originPoints_i$speaker <- gsub('originPoint_|\\.csv', '', basename(ii))
      
      if(ii == flsoriginPoints[1]){
        originPoints_df <- originPoints_i
      }else{
        originPoints_df <- rbind(originPoints_df, originPoints_i)
      }
    }
    
    names(originPoints_df) <- c('x1', 'y1', 'speaker')
    
    gridlines_df <- merge(gridlines_df, originPoints_df, by = 'speaker')
    
    uniqueLMlabels <- unique(speaker_df$landmarks)
    uniqueLMlabels <- uniqueLMlabels[uniqueLMlabels != '']
    
    write.csv(speaker_df, 'speaker_df.csv', row.names = F)
    write.csv(gridlines_df, 'gridlines_df.csv', row.names = F)
    
    return(list(uniqueLMlabels, speaker_df, gridlines_df))
  })
  
  output$dynamicAnalysisSingle_landmark_selection_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    dflabels <- prepare_landmark_data()[[1]]
    
    selectInput(inputId = 'dynamicAnalysisSingle_landmark_selection', label = 'Landmarks', choices = sort(unique(dflabels)), multiple = T)
  })
  
  output$dynamicAnalysisSingle_speaker_selection_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    df <- prepare_landmark_data()[[2]]
    
    selectInput(inputId = 'dynamicAnalysisSingle_speaker_selection', label = 'Speaker', choices = sort(unique(df$speaker)))
  })
  
  output$dynamicAnalysisSingle_segment_selection_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    df <- prepare_landmark_data()[[2]]
    
    df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection,]
    
    selectInput(inputId = 'dynamicAnalysisSingle_segment_selection', label = 'Segment', 
                choices = sort(unique(df$segment)), multiple = T, selected = sort(unique(df$segment))[1])
  })
  
  output$dynamicAnalysisSingle_repetition_selection_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_segment_selection))
      return()
    
    df <- prepare_landmark_data()[[2]]
    
    df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection & df$segment %in% input$dynamicAnalysisSingle_segment_selection,]
    
    selectInput(inputId = 'dynamicAnalysisSingle_repetition_selection', label = 'Repetition', 
                choices = sort(unique(df$repetition)), multiple = T, selected = sort(unique(df$repetition))[1])
  })
  
  output$dynamicAnalysisSingle_plot <- renderPlotly({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_segment_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_repetition_selection))
      return()
    
    df <- prepare_landmark_data()[[2]]
    
    dforigin <- prepare_landmark_data()[[3]]
    
    originx <- unique(dforigin$x1)
    originy <- unique(dforigin$y1)
    
    df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection & 
               df$segment %in% input$dynamicAnalysisSingle_segment_selection &
               df$repetition %in% input$dynamicAnalysisSingle_repetition_selection,]
    
    #define colors and linetypes
    if(length(input$dynamicAnalysisSingle_landmark_selection) == 1){
      #one landmark
      if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
        #one segment
        if(input$dynamicAnalysisSingle_tokens){
          #single repetitions
          tmpgroup <- 'interaction(repetition)'
          tmpcolor <- 'repetition'
          tmpLineType <- 'segment'
        }else{
          #aggregate
          tmpgroup <- 'interaction(repetition)'
          tmpcolor <- 'repetition'
          tmpLineType <- 'segment'
        }
      }else{
        #multiple segments
        if(input$dynamicAnalysisSingle_tokens){
          #single repetitions
          tmpgroup <- 'interaction(segment,repetition)'
          tmpcolor <- 'repetition'
          tmpLineType <- 'segment'
        }else{
          #aggregate
          tmpgroup <- 'interaction(segment)'
          tmpcolor <- 'segment'
          tmpLineType <- 'segment'
        }
      }
    }else{
      #multiple landmarks...........................................
      df <- df[df$landmarks %in%input$dynamicAnalysisSingle_landmark_selection, ]
      
      if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
        #one segment
        if(input$dynamicAnalysisSingle_tokens){
          #single repetitions
          tmpgroup <- 'interaction(repetition,landmarks)'
          tmpcolor <- 'repetition'
          tmpLineType <- 'landmarks'
        }else{
          #aggregate
          tmpgroup <- 'interaction(landmarks)'
          tmpcolor <- 'landmarks'
          tmpLineType <- 'landmarks'
        }
      }else{
        #multiple segments
        if(input$dynamicAnalysisSingle_tokens){
          #single repetitions
          tmpgroup <- 'interaction(segment,repetition,landmarks)'
          tmpcolor <- 'landmarks'
          tmpLineType <- 'segment'
        }else{
          #aggregate
          tmpgroup <- 'interaction(segment,landmarks)'
          tmpcolor <- 'landmarks'
          tmpLineType <- 'segment'
        }
      }
    }
    
    
    if(input$dynamicPlot_type == 'Contours'){
      #start plot
      ggp <- ggplot(df, aes_string(x='x', y='y', group=tmpgroup,color=tmpcolor,linetype=tmpLineType))
      
      #confidence intervals
      if(input$dynamicAnalysisSingle_ciplot){
        #with confidence intervals
        ggp <- ggp + stat_smooth()
      }else{
        #without confidence intervals
        ggp <- ggp + stat_smooth(se = F)
      }
      #gridlines
      if(input$dynamicAnalysisSingle_gridlines == T){
        
        originy <- input$dynamicAnalysisSingle_originPoint
        dforigin$y1 <- input$dynamicAnalysisSingle_originPoint
        
        ggp <- ggp + geom_point(inherit.aes = F, data = data.frame(x = originx, y = originy),
                                aes(x = originx, y = originy), size = 5,
                                colour = "#FF6A6A", show.legend = FALSE)
        
        ggp <- ggp + geom_segment(inherit.aes = F, data = dforigin, aes(x = x1, y = y1, xend = x, yend = y), 
                                  alpha = input$dynamicAnalysisSingle_alpha)
        
      }
      
    }else{
      
      ggp <- ggplot(data=df,aes(x,y)) +
        stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='white') + 
        scale_fill_continuous(low="white",high="red") +
        guides(alpha="none") +
        scale_colour_gradient(low="white",high="red")
      
      if(length(input$dynamicAnalysisSingle_landmark_selection) == 1){
        #one landmark
        if(length(input$dynamicAnalysisSingle_segment_selection) > 1){
          #onse segment
          ggp <- ggp + facet_wrap(~segment)
        }
      }else{
        #multiple landmarks
        if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
          #onse segment
          
          if(input$dynamicAnalysisSingle_tokens){
            #tokens = plot individual landmarks
            ggp <- ggp + facet_wrap(~landmarks)
          }
          
        }else{
          #multiple landmarks
          ggp <- ggp + facet_wrap(~segment)
        }
      }
      
    }
    
    #final settings
    ggp <- ggp + theme_minimal(base_size = input$dynamicAnalysisSingle_fontsize) + 
      scale_y_reverse() + xlab('Tongue Advancement') + ylab('Tongue Height')
    
    ggp
    
  })
  
  output$dynamicAnalysisSingle_downloadPlot <- downloadHandler(
    filename = function(){paste("dynamicAnalysisSingle_plot",'.png',sep='')},
    content = function(file){
      ggsave(file)
    }
  )
  
  output$dynamicAnalysisSingle_originPoint_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_segment_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_repetition_selection))
      return()
    
    if(!input$dynamicAnalysisSingle_gridlines)
      return()
    
    dforigin <- prepare_landmark_data()[[3]]
    
    originx <- unique(dforigin$x1)
    originy <- unique(dforigin$y1)
    
    numericInput(inputId = 'dynamicAnalysisSingle_originPoint', label = 'Origin Y', value = originy)
  })
  
  
  output$dynamicAnalysisSingle_fontsize_ui <- renderUI({
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    numericInput(inputId = 'dynamicAnalysisSingle_fontsize', label = 'Font size', 
                 value = 15, min = 0, max = 50, step = 1)
  })
  
  output$dynamicAnalysisSingle_displacements = renderHighchart({
    
    if(is.null(prepare_landmark_data()))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_landmark_selection))
      return()
    
    if(input$dynamicAnalysisSingle_landmark_selection == '')
      return()
    
    if(is.null(input$dynamicAnalysisSingle_speaker_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_segment_selection))
      return()
    
    if(is.null(input$dynamicAnalysisSingle_repetition_selection))
      return()
    
    if(length(input$dynamicAnalysisSingle_landmark_selection) != 2)
      return()
    
    landmarksin <- input$dynamicAnalysisSingle_landmark_selection
    
    df <- prepare_landmark_data()[[2]]
    
    dforigin <- prepare_landmark_data()[[3]]
    
    originx <- unique(dforigin$x1)
    originy <- unique(dforigin$y1)
    
    df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection & 
               df$segment %in% input$dynamicAnalysisSingle_segment_selection &
               df$repetition %in% input$dynamicAnalysisSingle_repetition_selection,]
    
    #df <- df[df$landmarks %in% input$dynamicAnalysisSingle_landmark_selection,]
    
    df$distance <- sqrt((originx-df$x)^2+(originy-df$y)^2)
    
    for(i in sort(unique(df$segment))){
      dfi <- df[df$segment == i,]
      for(j in sort(unique(dfi$repetition))){
        dfj <- dfi[dfi$repetition == j,]
        
        dfjframe <- dfj[dfj$landmarks %in% input$dynamicAnalysisSingle_landmark_selection,]
        
        sequenceFrames <- sort(as.numeric(unique(dfjframe$frame)))
        frameDifference <- diff(sequenceFrames)
        tokenTime <- input$frameRate * frameDifference
        
        #calculate individual accelerations........................................................
        #get distances
        tmpAccMatrixDistancesAll <- matrix(ncol = frameDifference, nrow = length(unique(dfj$point)))
        cntr <- 1
        for(acci in sequenceFrames[1]:(sequenceFrames[2]-1)){
          tmpAccMatrixDistancesAll[,cntr] <- dfj[dfj$frame == acci,'distance'] - dfj[dfj$frame == (acci + 1),'distance']
          cntr <- cntr + 1
        }
        
        #mean distances per gridline
        tmpAccMatrixDistances <- NULL
        for(veloi in 1:nrow(tmpAccMatrixDistancesAll)){
          tmpAccMatrixDistances[veloi] <- mean(diff(tmpAccMatrixDistancesAll[veloi,]))
        }
        
        tmpAccMatrixVelocitiesAll <- tmpAccMatrixDistancesAll / input$frameRate
        tmpAccMatrixVelocities <- NULL
        for(veloi in 1:nrow(tmpAccMatrixVelocitiesAll)){
          tmpAccMatrixVelocities[veloi] <- mean(diff(tmpAccMatrixVelocitiesAll[veloi,]))
        }
        
        #calculate individual accelerations
        tmpAccMatrixAccelerationsAll <- matrix(ncol = frameDifference, nrow = length(unique(dfj$point)))
        for(veloi in 1:nrow(tmpAccMatrixVelocitiesAll)){
          for(veloj in 1:frameDifference){
            
            
            if(veloj == 1){
              currentVelocity <- tmpAccMatrixVelocitiesAll[veloi,veloj]
              
              if(currentVelocity < 0){
                currentVelocity <- currentVelocity * -1
              }
              
              tmpAccMatrixAccelerationsAll [veloi, veloj] <- currentVelocity / input$frameRate
            }else{
              #get the previous velocity value
              previousVelocity <- tmpAccMatrixVelocitiesAll[veloi, veloj-1]
              currentVelocity <- tmpAccMatrixVelocitiesAll[veloi,veloj]
              
              tmpAccMatrixAccelerationsAll [veloi, veloj] <- (currentVelocity / input$frameRate)
              
              if(currentVelocity < previousVelocity){
                #negative acceleration
                if(tmpAccMatrixAccelerationsAll [veloi, veloj] > 0){
                  tmpAccMatrixAccelerationsAll [veloi, veloj] <- tmpAccMatrixAccelerationsAll [veloi, veloj] * -1
                }
              }
            }
          }
          #tmpAccMatrixVelocities[veloi] <- mean(diff(tmpAccMatrixDistancesAll[veloi,])) / input$frameRate
        }
        View(tmpAccMatrixVelocitiesAll)
        View(tmpAccMatrixAccelerationsAll)
        
        
        
        tmpAccMatrixAccelerations <- NULL
        for(veloi in 1:nrow(tmpAccMatrixAccelerationsAll )){
          tmpAccMatrixAccelerations[veloi] <- mean(diff(tmpAccMatrixAccelerationsAll[veloi,]))
        }
        
        #tmpAccMatrixAccelerations <- tmpAccMatrixVelocities / input$frameRate
        
        dfja <- dfj[dfj$landmarks == landmarksin[1],]
        dfja <- dfja[with(dfja, order(point)),]
        dfjb <- dfj[dfj$landmarks == landmarksin[2],]
        dfjb <- dfjb[with(dfjb, order(point)),]
        
        tmpdf <- dfja[unlist(strsplit('segment repetition speaker point', ' '))]
        tmpdf$landmarks <- paste(landmarksin[1], landmarksin[2], sep = '-')
        tmpdf$tokenLabel <- paste(tmpdf$segment, tmpdf$repetition, 
                                  tmpdf$speaker, tmpdf$landmarks, sep = '_')
        
        tmpdf$distance <- tmpAccMatrixDistances
        tmpdf$displacement <- dfjb$distance - dfja$distance
        #tmpdf$acceleration <- tmpdf$velocity / tokenTime
        
        #tmpdf$displacement <- (tmpdf$velocity * tokenTime) + ((tmpdf$acceleration / 2) * (tokenTime ^ 2))
        
        #tmpdf$displacement <- rowMeans(tmpAccMatrixDistances, na.rm = FALSE, dims = 1)
        tmpdf$velocity <- tmpAccMatrixVelocities#rowMeans(abs(tmpAccMatrixVelocities), na.rm = FALSE, dims = 1)
        tmpdf$acceleration <- rowMeans(tmpAccMatrixAccelerationsAll, na.rm = FALSE, dims = 1)
        
        # tmpdf$displacement <- rowMeans(tmpAccMatrixDisplacements, na.rm = FALSE, dims = 1)
        
        if(i == sort(unique(df$segment))[1] & j == sort(unique(dfi$repetition))[1]){
          newdf <- tmpdf
        }else{
          newdf <- rbind(newdf, tmpdf)
        }
        
      }
    }
    
    if(!input$dynamicAnalysisSingle_tokens){
      #multiple repetitions to aggregate
      
      if(input$dynamicType == 'Displacement'){
        newdf <- newdf %>%
          group_by(speaker, segment, point, landmarks) %>%
          summarise(`Displacement(mm)` = mean(displacement))
      }else if(input$dynamicType == 'Velocity'){
        newdf <- newdf %>%
          group_by(speaker, segment, point, landmarks) %>%
          summarise(`Velocity(mm/s)` = mean(velocity))
      }else if(input$dynamicType == 'Acceleration'){
        newdf <- newdf %>%
          group_by(speaker, segment, point, landmarks) %>%
          summarise(`Acceleration(mm/s2)` = mean(acceleration))
      }else{
        newdf <- newdf %>%
          group_by(speaker, segment, point, landmarks) %>%
          summarise(`Distance(mm)` = mean(distance))
      }
      
      newdf$tokenLabel <- paste(newdf$speaker, newdf$segment, newdf$landmarks, sep = '_')
    }else{
      if(input$dynamicType == 'Displacement'){
        newdf$`Displacement(mm)` <- newdf$displacement
      }else if(input$dynamicType == 'Velocity'){
        newdf$`Velocity(mm/s)` <- newdf$velocity
      }else if(input$dynamicType == 'Acceleration'){
        newdf$`Acceleration(mm/s2)` <- newdf$acceleration
      }else{
        newdf$`Distance(mm)` <- newdf$distance
      }
      newdf$tokenLabel <- paste(newdf$speaker, newdf$segment, newdf$repetition, newdf$landmarks, sep = '_')
    }
    
    colnames(newdf)[which(names(newdf) == "point")] <- "Gridlines"
    
    if(input$dynamicType == 'Displacement'){
      hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Displacement(mm)`, group = tokenLabel)) %>%
        hc_xAxis(reversed = TRUE)
    }else if(input$dynamicType == 'Velocity'){
      hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Velocity(mm/s)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
        hc_xAxis(reversed = TRUE)
    }else if(input$dynamicType == 'Acceleration'){
      hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Acceleration(mm/s2)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
        hc_xAxis(reversed = TRUE)
    }else{
      hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Distance(mm)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
        hc_xAxis(reversed = TRUE)
    }
    
    hc %>% 
      hc_xAxis(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)),
               labels = list(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)))) %>%
      hc_yAxis(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)),
               labels = list(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)))) %>%
      hc_exporting(enabled = TRUE,
                   filename = "custom-file-name")
    
  })
  
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #creates the speaker selection widget
  output$spkrSSANOVA <- renderUI({
    #if there is no input file
    if (is.null(input$file1))
      return()
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    #gets the unique labels of the speakers
    spkrs <- unique(fileIn$speaker)
    
    #creates the speaker widget
    selectInput(inputId = 'speakerANOVA', label = 'Select\nSpeaker',
                choices = spkrs, multiple = FALSE)
  })
  
  #create comparisons
  output$comp1SSANOVA <- renderUI({
    #if there is no input file
    if (is.null(input$file1))
      return()
    
    if (is.null(input$speakerANOVA))
      return()
    
    #gets the unique labels of the speakers
    spkrs <- input$speakerANOVA
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    fileIn <- fileIn[fileIn$speaker == spkrs,]
    
    #gets the unique labels of the segments
    segs <- unique(fileIn$segment)
    
    #creates the speaker widget
    selectInput(inputId = 'comparison1SSANOVA', label = 'First Comparison', 
                choices = segs, multiple = FALSE)
  })
  
  #create comparisons
  output$comp2SSANOVA <- renderUI({
    #if there is no input file
    if (is.null(input$file1))
      return()
    
    if (is.null(input$speakerANOVA))
      return()
    
    #gets the unique labels of the speakers
    spkrs <- input$speakerANOVA
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[2]]
    
    fileIn <- fileIn[fileIn$speaker == spkrs,]
    
    #gets the unique labels of the segments
    segs <- unique(fileIn$segment)
    
    #creates the speaker widget
    selectInput(inputId = 'comparison2SSANOVA', label = 'Second Comparison', 
                choices = segs, multiple = FALSE)
  })
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #observeEvent(input$ssbtn,{
  
  
  plotSSANOVAData <- reactive({
    #if there is no input file
    if (is.null(input$file1))
      return()
    if (is.null(input$speakerANOVA))
      return()
    if (is.null(input$comparison1SSANOVA))
      return()
    if (is.null(input$comparison2SSANOVA))
      return()
    if(input$comparison1SSANOVA == input$comparison2SSANOVA)
      return()
    
    if(input$comparison1SSANOVA == 'pal' || 
       input$comparison2SSANOVA == 'pal')
      return()
    
    show_modal_spinner(spin = 'bounce', text = 'Please wait. SSANOVA calculations in progress.') # show the modal window
    
    #gets the unique labels of the speakers
    spkrs <- input$speakerANOVA
    
    cmp1 <- input$comparison1SSANOVA
    cmp2 <- input$comparison2SSANOVA
    
    #imports the input file
    fileIn <- importFiles()
    #selects the dataframe with values
    fileIn <- fileIn[[1]]
    
    fileIn <- fileIn[fileIn$speaker == spkrs,]
    
    #gets input measurement unit
    ms <- as.character(input$radio_measures)
    #Measurement*********************************************
    if (ms == 1){
      #measurements in millimiters
      #d <- subset(d, select = -c(pixel))
      measure_val <- "mm"
      measure_valPlot <- "mm"
    }else if (ms == 2){
      #measurements in pixels
      #d <- subset(d, select = -c(mm))
      measure_val <- "pixel"
      measure_valPlot <- "pixels"
    }
    
    tmpspeaker <- spkrs
    tmpfirstsegment <- cmp1
    tmpsecondsegment <- cmp2
    measuremtnUnit <- measure_val
    
    print('start1')
    
    dat <- fileIn
    dat <- dat[dat$speaker == tmpspeaker & dat$segment %in% c(tmpfirstsegment, tmpsecondsegment),]
    
    dat$speaker <- NULL
    dat$X <- NULL
    
    dat <- spread(dat, coord, measure_val)
    
    dat$point <- NULL
    
    names(dat) <- c('word', 'repetition', 'time.frame', 'X', 'Y')
    
    dat <- dat[c('X', 'Y', 'word', 'time.frame', 'repetition')]
    
    write.csv(dat, 'testdata.csv', row.names = F)
    
    tmpxpoint <- max(dat$X)
    tmpypoint <- min(dat$Y) - (1/abs(min(dat$Y) - max(dat$Y)))
    
    if(input$polarPlot){
      
      datraw <- dat
      
      dat$X <- abs(tmpxpoint - dat$X)
      dat$Y <- abs(tmpypoint - dat$Y)
      
      dat$X <- sqrt((dat$X^2) + (dat$Y^2))#radius
      dat$Y <- atan2(dat$Y, dat$X)#theta
    }
    
    group1 <- tmpfirstsegment
    group2 <- tmpsecondsegment
    
    #making a data frame containing just the target words/groups
    w1w2<-droplevels(subset(dat,word%in%c(group1,group2)))
    
    if(input$polarPlot){
      w1w2raw<-droplevels(subset(datraw,word%in%c(group1,group2)))
    }
    
    # Add individual data points:
    comparisonInd <- ggplot(data= w1w2, aes(x=jitter(X),y=Y,colour = word,
                                            group = interaction(word, time.frame, repetition))) +
      stat_smooth(geom='line',alpha = 0.75) +
      #geom_point(alpha = 0.75) + 
      scale_y_reverse() + ylab("y")+ theme_bw()
    
    # fit model
    ssa.fit <-ssanova(Y~word+X+word:X,data=w1w2)
    
    # get predictions for constructing confidence intervals
    X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
    grid <- expand.grid(X=X ,word = c(group1,group2))
    grid$ssa.fit <- predict(ssa.fit,newdata = grid,se = T)$fit
    grid$ssa.SE <- predict(ssa.fit,newdata = grid,se = T)$se.fit
    
    if(input$polarPlot){
      tmpnewx <- ssa.fit$mf$X
      tmpnewy <- ssa.fit$mf$Y
      
      ssa.fit$mf$X <- tmpnewx * cos(tmpnewy)
      ssa.fit$mf$Y <- tmpnewx * sin(tmpnewy)
    }
    
    # plotting comparison:
    # set up plot object:
    comparison <- ggplot(grid,aes(x = X,colour = word,group = word))+ theme_bw()
    
    # for greyscale plots, uncomment the next line
    # comparison <- comparison + scale_fill_grey()
    cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    # for colors that will print well in greyscale, uncomment the next line
    # comparison <- comparison+scale_fill_brewer(palette="PuBuGn")
    
    # Main effects plot
    comparison<-comparison  + geom_line(aes(y = ssa.fit),alpha = 1,colour = "grey20")
    comparison<-comparison + geom_ribbon(aes(ymin = ssa.fit-(1.96*ssa.SE), ymax = ssa.fit+(1.96*ssa.SE),fill = word ),alpha = 0.75,colour = "NA")
    
    #flip the Y axis
    comparison<-comparison + scale_y_reverse()+ scale_fill_manual(values=cbPalette)
    # labels
    comparison<-comparison + ylab("y")
    
    if(input$polarPlot){
      # fit model
      ssa.fitraw <-ssanova(Y~word+X+word:X,data=w1w2raw)
      
      # Interaction effects plots:
      Xraw=seq(min(w1w2raw$X),max(w1w2raw$X),by=0.01)
      gridraw <- expand.grid(X = Xraw,word = c(group1,group2))
      gridraw$Fit <- predict(ssa.fitraw,gridraw,se = T,inc = c("word","word:X"))$fit
      gridraw$SE <- predict(ssa.fitraw,gridraw,se = T,inc = c("word","word:X"))$se.fit
      inter <- ggplot(gridraw,aes(x=X))+theme_bw()
    }else{
      # Interaction effects plots:
      X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
      grid <- expand.grid(X = X,word = c(group1,group2))
      grid$Fit <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$fit
      grid$SE <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$se.fit
      inter <- ggplot(grid,aes(x=X))+theme_bw()
    }
    
    inter <- inter + geom_line(aes(y = Fit))
    inter <- inter + geom_ribbon(aes(ymax = Fit+(1.96*SE),ymin = Fit -(1.96*SE)),alpha=0.5)
    inter <- inter + facet_wrap(~word)
    inter <- inter + geom_hline(yintercept = 0,lty = 2)
    inter <- inter + ylab("y")
    
    #add labels
    comparison <- comparison + xlab(paste0('Length (', measure_valPlot, ')')) +
      ylab(paste0('Height (', measure_valPlot, ')')) + labs(fill = "Segment") + 
      theme(text = element_text(size=20))
    comparisonInd <- comparisonInd + xlab(paste0('Length (', measure_valPlot, ')')) +
      ylab(paste0('Height (', measure_valPlot, ')')) + labs(colour = "Segment") + 
      theme(text = element_text(size=20))
    inter <- inter + xlab(paste0('Length (', measure_valPlot, ')')) +
      ylab(paste0('Difference (', measure_valPlot, ')')) + 
      theme(text = element_text(size=20))

    remove_modal_spinner() # remove it when done
    
    return(list(comparisonInd, comparison, inter, grid))

  })
  
  output$plotSSANOVAComparison <- renderPlot({
    
    if (is.null(input$file1))
      return()
    if (is.null(input$speakerANOVA))
      return()
    if (is.null(input$comparison1SSANOVA))
      return()
    if (is.null(input$comparison2SSANOVA))
      return()
    if(input$comparison1SSANOVA == input$comparison2SSANOVA)
      return()
    
    if(input$comparison1SSANOVA == 'pal' || 
       input$comparison2SSANOVA == 'pal')
      return()
    
    if(is.null(plotSSANOVAData()))
      return()
    
    plottype <- plotSSANOVAData()[[2]]
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    plottype
    
  })
  
  output$plotSSANOVAIndividual <- renderPlot({
    
    if (is.null(input$file1))
      return()
    if (is.null(input$speakerANOVA))
      return()
    if (is.null(input$comparison1SSANOVA))
      return()
    if (is.null(input$comparison2SSANOVA))
      return()
    if(input$comparison1SSANOVA == input$comparison2SSANOVA)
      return()
    
    if(input$comparison1SSANOVA == 'pal' || 
       input$comparison2SSANOVA == 'pal')
      return()
    
    if(is.null(plotSSANOVAData()))
      return()
    
    plottype <- plotSSANOVAData()[[1]]
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    plottype
    
  })
  
  output$plotSSANOVAInteraction <- renderPlot({
    
    if (is.null(input$file1))
      return()
    if (is.null(input$speakerANOVA))
      return()
    if (is.null(input$comparison1SSANOVA))
      return()
    if (is.null(input$comparison2SSANOVA))
      return()
    if(input$comparison1SSANOVA == input$comparison2SSANOVA)
      return()
    
    if(input$comparison1SSANOVA == 'pal' || 
       input$comparison2SSANOVA == 'pal')
      return()
    
    if(is.null(plotSSANOVAData()))
      return()
    
    plottype <- plotSSANOVAData()[[3]]
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    plottype
    
  })
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      'individualContours.png'
    },
    content = function(file) {
      
      plottype <- plotSSANOVAData()[[1]]
      plottype <- plottype + theme(text = element_text(size=25))
      
      if(input$barePlot){
        plottype <- plottype + theme_void()
      }
      
      png(file, width = 1000, height = 750)
      print(plottype)
      dev.off()
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      'overallComparison.png'
    },
    content = function(file) {
      
      plottype <- plotSSANOVAData()[[2]]
      plottype <- plottype + theme(text = element_text(size=25))
      
      if(input$barePlot){
        plottype <- plottype + theme_void()
      }
      
      png(file, width = 1000, height = 750)
      print(plottype)
      dev.off()
    }
  )
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      'confidenceIntervals.png'
    },
    content = function(file) {
      
      plottype <- plotSSANOVAData()[[3]]
      plottype <- plottype + theme(text = element_text(size=25))
      
      if(input$barePlot){
        plottype <- plottype + theme_void()
      }
      
      png(file, width = 1000, height = 750)
      print(plottype)
      dev.off()
    }
  )
  
  #save data
  output$downloadData1 <- output$downloadData2 <- output$downloadData3 <- downloadHandler(
    filename = function() {
      'ssanovadata.csv'
    },
    content = function(file) {
      
      savessanovadata <- plotSSANOVAData()[[4]]
      
      names(savessanovadata)[2] <- 'segment'
      
      write.csv(savessanovadata, file, row.names = F)
    }
  )

})



