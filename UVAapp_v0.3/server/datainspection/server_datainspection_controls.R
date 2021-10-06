
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Creates the speaker selection widget
output$speaker <- renderUI({
  #if there is no input file
  if (is.null(input$file1))
    return()
  
  #....................................................................................
  #imports the input file
  #selects the dataframe with values
  #gets the unique labels of the speakers
  spkrs <- importFiles()[[2]] %>%
    .$speaker %>% sort() %>% unique()

  #....................................................................................
  #creates the speaker widget
  selectInput(inputId = 'spkr', label = 'Speaker', choices = spkrs, multiple = FALSE)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This is part of the selection section
#Controls the SEGMENTS input from the user.

#Creates the segment selection widget
output$segment <- renderUI({
  #if there is no speaker widget
  if (is.null(input$spkr))
    return()
  
  #....................................................................................
  #gets speaker label
  speaker_label <- input$spkr
  
  #....................................................................................
  #imports the input file
  #selects the dataframe with values
  #gets segment labels
  segs <- importFiles()[[2]] %>%
    filter(segment != input$palateTraceName) %>%
    filter(speaker == speaker_label) %>%
    .$segment %>% sort() %>% unique()
  
  #....................................................................................
  if(input$segment_selection == 1){
    #if there is only one segment selected
    multiple_selection <- FALSE
  }else if(input$segment_selection == 2){
    #if there are two segments selected
    multiple_selection <- TRUE
  }
  
  #....................................................................................
  #creates the segment widget
  #if there is only one segment selected
  if(input$segment_selection %in% c(1, 2))
  {
    selectInput(inputId = 'sgmnt', label = NULL, 
                choices = c(segs), selected = segs[1], 
                multiple = multiple_selection)
  }else{
    #if all segments are selected
    return()
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section creates the radio buttons for repetition selection
output$repetition_radioUI = renderUI({
  if(is.null(input$sgmnt))
    return()
  
  #....................................................................................
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

#Creates the repetition selection widget
output$repetition <- renderUI({
  #if there is no segment widget
  if (is.null(input$sgmnt))
    return()
  
  if (is.null(input$repetition_radio))
    return()
  
  #....................................................................................
  #gets speaker label
  speaker_label = input$spkr
  
  #....................................................................................
  #gets segment label
  if(input$segment_selection == 3)
  {
    #if All segments are selected
    seg <- "ALL"
  }else{
    #if NOT All segments are selected (it can be a single segment(1), or multiple segments(2))
    seg <- input$sgmnt
  }

  #....................................................................................
  #imports the input file
  #selects the dataframe with values
  #subsets file to the selected segments and omits the palate trace from the data frame
  fileIn <- importFiles()[[2]] %>%
    filter(speaker == speaker_label) %>%
    filter(segment != input$palateTraceName)
  
  #....................................................................................
  #Gets the repetitions
  if(length(seg) == 1 && input$segment_selection != 3){
    #if only one segment is selected and this is NOT All segments
    
    #subsets data to the specified speaker, segment(s) and extracts the repetition numbers
    #unique values of repetitions
    unique_reps <- fileIn %>%
      filter(segment %in% seg) %>%
      .$repetition %>% sort() %>% unique()
    
    #maximum repetition number
    reps <- max(unique_reps)
    
    #.................................................................................... 
  }else if (length(seg) > 1 || input$segment_selection == 3){
    #if more than one segment is selected or All segments are selected
    
    if(seg == "ALL"){
      #if all segments are selected
      #get all the segments for the speaker
      segs <- fileIn %>%
        .$segment %>% sort() %>% unique()
    }else{
      #if NOT All segments are selected
      #subsets data to the specified segment(s)
      segs <- fileIn %>%
        filter(segment == seg) %>%
        .$segment %>% sort() %>% unique()
    }
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #initialises an empty array to store the maximum number of repetitions
    tmp_data <- c()
    
    #iterates for each segment
    for(i in 1:length(segs))
    {
      #gets the number of repetitions for the segment iterated
      reps <- fileIn %>%
        filter(segment == segs[i]) %>%
        .$repetition %>% sort() %>% unique()
      
      #stores the maximum number of repetitions per segment
      tmp_data[i] <- max(reps)
    }
    
    #checks whether the repetitions are INCLUSIVE or EXCLUSIVE
    if(input$repetition_radio %in% c(2, 4, 6)){
      #INCLUSIVE = gets all the repetitions
      reps <- max(tmp_data)
    }else if(input$repetition_radio %in% c(1, 3, 5)){
      #EXCLUSIVE = gets only the repetitions that are common across all segments
      reps <- min(tmp_data)
    }
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #creates the repetition selection based on input
  
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
#This section creates the radio buttons for frame selection
output$radio_framesUI = renderUI({
  
  if(is.null(input$sgmnt))
    return()
  
  if(is.null(input$rpttn))
    return()
  
  if(is.null(input$repetition_radio))
    return()
  
  #....................................................................................
  #repetition input
  rep = input$rpttn
  
  #first section has the options but having the same result: one segment and one repetition 
  #1. one segment in single selection and one repetition in single selection
  #2. one segment in multiple selection and one repetition in single selection
  #3. one segment in single selection and one repetition in multiple selection
  #4. one segment in multiple selection and one repetition in multiple selection
  #....................................................................................
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
#initiates a variable to store the maximum number of frames for plotting

max_frames <- reactiveValues(n=1)

#creates the frame selection widget
output$frame <- renderUI({
  
  if(is.null(input$spkr))
    return()
  
  if(is.null(input$sgmnt))
    return()
  
  if(is.null(input$repetition_radio))
    return()
  
  if(is.null(input$rpttn))
    return()
  
  if(is.null(input$radio_frames))
    return()

  #....................................................................................
  #gets speaker label
  speaker_label <- input$spkr

  #....................................................................................
  #selects the dataframe with values
  #subsets file to the selected segments and omits the palate trace from the data frame
  fileIn <- importFiles()[[2]] %>%
    filter(speaker == speaker_label) %>%
    filter(segment != input$palateTraceName)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #segment
  #Segment Selection
  if(input$segment_selection == 3){
    #if All segments is selected
    seg <- "ALL"
    
    #selects all segments for this speaker
    segs <- fileIn %>%
      .$segment %>% sort() %>% unique()
  }else{
    #if NOT All segments are selected
    
    #gets the segment input
    seg <- input$sgmnt
    
    #gets the unique values of the segments
    segs <- fileIn %>%
      filter(segment == seg) %>%
      .$segment %>% sort() %>% unique()
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Gets repetition values
  #Repetition and Repetition Selection
  if (rep == "ALL"){
    #if All repetitions is selected
    
    #determines the number of repetitions based on the selection
    
    #initiates an empty array to store the repetition values
    tmp_rep_vals <- c()
    
    #iterates for each segment
    #finds the repetition value for each segment
    for(i in 1:length(segs)){
      #gets the repetition values for the segment(s) selected
      #gets the unique values of the repetitions
      reps_labels <- fileIn %>%
        filter(segment == segs[i]) %>%
        .$repetition %>% sort() %>% unique()
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
    
    #....................................................................................
    #sets the repetition values, initial and final
    tmp_data_reps <- c()
    
    #iterates for each segment
    for(i in 1:length(segs)){
      #gets repetitions per segment
      #gets unique labels
      reps_labels <- fileIn %>%
        filter(segment == segs[i]) %>%
        .$repetition %>% sort() %>% unique()
      
      #gets length of repetition labels
      reps <- length(reps_labels)
      
      #....................................................................................
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
      #....................................................................................
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
      
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      #initiates an empty vector for storing the frame values for plotting
      tmp_data_frames <- c()
      
      #if only one repetition is selected
      if(reps == 1){
        #gets the frames for one segment and one repetition
        tmp_data_frames <- fileIn %>%
          filter(segment == segs[i]) %>%
          filter(repetition == init_rep) %>%
          .$frame
      }else{
        #if multiple repetitions are selected
        
        #iterates from initial to final repetition labels
        for(j in init_rep:final_rep){
          #gets the frames for one segment and multiple repetitions
          tmp_data_frames[j] <- fileIn %>%
            filter(segment == segs[i]) %>%
            filter(repetition == j) %>%
            .$frame
        }
      }
      
      #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #frames
    #If ALL segments are selected
    #....................................................................................
    if(seg == "ALL"){
      #ALL (Exclusive)
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
      #....................................................................................
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
        #....................................................................................
        #ALL (Exclisve)
        if(input$repetition_radio == 5){
          #Exclusive selections (Single, Range and ALL)
          if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
            frames <- min(tmp_data_reps)
            #Inclusive selections (Single, Range and ALL)
          }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
            frames <- max(tmp_data_reps)
          }
          
          #ALL (Inclusive)
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
    
  }else{
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #if not all repetitions are selected
    
    tmp_data_reps <- c()
    
    empty_frame <- 0
    empty_frame_array <- 0
    #....................................................................................
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
      
      #....................................................................................
      tmp_data_frames <- c()
      
      #stores the frames in an array
      if(reps == 1){
        
        frames <- fileIn %>%
          filter(segment == segs[i]) %>%
          filter(repetition == init_rep) %>%
          .$frame
        
        if(length(frames) == 0){
          empty_frame <- 1
        }else{
          empty_frame <- 0
          tmp_data_frames <- frames
        }
      }else{
        tmp_frm_loop_val <- 1
        for(j in init_rep:final_rep){
          
          frames <- fileIn %>%
            filter(segment == segs[i]) %>%
            filter(repetition == j,) %>%
            .$frame
          
          if(length(frames) == 0){
            empty_frame <- 1
          }else{
            empty_frame <- 0
            tmp_data_frames[tmp_frm_loop_val] <- frames
            tmp_frm_loop_val <- tmp_frm_loop_val + 1
          }
        }
      }
      #....................................................................................
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
          #....................................................................................
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
            #....................................................................................
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
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #frames
    #If ALL segments are selected
    #....................................................................................
    if(seg == "ALL"){
      #ALL (Exclusive)
      if(input$repetition_radio == 1 || input$repetition_radio == 3){
        #Exclusive selections (Single, Range and ALL)
        if(input$radio_frames == 1 || input$radio_frames == 3 || input$radio_frames == 5){
          frames <- min(tmp_data_reps)
          #Inclusive selections (Single, Range and ALL)
        }else if(input$radio_frames == 2 || input$radio_frames == 4 || input$radio_frames == 6){
          frames <- max(tmp_data_reps)
        }
        #ALL (Incisive)
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
      #....................................................................................
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
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #....................................................................................
  if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 3) ||
     ((input$segment_selection == 2 && length(input$sgmnt) == 1) && 
      input$repetition_radio == 1 && input$radio_frames == 3) ||
     (input$segment_selection == 1 && input$repetition_radio != 1 && input$radio_frames > 4) ||
     ((input$segment_selection == 2 && length(input$sgmnt) == 1) && 
      input$repetition_radio != 1 && input$radio_frames > 4) ||
     (input$segment_selection == 3 && input$radio_frames > 4) ||
     (input$segment_selection == 2 && length(input$sgmnt) > 1 && input$radio_frames > 4)){
    
    selectInput(inputId = 'frm', label = 'Frame', choices = frames)
    #....................................................................................  
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
      #....................................................................................
      if((input$segment_selection == 1 && input$repetition_radio == 1 && input$radio_frames == 1) && 
         !is.null(cntxt_plt$n) ||
         (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 1 && 
          input$radio_frames == 1)){
        selectInput(inputId = 'frm', label = 'Frame', choices = c(1:frames), selected = cntxt_plt$n)
      }else{
        selectInput(inputId = 'frm', label = 'Frame', choices = c(1:frames), selected = 1)
      }
    }
    #....................................................................................  
  }else if((input$segment_selection == 1 && input$repetition_radio == 2 && 
            length(rep) > 1 && rep[1] == rep[2] && input$radio_frames == 3) ||
           (input$segment_selection == 2 && length(input$sgmnt) == 1 && input$repetition_radio == 2 &&
            length(rep) > 1 && rep[1] == rep[2] && input$radio_frames == 3)){
    
    return(NULL)
  }else{
    #.................................................................................... 
    sliderInput(inputId = 'frm', label = 'Frames',
                min = 1, max = frames, value = c(1,1), step= 1, animate = T)
  }
  
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section controls the previous and following buttons creation and behaviors
output$prevBttn = renderUI({
  if(is.null(input$sgmnt))
    return()
  
  if(is.null(input$repetition_radio))
    return()
  
  if(is.null(input$radio_frames))
    return()
  
  if(
    (input$segment_selection == 1 & input$repetition_radio == 1 & input$radio_frames == 1) ||
    (input$segment_selection == 2 & length(input$sgmnt) == 1 & input$repetition_radio == 1 & 
     input$radio_frames == 1)){
    bsButton("previousButton", label = "Previous", icon("chevron-circle-left"), style = "primary")
  }
  
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$follBttn = renderUI({
  
  if(is.null(input$sgmnt))
    return()
  
  if(is.null(input$repetition_radio))
    return()
  
  if(is.null(input$radio_frames))
    return()
  
  if(
    (input$segment_selection == 1 & input$repetition_radio == 1 & input$radio_frames == 1) ||
    (input$segment_selection == 2 & length(input$sgmnt) == 1 & input$repetition_radio == 1 & input$radio_frames == 1)){
    bsButton("nextButton", label = "Next", icon("chevron-circle-right"), style = "primary")
  }
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

cntxt_plt <- reactiveValues(n=NULL)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
observeEvent(input$nextButton, {
  cntxt_plt$n = as.numeric(input$frm) + 1
  if(cntxt_plt$n > max_frames$n){
    cntxt_plt$n = max_frames$n
  }
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
observeEvent(input$previousButton, {
  cntxt_plt$n = as.numeric(input$frm) - 1
  if(cntxt_plt$n < 1){
    cntxt_plt$n = 1
  }
})
