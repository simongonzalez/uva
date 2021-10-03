

#This section creates the dataframe for plotting based on users input
plotStatic_data <- reactive({
  #returns NULL is the number of frames is NULL
  if (is.null(input$frm))
    return()
  
  #number of points per second
  frm_nmbr_per_contour <- 100
  
  #imports the plot data
  #gets the values of segment contours
  df_in <- importFiles()[[1]]
  
  #gets speaker input labels
  speaker_label <- input$spkr
  
  #gets input measurement unit
  ms <- as.character(input$radio_measures)
  commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
  #Measurement*********************************************
  if (ms == "1"){
    #measurements in millimeters
    #d <- subset(d, select = -c(pixel))
    df_in <- df_in[c(commonNamesInFile, 'mm')]
    measure_val <- "mm"
  }else if (ms == "2"){
    #measurements in pixels
    #d <- subset(d, select = -c(mm))
    df_in <- df_in[c(commonNamesInFile, 'pixel')]
    measure_val <- "pixel"
  }
  
  #SPEAKERS*********************************************
  fl <- df_in %>%
    filter(speaker %in% speaker_label)
  
  #Sets extrema points for the plot (xy limits)
  x_xtrm <- as.numeric(unique(fl[fl$coord == "x", measure_val]))
  x_xtrm_min <- min(x_xtrm) - 2
  x_xtrm_max <- max(x_xtrm) + 2
  
  y_xtrm <- as.numeric(unique(fl[fl$coord == "y", measure_val]))
  y_xtrm_min <- min(y_xtrm) - 2
  y_xtrm_max <- max(y_xtrm) + 2
  
  extrema <- c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
  
  #Palate*********************************************
  #sets the palate trace plot
  if(input$palate_plot == TRUE){
    pal_vals_data_in <- fl[fl$segment == input$palateTraceName,]
    
    tmp_df = pal_vals_data_in %>%
      filter(coord == 'x')
    
    colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
    tmp_df$y <- pal_vals_data_in[pal_vals_data_in$coord == 'y', ncol(tmp_df)]
    tmp_df <- tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
    
    pal_vals_datainspection$d <- tmp_df
    
    pal_vals <- tmp_df
    
  }else if(input$palate_plot == FALSE){
    pal_vals = data.frame(matrix(nrow = 0, ncol = 0))
    # unique_labels <- sort(unique(fl$segment))
    # if(length(grep(paste0('\\b', input$palateTraceName, '\\b'), unique_labels)) != 0){
    #   pal_vals_data_in <- fl[fl$segment == input$palateTraceName,]
    #   
    #   tmp_df = pal_vals_data_in %>%
    #     filter(coord == 'x')
    #   
    #   colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
    #   tmp_df$y <- pal_vals_data_in[pal_vals_data_in$coord == 'y', ncol(tmp_df)]
    #   tmp_df <- tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
    #   
    #   pal_vals_datainspection$d <- tmp_df
    # 
    # }else{
    #   pal_vals = data.frame(matrix(nrow = 0, ncol = 0))
    # }
  }
  
  fl <- fl %>%
    filter(speaker == speaker_label) %>%
    filter(segment != input$palateTraceName)
  
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
      
      tmp_fr <- fl %>%
        filter(segment == segment_lbl[tmp_seg]) %>%
        filter(repetition == tmp_frm_cntr) %>%
        .$frame
      
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