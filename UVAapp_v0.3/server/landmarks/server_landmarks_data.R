#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section creates the dataframe for plotting based on users input
plotStatic_data_LM <- reactive({
  #returns NULL is the number of frames is NULL
  if(is.null(input$frm_LM))
    return()
  
  if(is.null(input$radio_measures))
    return()
  
  #number of points per second
  frm_nmbr_per_contour = 100
  
  #imports the plot data
  #gets the values of segment contours
  df_in <- importFiles()[[1]]
  
  #gets speaker input labels
  speaker_label <- input$spkr_LM
  
  #gets input measurement unit
  ms <- as.character(input$radio_measures)

  commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
  #....................................................................................
  #Measurement
  if (ms == 1){
    #measurements in millimiters
    #d <- subset(d, select = -c(pixel))
    df_in <- df_in[c(commonNamesInFile, 'mm')]
    measure_val <- "mm"
  }else if (ms == 2){
    #measurements in pixels
    #d <- subset(d, select = -c(mm))
    df_in <- df_in[c(commonNamesInFile, 'pixel')]
    measure_val <- "pixel"
  }
  #....................................................................................
  #SPEAKERS
  fl <- df_in %>%
    filter(speaker %in% speaker_label)
  
  #Sets extrema points for the plot (xy limits)
  x_xtrm <- fl %>% 
    filter(coord == "x") %>%
    .[[measure_val]] %>% sort() %>% unique()
  x_xtrm_min <- min(x_xtrm) - 2
  x_xtrm_max <- max(x_xtrm) + 2
  
  y_xtrm <- fl %>% 
    filter(coord == "y") %>%
    .[[measure_val]] %>% sort() %>% unique()
  y_xtrm_min <- min(y_xtrm) - 2
  y_xtrm_max <- max(y_xtrm) + 2
  
  extrema <- c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
  #....................................................................................
  #Palate
  #sets the palate trace plot
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
  
  fl <- subset(fl, speaker == speaker_label & segment != "pal")
  #....................................................................................
  #SEGMENTS
  seg <- input$sgmnt_LM
  fl <- fl[fl$segment %in% seg, ]
  #....................................................................................
  #REPETITIONS
  rep <- input$rpttn_LM
  fl <- fl[fl$repetition %in% rep, ]
  
  frm <- as.numeric(input$frm_LM)
  
  #get maximun frame number
  #....................................................................................
  # FRAMES
  max_frm_nmbr <- fl %>%
    filter(segment == seg) %>%
    filter(repetition == rep) %>%
    .$frame %>% sort() %>% unique() %>% max()
  
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
    tmp_frm_array_x <- fl %>%
      filter(segment == seg) %>%
      filter(repetition == rep) %>%
      filter(frame == tmp_fr_id_loop) %>%
      filter(coord == "x") %>%
      .[[measure_val]] %>% as.numeric()
    
    tmp_frm_array_y <- fl %>%
      filter(segment == seg) %>%
      filter(repetition == rep) %>%
      filter(frame == tmp_fr_id_loop) %>%
      filter(coord == "y") %>%
      .[[measure_val]] %>% as.numeric()
    
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

