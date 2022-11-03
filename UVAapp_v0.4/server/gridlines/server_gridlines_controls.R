#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$speaker_GL = renderUI({
  if (is.null(importFiles))
    return()
  
  #speakers
  spkrs = importFiles()[[2]] %>% .$speaker %>% unique()
  selectInput('spkr_GL', 'Speaker', spkrs)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$segment_GL = renderUI({
  if (is.null(input$spkr_GL))
    return()
  
  #speaker
  speaker_label = input$spkr_GL
  
  #segments
  segs = importFiles()[[2]] %>%
    filter(segment != input$palateTraceName,) %>%
    filter(speaker == speaker_label) %>%
    .$segment %>% sort() %>% unique()
  
  selectInput('sgmnt_GL', label = 'Segment', c('All', segs), selected = segs[1], multiple = T)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
plotStatic_data_GL <- reactive({
  
  if (is.null(input$sgmnt_GL))
    return()
  
  frm_nmbr_per_contour <- 100
  
  speaker_label <- input$spkr_GL
  
  df_in = importFiles()[[1]]
  
  ms = as.character(input$radio_measures)
  
  commonNamesInFile <- unlist(strsplit('speaker segment repetition frame point coord', ' '))
  #Measurement*********************************************
  if (ms == "1"){
    #measurements in millimiters
    #d <- subset(d, select = -c(pixel))
    df_in <- df_in[c(commonNamesInFile, 'mm')]
    measure_val <- "mm"
  }else if (ms == "2"){
    #measurements in pixels
    df_in <- df_in[c(commonNamesInFile, 'pixel')]
    measure_val <- "pixel"
  }
  
  #SPEAKERS*********************************************
  fl <- df_in %>%
    filter(speaker %in% speaker_label)
  
  #Sets extrema
  x_xtrm <- fl %>% 
    filter(coord == "x") %>%
    .[[measure_val]] %>% sort() %>% unique() %>% as.numeric()
  x_xtrm_min = min(x_xtrm) - 2
  x_xtrm_max = max(x_xtrm) + 2
  
  y_xtrm <- fl %>% 
    filter(coord == "y") %>%
    .[[measure_val]] %>% sort() %>% unique() %>% as.numeric()
  y_xtrm_min = min(y_xtrm) - 2
  y_xtrm_max = max(y_xtrm) + 2
  
  extrema = c(x_xtrm_min,x_xtrm_max,y_xtrm_min,y_xtrm_max)
  
  #Palate*********************************************
  #If there is no palate trace
  if(is.null(pal_vals_data$d)){
    pal_vals = data.frame(matrix(nrow = 0, ncol = 0))
  }else{
    if(input$palate_plot_GL == T){
      pal_fl = subset(fl, segment == input$palateTraceName)
      
      pal_fl_x <- pal_fl %>% 
        filter(coord == "x") %>%
        .[[measure_val]] %>% sort() %>% as.numeric() %>% 
        approx(n = frm_nmbr_per_contour) %>% .$y
      
      pal_fl_y <- pal_fl %>% 
        filter(coord == "y") %>%
        .[[measure_val]] %>% sort() %>% as.numeric() %>% 
        approx(n = frm_nmbr_per_contour) %>% .$y
      
      tmp_seg_pal = rep(input$palateTraceName, length(pal_fl_x))
      tmp_rep_pal = rep(1, length(pal_fl_x))
      tmp_frm_pal = rep(1, length(pal_fl_x))
      tmp_pnt_pal = 1:length(pal_fl_x)
      
      pal_vals_data_in <- fl[fl$segment == input$palateTraceName,]
      
      tmp_df = pal_vals_data_in %>%
        filter(coord == 'x')
      
      colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
      tmp_df$y <- pal_vals_data_in[pal_vals_data_in$coord == 'y', ncol(tmp_df)]
      tmp_df <- tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
      
      pal_vals_data$d <- tmp_df
      
      pal_vals = data.frame(tmp_seg_pal, tmp_rep_pal, tmp_frm_pal, tmp_pnt_pal, pal_fl_x, pal_fl_y)
      names(pal_vals) = c("segment", "repetition", "frame", "point", "x", "y")
      
    }else{
      unique_labels <- sort(unique(fl$segment))
      if(length(grep(paste0('\\b', input$palateTraceName, '\\b'), unique_labels)) != 0){
        pal_vals_data_in <- fl[fl$segment == input$palateTraceName,]
        
        tmp_df = pal_vals_data_in %>%
          filter(coord == 'x')
        
        colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
        tmp_df$y <- pal_vals_data_in[pal_vals_data_in$coord == 'y', ncol(tmp_df)]
        tmp_df <- tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
        
        pal_vals_data$d <- tmp_df
        
        pal_vals = data.frame(matrix(nrow = 0, ncol = 0))
      }
      
    }
  }

  fl <- fl %>%
    filter(segment != input$palateTraceName)
  
  #SEGMENTS*********************************************
  if(input$sgmnt_GL != 'All'){
    seg <- input$sgmnt_GL
    fl <- fl %>% 
      filter(segment %in% seg)
  }
  
  prep_plot_df <- data.frame(matrix(ncol = 6, nrow = 0))
  names(prep_plot_df) <- c("segment", "repetition", "frame", "point", "x", "y")
  
  tmp_df = fl %>%
    filter(coord == 'x')
  
  colnames(tmp_df)[which(names(tmp_df) == measure_val)] <- "x"
  tmp_df$y <- fl[fl$coord == 'y', ncol(tmp_df)]
  tmp_df <- tmp_df[c("segment", "repetition", "frame", "point", "x", "y")]
  
  return(list(tmp_df, extrema, pal_vals))
})