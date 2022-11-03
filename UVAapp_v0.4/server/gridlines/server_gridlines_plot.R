#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
plotStatic_GL_plotdata <- reactiveValues()
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#observeEvent(input$setGridlinesBttn,{
appendName_value <- reactiveValues(d=NULL)
output$plotStatic_GL <- renderPlot({
  
  if(is.null(input$sgmnt_GL))
    return()
  
  #isolate({
  
  #Set name of the iterated speaker and segment
  appendName <- paste0(input$spkr_GL, '_', paste(input$sgmnt_GL, collapse = '_'))
  
  appendName_value$d <- appendName
  
  fl = plotStatic_data_GL()
  extrema = fl[[2]]
  
  if(!is.null(pal_vals_data$d)){
    pal = pal_vals_data$d
  }
  
  #pal = fl[[3]]
  fl = fl[[1]]
  
  ttl = 'get_plot_title()'
  
  pal_colour = input$palate_colour
  pal_line_type = as.numeric(input$line_type_palate)
  tongue_line_type = as.numeric(input$line_type_tongue)
  
  segs = unique(fl$segment)
  segs_nmbr = length(segs)
  
  pallete_colour = c()
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #This section creates the basic plot
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
  if(!is.null(pal_vals_data$d)){
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
    
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
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
      #add origin point-------------------------------------------------------------------------
      
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
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
    
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    
    #add fieldview points------------------------------------------------------------------------
    #if the manual origin point is not NULL OR the automatic origin point is not NULL
    if(!is.null(origin_point_GL$x) || !is.null(origin_point_automatic_GL$x)){
      #if the selection of the fieldview is NOT manual
      if(input$define_fieldview_GL != 'Manual'){
        #angle defined fieldview
        
        #checks whether the palate trace is included in the plotting
        if(!is.null(pal_vals_data$d)){
          if(input$include_palate_GL){
            all_contours <- rbind(fl, pal)
          }else{
            all_contours <- fl
          }
        }else{
          all_contours <- fl
        }
        
        #round the xy of the origin line
        tmp_line_origin_x <- round(x, digits = 2)
        tmp_line_origin_y <- round(y, digits = 2)
        
        if(!is.null(origin_point_GL$x) & is.null(origin_point_automatic_GL$x)){
          #Manual selection............................................................
          tmp_origin_point_x = round(origin_point_GL$x, digits = 2)
          tmp_origin_point_y = round(origin_point_GL$y, digits = 2)
        }else if(is.null(origin_point_GL$x) & !is.null(origin_point_automatic_GL$x)){
          #Automatic selection.........................................................
          tmp_origin_point_x = round(origin_point_automatic_GL$x, digits = 2)
          tmp_origin_point_y = round(origin_point_automatic_GL$y, digits = 2)
        }
        
        #sets the maximum default distance
        tmp_default_distance <- 10000
        
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        
        #calculate left line=================================================================
        #if the left line is not defined by an angle (Narrow and Wide)
        #--------------------------------------------------------------------------------------
        #if(input$define_fieldview_GL != 'Angle'){
        #create label_plotting_df
        label_plotting_df_GL_names <- c('segment', 'left_x', 'left_y', 'left_label', 
                                        'right_x', 'right_y', 'right_label', 'left_angle', 'right_angle', 'distance')
        label_plotting_df_GL = data.frame(matrix(ncol = length(label_plotting_df_GL_names), nrow = 1))
        names(label_plotting_df_GL) = label_plotting_df_GL_names
        
        #calculate left line==================================================================
        #This section calculates the left line of the fieldview
        #within server_plotstatic
        
        #calculate left line=================================================================
        #initial common angle
        tmp_initial_angle <- 179
        #final common angle
        tmp_final_angle <- 91
        #sequence from initial to final angle
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
        
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #Finds the left angle.....................................................................
        withProgress(message = 
                       paste0('Finding left angle for Speaker: ', 
                              input$spkr_GL, ' and segment: ', input$sgmnt_GL, '.'), 
                     value = 0, {
                       # Number of times we'll go through the loop
                       n <- tmp_fieldview_angles
                       
                       #finds the interaction in all lines for all segments
                       
                       cntr_i <- 1
                       for(i_n in 1:n){
                         #creates a temporary line from angle i
                         tmp_leftline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                                tmp_default_distance, 
                                                                tmp_final_angle_all[i_n]*-1)
                         
                         tmp_leftline_end_x <- tmp_leftline_from_function[1]
                         tmp_leftline_end_y <- tmp_leftline_from_function[2]
                         
                         #find angles
                         tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                                 c(tmp_leftline_end_x, tmp_leftline_end_y))
                         tmp_angle_left_first <- tmp_angle[[1]]
                         tmp_angle_right_first <- tmp_angle[[2]]
                         
                         #find intersections
                         #create label_plotting_df
                         
                         #iteration per segment
                         for(segmenti in unique(all_contours$segment)){
                           tmp_df_intersection_segment <- all_contours %>%
                             filter(segment == segmenti)
                           for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                             tmp_df_intersection_repetition <- tmp_df_intersection_segment %>%
                               filter(repetition == repetitioni)
                             for(framei in unique(tmp_df_intersection_repetition$frame)){
                               tmp_df_intersection_i <- tmp_df_intersection_repetition %>%
                                 filter(frame == framei)
                               
                               iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                           tmp_df_intersection_i$y))), 
                                                                           1)))
                               
                               if(!is.null(tmp_leftline_from_function)){
                                 tmp_df <- data.frame(segment = segmenti)
                                 
                                 if(is.na(tmp_leftline_end_x)){
                                   seadfare <- 1
                                 }
                                 #left angle-------------------------------------------------------------
                                 basis_line_left <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, 
                                                                                              tmp_leftline_end_x),
                                                                                            c(tmp_line_origin_y, 
                                                                                              tmp_leftline_end_y)))), 1)))
                                 
                                 #if the temporal line intersects with the tongue contour
                                 if(!is.null(gIntersection(iterated_contour, basis_line_left)$x)){
                                   
                                   int_x <- round(gIntersection(iterated_contour, basis_line_left)$x, digits = 2)
                                   int_y <- round(gIntersection(iterated_contour, basis_line_left)$y, digits = 2)
                                   
                                   tmp_df$angle <- tmp_final_angle_all[i_n]
                                   tmp_df$segment <- segmenti
                                   tmp_df$repetition <- repetitioni
                                   tmp_df$frame <- framei
                                   tmp_df$left_x <-  int_x
                                   tmp_df$left_y <-  int_y
                                   
                                   #new editing
                                   tmpInternalDistance <- sqrt((tmp_line_origin_x-int_x)^2+(tmp_line_origin_y-int_y)^2)
                                   
                                   left_int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                                   
                                   tmp_df$left_label <- left_int_label
                                   
                                   #store the largest distance
                                   tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                                   
                                   tmp_df$distance_int <- tmp_store_distance
                                   tmp_df$distance <- store_larger_distance$n
                                   
                                   if(tmp_store_distance > store_larger_distance$n){
                                     
                                     store_larger_distance$n <- tmp_store_distance
                                   }
                                 }else{
                                   #if the temporal line does not intersect with the tongue contour
                                   int_x <- 0
                                   int_y <- 0
                                   
                                   tmp_df$angle <- tmp_final_angle_all[i_n]
                                   tmp_df$segment <- segmenti
                                   tmp_df$repetition <- repetitioni
                                   tmp_df$frame <- framei
                                   tmp_df$left_x <-  int_x
                                   tmp_df$left_y <-  int_y
                                   
                                   left_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                                   
                                   tmp_df$left_label <- left_int_label
                                   
                                   #store the largest distance
                                   tmp_store_distance <- 0
                                   tmp_df$distance_int <- tmp_store_distance
                                   tmp_df$distance <- store_larger_distance$n
                                 }
                                 
                                 if(cntr_i == 1){
                                   tmp_label_plotting_df_GL <- tmp_df
                                   cntr_i <- 2
                                 }else{
                                   tmp_label_plotting_df_GL <- rbind(tmp_label_plotting_df_GL, tmp_df)
                                 }
                                 
                               }
                               
                             }
                           }
                         }
                         
                         complete_intersections <- nrow(tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$left_x != 0,])
                         incProgress(1/n, detail = paste("PLEASE WAIT"))
                       }
                       
                     })
        
        #assigns the intersection table
        tmp_label_plotting_df_GL_left <- tmp_label_plotting_df_GL
        
        #keeps the smallest left angle
        tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL %>%
          filter(left_x != 0) %>% #gets only where there has been an intersection
          filter(left_x == min(left_x)) #gets the smallest angle
        
        label_plotting_df_GL$segment <- 'multiple'
        label_plotting_df_GL$left_x <- tmp_label_plotting_df_GL$left_x
        label_plotting_df_GL$left_y <- tmp_label_plotting_df_GL$left_y
        
        label_plotting_df_GL$left_label <- paste0('x=', round(tmp_label_plotting_df_GL$left_x), ', ', 
                                                  'y=', round(tmp_label_plotting_df_GL$left_y))
        label_plotting_df_GL$left_angle <- tmp_label_plotting_df_GL$angle
        label_plotting_df_GL$distance <- tmp_label_plotting_df_GL$distance 
        
        #calculate right line======================================================
        #This section calculates the left line of the fieldview
        #within server_plotstatic
        
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        
        #calculate right line========================================================
        tmp_initial_angle <- 1
        tmp_final_angle <- 90
        tmp_final_angle_all <- seq(tmp_initial_angle, tmp_final_angle)
        tmp_fieldview_angles <- length(tmp_final_angle_all)
        tmp_final_angle_all_cntr <- 1
        
        complete_intersections <- 0
        
        if(input$define_fieldview_GL == 'Narrow'){
          tmp_total_iterated_contours <- nrow(all_contours[all_contours$point == 1,])
        }else if(input$define_fieldview_GL == 'Wide'){
          tmp_total_iterated_contours <- 1
        }
        
        withProgress(message = 
                       paste0('Finding right angle for Speaker: ', 
                              input$spkr_GL, ' and segment: ', input$sgmnt_GL, '.'), 
                     value = 0, {
                       # Number of times we'll go through the loop
                       n <- tmp_fieldview_angles
                       
                       cntr_i <- 1
                       for(i_n in 1:n){
                         tmp_rightline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                                 tmp_default_distance, 
                                                                 tmp_final_angle_all[i_n]*-1)
                         tmp_rightline_end_x <- tmp_rightline_from_function[1]
                         tmp_rightline_end_y <- tmp_rightline_from_function[2]
                         
                         #find angles
                         tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                                 c(tmp_rightline_end_x, tmp_rightline_end_y))
                         tmp_angle_left_second <- tmp_angle[[1]]
                         tmp_angle_right_second <- tmp_angle[[2]]
                         
                         
                         #find itersections
                         #create label_plotting_df
                         
                         #iteration per segment
                         
                         for(segmenti in unique(all_contours$segment)){
                           tmp_df_intersection_segment <- all_contours %>%
                             filter(segment == segmenti)
                           for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                             tmp_df_intersection_repetition <- tmp_df_intersection_segment %>%
                               filter(repetition == repetitioni)
                             for(framei in unique(tmp_df_intersection_repetition$frame)){
                               tmp_df_intersection_i <- tmp_df_intersection_repetition %>% 
                                 filter(frame == framei)
                               
                               iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                           tmp_df_intersection_i$y))), 1)))
                               
                               if(!is.null(tmp_rightline_from_function)){
                                 tmp_df <- data.frame(segment = segmenti)
                                 
                                 if(is.na(tmp_rightline_end_x)){
                                   seadfare <- 1
                                 }
                                 
                                 #right angle------------------------------------------------------
                                 basis_line_right <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_line_origin_x, 
                                                                                               tmp_rightline_end_x),
                                                                                             c(tmp_line_origin_y, 
                                                                                               tmp_rightline_end_y)))), 1)))
                                 
                                 if(!is.null(gIntersection(iterated_contour, basis_line_right)$x)){
                                   
                                   int_x <- round(gIntersection(iterated_contour, basis_line_right)$x, digits = 2)
                                   int_y <- round(gIntersection(iterated_contour, basis_line_right)$y, digits = 2)
                                   
                                   tmp_df$angle <- tmp_final_angle_all[i_n]
                                   tmp_df$segment <- segmenti
                                   tmp_df$repetition <- repetitioni
                                   tmp_df$frame <- framei
                                   tmp_df$right_x <- int_x
                                   tmp_df$right_y <- int_y
                                   
                                   right_int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                                   
                                   tmp_df$right_label <- right_int_label
                                   
                                   #store the largest distance
                                   tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                                   tmp_df$distance_int <- tmp_store_distance
                                   tmp_df$distance <- store_larger_distance$n
                                   
                                   if(tmp_store_distance > store_larger_distance$n){
                                     store_larger_distance$n <- tmp_store_distance
                                   }
                                 }else{
                                   int_x <- 0
                                   int_y <- 0
                                   
                                   tmp_df$angle <- tmp_final_angle_all[i_n]
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
                                   tmp_df$distance_int <- tmp_store_distance
                                   tmp_df$distance <- store_larger_distance$n
                                 }
                                 
                                 if(cntr_i == 1){
                                   tmp_label_plotting_df_GL <- tmp_df
                                   cntr_i <- 2
                                 }else{
                                   tmp_label_plotting_df_GL <- rbind(tmp_label_plotting_df_GL, tmp_df)
                                 }
                                 
                               }
                               
                             }
                           }
                         }
                         
                         complete_intersections <- nrow(tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$right_x != 0,])
                         incProgress(1/n, detail = paste("PLEASE WAIT"))
                       }
                       
                     })
        
        #assigns the intersection table
        tmp_label_plotting_df_GL_right <- tmp_label_plotting_df_GL
        
        
        #Puts together both tables in a single dataframe
        tmp_label_plotting_df_GL_left <- tmp_label_plotting_df_GL_left %>%
          mutate(speaker = input$spkr_GL, angle_type = 'left') %>%
          rename(x = left_x, y = left_y) %>%
          dplyr::select(-one_of(c('left_label')))
        
        tmp_label_plotting_df_GL_right <- tmp_label_plotting_df_GL_right %>%
          mutate(speaker = input$spkr_GL, angle_type = 'right') %>%
          rename(x = right_x, y = right_y) %>%
          dplyr::select(-one_of(c('right_label')))
        
        intersections_data_all <- bind_rows(tmp_label_plotting_df_GL_left, tmp_label_plotting_df_GL_right) %>%
          dplyr::select(speaker, angle_type, angle, segment, repetition, frame, x, y, distance_int, distance) #%>%
        #arrange(speaker, angle_type, angle, repetition, frame)
        
        #saves the dataframe to the working folder
        
        
        write.csv(intersections_data_all, 
                  paste0('./workingFiles/intersections_data_all_', 
                         appendName, '.csv'), 
                  row.names = FALSE)
        
        # dftestin <- read.csv('./workingFiles/intersections_data_all.csv')
        # tmp_label_plotting_df_GL <- dftestin
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        
        tmp_label_plotting_df_GL <- tmp_label_plotting_df_GL %>%
          filter(right_x != 0) %>% #gets only where there has been an intersection
          filter(right_x == max(right_x)) #gets the largest angle
        
        label_plotting_df_GL$segment <- 'multiple'
        label_plotting_df_GL$right_x <- tmp_label_plotting_df_GL$right_x
        label_plotting_df_GL$right_y <- tmp_label_plotting_df_GL$right_y
        label_plotting_df_GL$right_label <- paste0('x=', round(tmp_label_plotting_df_GL$right_x), ', ', 
                                                   'y=', round(tmp_label_plotting_df_GL$right_y))
        label_plotting_df_GL$right_angle <- tmp_label_plotting_df_GL$angle
        label_plotting_df_GL$distance <- tmp_label_plotting_df_GL$distance 
        
        #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        
        #If both reference angles are not NULL
        if(!is.null(tmp_angle_right_first) & !is.null(tmp_angle_right_second)){
          if(tmp_angle_right_first < 0){
            tmp_angle_right_first = tmp_angle_right_first * -1
          }
          
          if(tmp_angle_right_second < 0){
            tmp_angle_right_second = tmp_angle_right_second * -1
          }
          
          mid_angle <- tmp_angle_right_second + ((tmp_angle_right_first - tmp_angle_right_second) / 2)
          print(paste0('here1:', input$sgmnt_GL, ':', mid_angle))
        }
        
        if(input$define_fieldview_GL == 'Angle'){
          
          #If the lines are selected based on the ANGLE
          
          #calculate left right lines=========================================================
          #This section calculates both left and right line of the fieldview
          
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #calculate left right line===========================================================
          tmp_leftline_from_function <- NULL
          #if the left angle input is not NULL
          if(!is.null(input$leftlineangle_fieldview_GL)){
            
            #if the left angle input is not == 0
            if(input$leftlineangle_fieldview_GL != 0){
              
              tmp_leftline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                     tmp_default_distance, 
                                                     input$leftlineangle_fieldview_GL*-1)
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
                store_larger_distance$n <- tmp_store_distance
              }
            }
          }
          
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
          #calculate right line===========================================================
          tmp_rightline_from_function <- NULL
          
          if(!is.null(input$rightlineangle_fieldview_GL)){
            if(input$rightlineangle_fieldview_GL != 0){
              tmp_rightline_from_function <- lines_fn(tmp_line_origin_x, tmp_line_origin_y, 
                                                      tmp_default_distance, 
                                                      input$rightlineangle_fieldview_GL*-1)
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
            print(paste0('here2:', input$sgmnt_GL, ':', mid_angle))
          }
          
          
          
          #find intersections
          #create label_plotting_df
          label_plotting_df_GL_names <- c('segment', 'left_x', 'left_y', 'left_label', 
                                          'right_x', 'right_y', 'right_label', 'left_angle', 'right_angle', 'distance')
          label_plotting_df_GL <- data.frame(matrix(ncol = length(label_plotting_df_GL_names), nrow = 1))
          names(label_plotting_df_GL) <- label_plotting_df_GL_names
          
          tmp_label_plotting_df_GL_leftangle <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            filter(angle >= tmp_angle_right_first & angle <= tmp_angle_right_second) %>%
            filter(angle == max(angle)) %>%
            filter(x == min(x))
          
          tmp_label_plotting_df_GL_rightangle <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            filter(angle >= tmp_angle_right_first & angle <= tmp_angle_right_second) %>%
            filter(angle == min(angle)) %>%
            filter(x == max(x))
          
          label_plotting_df_GL$segment <- 'multiple'
          
          label_plotting_df_GL$left_x <- tmp_label_plotting_df_GL_leftangle$x
          label_plotting_df_GL$left_y <- tmp_label_plotting_df_GL_leftangle$y
          label_plotting_df_GL$left_label <- paste0('x=', round(tmp_label_plotting_df_GL_leftangle$x), ', ', 
                                                    'y=', round(tmp_label_plotting_df_GL_leftangle$y))
          label_plotting_df_GL$left_angle <- tmp_label_plotting_df_GL_leftangle$angle
          
          label_plotting_df_GL$right_x <- tmp_label_plotting_df_GL_rightangle$x
          label_plotting_df_GL$right_y <- tmp_label_plotting_df_GL_rightangle$y
          label_plotting_df_GL$right_label <- paste0('x=', round(tmp_label_plotting_df_GL_rightangle$x), ', ', 
                                                     'y=', round(tmp_label_plotting_df_GL_rightangle$y))
          label_plotting_df_GL$right_angle <- tmp_label_plotting_df_GL_rightangle$angle
          
          label_plotting_df_GL$distance <- 
            max(c(tmp_label_plotting_df_GL_leftangle$distance,
                  tmp_label_plotting_df_GL_rightangle$distance))
          
          store_larger_distance$n <- label_plotting_df_GL$distance
        }
        
        #label_plotting_df_GL_test <- 
        
        #plot non manual lines================================================================
        #This section plots automatic lines of the fieldview
        
        #View(label_plotting_df_GL)
        if((length(unique(label_plotting_df_GL$left_x)) == 1 & unique(label_plotting_df_GL$left_x) == 0) && 
           (length(unique(label_plotting_df_GL$right_x)) == 1 & unique(label_plotting_df_GL$right_x) == 0)){
          showNotification("No intersection with the specified angles")
        }else{
          if(length(unique(label_plotting_df_GL$left_x)) == 1 & unique(label_plotting_df_GL$left_x) == 0){
            showNotification("No intersection with the specified LEFT angle")
          }else if(length(unique(label_plotting_df_GL$right_x)) == 1 & 
                   unique(label_plotting_df_GL$right_x) == 0){
            showNotification("No intersection with the specified RIGHT angle")
          }else{
            showNotification("READY")
            
            #set text xy
            if(input$radio_measures == "1"){
              #millimiters
              symbol_sep_measure = 5
            }else if(input$radio_measures == "2"){
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
            #-----------------------------------------------------------------------------
            
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
            #------------------------------------------------------------------------------------
            
            #add point
            p = p + geom_point(data = data.frame(x = max_x_right_angle$right_x, 
                                                 y = max_x_right_angle$right_y),
                               aes(x = x, y = y), size = 7,
                               colour = "black", show.legend = FALSE)
            
          }
          
        } 
        
      }else{
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
            
            #--------------------------------------------------------------------------
            #find angles
            tmp_angle = find_angle(c(x,y), c(tmp_origin_point_x,tmp_origin_point_y))
            tmp_angle_left = round(tmp_angle[[1]])
            tmp_angle_right = round(tmp_angle[[2]])
            
            #set text xy
            if(input$radio_measures == "1"){
              #millimeters
              symbol_sep_measure = 5
            }else if(input$radio_measures == "2"){
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
            #-------------------------------------------------------------------------------
            
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
            #----------------------------------------------------------------------------------
            #find angles
            tmp_angle_first = find_angle(c(x1,y1), c(tmp_origin_point_x,tmp_origin_point_y))
            tmp_angle_left_first = round(tmp_angle_first[[1]])
            tmp_angle_right_first = round(tmp_angle_first[[2]])
            
            #set text xy
            if(input$radio_measures == "1"){
              #millimeters
              symbol_sep_measure = 5
            }else if(input$radio_measures == "2"){
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
            #=====================================================================================
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
            
            #....................................................................................
            #plot the middle angle
            mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
            print(paste0('here3:', input$sgmnt_GL, ':', mid_angle))
            
            p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                             label= paste0('M: ', mid_angle, '°'))
            #------------------------------------------------------------------------------
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
            
            #-----------------------------------------------------------------------------------
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
            if(input$radio_measures == "1"){
              #millimeters
              symbol_sep_measure = 5
            }else if(input$radio_measures == "2"){
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
            #=======================================================================================
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
            
            #...............................................................................
            #plot the middle angle
            mid_angle = abs(tmp_angle_right_first - tmp_angle_right_second)
            print(paste0('here4:', input$sgmnt_GL, ':', mid_angle))
            
            p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                             label= paste0('M: ', mid_angle, '°'))
            #---------------------------------------------------------------------------------
            
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
            #--------------------------------------------------------------------------
            #find angles
            tmp_angle_first = find_angle(c(x1,y1), c(tmp_origin_point_x,tmp_origin_point_y))
            tmp_angle_left_first = round(tmp_angle_first[[1]])
            tmp_angle_right_first = round(tmp_angle_first[[2]])
            
            #set text xy
            if(input$radio_measures == "1"){
              #millimeters
              symbol_sep_measure = 5
            }else if(input$radio_measures == "2"){
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
            #=============================================================================
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
            
            #..........................................................................
            #plot the middle angle
            mid_angle = tmp_angle_right_first + ((tmp_angle_right_second-tmp_angle_right_first)/2)
            print(paste0('here5:', input$sgmnt_GL, ':', mid_angle))
            
            p = p + annotate("text", x=tmp_origin_point_x, y=tmp_origin_point_y - symbol_sep_measure, 
                             label= paste0('M: ', mid_angle, '°'))
            #---------------------------------------------------------------------------
            
            
            p = p + geom_point(data = data.frame(x = x1, y = y1),
                               aes(x = x1, y = y1), size = 7,
                               colour = "#3CB371", show.legend = FALSE)
            
            p = p + geom_point(data = data.frame(x = x, y = y),
                               aes(x = x, y = y), size = 7,
                               colour = "#3CB371", show.legend = FALSE)
            
            #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            
            #Calculate the intersections for the manual selection
            final_right_angle <- tmp_angle_right_first
            final_left_angle <- tmp_angle_right_second
            
            manual_left_angle$d <- final_left_angle 
            manual_right_angle$d <- final_right_angle
            
            #checks whether the palate trace is included in the plotting
            if(!is.null(pal_vals_data$d)){
              if(input$include_palate_GL){
                all_contours <- rbind(fl, pal)
              }else{
                all_contours <- fl
              }
            }else{
              all_contours <- fl
            }
            
            #calculate intersections
            
            #calculate left line==================================================================
            #This section calculates the left line of the fieldview
            #within server_plotstatic
            
            #calculate left line=================================================================
            #initial common angle
            tmp_initial_angle <- final_left_angle
            #final common angle
            tmp_final_angle <- final_right_angle
            #sequence from initial to final angle
            tmp_final_angle_all <- seq(tmp_initial_angle, tmp_final_angle)
            #length of angles from initial to final
            tmp_fieldview_angles <- length(tmp_final_angle_all)
            #angle counter
            tmp_final_angle_all_cntr <- 1
            
            #while counter to check the number of iterated contours
            complete_intersections <- 0
            
            #the algorithm takes into account all areas
            tmp_total_iterated_contours <- 1
            
            tmp_default_distance <- 10000
            
            #Finds angles.....................................................................
            withProgress(message = 
                           paste0('Finding angles for Speaker: ', 
                                  input$spkr_GL, ' and segment: ', input$sgmnt_GL, '.'), 
                         value = 0, {
                           # Number of times we'll go through the loop
                           n <- tmp_fieldview_angles
                           
                           #finds the interaction in all lines for all segments
                           
                           cntr_i <- 1
                           for(i_n in 1:n){
                             #creates a temporary line from angle i
                             tmp_leftline_from_function <- lines_fn(tmp_origin_point_x, tmp_origin_point_y, 
                                                                    tmp_default_distance, 
                                                                    tmp_final_angle_all[i_n]*-1)
                             
                             tmp_leftline_end_x <- tmp_leftline_from_function[1]
                             tmp_leftline_end_y <- tmp_leftline_from_function[2]
                             
                             #find angles
                             tmp_angle <- find_angle(c(tmp_origin_point_x,tmp_origin_point_y), 
                                                     c(tmp_leftline_end_x, tmp_leftline_end_y))
                             tmp_angle_left_first <- tmp_angle[[1]]
                             tmp_angle_right_first <- tmp_angle[[2]]
                             
                             #find intersections
                             #create label_plotting_df
                             
                             #iteration per segment
                             for(segmenti in unique(all_contours$segment)){
                               tmp_df_intersection_segment <- all_contours %>%
                                 filter(segment == segmenti)
                               for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                                 tmp_df_intersection_repetition <- tmp_df_intersection_segment %>%
                                   filter(repetition == repetitioni)
                                 for(framei in unique(tmp_df_intersection_repetition$frame)){
                                   tmp_df_intersection_i <- tmp_df_intersection_repetition %>%
                                     filter(frame == framei)
                                   
                                   iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                               tmp_df_intersection_i$y))), 
                                                                               1)))
                                   
                                   if(!is.null(tmp_leftline_from_function)){
                                     tmp_df <- data.frame(segment = segmenti)
                                     
                                     if(is.na(tmp_leftline_end_x)){
                                       seadfare <- 1
                                     }
                                     #left angle-------------------------------------------------------------
                                     basis_line_left <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_origin_point_x, 
                                                                                                  tmp_leftline_end_x),
                                                                                                c(tmp_origin_point_y, 
                                                                                                  tmp_leftline_end_y)))), 1)))
                                     
                                     #if the temporal line intersects with the tongue contour
                                     if(!is.null(gIntersection(iterated_contour, basis_line_left)$x)){
                                       
                                       int_x <- round(gIntersection(iterated_contour, basis_line_left)$x, digits = 2)
                                       int_y <- round(gIntersection(iterated_contour, basis_line_left)$y, digits = 2)
                                       
                                       tmp_df$angle <- tmp_final_angle_all[i_n]
                                       tmp_df$segment <- segmenti
                                       tmp_df$repetition <- repetitioni
                                       tmp_df$frame <- framei
                                       tmp_df$left_x <-  int_x
                                       tmp_df$left_y <-  int_y
                                       
                                       #new editing
                                       tmpInternalDistance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                                       
                                       left_int_label <- format(round(tmpInternalDistance, 2), nsmall = 2)
                                       
                                       tmp_df$left_label <- left_int_label
                                       
                                       #store the largest distance
                                       tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                                       
                                       tmp_df$distance_int <- tmp_store_distance
                                       tmp_df$distance <- store_larger_distance$n
                                       
                                       if(tmp_store_distance > store_larger_distance$n){
                                         
                                         store_larger_distance$n <- tmp_store_distance
                                       }
                                     }else{
                                       #if the temporal line does not intersect with the tongue contour
                                       int_x <- 0
                                       int_y <- 0
                                       
                                       tmp_df$angle <- tmp_final_angle_all[i_n]
                                       tmp_df$segment <- segmenti
                                       tmp_df$repetition <- repetitioni
                                       tmp_df$frame <- framei
                                       tmp_df$left_x <-  int_x
                                       tmp_df$left_y <-  int_y
                                       
                                       left_int_label <- paste0('x=', 0, ', ', 'y=', 0)
                                       
                                       tmp_df$left_label <- left_int_label
                                       
                                       #store the largest distance
                                       tmp_store_distance <- 0
                                       tmp_df$distance_int <- tmp_store_distance
                                       tmp_df$distance <- store_larger_distance$n
                                     }
                                     
                                     if(cntr_i == 1){
                                       tmp_label_plotting_df_GL <- tmp_df
                                       cntr_i <- 2
                                     }else{
                                       tmp_label_plotting_df_GL <- rbind(tmp_label_plotting_df_GL, tmp_df)
                                     }
                                     
                                   }
                                   
                                 }
                               }
                             }
                             
                             complete_intersections <- 
                               nrow(tmp_label_plotting_df_GL[tmp_label_plotting_df_GL$left_x != 0,])
                             incProgress(1/n, detail = paste("PLEASE WAIT"))
                           }
                           
                         })
            
            #assigns the intersection table
            tmp_label_plotting_df_GL_left <- tmp_label_plotting_df_GL
            
            #Puts together both tables in a single dataframe
            tmp_label_plotting_df_GL_left <- tmp_label_plotting_df_GL_left %>%
              mutate(speaker = input$spkr_GL, angle_type = 'left') %>%
              rename(x = left_x, y = left_y) %>%
              mutate(angle_type = 'all') %>%
              dplyr::select(-one_of(c('left_label')))
            
            intersections_data_all <- tmp_label_plotting_df_GL_left %>%
              dplyr::select(speaker, angle_type, angle, segment, repetition, frame, x, y, distance_int, distance)
            
            write.csv(intersections_data_all, 
                      paste0('./workingFiles/intersections_data_all_', 
                             appendName, '.csv'), 
                      row.names = FALSE)
            
            #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
            
          }
          
        }
      }
      
    }
    
  }
  
  #add the grid lines
  #This section adds the grid lines to the plot
  #plots the gridlines after the number of gridlines has been entered in the GRID LINES BY
  
  #::--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::
  #if input$number_of_gridlines_input is not NULL
  if(!is.null(input$number_of_gridlines_input)){
    #if input$number_of_gridlines_radio equals NUMBER
    #The location of gridlines is based on number of Gridlines
    if((input$number_of_gridlines_radio == 'Number' &
        (input$number_of_gridlines_input > 2 & input$number_of_gridlines_input < 101)) ||
       input$number_of_gridlines_radio == 'Angle'){
      #if the number of gridlines is larger than 2 and lower than 101, i.e. from 3 to 100
      #if(input$number_of_gridlines_input > 2 & input$number_of_gridlines_input < 101){
      #get the angles for each gridline
      
      #gets the mind_angle, the angle between the right gridline and the left gridline
      
      mid_angle_value <-  mid_angle
      
      #creates the column names of the dataframe to store the values of the gridlines
      grid_lines_data_frame_names <- unlist(strsplit('gridLine initial_angle final_angle mid_angle line_angle x y', ' '))
      
      #creates dataframe to store values of the gridlines
      grid_lines_data_frame <- data.frame(matrix(nrow = 0, ncol = length(grid_lines_data_frame_names)))
      #sets the names of the dataframe
      names(grid_lines_data_frame) <- grid_lines_data_frame_names
      #creates a sequence of angle values from right to lefty angle, separated by the step angles
      #print(label_plotting_df_GL)
      
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      #Manual selection
      if(input$define_fieldview_GL != 'Manual'){
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        #Non-manual selection
        
        ttl_contours <- intersections_data_all %>%
          distinct(speaker, segment, repetition, frame)
        
        if(length(sort(unique(intersections_data_all$segment))) == 1){
          common_angles_narrow <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            group_by(angle, speaker, segment) %>%
            mutate(contour_count = n()) %>%
            filter(contour_count == nrow(ttl_contours)) %>%
            ungroup()
          
          common_angles_wide <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            group_by(angle, speaker, segment) %>%
            mutate(contour_count = n()) %>%
            ungroup()
          
          if(input$define_fieldview_GL == 'Angle'){
            common_angles_angle <- intersections_data_all %>%
              filter(distance_int > 0 & distance > 0) %>%
              group_by(angle, speaker, segment) %>%
              mutate(contour_count = n()) %>%
              filter(angle >= tmp_angle_right_first &
                       angle <= tmp_angle_right_second) %>%
              ungroup()
          }
        }else if(length(sort(unique(intersections_data_all$segment))) > 1){
          common_angles_narrow <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            group_by(angle, speaker) %>%
            mutate(contour_count = n()) %>%
            filter(contour_count == nrow(ttl_contours)) %>%
            ungroup()
          
          common_angles_wide <- intersections_data_all %>%
            filter(distance_int > 0 & distance > 0) %>%
            group_by(angle, speaker) %>%
            mutate(contour_count = n()) %>%
            ungroup()
          
          if(input$define_fieldview_GL == 'Angle'){
            common_angles_angle <- intersections_data_all %>%
              filter(distance_int > 0 & distance > 0) %>%
              group_by(angle, speaker) %>%
              mutate(contour_count = n()) %>%
              filter(angle >= tmp_angle_right_first &
                       angle <= tmp_angle_right_second) %>%
              ungroup()
          }
        }
        
        if(input$define_fieldview_GL == 'Narrow'){
          #get the common angles
          common_angles <- common_angles_narrow 
        }else if(input$define_fieldview_GL == 'Wide'){
          common_angles <- common_angles_wide
        }else if(input$define_fieldview_GL == 'Angle'){
          common_angles <- common_angles_angle
        }
        
        if(input$define_fieldview_GL == 'Angle'){
          final_right_angle <- common_angles %>%
            filter(angle == min(angle)) %>%
            .$angle %>% unique()
          
          final_left_angle <- common_angles %>%
            filter(angle == max(angle)) %>%
            .$angle %>% unique()
        }else if(input$define_fieldview_GL %in% c('Narrow', 'Wide')){
          final_right_angle <- common_angles %>%
            filter(angle_type == 'right') %>%
            filter(angle == min(angle)) %>%
            .$angle %>% unique()
          
          final_left_angle <- common_angles %>%
            filter(angle_type == 'left') %>%
            filter(angle == max(angle)) %>%
            .$angle %>% unique()
        }
        
        write.csv(common_angles_narrow, 
                  paste0('./workingFiles/intersections_data_narrow_', appendName, '.csv'), 
                  row.names = FALSE)
        write.csv(common_angles_wide, 
                  paste0('./workingFiles/intersections_data_wide_', appendName, '.csv'), 
                  row.names = FALSE)
        
        if(input$define_fieldview_GL == 'Angle'){
          write.csv(common_angles_angle, 
                    paste0('./workingFiles/intersections_data_angle_', appendName, '.csv'), 
                    row.names = FALSE)
        }
        
      }
      
      if(input$number_of_gridlines_radio == 'Number'){
        #Angle sequence
        angles_seq <- seq(from = final_right_angle, to = final_left_angle, 
                          length.out = input$number_of_gridlines_input)
        #Number of gridlines
        number_of_lines <- input$number_of_gridlines_input
        #creates the value of the angle step between all gridlines
        step_angle <- mid_angle_value/(input$number_of_gridlines_input - 1)
      }else if(input$number_of_gridlines_radio == 'Angle'){
        #Angle sequence
        angles_seq <- seq(from = final_right_angle, to = final_left_angle, 
                          by = input$number_of_gridlines_input)
        #Number of gridlines
        number_of_lines <- length(angles_seq)
        #creates the value of the angle step between all gridlines
        step_angle <- input$number_of_gridlines_input
      }
      
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
      
      #adds the speaker label to the gridlines dataframe
      grid_lines_data_frame$speaker <- as.character(input$spkr_GL)
      #adds the segment label to the gridlines dataframe
      grid_lines_data_frame$segment <- as.character(input$sgmnt_GL)
      
      #calculates all intersection lines with all contours
      
      #iteration per segment
      withProgress(message = 
                     paste0('Calculating final intersections for Speaker: ', 
                            input$spkr_GL, ' and segment: ', input$sgmnt_GL, '.'), 
                   value = 0, {
                     # Number of times we'll go through the loop
                     maxgridlines <- max(grid_lines_data_frame$gridLine)
                     
                     cntr_i <- 1
                     for(i_n in 1:maxgridlines){
                       tmp_rightline_end_x <- grid_lines_data_frame$x[i_n]
                       tmp_rightline_end_y <- grid_lines_data_frame$y[i_n]
                       
                       #iteration per segment
                       
                       for(segmenti in unique(all_contours$segment)){
                         tmp_df_intersection_segment <- all_contours %>%
                           filter(segment == segmenti)
                         for(repetitioni in unique(tmp_df_intersection_segment$repetition)){
                           tmp_df_intersection_repetition <- tmp_df_intersection_segment %>%
                             filter(repetition == repetitioni)
                           for(framei in unique(tmp_df_intersection_repetition$frame)){
                             tmp_df_intersection_i <- tmp_df_intersection_repetition %>% 
                               filter(frame == framei)
                             
                             iterated_contour <- SpatialLines(list(Lines(list(Line(cbind(tmp_df_intersection_i$x, 
                                                                                         tmp_df_intersection_i$y))), 1)))
                             
                             tmp_df <- data.frame(segment = segmenti)
                             
                             if(is.na(tmp_rightline_end_x)){
                               seadfare <- 1
                             }
                             
                             #angle line------------------------------------------------------------
                             basis_line <- SpatialLines(list(Lines(list(Line(cbind(c(tmp_origin_point_x, 
                                                                                     tmp_rightline_end_x),
                                                                                   c(tmp_origin_point_y, 
                                                                                     tmp_rightline_end_y)))), 1)))
                             
                             if(!is.null(gIntersection(iterated_contour, basis_line)$x)){
                               
                               int_x <- round(gIntersection(iterated_contour, basis_line)$x, digits = 2)
                               int_y <- round(gIntersection(iterated_contour, basis_line)$y, digits = 2)
                               
                               tmp_df$angle <- grid_lines_data_frame$line_angle[i_n]
                               tmp_df$segment <- segmenti
                               tmp_df$repetition <- repetitioni
                               tmp_df$frame <- framei
                               tmp_df$x <- int_x
                               tmp_df$y <- int_y
                               
                               #store the largest distance
                               tmp_store_distance <- sqrt((tmp_origin_point_x-int_x)^2+(tmp_origin_point_y-int_y)^2)
                               tmp_df$distance_int <- tmp_store_distance
                               
                             }else{
                               int_x <- 0
                               int_y <- 0
                               
                               tmp_df$angle <- grid_lines_data_frame$line_angle[i_n]
                               tmp_df$segment <- segmenti
                               tmp_df$repetition <- repetitioni
                               tmp_df$frame <- framei
                               tmp_df$x <- int_x
                               tmp_df$y <- int_y
                               
                               #store the largest distance
                               tmp_store_distance <- 0
                               tmp_df$distance_int <- tmp_store_distance
                             }
                             
                             if(cntr_i == 1){
                               intersection_contours_df_GL <- tmp_df
                               cntr_i <- 2
                             }else{
                               intersection_contours_df_GL <- rbind(intersection_contours_df_GL, tmp_df)
                             }
                             
                           }
                         }
                       }
                       
                       incProgress(1/n, detail = paste("PLEASE WAIT"))
                     }
                     
                   })
      
      intersection_contours_df_GL_save <- intersection_contours_df_GL %>%
        group_by(angle) %>%
        mutate(angle_id = group_indices()) %>%
        mutate(speaker = input$spkr_GL)
      
      write.csv(intersection_contours_df_GL_save, 
                paste0('./workingFiles/intersection_contours_GL_', appendName, '.csv'), 
                row.names = FALSE)
      
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
  #}
  #::--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::--:--::--::--::
  
  #creates a duplicate dataframe to store the information of the gridlines
  grid_lines_data_frame2$d <- grid_lines_data_frame$d
  
  if(!is.null(grid_lines_data_frame2$d)){
    
    grid_lines_data_frame_saveData <- grid_lines_data_frame2$d %>%
      mutate(segment = paste(input$sgmnt_GL, collapse = '_'),
             origin_x = tmp_origin_point_x,
             origin_y = tmp_origin_point_y)
    
    write.csv(grid_lines_data_frame_saveData, 
              paste0('./workingFiles/gridlines_data_', appendName, '.csv'), 
              row.names = FALSE)
    
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
  
  plotStatic_GL_plotdata$plot <- p
  
  p
  
  #})
  
})

#saves the plot
output$gridlinesPlot <- downloadHandler(
  filename = function() { paste0('visualisation_GL_', appendName_value$d, '.', input$saveFormat) },
  content = function(file) {
    
    ggsave(file,plot=plotStatic_GL_plotdata$plot, device = input$saveFormat, 
           width = input$widthGraphics, height = input$heightGraphics, units = 'cm')
    
  }
)

#})