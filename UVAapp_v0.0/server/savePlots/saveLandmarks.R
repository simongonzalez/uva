#source(file.path("server/savePlots", "saveVisualisation.R"),  local = TRUE)$values
output$landmarksPlot <- downloadHandler(
  filename = function() { paste('incontext.png', sep='') },
  content = function(file) {
    
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
  p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) +
    scale_y_reverse(limits = c(extrema[4], extrema[3])) +
    labs(x ='Tongue Advancement', y = 'Tongue Height')
  
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
              #creates the intersection label
              int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
              
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
                #creates the intersection label
                int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                
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
                #creates an intersection label
                int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                
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
                #creates an intersection label
                int_label <- paste0('x=', round(int_x), ', ', 'y=', round(int_y))
                
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
              #creates the intersection label
              plot_data_pal$label <- paste0('x=', round(plot_data_pal$x), ', ', 'y=', round(plot_data_pal$y))
              
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
  
  ggsave(file, plot = p, device = "png", width = 20, height = 15, units = 'cm')
  }
)