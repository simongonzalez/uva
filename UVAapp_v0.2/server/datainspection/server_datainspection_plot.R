#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
plotStatic_plotdata <- reactiveValues()
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
  if(!is.null(pal_vals_datainspection$d)){
    pal = pal_vals_datainspection$d
  }
  
  #pal <- fl[[4]]
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
    theme(title = element_text(family = input$font_type, 
                               colour = input$text_color, size = input$title_size),
          axis.title.x = element_text(family = input$font_type, 
                                      colour = input$text_color, size = input$axis_size),
          axis.title.y = element_text(family = input$font_type, 
                                      colour = input$text_color, size = input$axis_size),
          legend.title = element_text(family = input$font_type, 
                                      colour = input$text_color, size = input$legend_size),
          axis.text = element_text(size = input$ticks_size))
  
  #if contour smoothing is selected-------------------------------------------------------------------
  if(input$smooth_contour == T){
    if(segs_nmbr > 1){
      #if more than one segment is plotted............................................................
      p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(segment, repetition, frame), 
                                        colour = segment),
                         stat="smooth", formula = y ~ x, method = "loess", 
                         linetype = tongue_line_type, 
                         alpha = input$tongue_alpha_slider,
                         se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
    }else{
      #if only one segment is plotted..................................................................
      if(reps_nmbr > 1){
        #if more than one repetition is selected.......................................................
        p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                          colour = repetition),
                           stat="smooth", formula = y ~ x, method = "loess", 
                           linetype = tongue_line_type, 
                           alpha = input$tongue_alpha_slider,
                           se = F, size = input$tongue_width_slider, span = input$tongue_smooth_slider)
      }else{
        #if only one repetition is selected............................................................
        p <- p + geom_line(data = fl, aes(x = x, y = y, group = interaction(repetition, frame), 
                                          colour = frame),
                           stat="smooth", formula = y ~ x, method = "loess", linetype = tongue_line_type, 
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
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  #plot palate trace
  #if(!is.null(pal_vals_data$d)){
  if(input$palate_plot == T){
    #if plotting the palate has been selected
    if(input$smooth_contour == T){
      #if contours are smoothed
      p <- p + geom_line(data = pal, aes(x = x, y = y),
                         stat="smooth", formula = y ~ x, method = "loess", 
                         linetype = pal_line_type, alpha = input$palate_alpha_slider,
                         se = F, size = input$palate_width_slider, span = input$palate_smooth_slider, 
                         colour = pal_colour)
    }else{
      #if no smoothing is selected
      p <- p + geom_line(data = pal, aes(x = x, y = y), 
                         size = input$palate_width_slider, linetype = pal_line_type, 
                         alpha = input$palate_alpha_slider, colour = pal_colour)
    }
  }
  #}
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #if a colour is selected for the palate contour
  if(!is.null(pallete_colour)){
    p <- p + scale_colour_gradientn(colours=pallete_colour)
  }
  
  #adds the limits to axes and inverts values for the Y axis
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(input$main_invert_y == T){
    p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) + 
      scale_y_continuous(limits = c(extrema[3], extrema[4])) + 
      labs(x ='Tongue Advancement', y = 'Tongue Height')
  }else{
    p <- p + scale_x_continuous(limits = c(extrema[1], extrema[2])) + 
      scale_y_reverse(limits = c(extrema[4], extrema[3])) + 
      labs(x ='Tongue Advancement', y = 'Tongue Height')
  }
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #add annotations----------------------------------------------------------
  #adds single click information
  if(input$xy_point_main == T){
    #if plotting points is selected
    if(!is.null(clicked_reactive_main$n)){
      #if clicking information is not empty
      
      #gets the x value of click information
      x <- clicked_reactive_main$n$x
      #gets the y value of click information
      y <- clicked_reactive_main$n$y
      
      p <- p + geom_label_repel(data = data.frame(x = x, y = y, 
                                                  text = paste0('x=', round(x), '\n', 'y=', round(y))),
                                aes(x = x, y = y, label = text, fill = '#FF6A6A'), size = 5,
                                colour = "white", fontface = "bold", show.legend = FALSE)
      
      p <- p + geom_point(data = data.frame(x = x, y = y),
                          aes(x = x, y = y), size = 5,
                          colour = "#FF6A6A", show.legend = FALSE)
    }
  }
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
          
          #creates a value to separate the label from the exact xy point
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
              #creates a temporary dataframe with the iterated repetition
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
                
                p <- p + geom_label_repel(data = label_plotting_df, 
                                          aes(x = x, y = y, label = label, fill = segment),
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
                                                                          fill = factor(repetition)), 
                                            size = 3.5, fontface = "bold", show.legend = FALSE)
                }else{
                  #if the labels are not repelled, i.e. messier plotting
                  p <- p + geom_label(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                    fill = factor(repetition)), 
                                      size = 3.5, fontface = "bold", show.legend = FALSE)
                }
              }else{
                
                #if only one repetition is plotted
                if(input$intersection_labelsRepel_main == T){
                  #if the labels are to be repelled, i.e. clearer plotting
                  p <- p + geom_label_repel(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                          fill = factor(frame)),
                                            # fill = unique(fl$frame)[frame_i]), 
                                            size = 3.5,fontface = "bold", show.legend = FALSE)
                }else{
                  #if the labels are not repelled, i.e. messier plotting
                  p <- p + geom_label(data = label_plotting_df, aes(x = x, y = y, label = label, 
                                                                    fill = factor(frame)),
                                      # fill = unique(fl$frame)[frame_i]), 
                                      size = 3.5,fontface = "bold", show.legend = FALSE)
                }
              }
            }
          }
          
          #Plotting the Palate Trace----------------------------------------------------------------
          #if(!is.null(pal_vals_data$d)){
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
          #}
        }
      }
      
    }
  }

  plotStatic_plotdata$plot <- p

  p
})

#saves the plot
output$visualisationPlot <- downloadHandler(

  filename = function() { paste0('visualisation.', input$saveFormat) },
  content = function(file) {

    ggsave(file,plot=plotStatic_plotdata$plot, device = input$saveFormat, 
           width = input$widthGraphics, height = input$heightGraphics, units = 'cm')
  }
)
