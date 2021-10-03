#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#UI to show the xy values of the point clicked
output$coordenate_info_ui_GL <- renderUI({
  verbatimTextOutput("coordenate_info_GL")
})

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#THIS SECTION OBSERVES THE CLICKING ACTIVITY IN THE GRIDLINES TAB
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

#stores the xy values of the line plotted, both the begin and end points
line_fill_cntr_GL <- reactiveValues(x1 = 0, y1 = 0, x2 = 0, y2 = 0)

#stores the xy values of the double click
dbl_clicked_reactive_GL <- reactiveValues(n = NULL)
#counts the number of points for the line
line_point_counter_GL <- reactiveValues(n = 0)

#stores the values of the first point double clicked in the image
first_point_GL <- reactiveValues(x = 0, y = 0)

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
#shows the xy values of the point clicked
#prints the coordenate information to the user
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
                       ' - Second Point: ', format(round(x2, 1), nsmall = 1), ',', 
                       format(round(y2, 1), nsmall = 1)))
        }
        
      }
      
    }
    
  }
})