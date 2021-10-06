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
                       ' - Second Point: ', format(round(x2, 1), nsmall = 1), ',', 
                       format(round(y2, 1), nsmall = 1)))
        }
        
      }
      
    }
    
  }
  
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##THIS SECTION OBSERVES THE CLICKING ACTIVITY IN THE LANDMARKS TAB
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
#counts the number of clicks for the line
plot_line_click_counter <- reactiveValues(n = 0)

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

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#this section controls checkboxes and button to plot/display lines and labels in the plot object

#checkbox to show the intersection labels
output$intersection_labels_UI <- renderUI({
  if(input$line_point == T){
    checkboxInput("intersection_labels", label = "Labels", value = T)
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#checkbox to repel labels in the plot object
output$intersection_labelsRepel_UI <- renderUI({
  if(input$line_point == T){
    checkboxInput("intersection_labelsRepel", label = "Repel", value = T)
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#checkbox to choose showing only the extreme labels in the intersections
output$intersection_labelsExtreme_UI <- renderUI({
  if(input$line_point == T){
    checkboxInput("intersection_labelsExtreme", label = "Extreme", value = T)
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section controls the visualisation of the xy information of the point clicked

#UI to show the xy values of the point clicked
output$coordenate_info_ui_LM <- renderUI({
  verbatimTextOutput("coordenate_info_LM")
})

