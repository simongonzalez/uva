
output$palate_plot_GL_UI <- renderUI({

  if(is.null(pal_vals_data$d))
    return()
  
  checkboxInput("palate_plot_GL", label = "Palate Trace", value = FALSE)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#stores the largest distance from the point of origin and the intersection for each line
store_larger_distance = reactiveValues(n= 0)
#stores all the data frame information
grid_lines_data_frame = reactiveValues(d = NULL)
grid_lines_data_frame2 = reactiveValues(d = NULL)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$GL_intersection_labels_UI = renderUI({
  if(input$line_point_GL == T){
    checkboxInput("GL_intersection_labels", label = "Labels", value = T)
  }
})

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
#creates the button to manually set the origin point
output$manual_originPoint_btn_UI = renderUI({
  if(input$define_origin_GL != 'Manual')
    return()
    #bsButton("manual_originPoint_btn", label = "Set", icon("check-circle"), style = "primary")
    if(origin_point_cntr$n == 0){
      bsButton("manual_originPoint_btn", label = "Set", icon("check-circle"), style = "primary")
    }else{
      bsButton("manual_originPoint_btn", label = "Clear Origin Point", icon("times-circle"), style = "danger")
    }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#stores the input from the double clicking
observe({                
  if (!is.null(input$gl_dblclick)) {
    dbl_clicked_reactive_GL$n <- input$gl_dblclick 
  }
})

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
origin_point_automatic_GL <- reactiveValues(x = NULL, y = NULL)

output$include_palate_GL_UI <- renderUI({
  
  if(is.null(plotStatic_data_GL()))
    return()
  
  if(is.null(pal_vals_data$d))
    return()
  
  checkboxInput("include_palate_GL", label = "Include Palate Trace", value = TRUE)
})

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
#creates the button to automatically clear the origin point
output$automatic_originPoint_btn_UI = renderUI({
  if(is.null(origin_point_automatic_GL$x))
    return()
  
  bsButton("automatic_originPoint_btn", label = "Clear", icon("times-circle"), style = "danger")
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
observeEvent(input$automatic_originPoint_btn,{ 
  origin_point_automatic_GL$x = NULL
  origin_point_automatic_GL$y = NULL
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#This section controls the setting of the fieldview

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
observe({                
  if (!is.null(input$gl_dblclick)) {
    dbl_clicked_reactive_GL$n <- input$gl_dblclick 
  }
})