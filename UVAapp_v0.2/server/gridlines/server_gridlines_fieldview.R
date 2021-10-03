#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
observeEvent(input$manual_fieldviewPoint_btn,{
  if(fieldview_points_cntr$n == 2){
    fieldview_point_GL$x1 <- NULL
    fieldview_point_GL$y1 <- NULL
    fieldview_point_GL$x2 <- NULL
    fieldview_point_GL$y2 <- NULL
    
    fieldview_point_1_vals$x <- NULL
    fieldview_point_1_vals$y <- NULL
    fieldview_point_2_vals$x <-  NULL
    fieldview_point_2_vals$y <- NULL
    
    fieldview_points_cntr$n <- 3
  }else if(fieldview_points_cntr$n == 0){
    fieldview_point_GL$x1 <- dbl_clicked_reactive_GL$n$x
    fieldview_point_GL$y1 <- dbl_clicked_reactive_GL$n$y
    fieldview_points_cntr$n <- fieldview_points_cntr$n + 1
  }else if(fieldview_points_cntr$n == 1){
    fieldview_point_GL$x2 <- dbl_clicked_reactive_GL$n$x
    fieldview_point_GL$y2 <- dbl_clicked_reactive_GL$n$y
    fieldview_points_cntr$n <- fieldview_points_cntr$n + 1
  }else if(fieldview_points_cntr$n == 3){
    fieldview_points_cntr$n <- 0
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#stores the final values of the fieldview points
fieldview_point_1_vals <- reactiveValues(x = NULL, y = NULL)
fieldview_point_2_vals <- reactiveValues(x = NULL, y = NULL)

#controls the behaviour of the SET button
observeEvent(input$set_fieldviewPoint_btn,{ 
  
  if(input$define_origin_GL == 'Manual'){
    origin_point <- c(origin_point_GL$x,origin_point_GL$y)
  }else{
    origin_point <- c(origin_point_automatic_GL$x,origin_point_automatic_GL$y)
  }
  
  fieldview_point_1 <- c(fieldview_point_GL$x1, fieldview_point_GL$y1)
  fieldview_point_2 <- c(fieldview_point_GL$x2, fieldview_point_GL$y2)
  
  if(is.null(fieldview_point_GL$x1) || is.null(fieldview_point_GL$x2) ||
     length(setdiff(origin_point, fieldview_point_1)) == 0 ||
     length(setdiff(origin_point, fieldview_point_2)) == 0 ||
     length(setdiff(fieldview_point_1, fieldview_point_2)) == 0 ){
    fieldview_points_cntr$n <- 0
  }else{
    fieldview_points_cntr$n <- 3
    fieldview_point_1_vals$x <- fieldview_point_GL$x1
    fieldview_point_1_vals$y <- fieldview_point_GL$y1
    fieldview_point_2_vals$x <- fieldview_point_GL$x2
    fieldview_point_2_vals$y <- fieldview_point_GL$y2
  }
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#controls the behaviour of the CLEAR button
observeEvent(input$clear_fieldviewPoint_btn,{ 
  fieldview_point_GL$x1 <- NULL
  fieldview_point_GL$y1 <- NULL
  fieldview_point_GL$x2 <- NULL
  fieldview_point_GL$y2 <- NULL
  
  fieldview_point_1_vals$x <- NULL
  fieldview_point_1_vals$y <- NULL
  fieldview_point_2_vals$x <- NULL
  fieldview_point_2_vals$y <- NULL
  
  fieldview_points_cntr$n <- 0
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$leftlineangle_fieldview_GL_UI = renderUI({
  if(is.null(input$define_fieldview_GL))
    return()
  
  if(input$define_fieldview_GL != 'Angle')
    return()
  
  numericInput('leftlineangle_fieldview_GL', label = 'Left', value = 135, min = 1)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$rightlineangle_fieldview_GL_UI = renderUI({
  if(is.null(input$define_fieldview_GL))
    return()
  
  if(input$define_fieldview_GL != 'Angle')
    return()
  
  numericInput('rightlineangle_fieldview_GL', label = 'Right', value = 45, min = 1)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$number_of_gridlines_radio_ui <- renderUI({
  
  # if(input$define_origin_GL == 'Manual' & fieldview_points_cntr$n != 3)
  #   return()
  
  if(is.null(input$define_fieldview_GL))
    return()
  
  if(input$define_fieldview_GL == 'Manual' & fieldview_points_cntr$n != 3)
    return()
  
  radioButtons("number_of_gridlines_radio", label = h5("Gridlines by:"),
               choices = c("Number", "Angle"), 
               selected = "Number", inline = TRUE)
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
manual_left_angle <- reactiveValues(d=NULL)
manual_right_angle <- reactiveValues(d=NULL)

output$number_of_gridlines_input_ui <- renderUI({
  
  if(is.null(input$number_of_gridlines_radio))
    return()
  
  if(input$number_of_gridlines_radio == 'Angle'){

    if(input$define_fieldview_GL == 'Manual'){
      max_angle <- manual_left_angle$d
      min_angle <- manual_right_angle$d
      total_angles <- max_angle - min_angle
      mid_angle <- round(total_angles/2)
    }else{
      max_angle <- input$leftlineangle_fieldview_GL
      min_angle <- input$rightlineangle_fieldview_GL
      total_angles <- max_angle - min_angle
      mid_angle <- round(total_angles/2)
    }
    
    sliderInput("number_of_gridlines_input", label = NULL,
                min = 1, max = total_angles, value = 1)
  }else{
    sliderInput("number_of_gridlines_input", label = NULL,
                min = 2, max = 100, value = 20)
  }
  
})

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Creates the set grid lines button
output$setGridlinesBttnUI <- renderUI({
  actionButton('setGridlinesBttn', 'Set Gridlines')
})

