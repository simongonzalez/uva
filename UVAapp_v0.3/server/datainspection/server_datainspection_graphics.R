#This section controls the graphics parameters
#Text
#colours


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$text_main = renderUI({
  ttl = 'get_plot_title()'
  
  return(NULL)
  
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$tongue_colour_setUI = renderUI({
  if(input$tongue_colour_set_radio == "1")
  {  
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices = c("Default", "Grey", "Blue", "Green", "Red", "Purple", "Orange"), selected = "Default")
  }else if(input$tongue_colour_set_radio == "2")
  {
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices = c("Red-Grey", "Red-Blue", "Purple-Orange", "Purple-Green", "Brown-Green"))
  }else if(input$tongue_colour_set_radio == "3")
  {
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices = c("RedYellowGreen", "RedYellowBlue", "YellowGreenBlue"))
  }else if(input$tongue_colour_set_radio == "4")
  {
    selectInput("tongue_colour_set", label = "Select Colour", 
                choices = c("Spectral", "Rainbow", "Heat", "Terrain", "Topo", "Random"))
  }
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
output$invert_coloursUI = renderUI({
  fl = plotStatic_data()
  fl = fl[[1]]
  frms = unique(fl$frame)
  frms_nmbr = length(frms)
  
  if(frms_nmbr > 1 && input$tongue_colour_set!='Default')
  {
    checkboxInput("invert_colours", label = "Invert Colour Order", value = F)
  }else{
    return()
  }
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

#This section controls the visualisation of the xy information of the point clicked

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#UI to show the xy values of the point clicked
output$coordenate_info_ui <- renderUI({
  verbatimTextOutput("coordenate_info")
})
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::