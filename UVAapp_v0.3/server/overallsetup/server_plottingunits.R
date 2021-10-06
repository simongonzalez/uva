#======================================================================================
#======================================================================================
# PLOTTING UNITS-----------------------------------------------------------------------
#======================================================================================
#======================================================================================

output$radio_measuresUI = output$radio_measures_LMUI = output$radio_measures_GLUI = renderUI({
  if(is.null(importFiles()))
    return()
  
  unitsOption <- importFiles()[[3]]
  
  if(unitsOption == 'mm'){
    radioButtons("radio_measures", label = h5("Plot in"),
                 choices = list("Millimeters" = 1, "Pixels" = 2),
                 selected = 1, inline = T)
  }else if(unitsOption == 'pixel'){
    radioButtons("radio_measures", label = h5("Plot in"),
                 choices = list("Millimeters" = 1, "Pixels" = 2),
                 selected = 2, inline = T)
  }
})

# output$radio_measures_LMUI <- renderUI({
#   if(is.null(importFiles()))
#     return()
#   
#   unitsOption <- importFiles()[[3]]
#   
#   if(unitsOption == 'mm'){
#     radioButtons("radio_measures_LM", label = h5("Plot in"),
#                  choices = list("Millimeters" = 1, "Pixels" = 2),
#                  selected = 1, inline = T)
#   }else if(unitsOption == 'pixel'){
#     radioButtons("radio_measures_LM", label = h5("Plot in"),
#                  choices = list("Millimeters" = 1, "Pixels" = 2),
#                  selected = 2, inline = T)
#   }
# })
# 
# output$radio_measures_GLUI <- renderUI({
#   if(is.null(importFiles()))
#     return()
#   
#   unitsOption <- importFiles()[[3]]
#   
#   if(unitsOption == 'mm'){
#     radioButtons("radio_measures_GL", label = h5("Plot in"),
#                  choices = list("Millimeters" = 1, "Pixels" = 2),
#                  selected = 1, inline = T)
#   }else if(unitsOption == 'pixel'){
#     radioButtons("radio_measures_GL", label = h5("Plot in"),
#                  choices = list("Millimeters" = 1, "Pixels" = 2),
#                  selected = 2, inline = T)
#   }
# })