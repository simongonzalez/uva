output$plotSSANOVAComparison <- renderPlot({
  #Checks for each of the inputs to run the analysis
  if (is.null(input$file1))
    return()
  if (is.null(input$speakerANOVA))
    return()
  if (is.null(input$comparison1SSANOVA))
    return()
  if (is.null(input$comparison2SSANOVA))
    return()
  if(input$comparison1SSANOVA == input$comparison2SSANOVA)
    return()
  
  if(input$comparison1SSANOVA == 'pal' || 
     input$comparison2SSANOVA == 'pal')
    return()
  
  if(is.null(plotSSANOVAData()))
    return()
  
  plottype <- plotSSANOVAData()[[2]]
  
  if(input$barePlot){
    plottype <- plottype + theme_void()
  }
  
  plottype
  
})

output$plotSSANOVAIndividual <- renderPlot({
  
  if (is.null(input$file1))
    return()
  if (is.null(input$speakerANOVA))
    return()
  if (is.null(input$comparison1SSANOVA))
    return()
  if (is.null(input$comparison2SSANOVA))
    return()
  if(input$comparison1SSANOVA == input$comparison2SSANOVA)
    return()
  
  if(input$comparison1SSANOVA == 'pal' || 
     input$comparison2SSANOVA == 'pal')
    return()
  
  if(is.null(plotSSANOVAData()))
    return()
  
  plottype <- plotSSANOVAData()[[1]]
  
  if(input$barePlot){
    plottype <- plottype + theme_void()
  }
  
  plottype
  
})

output$plotSSANOVAInteraction <- renderPlot({
  
  if (is.null(input$file1))
    return()
  if (is.null(input$speakerANOVA))
    return()
  if (is.null(input$comparison1SSANOVA))
    return()
  if (is.null(input$comparison2SSANOVA))
    return()
  if(input$comparison1SSANOVA == input$comparison2SSANOVA)
    return()
  
  if(input$comparison1SSANOVA == 'pal' || 
     input$comparison2SSANOVA == 'pal')
    return()
  
  if(is.null(plotSSANOVAData()))
    return()
  
  plottype <- plotSSANOVAData()[[3]]
  
  if(input$barePlot){
    plottype <- plottype + theme_void()
  }
  
  plottype
  
})

output$downloadPlot1 <- downloadHandler(
  filename = function() {
    'individualContours.png'
  },
  content = function(file) {
    
    plottype <- plotSSANOVAData()[[1]]
    plottype <- plottype + theme(text = element_text(size=25))
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    png(file, width = 1000, height = 750)
    print(plottype)
    dev.off()
  }
)

output$downloadPlot2 <- downloadHandler(
  filename = function() {
    'overallComparison.png'
  },
  content = function(file) {
    
    plottype <- plotSSANOVAData()[[2]]
    plottype <- plottype + theme(text = element_text(size=25))
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    png(file, width = 1000, height = 750)
    print(plottype)
    dev.off()
  }
)

output$downloadPlot3 <- downloadHandler(
  filename = function() {
    'confidenceIntervals.png'
  },
  content = function(file) {
    
    plottype <- plotSSANOVAData()[[3]]
    plottype <- plottype + theme(text = element_text(size=25))
    
    if(input$barePlot){
      plottype <- plottype + theme_void()
    }
    
    png(file, width = 1000, height = 750)
    print(plottype)
    dev.off()
  }
)

#save data
output$downloadData1 <- output$downloadData2 <- output$downloadData3 <- downloadHandler(
  filename = function() {
    'ssanovadata.csv'
  },
  content = function(file) {
    
    savessanovadata <- plotSSANOVAData()[[4]]
    
    names(savessanovadata)[2] <- 'segment'
    
    write.csv(savessanovadata, file, row.names = F)
  }
)
