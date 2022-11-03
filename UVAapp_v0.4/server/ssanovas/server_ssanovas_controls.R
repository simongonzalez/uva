#creates the speaker selection widget
output$spkrSSANOVA <- renderUI({
  #if there is no input file
  if (is.null(input$file1))
    return()
  
  #imports the input file
  fileIn <- importFiles()
  #selects the dataframe with values
  fileIn <- fileIn[[2]]
  
  #gets the unique labels of the speakers
  spkrs <- unique(fileIn$speaker)
  
  #creates the speaker widget
  selectInput(inputId = 'speakerANOVA', label = 'Select\nSpeaker',
              choices = spkrs, multiple = FALSE)
})

#create comparisons
output$comp1SSANOVA <- renderUI({
  #if there is no input file
  if (is.null(input$file1))
    return()
  
  if (is.null(input$speakerANOVA))
    return()
  
  #gets the unique labels of the speakers
  spkrs <- input$speakerANOVA
  
  #imports the input file
  fileIn <- importFiles()
  #selects the dataframe with values
  fileIn <- fileIn[[2]]
  
  fileIn <- fileIn[fileIn$speaker == spkrs,]
  
  #gets the unique labels of the segments
  segs <- unique(fileIn$segment)
  
  #creates the speaker widget
  selectInput(inputId = 'comparison1SSANOVA', label = 'First Comparison', 
              choices = segs, multiple = FALSE)
})

#create comparisons
output$comp2SSANOVA <- renderUI({
  #if there is no input file
  if (is.null(input$file1))
    return()
  
  if (is.null(input$speakerANOVA))
    return()
  
  #gets the unique labels of the speakers
  spkrs <- input$speakerANOVA
  
  #imports the input file
  fileIn <- importFiles()
  #selects the dataframe with values
  fileIn <- fileIn[[2]]
  
  fileIn <- fileIn[fileIn$speaker == spkrs,]
  
  #gets the unique labels of the segments
  segs <- unique(fileIn$segment)
  
  #creates the speaker widget
  selectInput(inputId = 'comparison2SSANOVA', label = 'Second Comparison', 
              choices = segs, multiple = FALSE)
})