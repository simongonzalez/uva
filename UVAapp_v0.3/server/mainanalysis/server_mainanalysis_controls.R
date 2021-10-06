output$dynamicAnalysisSingle_landmark_selection_ui <- renderUI({
  if(is.null(prepare_landmark_data()))
    return()
  
  dflabels <- prepare_landmark_data()[[1]]
  
  selectInput(inputId = 'dynamicAnalysisSingle_landmark_selection', label = 'Landmarks', choices = sort(unique(dflabels)), multiple = T)
})

output$dynamicAnalysisSingle_speaker_selection_ui <- renderUI({
  if(is.null(prepare_landmark_data()))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_landmark_selection))
    return()
  
  if(input$dynamicAnalysisSingle_landmark_selection == '')
    return()
  
  df <- prepare_landmark_data()[[2]]
  
  selectInput(inputId = 'dynamicAnalysisSingle_speaker_selection', label = 'Speaker', choices = sort(unique(df$speaker)))
})

output$dynamicAnalysisSingle_segment_selection_ui <- renderUI({
  if(is.null(prepare_landmark_data()))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_landmark_selection))
    return()
  
  if(input$dynamicAnalysisSingle_landmark_selection == '')
    return()
  
  if(is.null(input$dynamicAnalysisSingle_speaker_selection))
    return()
  
  df <- prepare_landmark_data()[[2]]
  
  df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection,]
  
  selectInput(inputId = 'dynamicAnalysisSingle_segment_selection', label = 'Segment', 
              choices = sort(unique(df$segment)), multiple = T, selected = sort(unique(df$segment))[1])
})

output$dynamicAnalysisSingle_repetition_selection_ui <- renderUI({
  if(is.null(prepare_landmark_data()))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_landmark_selection))
    return()
  
  if(input$dynamicAnalysisSingle_landmark_selection == '')
    return()
  
  if(is.null(input$dynamicAnalysisSingle_speaker_selection))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_segment_selection))
    return()
  
  df <- prepare_landmark_data()[[2]]
  
  df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection & df$segment %in% input$dynamicAnalysisSingle_segment_selection,]
  
  selectInput(inputId = 'dynamicAnalysisSingle_repetition_selection', label = 'Repetition', 
              choices = sort(unique(df$repetition)), multiple = T, selected = sort(unique(df$repetition))[1])
})