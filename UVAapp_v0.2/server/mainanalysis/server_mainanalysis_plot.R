output$dynamicAnalysisSingle_plot <- renderPlotly({
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
  
  if(is.null(input$dynamicAnalysisSingle_repetition_selection))
    return()
  
  df <- prepare_landmark_data()[[2]]
  
  dforigin <- prepare_landmark_data()[[3]]
  
  originx <- unique(dforigin$x1)
  originy <- unique(dforigin$y1)
  
  df <- df[df$speaker == input$dynamicAnalysisSingle_speaker_selection & 
             df$segment %in% input$dynamicAnalysisSingle_segment_selection &
             df$repetition %in% input$dynamicAnalysisSingle_repetition_selection,]
  
  #define colors and linetypes
  if(length(input$dynamicAnalysisSingle_landmark_selection) == 1){
    #one landmark
    if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
      #one segment
      if(input$dynamicAnalysisSingle_tokens){
        #single repetitions
        tmpgroup <- 'interaction(repetition)'
        tmpcolor <- 'repetition'
        tmpLineType <- 'segment'
      }else{
        #aggregate
        tmpgroup <- 'interaction(repetition)'
        tmpcolor <- 'repetition'
        tmpLineType <- 'segment'
      }
    }else{
      #multiple segments
      if(input$dynamicAnalysisSingle_tokens){
        #single repetitions
        tmpgroup <- 'interaction(segment,repetition)'
        tmpcolor <- 'repetition'
        tmpLineType <- 'segment'
      }else{
        #aggregate
        tmpgroup <- 'interaction(segment)'
        tmpcolor <- 'segment'
        tmpLineType <- 'segment'
      }
    }
  }else{
    #multiple landmarks...........................................
    df <- df[df$landmarks %in%input$dynamicAnalysisSingle_landmark_selection, ]
    
    if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
      #one segment
      if(input$dynamicAnalysisSingle_tokens){
        #single repetitions
        tmpgroup <- 'interaction(repetition,landmarks)'
        tmpcolor <- 'repetition'
        tmpLineType <- 'landmarks'
      }else{
        #aggregate
        tmpgroup <- 'interaction(landmarks)'
        tmpcolor <- 'landmarks'
        tmpLineType <- 'landmarks'
      }
    }else{
      #multiple segments
      if(input$dynamicAnalysisSingle_tokens){
        #single repetitions
        tmpgroup <- 'interaction(segment,repetition,landmarks)'
        tmpcolor <- 'landmarks'
        tmpLineType <- 'segment'
      }else{
        #aggregate
        tmpgroup <- 'interaction(segment,landmarks)'
        tmpcolor <- 'landmarks'
        tmpLineType <- 'segment'
      }
    }
  }
  
  
  if(input$dynamicPlot_type == 'Contours'){
    #start plot
    ggp <- ggplot(df, aes_string(x='x', y='y', group=tmpgroup,color=tmpcolor,linetype=tmpLineType))
    
    #confidence intervals
    if(input$dynamicAnalysisSingle_ciplot){
      #with confidence intervals
      ggp <- ggp + stat_smooth()
    }else{
      #without confidence intervals
      ggp <- ggp + stat_smooth(se = F)
    }
    #gridlines
    if(input$dynamicAnalysisSingle_gridlines == T){
      
      originy <- input$dynamicAnalysisSingle_originPoint
      dforigin$y1 <- input$dynamicAnalysisSingle_originPoint
      
      ggp <- ggp + geom_point(inherit.aes = F, data = data.frame(x = originx, y = originy),
                              aes(x = originx, y = originy), size = 5,
                              colour = "#FF6A6A", show.legend = FALSE)
      
      ggp <- ggp + geom_segment(inherit.aes = F, data = dforigin, aes(x = x1, y = y1, xend = x, yend = y), 
                                alpha = input$dynamicAnalysisSingle_alpha)
      
    }
    
  }else{
    
    ggp <- ggplot(data=df,aes(x,y)) +
      stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='white') + 
      scale_fill_continuous(low="white",high="red") +
      guides(alpha="none") +
      scale_colour_gradient(low="white",high="red")
    
    if(length(input$dynamicAnalysisSingle_landmark_selection) == 1){
      #one landmark
      if(length(input$dynamicAnalysisSingle_segment_selection) > 1){
        #onse segment
        ggp <- ggp + facet_wrap(~segment)
      }
    }else{
      #multiple landmarks
      if(length(input$dynamicAnalysisSingle_segment_selection) == 1){
        #onse segment
        
        if(input$dynamicAnalysisSingle_tokens){
          #tokens = plot individual landmarks
          ggp <- ggp + facet_wrap(~landmarks)
        }
        
      }else{
        #multiple landmarks
        ggp <- ggp + facet_wrap(~segment)
      }
    }
    
  }
  
  #final settings
  ggp <- ggp + theme_minimal(base_size = input$dynamicAnalysisSingle_fontsize) + 
    scale_y_reverse() + xlab('Tongue Advancement') + ylab('Tongue Height')
  
  ggp
  
})

output$dynamicAnalysisSingle_downloadPlot <- downloadHandler(
  filename = function(){paste("dynamicAnalysisSingle_plot",'.png',sep='')},
  content = function(file){
    ggsave(file)
  }
)
