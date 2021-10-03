
output$dynamicAnalysisSingle_originPoint_ui <- renderUI({
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
  
  if(!input$dynamicAnalysisSingle_gridlines)
    return()
  
  dforigin <- prepare_landmark_data()[[3]]
  
  originx <- unique(dforigin$x1)
  originy <- unique(dforigin$y1)
  
  numericInput(inputId = 'dynamicAnalysisSingle_originPoint', label = 'Origin Y', value = originy)
})


output$dynamicAnalysisSingle_fontsize_ui <- renderUI({
  if(is.null(prepare_landmark_data()))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_landmark_selection))
    return()
  
  if(is.null(input$dynamicAnalysisSingle_speaker_selection))
    return()
  
  numericInput(inputId = 'dynamicAnalysisSingle_fontsize', label = 'Font size', 
               value = 15, min = 0, max = 50, step = 1)
})

output$dynamicAnalysisSingle_displacements = renderHighchart({
  
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
  
  if(length(input$dynamicAnalysisSingle_landmark_selection) != 2)
    return()

  landmarksin <- input$dynamicAnalysisSingle_landmark_selection
  
  df <- prepare_landmark_data()[[2]]
  
  dforigin <- prepare_landmark_data()[[3]]
  
  originx <- unique(dforigin$origin_x)
  originy <- unique(dforigin$origin_y)
  
  df <- df %>%
    filter(speaker == input$dynamicAnalysisSingle_speaker_selection) %>%
    filter(segment %in% input$dynamicAnalysisSingle_segment_selection) %>%
    filter(repetition %in% input$dynamicAnalysisSingle_repetition_selection)
  
  #df <- df[df$landmarks %in% input$dynamicAnalysisSingle_landmark_selection,]
  
  df$distance <- sqrt((originx-df$x)^2+(originy-df$y)^2)
  
  for(i in sort(unique(df$segment))){
    dfi <- df %>%
      filter(segment == i)
    for(j in sort(unique(dfi$repetition))){
      dfj <- dfi %>%
        filter(repetition == j)
      
      dfjframe <- dfj %>%
        filter(landmarks %in% input$dynamicAnalysisSingle_landmark_selection)
      
      sequenceFrames <- sort(as.numeric(unique(dfjframe$frame)))
      frameDifference <- diff(sequenceFrames)
      tokenTime <- input$frameRate * frameDifference
      
      #calculate individual accelerations........................................................
      #get distances
      tmpAccMatrixDistancesAll <- matrix(ncol = frameDifference, nrow = length(unique(dfj$angle_id)))
      
      cntr <- 1
      
      for(acci in sequenceFrames[1]:(sequenceFrames[2]-1)){
        tmpAccMatrixDistancesAll[,cntr] <- 
          dfj[dfj$frame == acci,'distance'] - dfj[dfj$frame == (acci + 1),'distance']
        cntr <- cntr + 1
      }

      #mean distances per gridline
      tmpAccMatrixDistances <- NULL
      for(veloi in 1:nrow(tmpAccMatrixDistancesAll)){
        tmpAccMatrixDistances[veloi] <- mean(diff(tmpAccMatrixDistancesAll[veloi,]))
      }
      
      tmpAccMatrixVelocitiesAll <- tmpAccMatrixDistancesAll / input$frameRate
      tmpAccMatrixVelocities <- NULL
      for(veloi in 1:nrow(tmpAccMatrixVelocitiesAll)){
        tmpAccMatrixVelocities[veloi] <- mean(diff(tmpAccMatrixVelocitiesAll[veloi,]))
      }
      
      #calculate individual accelerations
      tmpAccMatrixAccelerationsAll <- matrix(ncol = frameDifference, nrow = length(unique(dfj$angle_id)))
      for(veloi in 1:nrow(tmpAccMatrixVelocitiesAll)){
        for(veloj in 1:frameDifference){
          
          
          if(veloj == 1){
            currentVelocity <- tmpAccMatrixVelocitiesAll[veloi,veloj]
            
            if(currentVelocity < 0){
              currentVelocity <- currentVelocity * -1
            }
            
            tmpAccMatrixAccelerationsAll [veloi, veloj] <- currentVelocity / input$frameRate
          }else{
            #get the previous velocity value
            previousVelocity <- tmpAccMatrixVelocitiesAll[veloi, veloj-1]
            currentVelocity <- tmpAccMatrixVelocitiesAll[veloi,veloj]
            
            tmpAccMatrixAccelerationsAll [veloi, veloj] <- (currentVelocity / input$frameRate)
            
            if(currentVelocity < previousVelocity){
              #negative acceleration
              if(tmpAccMatrixAccelerationsAll [veloi, veloj] > 0){
                tmpAccMatrixAccelerationsAll [veloi, veloj] <- tmpAccMatrixAccelerationsAll [veloi, veloj] * -1
              }
            }
          }
        }
        #tmpAccMatrixVelocities[veloi] <- mean(diff(tmpAccMatrixDistancesAll[veloi,])) / input$frameRate
      }
      View(tmpAccMatrixVelocitiesAll)
      View(tmpAccMatrixAccelerationsAll)
      
      
      
      tmpAccMatrixAccelerations <- NULL
      for(veloi in 1:nrow(tmpAccMatrixAccelerationsAll )){
        tmpAccMatrixAccelerations[veloi] <- mean(diff(tmpAccMatrixAccelerationsAll[veloi,]))
      }
      
      #tmpAccMatrixAccelerations <- tmpAccMatrixVelocities / input$frameRate
      
      dfja <- dfj[dfj$landmarks == landmarksin[1],]
      dfja <- dfja[with(dfja, order(angle_id)),]
      dfjb <- dfj[dfj$landmarks == landmarksin[2],]
      dfjb <- dfjb[with(dfjb, order(angle_id)),]
      
      tmpdf <- dfja[unlist(strsplit('segment repetition speaker angle_id', ' '))]
      tmpdf$landmarks <- paste(landmarksin[1], landmarksin[2], sep = '-')
      tmpdf$tokenLabel <- paste(tmpdf$segment, tmpdf$repetition, 
                                tmpdf$speaker, tmpdf$landmarks, sep = '_')
      
      tmpdf$distance <- tmpAccMatrixDistances
      tmpdf$displacement <- dfjb$distance - dfja$distance
      #tmpdf$acceleration <- tmpdf$velocity / tokenTime
      
      #tmpdf$displacement <- (tmpdf$velocity * tokenTime) + ((tmpdf$acceleration / 2) * (tokenTime ^ 2))
      
      #tmpdf$displacement <- rowMeans(tmpAccMatrixDistances, na.rm = FALSE, dims = 1)
      tmpdf$velocity <- tmpAccMatrixVelocities#rowMeans(abs(tmpAccMatrixVelocities), na.rm = FALSE, dims = 1)
      tmpdf$acceleration <- rowMeans(tmpAccMatrixAccelerationsAll, na.rm = FALSE, dims = 1)
      
      # tmpdf$displacement <- rowMeans(tmpAccMatrixDisplacements, na.rm = FALSE, dims = 1)
      
      if(i == sort(unique(df$segment))[1] & j == sort(unique(dfi$repetition))[1]){
        newdf <- tmpdf
      }else{
        newdf <- rbind(newdf, tmpdf)
      }
      
    }
  }
  
  if(!input$dynamicAnalysisSingle_tokens){
    #multiple repetitions to aggregate
    
    if(input$dynamicType == 'Displacement'){
      newdf <- newdf %>%
        group_by(speaker, segment, angle_id, landmarks) %>%
        summarise(`Displacement(mm)` = mean(displacement))
    }else if(input$dynamicType == 'Velocity'){
      newdf <- newdf %>%
        group_by(speaker, segment, angle_id, landmarks) %>%
        summarise(`Velocity(mm/s)` = mean(velocity))
    }else if(input$dynamicType == 'Acceleration'){
      newdf <- newdf %>%
        group_by(speaker, segment, angle_id, landmarks) %>%
        summarise(`Acceleration(mm/s2)` = mean(acceleration))
    }else{
      newdf <- newdf %>%
        group_by(speaker, segment, angle_id, landmarks) %>%
        summarise(`Distance(mm)` = mean(distance))
    }
    
    newdf$tokenLabel <- paste(newdf$speaker, newdf$segment, newdf$landmarks, sep = '_')
  }else{
    if(input$dynamicType == 'Displacement'){
      newdf$`Displacement(mm)` <- newdf$displacement
    }else if(input$dynamicType == 'Velocity'){
      newdf$`Velocity(mm/s)` <- newdf$velocity
    }else if(input$dynamicType == 'Acceleration'){
      newdf$`Acceleration(mm/s2)` <- newdf$acceleration
    }else{
      newdf$`Distance(mm)` <- newdf$distance
    }
    newdf$tokenLabel <- paste(newdf$speaker, newdf$segment, newdf$repetition, newdf$landmarks, sep = '_')
  }
  
  colnames(newdf)[which(names(newdf) == "angle_id")] <- "Gridlines"
  
  if(input$dynamicType == 'Displacement'){
    hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Displacement(mm)`, group = tokenLabel)) %>%
      hc_xAxis(reversed = TRUE)
  }else if(input$dynamicType == 'Velocity'){
    hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Velocity(mm/s)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
      hc_xAxis(reversed = TRUE)
  }else if(input$dynamicType == 'Acceleration'){
    hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Acceleration(mm/s2)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
      hc_xAxis(reversed = TRUE)
  }else{
    hc <- hchart(newdf, "column", hcaes(x = Gridlines, y = `Distance(mm)`, group = tokenLabel, style=list(fontSize = "2em") )) %>%
      hc_xAxis(reversed = TRUE)
  }
  
  hc %>% 
    hc_xAxis(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)),
             labels = list(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)))) %>%
    hc_yAxis(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)),
             labels = list(style=list( fontSize = (input$dynamicAnalysisSingle_fontsize + 5)))) %>%
    hc_exporting(enabled = TRUE,
                 filename = "custom-file-name")
  
})