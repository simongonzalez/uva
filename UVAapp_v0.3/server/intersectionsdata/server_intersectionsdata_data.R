#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
prepare_landmark_data <- reactive({

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reads in Landmarks file
  landmarks <- read.csv('./workingFiles/df_landmark.csv', stringsAsFactors = F) %>%
    mutate_all(na_if,"") %>%
    unite("landmarks", 5:ncol(.), na.rm = TRUE, remove = FALSE) %>%
    mutate(landmarks = trim(landmarks)) %>%
    filter(landmarks != '') %>%
    dplyr::select(speaker, segment, repetition, frame, landmarks)

  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #reads intersections file
  fls <- list.files('workingFiles', pattern = 'intersection_contours_GL_', full.names = T)
  
  for(ii in fls){
    speaker_i <- read.csv(ii)
    
    if(ii == fls[1]){
      speaker_df <- speaker_i
    }else{
      speaker_df <- rbind(speaker_df, speaker_i)
    }
  }
  
  speaker_df <- speaker_df %>%
    left_join(landmarks, by = c('segment', 'repetition', 'frame', 'speaker')) %>%
    replace_na(list(landmarks = "UNCL"))
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #merge gridlines

  flsGridlines <- list.files('workingFiles', pattern = 'gridlines_data_', full.names = T)
  
  for(ii in flsGridlines){
    gridlines_i <- read.csv(ii)
    
    if(ii == flsGridlines[1]){
      gridlines_df <- gridlines_i
    }else{
      gridlines_df <- rbind(gridlines_df, gridlines_i)
    }
  }
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #Gets the Landmark labels
  uniqueLMlabels <- unique(speaker_df$landmarks)
  uniqueLMlabels <- uniqueLMlabels[uniqueLMlabels != '']
  
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  #Saves the datasets
  write.csv(speaker_df, paste0('./workingFiles/speaker_df_analysis.csv'), row.names = F)
  write.csv(gridlines_df, paste0('./workingFiles/gridlines_df_analysis.csv'), row.names = F)
  
  #returns the datasets to the App environment
  return(list(uniqueLMlabels, speaker_df, gridlines_df))
})