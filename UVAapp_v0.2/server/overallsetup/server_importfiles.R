#======================================================================================
#======================================================================================
# INPUT FILES--------------------------------------------------------------------------
#======================================================================================
#======================================================================================

pal_vals_data <- reactiveValues(d=NULL)
pal_vals_datainspection <- reactiveValues(d=NULL)

importFiles <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local file names where the data can
  # be found.
  
  #input file from upload
  inFile <- input$file1
  
  #if a file has been input
  if (is.null(inFile))
    return(NULL)

  #shows the progress of the data upload
  withProgress(message = 'Importing Data - Please wait', value = 0.1,{
    
    #reads in the csv file
    file <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    file$X <- NULL
    #make sure there are 100 points per tongue contour......................
    #..............................................
    #..............................................
    nameOfUnit <- names(file)[ncol(file)]
    
    dat <- file
    #..............................................
    #..............................................
    #..............................................
    file <- dat
    
    df <- file
    
    cntr <- 1
    
    for(spi in sort(unique(df$speaker))){
      dfspi <- df[df$speaker == spi,]
      
      for(segi in sort(unique(dfspi$segment))){
        dfsegi <- dfspi[dfspi$segment == segi,]
        
        for(repi in sort(unique(dfsegi$repetition))){
          dfrepi <- dfsegi[dfsegi$repetition == repi,]
          
          for(fri in sort(unique(dfrepi$frame))){
            dffri <- dfrepi[dfrepi$frame == fri,]
            
            xvalues <- dffri[dffri$coord == 'x',nameOfUnit]
            yvalues <- dffri[dffri$coord == 'y',nameOfUnit]
            
            xvalues <- approx(xvalues, n = 100)$y
            yvalues <- approx(yvalues, n = 100)$y
            
            newvalues <- as.vector(rbind(xvalues,yvalues))
            
            tmpdat <- data.frame(speaker = spi, segment = segi, repetition = repi, 
                                 frame = fri, point = rep(1:100, each = 2), 
                                 coord = rep(c('x', 'y')))
            tmpdat[[nameOfUnit]] <- newvalues
            
            if(cntr == 1){
              newdat <- tmpdat
            }else{
              newdat <- rbind(newdat, tmpdat)
            }
            
            cntr <- cntr + 1
            
          }
        }
      }
    }
    #..............................................
    # Establishes the length of contours for all data
    #..............................................
    file <- newdat
    
    file$speaker <- as.character(file$speaker)
    
    #gest the label of the speaker
    speakers_lbl <- unique(file$speaker)
    #gets number of speakers
    speakers_nmbr <- length(speakers_lbl)
    
    #creates an empty dataframe
    df0 <- data.frame(matrix(ncol = 4, nrow = 0))
    #creates the names of the dataframe
    names(df0) <- c("speaker", "segment", "repetition", "frame")
    #creates a dataframe to store running values
    old_df <- df0
    
    #iterates for the total number of speakers
    for(i in 1:speakers_nmbr)
    {
      #gets the label of the segment
      segments_lbl <- unique(file[file$speaker == speakers_lbl[i], 'segment'])
      #gets the number of segments
      segments_nmbr <- length(segments_lbl)
      
      #iterates for the total number of segments
      for(j in 1:segments_nmbr)
      {
        #gets the labels of the rpetitions for the iterated segment
        repetitions_lbl <- unique(file[file$speaker == speakers_lbl[i] & 
                                         file$segment == segments_lbl[j], 'repetition'])
        #gets the number of repetitions for the iterated segment
        repetitions_nmbr <- length(repetitions_lbl)
        
        #iterates for the total number of repetitions in the iterated segment
        for(k in 1:repetitions_nmbr)
        {
          #gets the labels of the frames for each repetition
          frames_lbl <- unique(file[file$speaker == speakers_lbl[i] & file$segment == segments_lbl[j] & 
                                      file$repetition == repetitions_lbl[k], 'frame'])
          #gets the number of frames for each repetition
          frames_nmbr <- length(frames_lbl)
          
          #creates a temporal dataframe to store the iterated values
          df1 <- data.frame(matrix(ncol = 4, nrow = 1))
          #creates names of the uterated dataframe
          names(df1) <- c("speaker", "segment", "repetition", "frame")
          
          #gets the segment labels
          tmp_seg_lbl <- sort(unique(segments_lbl[j]))
          
          #stores speaker label
          df1[1,1] <- speakers_lbl[i]
          #stores segment label
          df1[1,2] <- tmp_seg_lbl
          #stores the repetition label
          df1[1,3] <- repetitions_lbl[k]
          #stores the frame label
          df1[1,4] <- frames_nmbr
          
          #adds the new line to the working dataframe
          current_df <- rbind(old_df, df1)
          
          #adds the working dataframe to the final dataframe
          old_df <- current_df
        }
      }
    }
    #names the columns of the dataframe
    names(current_df) <- c("speaker", "segment", "repetition", "frame")
  })
  
  #Returns the values from the reactive value
  #There are three outputs:
  #1. The csv file of all the points of all contours
  #2. The csv file of the summarised dataframe: per speaker, segment, repetition, and frame
  #3. The name of the units of the dataframe

  unique_labels <- sort(unique(file$segment))
  
  palateTraceNameID <- input$palateTraceName
  
  if(length(grep(paste0('\\b', palateTraceNameID, '\\b'), unique_labels)) != 0){
    pal_vals_data$d <- file[file$segment == palateTraceNameID,]
    pal_vals_datainspection$d <- file[file$segment == palateTraceNameID,]
  }
  
  return(list(file, current_df, nameOfUnit))
})