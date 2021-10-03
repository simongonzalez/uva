plotSSANOVAData <- reactive({
  #if there is no input file
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
  
  show_modal_spinner(spin = 'bounce', text = 'Please wait. SSANOVA calculations in progress.') # show the modal window
  
  #gets the unique labels of the speakers
  spkrs <- input$speakerANOVA
  
  cmp1 <- input$comparison1SSANOVA
  cmp2 <- input$comparison2SSANOVA
  
  #imports the input file
  fileIn <- importFiles()
  #selects the dataframe with values
  fileIn <- fileIn[[1]]
  
  fileIn <- fileIn[fileIn$speaker == spkrs,]
  
  #gets input measurement unit
  ms <- as.character(input$radio_measures)
  #Measurement*********************************************
  if (ms == 1){
    #measurements in millimiters
    #d <- subset(d, select = -c(pixel))
    measure_val <- "mm"
    measure_valPlot <- "mm"
  }else if (ms == 2){
    #measurements in pixels
    #d <- subset(d, select = -c(mm))
    measure_val <- "pixel"
    measure_valPlot <- "pixels"
  }
  
  tmpspeaker <- spkrs
  tmpfirstsegment <- cmp1
  tmpsecondsegment <- cmp2
  measuremtnUnit <- measure_val
  
  print('start1')
  
  dat <- fileIn
  dat <- dat[dat$speaker == tmpspeaker & dat$segment %in% c(tmpfirstsegment, tmpsecondsegment),]
  
  dat$speaker <- NULL
  dat$X <- NULL
  
  dat <- spread(dat, coord, measure_val)
  
  dat$point <- NULL
  
  names(dat) <- c('word', 'repetition', 'time.frame', 'X', 'Y')
  
  dat <- dat[c('X', 'Y', 'word', 'time.frame', 'repetition')]
  
  write.csv(dat, 'testdata.csv', row.names = F)
  
  tmpxpoint <- max(dat$X)
  tmpypoint <- min(dat$Y) - (1/abs(min(dat$Y) - max(dat$Y)))
  
  if(input$polarPlot){
    
    datraw <- dat
    
    dat$X <- abs(tmpxpoint - dat$X)
    dat$Y <- abs(tmpypoint - dat$Y)
    
    dat$X <- sqrt((dat$X^2) + (dat$Y^2))#radius
    dat$Y <- atan2(dat$Y, dat$X)#theta
  }
  
  group1 <- tmpfirstsegment
  group2 <- tmpsecondsegment
  
  #making a data frame containing just the target words/groups
  w1w2<-droplevels(subset(dat,word%in%c(group1,group2)))
  
  if(input$polarPlot){
    w1w2raw<-droplevels(subset(datraw,word%in%c(group1,group2)))
  }
  
  # Add individual data points:
  comparisonInd <- ggplot(data= w1w2, aes(x=jitter(X),y=Y,colour = word,
                                          group = interaction(word, time.frame, repetition))) +
    stat_smooth(geom='line',alpha = 0.75) +
    #geom_point(alpha = 0.75) + 
    scale_y_reverse() + ylab("y")+ theme_bw()
  
  # fit model
  ssa.fit <-ssanova(Y~word+X+word:X,data=w1w2)
  
  # get predictions for constructing confidence intervals
  X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
  grid <- expand.grid(X=X ,word = c(group1,group2))
  grid$ssa.fit <- predict(ssa.fit,newdata = grid,se = T)$fit
  grid$ssa.SE <- predict(ssa.fit,newdata = grid,se = T)$se.fit
  
  if(input$polarPlot){
    tmpnewx <- ssa.fit$mf$X
    tmpnewy <- ssa.fit$mf$Y
    
    ssa.fit$mf$X <- tmpnewx * cos(tmpnewy)
    ssa.fit$mf$Y <- tmpnewx * sin(tmpnewy)
  }
  
  # plotting comparison:
  # set up plot object:
  comparison <- ggplot(grid,aes(x = X,colour = word,group = word))+ theme_bw()
  
  # for greyscale plots, uncomment the next line
  # comparison <- comparison + scale_fill_grey()
  cbPalette <- c("#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # for colors that will print well in greyscale, uncomment the next line
  # comparison <- comparison+scale_fill_brewer(palette="PuBuGn")
  
  # Main effects plot
  comparison<-comparison  + geom_line(aes(y = ssa.fit),alpha = 1,colour = "grey20")
  comparison<-comparison + geom_ribbon(aes(ymin = ssa.fit-(1.96*ssa.SE), ymax = ssa.fit+(1.96*ssa.SE),fill = word ),alpha = 0.75,colour = "NA")
  
  #flip the Y axis
  comparison<-comparison + scale_y_reverse()+ scale_fill_manual(values=cbPalette)
  # labels
  comparison<-comparison + ylab("y")
  
  if(input$polarPlot){
    # fit model
    ssa.fitraw <-ssanova(Y~word+X+word:X,data=w1w2raw)
    
    # Interaction effects plots:
    Xraw=seq(min(w1w2raw$X),max(w1w2raw$X),by=0.01)
    gridraw <- expand.grid(X = Xraw,word = c(group1,group2))
    gridraw$Fit <- predict(ssa.fitraw,gridraw,se = T,inc = c("word","word:X"))$fit
    gridraw$SE <- predict(ssa.fitraw,gridraw,se = T,inc = c("word","word:X"))$se.fit
    inter <- ggplot(gridraw,aes(x=X))+theme_bw()
  }else{
    # Interaction effects plots:
    X=seq(min(w1w2$X),max(w1w2$X),by=0.01)
    grid <- expand.grid(X = X,word = c(group1,group2))
    grid$Fit <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$fit
    grid$SE <- predict(ssa.fit,grid,se = T,inc = c("word","word:X"))$se.fit
    inter <- ggplot(grid,aes(x=X))+theme_bw()
  }
  
  inter <- inter + geom_line(aes(y = Fit))
  inter <- inter + geom_ribbon(aes(ymax = Fit+(1.96*SE),ymin = Fit -(1.96*SE)),alpha=0.5)
  inter <- inter + facet_wrap(~word)
  inter <- inter + geom_hline(yintercept = 0,lty = 2)
  inter <- inter + ylab("y")
  
  #add labels
  comparison <- comparison + xlab(paste0('Length (', measure_valPlot, ')')) +
    ylab(paste0('Height (', measure_valPlot, ')')) + labs(fill = "Segment") + 
    theme(text = element_text(size=20))
  comparisonInd <- comparisonInd + xlab(paste0('Length (', measure_valPlot, ')')) +
    ylab(paste0('Height (', measure_valPlot, ')')) + labs(colour = "Segment") + 
    theme(text = element_text(size=20))
  inter <- inter + xlab(paste0('Length (', measure_valPlot, ')')) +
    ylab(paste0('Difference (', measure_valPlot, ')')) + 
    theme(text = element_text(size=20))
  
  remove_modal_spinner() # remove it when done
  
  return(list(comparisonInd, comparison, inter, grid))
  
})