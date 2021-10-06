#creates the reactive value to create the landmark table
prep_landmark_table <- reactive({
  #if plot data is null
  if(is.null(all_plot_data$d))
    return()
  
  #creates a vector with the names of the columns to be dropped
  drops <- c('point', 'coord', 'pixel', 'mm')
  
  #imports the plot data
  #subsets data to the first point
  #subsets data to the x points
  #drops the columns in the vector
  
  df <- all_plot_data$d %>%
    filter(point == 1) %>%
    filter(coord == 'x') %>%
    dplyr::select(-one_of(drops))
  
  write.csv(df, paste0('./workingFiles/df_landmark.csv'), row.names = F)
  
  #returns the dataframe
  return(df)
})

#creates the landmark table display
output$landmark_table = output$gridline_table = DT::renderDataTable(
  prep_landmark_table(), 
  options = list(lengthChange = T, dom = 'tip', scrollX = TRUE),
  rownames = FALSE, style = "bootstrap"
)


#This section controls the data management

#sets the dataframe and their values
vals <- reactiveValues()
observe({
  if(!is.null(importFiles()[[1]])){
    dat <- as.data.table(importFiles()[[1]])

    dat_short <- dat %>%
      filter(point == 1) %>%
      filter(coord == 'x') %>%
      dplyr::select(speaker,segment,repetition,frame)
    
    vals$Data <- dat_short
  }
})

output$Main_table <- renderDataTable({
  DT <- vals$Data
  DT[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
  
  DT[["Actions"]] <-
    paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
             <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Modify</button>
             </div>
             
             ')
  datatable(DT,
            escape=F, style = "bootstrap")}
)

observeEvent(input$Del_row_head,{
  #gets the numeric value of the row
  row_to_del <- as.numeric(gsub("Row","",input$checked_rows))
  #deletes the row
  vals$Data <- vals$Data[-row_to_del]}
)
#Visualisation 
observeEvent(input$Compare_row_head,{
  #gets the index of the rows
  row_to_del <- as.numeric(gsub("Row","",input$checked_rows))
  #gets the total number of rows to be compared
  number_brands <- length(row_to_del)
  
  #gets the plotting values
  
  #get the index of the rows
  tmp_dat_short <- vals$Data
  tmp_dat_short <- tmp_dat_short[row_to_del]
  plot_dat <- data.frame(matrix(nrow = 0, ncol = ncol(importFiles()[[1]])))
  names(plot_dat) <- names(importFiles()[[1]])
  names(plot_dat)[7] <- 'measurement'
  
  tpm_incoming_data <- as.data.table(importFiles()[[1]])
  names(tpm_incoming_data)[7] <- 'measurement'
  
  for(speaker_i in unique(tmp_dat_short$speaker)){
    speaker_df <- tmp_dat_short[tmp_dat_short$speaker == speaker_i,]
    for(segment_i in unique(speaker_df$segment)){
      segment_df <- speaker_df[speaker_df$segment == segment_i,]
      for(repetition_i in unique(segment_df$repetition)){
        repetition_df <- segment_df[segment_df$repetition == repetition_i,]
        for(frame_i in unique(repetition_df$frame)){
          iterated_df <- tpm_incoming_data[tpm_incoming_data$speaker == speaker_i & 
                                             tpm_incoming_data$segment == segment_i &
                                             tpm_incoming_data$repetition == repetition_i & 
                                             tpm_incoming_data$frame == frame_i,]
          
          names(iterated_df) <- names(plot_dat)
          
          plot_dat <- rbind(plot_dat, iterated_df) 
        }
      }
    }
  }
  
  plot_wide <- spread(plot_dat, coord, measurement)
  
  #creates baseline values
  vals$plotvalues <- as.data.table(plot_wide)
  
  #as factor
  vals$plotvalues[,speaker:=as.factor(speaker)]
  vals$plotvalues[,segment:=as.factor(segment)]
  vals$plotvalues[,repetition:=as.factor(repetition)]
  vals$plotvalues[,frame:=as.factor(frame)]
  #shows the plot in a UI modal window
  showModal(plotvalues_modal)
}
)

#creates a modal dialog with the plot
plotvalues_modal<-modalDialog(
  fluidPage(
    h3(strong("Contours for selected tokens"),align="center"),
    plotOutput('table_view_plot')
  ),
  #size of the modal window, large in this case
  size <- "l"
)

#creates the plot
output$table_view_plot <- renderPlot({
  #line ggplot of table plots
  
  if(length(unique(vals$plotvalues$speaker)) == 1){
    #if only one speaker is selected
    if(length(unique(vals$plotvalues$segment)) == 1){
      #if only one segment number is selected
      if(length(unique(vals$plotvalues$repetition)) == 1){
        #if only one repetition is selected
        if(length(unique(vals$plotvalues$frame)) == 1){
          #if only one frame is selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = frame, colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = frame, colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
        }else{
          #if multiple frames are selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(repetition,frame), 
                                             colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(repetition,frame), 
                                             colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
        }
      }else{
        #if multiple repetitions are selected
        if(length(unique(vals$plotvalues$frame)) == 1){
          #if only one frame is selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = repetition, colour = repetition)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = repetition, colour = repetition)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
          
          
        }else{
          #if multiple frames are selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(repetition,frame), 
                                             colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(repetition,frame), 
                                             colour = frame)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
        }
      }
    }else{
      #if multiple segment numbers are selected
      if(length(unique(vals$plotvalues$repetition)) == 1){
        #if only one repetition is selected
        if(length(unique(vals$plotvalues$frame)) == 1){
          #if only one frame is selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = segment, colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = segment, colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
        }else{
          #if multiple frames are selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, 
                                             group = interaction(segment,frame), colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, 
                                             group = interaction(segment,frame), colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
          
          
        }
      }else{
        #if multiple repetitions are selected
        if(length(unique(vals$plotvalues$frame)) == 1){
          #if only one frame is selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(segment,repetition), 
                                             colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(segment,repetition), 
                                             colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
          
          
        }else{
          #if multiple frames are selected
          
          if(input$main_invert_y){
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(segment,repetition), 
                                             colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }else{
            p <- ggplot(vals$plotvalues, aes(x = point, y = y, group = interaction(segment,repetition), 
                                             colour = segment)) + 
              geom_line(formula = y ~ x, stat = 'smooth', method = 'loess') +
              scale_x_continuous() + scale_y_reverse() + labs(x ='Tongue Advancement', y = 'Tongue Height')
          }
          
          
        }
      }
    }
  }
  
  #print('here')
  p <- p + theme_minimal()
  
  print(p)
  
})

##Managing in row deletion / modification
modal_modify <- modalDialog(
  fluidPage(
    h3(strong("Row modification"),align="center"),
    hr(),
    #row selected
    dataTableOutput('row_modif'),
    #action button to save the changes
    actionButton("save_changes","Save changes"),
    
    tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       
                       Shiny.onInputChange('newValue', list_value)
});"))
  ),
  size="l"
)

#checks the last clicked button
observeEvent(input$lastClick,
             {
               if (input$lastClickId%like%"delete")
               {
                 #if the user clicks the delete button
                 #gets the row index
                 row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                 #deletes it from the dataset
                 vals$Data=vals$Data[-row_to_del]
               }
               else if (input$lastClickId%like%"modify")
               {
                 #if the user clicks the modify button
                 #open a modal window
                 showModal(modal_modify)
               }
             }
)

#modifying the dataset
output$row_modif <- renderDataTable({
  #seltected row to modify
  selected_row <- as.numeric(gsub("modify_","",input$lastClickId))
  #gets the old row values
  old_row <- vals$Data[selected_row]
  #creates a list to store the new values
  row_change <- list()
  
  #iterates through all the columns
  for (i in colnames(old_row))
  {
    if (is.numeric(vals$Data[[i]]))
    {
      #if the column value is numeric
      row_change[[i]] <- paste0('<input class="new_input" type="number" id=new_',i,'><br>')
    }
    else
      #if the column value is a character
      row_change[[i]] <- paste0('<input class="new_input" type="text" id=new_',i,'><br>')
  }
  
  #converts the list values to a datatable class
  row_change <- as.data.table(row_change)
  #sets the names to the column names of the original data
  setnames(row_change,colnames(old_row))
  #adds the row to the original data
  DT <- rbind(old_row,row_change)
  rownames(DT) <- c("Current values","New values")
  DT
  
},escape=F,options=list(dom='t',ordering=F)
)

#if new values are entered
observeEvent(input$newValue,
             {
               newValue=lapply(input$newValue, function(col) {
                 if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                   as.numeric(as.character(col))
                 } else {
                   col
                 }
               })
               DF <- data.frame(lapply(newValue, function(x) t(data.frame(x))))
               colnames(DF) <- colnames(vals$Data)
               vals$Data[as.numeric(gsub("modify_","",input$lastClickId))] <- DF
             }
)