#this reactive value creates the classification button seen in every tongue contour
#if no landmarks have been defined, the button show the label "Undefined"
#if at least one landmark has been created, then the label is "Unclassified"
#if the tongue contour has been classified, then the label shows its classification

#This defines the base number of columns for the dataframe
#Any columns added to this represent added Landmarks

baseline_column_number <- 7

output$see_classification_UI <- renderUI({
  
  #==============================
  #get the speaker
  sp <- input$spkr_LM
  #get the segment
  seg <- input$sgmnt_LM
  #get the repetition
  rep <- input$rpttn_LM
  #get the frame
  frm <- input$frm_LM
  
  inside_df <- all_plot_data$d
  
  tmp_line <- inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                          inside_df$repetition == rep & inside_df$frame == frm, ]
  
  #if no extra labels have been added
  if(length(colnames(tmp_line)) == baseline_column_number){
    
    
    bsButton('see_classification', label = 'Undefined', icon("exclamation"), style = "warning")
  }else{
    
    tmp_vector <- tmp_line[1,8:length(colnames(tmp_line))]
    names(tmp_vector) <- NULL
    tmp_vector <- unlist(c(tmp_vector))
    
    #if extra labels have been added but the frame has not been assigned
    if(length(which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))) == 0){
      bsButton('see_classification', label = 'Unclassified', icon("question-circle-o"), style = "success")
    }else{
      tmp_landmark_label <- tmp_vector[which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))]
      bsButton('see_classification', label = tmp_landmark_label, icon("close"), style = "danger")
    }
  }
})
#==================================================================================================
#This section observes the activity of the see classification button.
observeEvent(input$see_classification,{
  
  #==============================
  #get the speaker
  sp <- input$spkr_LM
  #get the segment
  seg <- input$sgmnt_LM
  #get the repetition
  rep <- input$rpttn_LM
  #get the frame
  frm <- input$frm_LM
  
  inside_df <- all_plot_data$d
  
  tmp_line <- inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                          inside_df$repetition == rep & inside_df$frame == frm, ]
  
  #if no extra labels have been added
  if(length(colnames(tmp_line)) > baseline_column_number){
    tmp_vector <- tmp_line[1,8:length(colnames(tmp_line))]
    names(tmp_vector) <- NULL
    tmp_vector <- unlist(c(tmp_vector))
    
    #if extra labels have been added but the frame has not been assigned
    if(length(which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))) != 0){
      tmp_landmark_label <- tmp_vector[which(grepl("[[:alpha:]]", tmp_vector) | grepl("[[:digit:]]", tmp_vector))]
      
      inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                  inside_df$repetition == rep & inside_df$frame == frm, tmp_landmark_label] <- ""
      
      all_plot_data$d <- inside_df
      
    }
  }
})

#=======================================================================================================
#clicking controls
#counts the number of times user clicks the SET button
set_landmark_count <- reactiveValues(n = 0)
#counts the number of times user clicks the EDIT button
edit_landmark_count <- reactiveValues(n = 0)
#counts the number of times user clicks the DONE button
done_landmark_count <- reactiveValues(n = 0)

existing_landmark_buttons_cntr <- reactiveValues(n = NULL)
existing_landmark_buttons_cntr_ADDBTN <- reactiveValues(n = NULL)

incoming_labels <- reactiveValues(val = NULL)
edit_textInput <- reactiveValues(val = NULL)
edit_textInput_clicked <- reactiveValues(n = 0)
initial_textInput_blank <- reactiveValues(val = 0)

#controls the ADD button behaviour
delete_confirmation <- reactiveValues(val = 0)

#define data==========================================================================================
all_plot_data <- reactiveValues(d = NULL)
button_column_correlation <- reactiveValues(d = NULL)
landmark_labels <- reactiveValues(val = NULL)

inContextButtonBehaviour <- reactiveValues(n = NULL)

#=====================================================================================================
#define Land Marks numbers
output$landMark_numbers_ui <- renderUI({
  #if the number of landmarks has been modified internally after pressing the DONE button
  if(!is.null(existing_landmark_buttons_cntr$n)){
    #number of current active buttons
    tmp_selection <- length(existing_landmark_buttons_cntr$n)
    selectInput("landMark_numbers", label = h5("Number of Landmarks"), 
                choices = c(1:20), selected = tmp_selection)
  }else{
    selectInput("landMark_numbers", label = h5("Number of Landmarks"), 
                choices = c(1:20))
  }
})

#-----------------------------------------------------------------------------------------------------
#creates the input text fields for the labels of the landmarks based on the number entered by the user
observeEvent(input$create_landmarks,{
  #setting this variable to 0 indicates that the textfields are those of the initial run of the program
  set_landmark_count$n <- 0
  input_numbers <- input$landMark_numbers
  lapply(1:input_numbers, function(i) {
    output[[paste0('inLM', i)]] <- renderUI({
      textInput(paste0("inLM_text", i), label = h5(paste0("Landmark ", i)), value = "")
    })
  })
})

#-----------------------------------------------------------------------------------------------------
#creates the button to control setting and Editting of landmarks (labels, deleting, adding)
observeEvent(input$create_landmarks,{
  output$set_LMs_UI <- renderUI({
    #if the first textinout is null, it is the initial run of the program
    if(is.null(input[[paste0("inLM_text", 1)]])){
      bsButton("set_LMs", label = "Set", icon("chevron-circle-left"), style = "primary")
    }else{
      #if a landmark has already been created and is edited or requested to be edited
      if(set_landmark_count$n == 1){
        #checks whether a landmark has been edited or request to be edited
        if(edit_landmark_count$n == 1){
          #if a landmakr has been edited
          bsButton("done_LMs", label = "Done", icon("check-circle"), style = "warning")
        }else{
          #if a landmark is requested to be edited
          bsButton("edit_LMs", label = "Edit", icon("pencil"), style = "danger")
        }
      }else if(set_landmark_count$n == 0){
        #if a landmark has already existed and all have been deleted
        bsButton("set_LMs", label = "Set", icon("chevron-circle-left"), style = "primary")
      }
    }
  })
})

#----------------------------------------------------------------------------------------------------
#creates the landmark buttons and checks whether no landmark label has been entered
observeEvent(input$set_LMs,{
  #input number of Landmarks
  input_numbers <- input$landMark_numbers
  
  #stores the active Landmark labels
  incoming_labels$val <- NULL
  
  #empty conditions
  #if the user presses Set and no input has been made, check whether all inputs are blank
  initial_textInput_blank$val <- 0
  
  lapply(1:input_numbers, function(i) {
    tmp_label <- input[[paste0("inLM_text", i)]]
    
    if(tmp_label != ""){
      initial_textInput_blank$val <- isolate(initial_textInput_blank$val) + 1
      incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
    }
  })
  
  #if not input has been made, clear all the text inputs and the Set button
  if(initial_textInput_blank$val == 0){
    lapply(1:input_numbers, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        return()
      })
    })
    #sets SET button to null
    output$set_LMs_UI <- renderUI({
      return()
    })
    
    #sets the buttons in context in the InContext window to NULL
    lapply(1:input_numbers, function(i) {
      output[[paste0('inLM_inContext', i)]] <- renderUI({
        return()
      })
    })
    
    landmark_labels$val <- NULL
    button_column_correlation$d$btn_id <- NULL
    button_column_correlation$d$column_label <- NULL
  }else{
    landmark_labels$val <- incoming_labels$val
    button_column_correlation$d$btn_id <- paste0("inLM_inContext_btn", 1:length(landmark_labels$val))
    button_column_correlation$d$column_label <- landmark_labels$val
    
    #if some input has been made
    set_landmark_count$n <- 1
    
    lapply(1:initial_textInput_blank$val, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        tmp_label <- incoming_labels$val[i]
        
        if(tmp_label != ''){
          bsButton(paste("set_LMs_btn", i), label = tmp_label, icon("flag-o"), style = "warning", block = T)
        }
      })
    })
    
    #sets the InContext buttons
    #plots the buttons in the inContext window
    lapply(1:initial_textInput_blank$val, function(i) {
      output[[paste0('inLM_inContext', i)]] <- renderUI({
        tmp_label <- incoming_labels$val[i]
        
        if(tmp_label != ''){
          
          bsButton(paste0("inLM_inContext_btn", i), label = tmp_label, icon("flag-o"), style = "warning")
          
        }
      })
    })
    
    #adds counter to all the in context buttons
    for(add_counter_i in 1:length(landmark_labels$val)){
      inContextButtonBehaviour$n[add_counter_i] <- 0
    }
    
    #print(inContextButtonBehaviour$n)
    
    #creates the dataframe
    if(!is.null(landmark_labels$val)){
      tmp_df <- importFiles()
      tmp_df <- tmp_df[[1]]
      
      input_numbers <- length(landmark_labels$val)
      
      for(i_add_col in landmark_labels$val){
        tmp_df[[i_add_col]] <- NULL
      }
      
      all_plot_data$d <- tmp_df
      
      #View(all_plot_data$d)
    }
    
    edit_textInput_clicked$n <- 0
  }
})

#adds the values to the data shown in inContext table
observe({
  lapply(1:length(landmark_labels$val), function(i) {
    observeEvent(input[[paste0("inLM_inContext_btn", i)]],{
      
      tmp_display_df <- data.frame(button_id = button_column_correlation$d$btn_id, 
                                   column_label = button_column_correlation$d$column_label)
      
      #get the Landmark column ID
      tmp_column_name <- as.character(tmp_display_df[tmp_display_df$button_id == 
                                                       paste0("inLM_inContext_btn", i), 'column_label'])
      
      #set the landmark in the file
      #==============================
      #get the speaker
      sp <- input$spkr_LM
      
      #get the segment
      seg <- input$sgmnt_LM
      
      #get the repetition
      rep <- input$rpttn_LM
      
      #get the frame
      frm <- input$frm_LM
      
      #store the value
      inside_df <- all_plot_data$d
      
      #deletes the current labels of the frame or sets it to ""
      for(delete_i in incoming_labels$val){
        if(length(which(delete_i %in% colnames(all_plot_data$d))) != 0){
          inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                      inside_df$repetition == rep & inside_df$frame == frm, delete_i] <- ""
        }
      }
      
      #find the row with the specified information
      inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                  inside_df$repetition == rep & inside_df$frame == frm, tmp_column_name] = tmp_column_name
      
      all_plot_data$d <- inside_df
      #View(all_plot_data$d)
    })
  })
})

#=======================================================================================================
#creates the edit button landmarks
#displays as text
observeEvent(input$edit_LMs,{
  edit_landmark_count$n <- 1
  input_numbers <- input$landMark_numbers
  lapply(1:input_numbers, function(i) {
    output[[paste0('inLM', i)]] <- renderUI({
      if(edit_textInput_clicked$n  == 1){
        tmp_label <- incoming_labels$val[i]
      }else{
        tmp_label <- input[[paste0("inLM_text", i)]]
      }
      textInput(paste0("inLM_text", i), label = NULL, value = tmp_label)
    })
  })
})
#-----------------------------------------------------------------------------------------------------
#add button
observeEvent(input$edit_LMs,{
  output$add_LMs_UI <- renderUI({
    bsButton("add_LMs_btn", label = NULL, icon("plus-circle"), style = "warning", block = T)
  })
  output$delete_LMs_UI <- renderUI({
    if(delete_confirmation$val == 0){
      bsButton("delete_LMs_btn", label = 'All', icon("times-circle"), style = "danger", block = T)
    }else{
      bsButton("delete_LMs_btn", label = 'Sure', icon("question-circle"), style = "danger", block = T)
    }
  })
})
#---------------------------------------------------------------------------------------------------
#creates the input text fields for the labels of the landmarks when the ADD button is pressed
observeEvent(input$add_LMs_btn,{
  set_landmark_count$n <- 1
  
  existing_landmark_buttons_cntr$n <- as.numeric(input$landMark_numbers) + 1
  existing_landmark_buttons_cntr_ADDBTN$n <- existing_landmark_buttons_cntr$n
  input_numbers <- as.numeric(input$landMark_numbers)
  incoming_labels$val <- NULL
  
  #find and redefine the number id of the textfields
  lapply(1:input_numbers, function(i) {
    tmp_label <- input[[paste0("inLM_text", i)]]
    
    if(tmp_label != ""){
      existing_landmark_buttons_cntr_ADDBTN$n <- append(isolate(existing_landmark_buttons_cntr_ADDBTN$n), i)
      incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
    }
  })
  
  incoming_labels$val <- append(isolate(incoming_labels$val), '')
  
  lapply(1:(existing_landmark_buttons_cntr_ADDBTN$n), function(i) {
    output[[paste0('inLM', i)]] <- renderUI({
      textInput(paste0("inLM_text", i), label = NULL, value = incoming_labels$val[i])
    })
  })
})

#----------------------------------------------------------------------------------------------------
#controls for the delete button
observeEvent(input$delete_LMs_btn,{
  
  if(delete_confirmation$val == 1){
    #deletes the columns from the dataframe
    tmp_df <- all_plot_data$d
    tmp_current_column_labels <- names(tmp_df)[-c(1:baseline_column_number)]
    for(delete_i in tmp_current_column_labels){
      tmp_df[[delete_i]] <- NULL
    }
    
    all_plot_data$d <- tmp_df
    
    #View(all_plot_data$d)
    
    #deletes all inputs
    lapply(1:20, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        return()
      })
    })
    
    #deletes in context buttons UIs
    lapply(1:20, function(i) {
      output[[paste0('inLM_inContext', i)]] <- renderUI({
        return()
      })
    })
    
    #deletes set button
    output$set_LMs_UI <- renderUI({
      return()
    })
    
    #deletes add button
    output$add_LMs_UI <- renderUI({
      return()
    })
    
    #deletes delete button
    output$delete_LMs_UI <- renderUI({
      return()
    })
  }
  
  #updates the value for asking the user
  if(delete_confirmation$val == 0){
    delete_confirmation$val <- 1
    landmark_labels$val <- NULL
    button_column_correlation$d$btn_id <- NULL
    button_column_correlation$d$column_label <- NULL
  }else{
    delete_confirmation$val <- 0
    set_landmark_count$n <- 1
    edit_landmark_count$n <- 0
  }
})

#------------------------------------------------------------------------------------------------------
#when editing is finished
observeEvent(input$done_LMs,{
  edit_landmark_count$n <- 0
  done_landmark_count$n <- 1
  existing_landmark_buttons_cntr$n <- NULL
  
  #checks if a new textinput field has been added by pressing the ADD button
  if(is.null(existing_landmark_buttons_cntr_ADDBTN$n)){
    input_numbers <- input$landMark_numbers
  }else{
    input_numbers <- length(existing_landmark_buttons_cntr_ADDBTN$n)
  }
  
  existing_landmark_buttons_cntr_ADDBTN$n <- NULL
  
  #gets the labels of the textinput fields
  incoming_labels$val <- NULL
  
  lapply(1:input_numbers, function(i) {
    tmp_label <- input[[paste0("inLM_text", i)]]
    
    if(tmp_label != ""){
      existing_landmark_buttons_cntr$n <- append(isolate(existing_landmark_buttons_cntr$n), i)
      incoming_labels$val <- append(isolate(incoming_labels$val), tmp_label)
    }
  })
  
  #deletes all textinput fields
  lapply(1:20, function(i) {
    output[[paste0('inLM', i)]] <- renderUI({
      return()
    })
  })
  
  lapply(1:20, function(i) {
    updateTextInput(session, paste0("inLM_text", i), label = h5(paste0("Landmark ", i)), value = NULL)
  })
  
  if(!is.null(existing_landmark_buttons_cntr$n)){
    for(change_value in 1:length(existing_landmark_buttons_cntr$n)){
      existing_landmark_buttons_cntr$n[change_value] <- change_value
    }
  }else{
    output$set_LMs_UI <- renderUI({
      return()
    })
  }
  
  if(is.null(incoming_labels$val)){
    
    landmark_labels$val <- NULL
    button_column_correlation$d$btn_id <- NULL
    button_column_correlation$d$column_label <- NULL
    
    lapply(1:20, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        return()
      })
    })
  }else{
    
    landmark_labels$val <- incoming_labels$val
    button_column_correlation$d$btn_id <- paste0("inLM_inContext_btn", 1:length(landmark_labels$val))
    button_column_correlation$d$column_label <- landmark_labels$val
    
    #creates the landmark buttons
    set_landmark_count$n <- 1
    lapply(existing_landmark_buttons_cntr$n, function(i) {
      output[[paste0('inLM', i)]] <- renderUI({
        tmp_label <- incoming_labels$val[i]
        bsButton(paste("set_LMs_btn", i), label = tmp_label, icon("flag-o"), 
                 style = "warning", block = T)
      })
    })
    
    #creates the InContext landmark buttons
    lapply(existing_landmark_buttons_cntr$n, function(i) {
      output[[paste0('inLM_inContext', i)]] <- renderUI({
        tmp_label <- incoming_labels$val[i]
        
        if(tmp_label != ''){
          bsButton(paste0("inLM_inContext_btn", i), label = tmp_label, icon("flag-o"), 
                   style = "warning")
        }
      })
    })
    
    #controls the columns editing
    tmp_df <- all_plot_data$d
    
    input_labels_number <- length(landmark_labels$val)
    
    #check whether changes have been made
    tmp_current_column_labels <- names(tmp_df)[-c(1:baseline_column_number)]
    
    #if a change has been made
    if((length(landmark_labels$val) == length(tmp_current_column_labels) &
        length(setdiff(landmark_labels$val, tmp_current_column_labels)) != 0) ||
       (length(landmark_labels$val) != length(tmp_current_column_labels))){
      #if a label has been added or removed
      if(length(landmark_labels$val) != length(tmp_current_column_labels)){
        #if a label has been added
        if(length(tmp_current_column_labels) < input_labels_number){
          #gets the new label
          tmp_new_label <- setdiff(landmark_labels$val, tmp_current_column_labels)
          #adds labels
          for(i_new_add_column in tmp_new_label){
            tmp_df[[i_new_add_column]] <- NULL
          }
        }else if(length(tmp_current_column_labels) > input_labels_number){
          #if a label has been deleted
          #gets the new label
          tmp_delete_label <- setdiff(tmp_current_column_labels, landmark_labels$val)
          for(i_delete_column in tmp_delete_label){
            tmp_df[[i_delete_column]] <- NULL
          }
        }
      }else{
        if(length(setdiff(landmark_labels$val, tmp_current_column_labels)) != 0){
          current_label <- setdiff(tmp_current_column_labels, landmark_labels$val)
          new_label <- setdiff(landmark_labels$val, tmp_current_column_labels)
          
          for(i_change_label in 1:length(current_label)){
            tmp_df[[new_label[i_change_label]]] <- NULL
            tmp_df[[current_label[i_change_label]]] <- NULL
          }
        }
      }
    }
    
    all_plot_data$d <- tmp_df
    
    #View(all_plot_data$d)
    
    #adds the clicked Landmark to the dataset
    if(!is.null(landmark_labels$val)){
      lapply(1:input_numbers, function(i) {
        observeEvent(input[[paste0("inLM_inContext_btn", i)]],{
          
          tmp_display_df <- data.frame(button_id = button_column_correlation$d$btn_id, 
                                       column_label = button_column_correlation$d$column_label)
          
          #get the Landmark column ID
          tmp_column_name <- as.character(tmp_display_df[tmp_display_df$button_id == 
                                                           paste0("inLM_inContext_btn", i), 'column_label'])
          #set the landmark in the file
          #==============================
          #get the speaker
          sp <- input$spkr_LM
          
          #get the segment
          seg <- input$sgmnt_LM
          
          #get the repetition
          rep <- input$rpttn_LM
          
          #get the frame
          frm <- input$frm_LM
          
          #store the value
          inside_df <- all_plot_data$d
          
          #find the row with the specified information
          inside_df[inside_df$speaker == sp & inside_df$segment == seg &
                      inside_df$repetition == rep & inside_df$frame == frm, 
                    tmp_column_name] <- tmp_column_name
          
          all_plot_data$d <- inside_df
          #View(all_plot_data$d)
        })
      })
    }
    
    edit_textInput_clicked$n <- 1
  }
  
  output$add_LMs_UI <- renderUI({
    return()
  })
  
  output$delete_LMs_UI <- renderUI({
    return()
  })
})