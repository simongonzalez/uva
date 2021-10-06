#======================================================================================
#======================================================================================
# DATA SUMMARY-------------------------------------------------------------------------
#======================================================================================
#======================================================================================

output$summary <- DT::renderDataTable({
  #if a file has not been input
  if (is.null(input$file1))
    return()
  
  #imports the input file
  file <- importFiles()
  #selects the dataframe with values
  file <- file[[2]]
  
  #print('im here')
  
  #creates the DT table output
  DT::datatable(file, style = "bootstrap", selection="none")
  
  #DT::datatable(file, options = list(searching = T), style = "bootstrap")
})