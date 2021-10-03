tabPanel("Load",
         titlePanel("Upload Files"),
         fluidPage(
           column(width = 4,
                  checkboxInput("main_invert_y", label = "DefaultY", value = F),
                  fileInput('file1', 'Choose file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                            )
                  ),
                  tags$hr(),
                  textInput('palateTraceName', 'Palate Trace label', value = 'pal'),
                  tags$hr(),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ','),
                  radioButtons('quote', 'Quote',
                               c(None='',
                                 'Double Quote'='"',
                                 'Single Quote'="'"),
                               '"'),
                  tags$hr()
                  
           )
         ))