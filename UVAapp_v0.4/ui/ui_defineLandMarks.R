tabPanel("Land Marks",
         sidebarLayout(
           sidebarPanel(
             uiOutput('landMark_numbers_ui'),
             bsButton("create_landmarks", label = "Create", icon("chevron-circle-left"), style = "primary")
           ),
           mainPanel(
             lapply(1:20, function(i) {
               uiOutput(paste0('inLM', i))
             }),
             
             div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('set_LMs_UI')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('add_LMs_UI')),
             div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput('delete_LMs_UI'))
           )))