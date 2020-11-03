tabPanel("Manage Data",
         titlePanel(NULL),
         fluidPage(
           box(width=12,
               h3(strong("Manage Data"),align="center"),
               hr(),
               column(12,
                      HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                      bsButton('Del_row_head', label = 'Delete selected rows', icon("chevron-circle-left"), style = "primary"),
                      bsButton('Compare_row_head', label = 'Compare selected rows', icon("chevron-circle-left"), style = "primary"),
                      HTML('</div>')
               ),
               
               column(12,DT::dataTableOutput("Main_table")),
               tags$script(HTML('$(document).on("click", "input", function () {
                                var checkboxes = document.getElementsByName("row_selected");
                                var checkboxesChecked = [];
                                for (var i=0; i<checkboxes.length; i++) {
                                
                                if (checkboxes[i].checked) {
                                checkboxesChecked.push(checkboxes[i].value);
                                }
                                }
                                Shiny.onInputChange("checked_rows",checkboxesChecked);
                                })')),
               tags$script("$(document).on('click', '#Main_table button', function () {
                                  Shiny.onInputChange('lastClickId',this.id);
                                  Shiny.onInputChange('lastClick', Math.random())
                                  });")
               
           )
         ))