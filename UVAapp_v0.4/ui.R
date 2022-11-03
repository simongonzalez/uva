library(colourpicker)
library(data.table)
library(DT)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggsci)
library(ggthemes)
library(gss)
library(highcharter)
library(periscope)
library(plotly)
library(pryr)
library(rAmCharts)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(rhandsontable)
library(shiny)
library(shinyBS)
library(shinybusy)
library(shinydashboard)
library(shinyjqui)
library(shinyjs)
library(sp) 
library(tidyverse)
library(tidyr)
library(wesanderson)

#shiny::runApp(display.mode="showcase")

options(shiny.maxRequestSize = 100*1024^2)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,class="input-small"))
}

textInput2<-function (inputId, label, value = "",...) 
{
  tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
                                                         type = "text", value = value,...))
}

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "number", value = value,...))
}

shinyUI(fluidPage(theme = "bootstrap.css",
                  use_busy_spinner(spin = "fading-circle"),
                  
                  navbarPage("Ultrasound Visualisation & Analysis",
                             source(file.path("ui", "ui_welcomePage.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_documentation.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_load.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_data.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_manageData.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_dataInspection.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_landmarks.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_inContext.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_graphics.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_gridlines.R"), 
                                    local = TRUE)$value,
                             source(file.path("ui", "ui_analysis.R"), 
                                    local = TRUE)$value#,
                             # source(file.path("ui", "ui_ssanova.R"), 
                             #        local = TRUE)$value
                             
                  )))
