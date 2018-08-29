###################################
# Infrastructure Geocoder UI Code #
###################################

### Libraries
library(shiny)
library(DT)
library(leaflet)
library(shinyjs)
library(sf)
library(dplyr)

appCSS <- ".mandatory_star { color: red; }"

### User Interface
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel("New Treatment"),
  
  div(
    id = "form",
    fluidRow(
      
      # First UI Bin
      column(4,
             uiOutput("treatment_type"),
             uiOutput("int_select"),
             conditionalPanel(condition = "input.treatment_type != null && input.treatment_type.length > 0")
      ),
      
      # Second UI Bin
      column(4, uiOutput("treatment_info1")),
      
      # Third UI Bin
      column(4, uiOutput("treatment_info2"))
    ),
    
    hr(),
    #br(),
    #h6(uiOutput("message"), align="center"),
    h6(htmlOutput("message"), align="center"),
    hr(),
    
    # Map Output
    leafletOutput("map")
    
  )

)