#######################################
# Infrastructure Geocoder Server Code #
#######################################

library(shiny)
library(shinyjs)
library(sf) 
library(googlesheets)

#### Functions
# Function to create Icons for map
createIcon <- function(color) {
  
  custom_icon <- awesomeIcons(
    icon = 'circle-o',
    iconColor = '#ffffff',
    library = 'fa',
    # The markercolor is from a fixed set of color choices
    markerColor = color
  )
  return(custom_icon)
}


#### Prep Code
# Load intersections
intersections <- read_sf('data/Intersections/Intersections.shp')
streets <- read_sf('data/Centerline/Streets.shp')
councilDistricts <- read_sf('data/CouncilDistricts/CnclDist_July2012_wgs84.shp')
dotDistricts <- read_sf('data/DOTDistricts/LADOT_District_wgs84.shp')

#### Server Code
server <- function(input, output, session) {
  
  ### UI Elements
  # Treatment Type Selection
  output$treatment_type <- renderUI({
    
    # Selection Input
    selectInput("treatment_type",
                label = "Treatment Type",
                c("Intersection Tightening"))
  })
  
  # Intersection Selection
  output$int_select <- renderUI({
    
    # Selection Input
    selectizeInput(inputId = "int",
                   label = "Intersection",
                   choices = intersections$TOOLTIP,
                   selected = NULL,
                   multiple = FALSE)
  })
  
  
  # Message Object
  output$message <- renderText({rv_msg$msg})
  # here we should render a list for each item in the msg
  output$message <- renderUI({
    #HTML(paste(rv_msg$msg, sep = '<br/>'))
    #HTML(lapply(rv_msg$msg, paste, collapse = " "))
    HTML(paste(rv_msg$msg, collapse ='<br/>'))
  })
  
  ### Reactive Objects
  #RV for location objects
  rv_location <- reactiveValues(Intersection=list(),
                                Segment=list())
  #RV storing UI message variable
  rv_msg <- reactiveValues()
  
  # Reactive expression to grab intersection data based on user selection
  intersection_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # int_query <- paste0("SELECT * FROM intersections WHERE tooltip=","'",toString(input$int),"'")
      # intersection_r <- sqlQuery(int_query, type = 'spatial')
      intersection_r <- intersections %>%
        filter(TOOLTIP == toString(input$int))

    } else {return(NULL)}
  })
  
  # Reactive expression to grab the council district based on the point
  cd_r <- reactive({
    if(!is.null(rv_location$Intersection)){
      cd_r <- sf::st_join(rv_location$Intersection, councilDistricts)
      return(cd_r$DISTRICT)
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to grab the DOT district based on the point
  dotR <- reactive({
    if(!is.null(rv_location$Intersection)){
      dotR <- sf::st_join(rv_location$Intersection, dotDistricts)
      return(dotR$DOT_DIST)
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreet_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersection_r <- intersection_r()
      # Query for streets related to the intersection
      # xstreet_query = paste0("SELECT *
      #                         FROM streets 
      #                         WHERE int_id_fro=",intersection_r$cl_node_id," OR int_id_to=",intersection_r$cl_node_id)
      # xstreet <- sqlQuery(xstreet_query, type='spatial')
      xstreet <- streets %>%
        filter(INT_ID_FRO == intersection_r$CL_NODE_ID | INT_ID_TO == intersection_r$CL_NODE_ID)
    } else {return(NULL)}
  })
  
  ### Map
  output$map <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11) 
  })
  
  # Map observer that updates based on the intersection
  observeEvent(input$int, {
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Get intersection reactive var, clear markers, clear RV
      intersection_r <- intersection_r()
      rv_location$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersection_r) == 1 && length(intersection_r) > 0) {
        # Add intersection to RV object
        rv_location$Intersection <- intersection_r
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          icon = createIcon('darkblue')
        )
        
        if(input$treatment_type == "Intersection Tightening") {
          rv_msg$msg <- c(paste0("Coordinates: ", toString(intersection_r$geometry)),
                          paste0("Council District: ",toString(cd_r())),
                          paste0("DOT District: ", toString(dotR())))

        } else {
          # Get cross streets
          xstreet_r <- xstreet_r()
          # Update message to choose a street
          rv_msg$msg <- c('Select a Cross Street')
          
          # If there is at least one related segment, add it
          if(length(xstreet_r) > 0) {
            proxy %>% addPolylines(
              data = xstreet_r,
              layerId = as.numeric(rownames(xstreet_r)),
              color = "gray"
            )
          }
        }

        # If there is > 1 marker, gray initially
      } else if(nrow(intersection_r) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          layerId = as.numeric(rownames(intersection_r)),
          icon = createIcon("gray")
        )
        rv_msg$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersection_r)[1]),
                          lat1 = as.double(st_bbox(intersection_r)[2]),
                          lng2 = as.double(st_bbox(intersection_r)[3]),
                          lat2 = as.double(st_bbox(intersection_r)[4]))
      
      }
  })
  
  # Map Observer based on int selection
  observeEvent(input$map_marker_click, {
    if(nrow(intersection_r()) > 1) {
      # Grab ID of the shape that was clicked
      click_id <- input$map_marker_click$id
      # Filter intersections based on the click
      intS <- intersection_r() %>%
        filter(rownames(.) == click_id)
      rv_location$Intersection <- intS
      # Add selected line on top
      proxy <- leafletProxy("map") %>%
        # Add selected intersection to map
        addAwesomeMarkers(
          data = intS,
          layerId = "intselected",
          icon = createIcon('darkblue')
        )
      
      rv_msg$msg <- c(paste0("Coordinates: ", toString(rv_location$Intersection$geometry)),
                      paste0("Council District: ",toString(cd_r())),
                      paste0("DOT District: ", toString(dotR())))
    }
  })
  
  # Map Observer based on the polyline selection
  observeEvent(input$map_shape_click, {
    if(!is.null(xstreet_r())){
      # Grab ID of the shape that was clicked
      click_id <- input$map_shape_click$id
      # Filter polylines based on the click
      polyline_s <- xstreet_r() %>%
        filter(rownames(.) == click_id )
      rv_location$Segment <- polyline_s
      # Add selected line on top as another color
      proxy <- leafletProxy("map") %>%
        # Add selected line shape to map
        addPolylines(
          data = polyline_s,
          layerId = "selected",
          color = "#0066a1",
          opacity = 1
        )
      # Once user has selected the street segment, becomes NULL
      rv_msg$msg <- c('.')
    }
    
  })

}

