# Save the Sound's 2020 Action Map
# Documentation https://docs.google.com/document/d/14kDLOjaSYhNqzDJXG9Zn2SWk5T-k_RPIiF2jXS9h_zA/edit?usp=sharing
# Github: https://github.com/ChesapeakeCommons/R-Shiny
# Created by Gabe Watson for The Commons 
# Created 7.20.2020 
# Last edited: 9.21.2020 
# Naming Convention  
#   - Major_Components_Name minor_componentsName, MajorVariable minorvariable 
#   

library(shiny)
library(googlesheets4)
library(leaflet)
library(dplyr)
library(plyr)
library(leaflet.extras)
library(htmltools)

###########  UI Display Script ############
ui <- fluidPage(
#(theme = "styler.css",

# Test
           
  ### DISPLAY COMPONENTS ###
  
  #Map
  div(id = "wrapper",
      
      
      
      #Layer Selection
      selectizeInput("inActionSelector", "Input checkbox",
                     c("Loading Layers"), multiple = TRUE),


      div(id = "main-panel",
          leafletOutput("map")
         )

      # Side Panel Display with Save the Sound Info
      ),
      div(id = "side-panel",
         div( id = "side-panel-wrapper",
              div(  id = "title",
                    img(    id = 'title-image',
                            src='images/save-the-sound-title.png'
                    )
                    
                    #Descriptive Text 
              ),
              div(  class = "description",
                    tags$h1("2020 Action Map"),
                    tags$p("
                        The waters of Long Island Sound
                        touch the lives of millions in
                        New York, Connecticut, and beyond.
                        Together, we can protect this
                        precious natural resource.
                    ")
              ),
              div(  class = "description",
                    tags$h1("200+"),
                    tags$p(
                        HTML("<font style='font-weight: 400;'>Efforts to improve the</font> Long Island Sound")
                   )
                   
                   # Zoom Controls 
              ),
              div(  id = "action-panel",
                 #Map extent switching buttons
                    tags$p("Long Island Region"),

                    actionButton("west", "Long Island"),
                    actionButton("east", "Western Narrows"),
                    actionButton("west", "Eastern Narrows"),
                    actionButton("east", "Western Basin"),
                    actionButton("west", "Central Basin"),
                    actionButton("east", "Eastern Basin"),

                     textOutput("mapclick")
              )
         )
      )
   )

########## Server Side Script ############

server <- function(input, output, session) {
  
  ##### DATASET IMPORT ####
  # this lets us bypass the Authorization step from gsheet4 if they are ok with just having the googlesheet be public - I can hook it up to be private though after the fact.
  gs4_deauth()
  
  Import_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1pZBhwo97IHkp-bNCA-m9UBJyBsO9ngnyuFylfAMbZs4/edit#gid=0")
  
  Symbology_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1N0L7-gZH4iqxrbQVYLtLJU7Dbuz2nVnYE-8wUrzPK6o/edit#gid=0")
  #### END DATASETIMPORT #####
  
  
  #### ACTION INPUT DATA HANDLING ###
  
  # Updates the Action selector once the input sheets are handled above 
  observe({
    updateSelectizeInput(session, "inActionSelector",
                         choices = Import_V1$Action)
  })
  
  # Reactive element that gets the correct Layer Selection and returns "ActionSelection()" for use in mapping from Action Selector
  ActionSelection <- reactive ({
    
    #Converts the input datatype into list --- I think...
    y <- input$inActionSelector
    
    #Sets dataframe to all Actions if no selection is chosen
    if (is.null(y))
       {
      Layers <- Import_V1
      return(Layers)
    }
    #Sets dataframe to selected Actions
    else
      {
    Layers <-filter(Import_V1, Action %in% y)
    return(Layers)
      }
    
  })
  
  
  #Gets the selected zoom extent
  
  extent <- reactiveValues(data=NULL)
  
  observeEvent(input$west, {
    extent$data$lat <- 41.37836624
    extent$data$long <- -82.49326 
    extent$data$zoom <- 12
  })
  
  
  observeEvent(input$east, {
    extent$data$lat <- 41.249059
    extent$data$long <- -82.012431 
    extent$data$zoom <- 12
  })
  
  
  #Gets the latlong from user click and creates variable of Dataframe
  Io_Map_Click <- reactive({
    # Gets the latlongs from the leaflet mapclick 
    lat <- input$map_marker_click$lat
    lon <- input$map_marker_click$lng
    
    # Creating a dataframe that contains the correct results based on user input.
    Io_Data <- Map_Data_All %>% 
      filter(latitude == lat, longitude == lon) 
      return(Io_Data)
    
  })
  
  #### END INPUT DATA HANDLING ###
  
  
  
  #### INFORMATION PANEL ####
  
  output$table <- renderTable({ 
    
    Io_Data <- Io_Map_Click()  
    write.csv(Io_Data, "Io_Data.csv")
    head(Io_Data)
    })
  #### END INFORMATION PANEL ####
  
  
  ##### MAP ####
  output$map <- renderLeaflet({
    
 # ActionLayers <- LayerSelection()
  
#  write.csv(ActionLayers, "ActionLayers2.csv")
    
  groups = as.character(unique(ActionSelection()$Action)) 
  
  map = leaflet(ActionSelection()) %>%
    
  addTiles(group = "OpenStreetMap")
 
  #Loop that runs through dataframe to 
   for(g in groups)
   {
     d = ActionSelection()[ActionSelection()$Action == g, ]
 
     map = map %>% addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                                  color = 277706L,
                                  group = g)
     }
      map %>% addLayersControl(overlayGroups = groups)
 })
    
  
  #### END MAP #####
    
  
  #### OLD MAP WILL DELETE AFTER HARVESTING CODE ####
  # output$map <- renderLeaflet({
  #   
  # 
  #   # map extent logic 
  #   if (is.null(extent$data))
  #   {
  #     extent$data$lat <- 41.314902
  #     extent$data$long <- -82.481357
  #     extent$data$zoom <- 10
  #   }
  #   
  #   
  #   #Icons 
  #   iconTest <- makeIcon(
  #     iconUrl = "images/logo.svg",
  #     iconWidth = 40, iconHeight = 95
  #    # iconAnchorX = 0, iconAnchorY = 0,
  #   )
  #   
  #    iconTest2 <- makeIcon(
  #      iconUrl = "images/greydrop.png",
  #      iconWidth = 10, iconHeight = 15
  #      # iconAnchorX = 0, iconAnchorY = 0,
  #   )
  #   
  #   leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  #      htmlwidgets::onRender("function(el, x) {
  #       L.control.zoom({ position: 'topright' }).addTo(this)
  #   }") %>%
  #     
  #     #Setting default extent and zoom based on user selection. 
  #     setView(lng = extent$data$long, lat = extent$data$lat, zoom = extent$data$zoom)%>%
  #     
  #     #Command to add Circle Markers as layers
  #     addTiles() %>%
  # 
  #     # Adding Satisfactory, Compliance 
  #     addMarkers(data = map_data_LegAct, lng = ~longitude, lat = ~latitude, icon = iconTest,
  #                    #  color = map_data_LegAct$MapColor, 
  #                      label = ~ as.character(map_data_LegAct$Action_Name),
  #                   #   stroke = FALSE, fillOpacity = 0.5,
  #                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
  #                      group = "Legal Actions")%>%
  #     
  #     # Adding Non Compliance
  #     addMarkers(data = map_data_PollAct, lng = ~longitude, lat = ~latitude,icon = iconTest2, popup = ~htmlEscape(map_data_PollAct$Volunteers),
  #                     # color = map_data_PollAct$MapColor, 
  #                      label = ~ as.character(map_data_PollAct$Action_Name),
  #                     # stroke = FALSE, fillOpacity = 0.5,
  #                      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
  #                      group = "Pollution Actions")%>%
  # 
  #     #Adding Basemap 
  #     addProviderTiles(providers$CartoDB.Voyager, group = "Streets")%>%
  #     addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Satellite")%>%
  # 
  #     #Adding geocoder
  #     addSearchOSM(options = searchOptions(zoom=15, position = 'topright',
  #                                           autoCollapse = TRUE,
  #                                           minLength = 2)) %>%
  # 
  #     #Adding layer control 
  #     addLayersControl(
  #       baseGroups = c("Streets", "Satellite"),
  #       overlayGroups = c("Legal Actions", "Pollution Actions"),
  #       options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
  #     )
  #     
  #   
  # })
  
  # TESTING TEXT OUTPUT - WILL BE TURNED INTO POPUP INFORMATION
  
  output$mapclick <- renderText({ 
    #Rreturns text of Map click
    #Asks user to select station if no station is selected. 
    
    if (is.null(input$map_marker_click$lat))  
    {
      Io_Data <- "Select a Station!"
    } 
    else 
    {
      Io_Data <- Io_Map_Click()
    }
    paste(Io_Data)
    
  }
  )
  
  #### END MAP ###
}

# Run the application 
shinyApp(ui = ui, server = server)
