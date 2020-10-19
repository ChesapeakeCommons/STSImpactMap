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
      
      uiOutput("Filters"),
      
      
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
  
  Data_Test_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/10VMsQ57EL25gDjb7bAEjOZDI2mEWiOkIoHwHWNW0MOE/edit#gid=0")
  Import_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1_kWe4pEOo7yur9WPMh1YabdCSjoIz-yS37jM6T5_oWw/edit#gid=0")
  ZoomExtent_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1viLwGCnhsdhfgsgIHjYYj6INNu7YqG_h8srlQsCNf6Y/edit#gid=0")
  # Symbology_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1N0L7-gZH4iqxrbQVYLtLJU7Dbuz2nVnYE-8wUrzPK6o/edit#gid=0")
  
  #### END DATASETIMPORT #####
  
  output$Filters <- renderUI({
    
    req(Data_Test_V1)
    req(ZoomExtent_V1)
    #Zoom Selction 
    tagList(
      selectizeInput("inZoomSelector", "Zoom Extent",
                     choices = ZoomExtent_V1$Extent, multiple = FALSE),
      
      #Action Selection
      selectizeInput("inActionSelector", "Choose an Action",
                     choices = Data_Test_V1$Action, multiple = TRUE),
      
      #Sub Action Selection
      selectizeInput("inSubActionSelector", "Choose a Sub Action",
                     choices = Data_Test_V1$SubAction, multiple = TRUE),
    )
  })
  
  
  
  
  
  
  #### ACTION INPUT DATA HANDLING ###
  
  
  
  
  
  
  
  
  
  
  
  # Reactive element that gets the correct Layer Selection and returns "ActionSelection()" for use in mapping from Action Selector
  ActionSelection <- reactive ({
    
    #Converts the input datatype into list --- I think...
    x <- as.character(input$inActionSelector)
    y <- as.character(input$inSubActionSelector)
     
    # Testing # 
    #write.csv(y, "y.csv")
    
    #Sets dataframe to all Actions if no selection is chosen
    if (length(x) == 0)
    {
      Layers <- Data_Test_V1
      write.csv(Layers, "Layers.csv")
      print(x)
      print(y)
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      return(Layers)
    }
    if (length(x) > 0 && length(y) == 0)
    {
      Layers <- filter(Data_Test_V1, Action %in% x)  
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      write.csv(Layers, "Layers.csv")
      print(x)
      print(y)
      return(Layers)
    }
    if (length(x) > 0 && length(y) > 0)
    {
      Layers <- filter(Data_Test_V1, Action %in% x)
      Layers <- filter(Layers, SubAction %in% y)
      #updateSelectizeInput(session, "inSubActionSelector",
                          # choices = Layers$SubAction)
      write.csv(Layers, "Layers.csv")
      print(x)
      print(y)
      print("test")
      return(Layers)
    }
    
  })
  
  ###### END ACTION INPUT HANDLING 
  
  
  ##### ZOOM EXTENT HANDLING ####
  
  # Updates the Zoom selector once the input sheets are handled above 
  ZoomSelection <- reactive ({
    req(input$inZoomSelector)
    x <- as.character(input$inZoomSelector) 
    print(x)
    # Testing write.csv(x, "x.csv")
    if (x == "Loading Layers")
    {
      ZoomChoice <- ZoomExtent_V1 %>%
        filter(Extent == "Whole Region")
      #  write.csv(ZoomChoice, "Zoom12345.csv")
    }
    else
    {
      ZoomChoice <- 
        filter(ZoomExtent_V1, Extent %in% x)
      write.csv(ZoomChoice, "Zoom12345.csv")
    }
    return(ZoomChoice)
  })
  
  
  #Gets the latlong from user click and creates variable of Dataframe
  MapSelection <- reactive({
    # Gets the latlongs from the leaflet mapclick 
    lat <- input$map_marker_click$lat
    lon <- input$map_marker_click$lng
    
    # Creating a dataframe that contains the correct results based on user input.
    MapClick <- Data_Test_V1 %>% 
      filter(LAT == lat, LONG == lon) 
    return(MapClick)
  })
  
  #### END INPUT DATA HANDLING ###
  
  
  
  
  ##### MAP ####
  output$map <- renderLeaflet({
    #  req(input$inZoomSelector)
    
    groups = as.character(unique(ActionSelection()$Action)) 
    
    
    
    map = leaflet(ActionSelection()) %>%
      addTiles(group = "OpenStreetMap") %>%
      setView(lng = ZoomSelection()$Longitude, lat = ZoomSelection()$Latitude, zoom = ZoomSelection()$Zoom) 
    
    #Loop that runs through dataframe to create the layers and the icons
    for(g in groups)
    {
      # Makes the map Icon from the googlesheet Marker column 
      url <- as.character(ActionSelection()$Marker)
      mapIcon <- makeIcon(
        iconUrl = url,
        iconWidth = 64, iconHeight = 64)
      
      #Instantiates each layer 
      map = map %>% addMarkers(data = ActionSelection(), lng = ~LONG, lat = ~LAT, icon = mapIcon)
      # color = 277706L,
      # group = g)
    }
    map %>% addLayersControl()
    #  Action_v3 <- ActionSelection()
    # write.csv(Action_v3, "Action_v3.csv")
  })
  
  
  #### END MAP #####
  
  
  # TESTING TEXT OUTPUT - WILL BE TURNED INTO POPUP INFORMATION
  
  output$mapclick <- renderText({ 
    #Rreturns text of Map click
    #Asks user to select station if no station is selected. 
    
    if (is.null(input$map_marker_click$lat))  
    {
      mapClick <- "Select a Station!"
    } 
    else 
    {
      mapClick <- MapSelection()
    }
    
    paste(mapClick)
    
  }
  )
  
  #### END MAP ###
}

# Run the application 
shinyApp(ui = ui, server = server)
