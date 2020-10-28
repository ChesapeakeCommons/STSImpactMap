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
library(rgdal)
library(remotes)
###########  UI Display Script ############
ui <- fluidPage(
  #(theme = "styler.css",
  
  
  ### DISPLAY COMPONENTS ###

  div(id = "wrapper",
      
      #Filter output block - see output$Filters for rendering code
      uiOutput("Filters"),
      
      #Map
      div(id = "main-panel",
          leafletOutput("leafmap")
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
           )
      )
  )
)

########## Server Side Script ############

server <- function(input, output, session) {
  
  ##### DATASET IMPORT ####
  # this lets us bypass the Authorization step from gsheet4 if they are ok with just having the googlesheet be public - I can hook it up to be private though after the fact.
  gs4_deauth()
  tryCatch(Data_Test_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/10VMsQ57EL25gDjb7bAEjOZDI2mEWiOkIoHwHWNW0MOE/edit#gid=0"))
 # Import_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1_kWe4pEOo7yur9WPMh1YabdCSjoIz-yS37jM6T5_oWw/edit#gid=0")
  ZoomExtent_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1viLwGCnhsdhfgsgIHjYYj6INNu7YqG_h8srlQsCNf6Y/edit#gid=0")
  # Symbology_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1N0L7-gZH4iqxrbQVYLtLJU7Dbuz2nVnYE-8wUrzPK6o/edit#gid=0")
  
  #### Block for calculating year span for uncompleted projects #### 
  Data_Test_V1$Status <- ""
  Data_Test_V1$Span <- 1
  
  #Loops through set and checks to see if data contains both start and end year 
  #If dif between start and end year are greater than 1, calculates delta between years 
  for (row in 1:nrow(Data_Test_V1))
  {
    if(Data_Test_V1$YearComplete[row] == "x")
    {
      Data_Test_V1$Status[row] <- "On Going"
    }
  else
    {
     Data_Test_V1$Span[row] <- as.numeric(Data_Test_V1$YearComplete[row]) - as.numeric(Data_Test_V1$Year[row]) + 1
     Data_Test_V1$Status[row] <- "Complete"
    }
  }

  #Converts year variables to numeric
  Data_Test_V1$YearComplete <- as.numeric(Data_Test_V1$YearComplete)
  Data_Test_V1$Year <- as.numeric(Data_Test_V1$Year)
  
  
  #Loops throug and adds additional layers
  Data_Test_V2 <- Data_Test_V1[rep(row.names(Data_Test_V1), Data_Test_V1$Span), 1:20]
              
  #Changes the start year 
  Data_Test_V2$Count <- ave(Data_Test_V2$Year, Data_Test_V2$ProjectName, FUN = seq_along)
  
  #Corrects the math
  Data_Test_V2$Year <- Data_Test_V2$Year - 1 + Data_Test_V2$Count
  
  

 # write.csv(Data_Test_V2, "Data_Test_V2.csv")
  
   
  
  
  
  #### END DATASETIMPORT #####
  
  output$Filters <- renderUI({
    
    req(Data_Test_V2)
    req(ZoomExtent_V1)
    #Zoom Selction 
    tagList(
      #Action Selection
      selectizeInput("inActionSelector", "Filter by Action:",
                     choices = Data_Test_V2$Action, multiple = TRUE, options = list(placeholder = 'All Actions')),
      
      #Sub Action Selection
      selectizeInput("inSubActionSelector", "Filter by Sub Action:",
                     choices = Data_Test_V2$SubAction, multiple = TRUE, options = list(placeholder = 'All Sub Actions')),
      
      
      selectizeInput("inYearSelector", "Filter by Year:",
                     choices = Data_Test_V2$Year, multiple = TRUE, selected = 2020),
      
      
      selectizeInput("inZoomSelector", "Zoom to:",
                     choices = ZoomExtent_V1$Extent, multiple = FALSE)
    )
  })
  
  #### ACTION INPUT DATA HANDLING ###
  
  
  
  
  # Reactive element that gets the correct Layer Selection and returns "ActionSelection()" for use in mapping from Action Selector
  ActionSelection <- reactive ({
    
    #Converts the input datatype into list --- I think...
    x <- as.character(input$inActionSelector)
    y <- as.character(input$inSubActionSelector)
    z <- as.character(input$inYearSelector)
    
    ## Filtering for Action and Sub Actions
    #Sets dataframe to all Actions and All years if no selection is chosen
    if (length(x) == 0 && length(y) == 0 && length(z) == 0)
    {
      Layers <- Data_Test_V2
      updateSelectizeInput(session, "inActionSelector",
                           choices = Layers$Action)
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      updateSelectizeInput(session, "inYearSelector",
                           choices = Layers$Year)
      return(Layers)
    }
    #Updates Subaction and Year selection when only Action is chosen
    if (length(x) > 0 && length(y) == 0 && length(z) == 0 )
    {
      Layers <- filter(Data_Test_V2, Action %in% x)  
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      updateSelectizeInput(session, "inYearSelector",
                           choices = Layers$Year)
      return(Layers)
    }
    # Updates Year when Action and SubAction are selected
    if (length(x) > 0 && length(y) > 0 && length(z) == 0)
    {
      Layers <- filter(Data_Test_V2, Action %in% x)
      Layers <- filter(Layers, SubAction %in% y)
      updateSelectizeInput(session, "inYearSelector",
                           choices = Layers$Year)
      return(Layers)
    }
    # Updates Action and Year when SubAction is Selected
   if (length(x) == 0 && length(y) > 0 && length(z) == 0)
    {
     Layers <- filter(Data_Test_V2, SubAction %in% y) 
     updateSelectizeInput(session, "inActionSelector",
                          choices = Layers$Action)
     updateSelectizeInput(session, "inYearSelector",
                          choices = Layers$Year)
     return(Layers)
    }
    #Updates Action when SubAction and Year are selected
    if (length(x) == 0 && length(y) > 0 && length(z) > 0)
    {
      Layers <- filter(Data_Test_V2, SubAction %in% y)
      Layers <- filter(Layers, Year %in% z)
      updateSelectizeInput(session, "inActionSelector",
                           choices = Layers$Action)
      return(Layers)
    }
    #Updates Action and Subaction when Year is selected
    if (length(x) == 0 && length(y) == 0 && length(z) > 0)
    {
      Layers <- filter(Data_Test_V2, Year %in% z)
      updateSelectizeInput(session, "inActionSelector",
                           choices = Layers$Action)
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      return(Layers)
    }
    #Updates Subaction when Action and Year are selected
    if (length(x) > 0 && length(y) == 0 && length(z) > 0)
    {
      Layers <- filter(Data_Test_V2, Action %in% x)
      Layers <- filter(Layers, Year %in% z)

      updateSelectizeInput(session, "inSubActionSelector",
                           choices = Layers$SubAction)
      return(Layers)
    }
    #No update, just filters when all are selected 
    if (length(x) > 0 && length(y) > 0 && length(z) > 0)
    {
      Layers <- filter(Data_Test_V2, Action %in% x)
      Layers <- filter(Layers, SubAction %in% y)
      Layers <- filter(Layers, Year %in% z)
      return(Layers)
    }
  })
  ###### END ACTION INPUT HANDLING 
  
  
  ##### ZOOM EXTENT HANDLING ####
  # Updates the Zoom selector once the input sheets are handled above 
  ZoomSelection <- reactive ({
    req(input$inZoomSelector)
    x <- as.character(input$inZoomSelector) 
    
    if (length(x) == 0)
    {
      ZoomChoice <- ZoomExtent_V1 %>%
      filter(Extent == "Whole Region")
    }
     else
      {
      ZoomChoice <- filter(ZoomExtent_V1, Extent %in% x)
      }
    return(ZoomChoice)
  })
  
  #### END INPUT DATA HANDLING ###
  
  
  
  
  ##### MAP ####
  #This creates the original drawing of the map, observeEvents below update only the dataframe and zoom extent such that the map does not completely re render
  output$leafmap <- renderLeaflet({
     leaflet() %>%
       addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
       setView(lng = 41, lat = -72, zoom = 8)
  })
  
#This updates the map based on the changes in the selected data such that the map doesn't need to redraw every time
  observeEvent(ActionSelection(),
    {
    
    #Creating Map Markers with URL (Will likely store this information in an Action only sheet and then join to layers in program after data importing)
    url <- as.character(ActionSelection()$Marker)
    
    mapIcon <- makeIcon(
      iconUrl = url,
      iconWidth = 64, iconHeight = 64)

      # Creating Popup Image
      PopupImage <- ActionSelection()$Image
      
 #Marker creation
     leafletProxy("leafmap") %>%
      #Clears markers and marker clusters for re render
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder")%>%
        #Adding Markers, Clusters, and Popups 
        addMarkers(data = ActionSelection(),
                 lng = ~LONG, lat = ~LAT,
            #    icon = mapIcon,
                #Label
                 label = ActionSelection()$ProjectName,
                #Marker Cluster Options
                clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                  zoomToBoundsOnClick = TRUE, 
                                  spiderfyOnMaxZoom = 5,
                                  removeOutsideVisibleBounds = TRUE,
                                  spiderLegPolylineOptions = list(weight = 5, color = "#222", opacity = 0.5), 
                                  freezeAtZoom = TRUE),
                 #Popup Code
                 popup = paste("Action: ", ActionSelection()$Action, "<br>",
                         "Year Started: ", ActionSelection()$Year, "<br>",
                         "Year Completed: ", ActionSelection()$YearComplete, "<br>",
                         "Status: ", ActionSelection()$Status, "<br>",
                         "Sub Action: ", ActionSelection()$SubAction, "<br>",
                         "Project Name: ", ActionSelection()$ProjectName, "<br>",
                         "Lat: ", ActionSelection()$LAT, "Long: ", ActionSelection()$LONG, "<br>",
                          ActionSelection()$KeyMetric1,"- ", ActionSelection()$Value1, "<br>",
                          ActionSelection()$ShortDescription))
      })
  
  #Updates Zoom selection without updating entire map 
  observeEvent(ZoomSelection(), 
         {
          leafletProxy("leafmap")%>%
          setView(lng = ZoomSelection()$Longitude, lat = ZoomSelection()$Latitude, zoom = ZoomSelection()$Zoom)
         })
  
  #### END MAP #####
  

}

# Run the application 
shinyApp(ui = ui, server = server)
