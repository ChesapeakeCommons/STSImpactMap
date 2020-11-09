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
library(jsonlite)
library(rjson)
library(geojsonR)
library(sp)
###########  UI Display Script ############
ui <- fluidPage(theme = "styler.css",
  #(theme = "styler.css",
  
  
  ### DISPLAY COMPONENTS ###

  div(id = "wrapper",
      
      
      #Map
      div(id = "main-panel",
          
          leafletOutput("leafmap"),
          div( class= "key-overlay-container",
              div(class = "key-overlay",
                  div(class="key-text",
                      HTML("Key")),
                  div(class="key-image")
              )
          )
          
      )
      
      # Side Panel Display with Save the Sound Info
  ),
  div(id = "side-panel",
      div(  id = "title"
        #    img(    id = 'title-image',
         #           src='images/save-the-sound-title-02.png'
        #    )
            
            #Descriptive Text 
      ),
     
      div( id = "side-panel-wrapper",
           div(class = "description",
             HTML("<a href='http://www.savethesound.org' style='color:#ffffff'>Go Back!</a>")
           ),
           div(  class = "description",
               #  tags$h1("2020 Action Map"),
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
                 tags$p(style="margin-top:5px;",
                   HTML("<font style='font-weight: 400;'>Efforts to improve the</font> Long Island Sound")
                 )
           ),
           #Filter output block - see output$Filters for rendering code
           div(
            class='filters',
            uiOutput("Filters")
           )
      )
  )
)
########## Server Side Script ############

server <- function(input, output, session) {
  
  ##### DATASET IMPORT ####
  # this lets us bypass the Authorization step from gsheet4 if they are ok with just having the googlesheet be public - I can hook it up to be private though after the fact.
  gs4_deauth()
  (Data_Test_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/10VMsQ57EL25gDjb7bAEjOZDI2mEWiOkIoHwHWNW0MOE/edit#gid=0"))
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
  
  
 #  geojson <- reactive({
   # STS_Boat_Raw <- system.file("www/STS_Boat.kml", package = "leaflet.extras")
    #STS_Boat_Kml <- readr::read_file(STS_Boat_Raw)
  
  STS_Boat <- rgdal::readOGR("www/STS_Boat_v5.geojson")

 #  })
  
  ##### MAP ####
  #This creates the original drawing of the map, observeEvents below update only the dataframe and zoom extent such that the map does not completely re render
  output$leafmap <- renderLeaflet({

    
     leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
       addPolygons(data = STS_Boat, fill = FALSE, weight = 2, dashArray = "3", color = "grey") %>%
       addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets") %>%
       addProviderTiles("Esri.WorldTopoMap", group = "Terrain")%>%
       addProviderTiles("GeoportailFrance.orthos", group = "Satellite")%>%
       addLayersControl(baseGroups = c("Streets", "Terrain", "Satellite"),
                        options = layersControlOptions(collapsed = FALSE,  position = 'bottomright'))%>%
       setView(lng = 41, lat = -72, zoom = 8)%>%
      #Adding Search service
      addSearchOSM(options = searchOptions(zoom=15, position = 'topright',
                                           autoCollapse = TRUE,
                                           minLength = 2))%>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
      }")
  })
  
#This updates the map based on the changes in the selected data such that the map doesn't need to redraw every time
  observeEvent(ActionSelection(),
    {

    
    #Creating Map Markers with URL (Will likely store this information in an Action only sheet and then join to layers in program after data importing)
    url <- as.character(ActionSelection()$Marker)
    
    mapIcon <- makeIcon(
      iconUrl = url,
      iconWidth = 32, iconHeight = 32)

      # Creating Popup Image
      PopupImage <- ActionSelection()$Image
      
 #Marker creation
     leafletProxy("leafmap") %>%
      #Clears markers and marker clusters for re render
        clearMarkers()%>%
        clearMarkerClusters()%>%
       # addProviderTiles("CartoDB.VoyagerLabelsUnder")%>%
        #Adding Markers, Clusters, and Popups 
        addMarkers(data = ActionSelection(),
                 lng = ~LONG, lat = ~LAT,
                icon = mapIcon,
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
                popup = paste(
                  "<div class='popup-wrapper'>",
                    "<div class='popup-image' style='border: 0px solid red;",
                      "background-image: url(\"",ActionSelection()$Image,"\")'>",
                  # "<img class='pu-img' src='", ActionSelection()$Image ,"'>",
                    "</div>",
                    "<div class='popup-text'>",
                      "<div class='popup-title'>",
                        "<div class='popup-title-marker' style='background-image:url(\"",ActionSelection()$Marker,"\")'>",
                        "</div>",
                      "<div class='popup-title-text'>",
                        "<span class='popup-title-h1' style=''>", ActionSelection()$Action, "</span>",
                        "<span class='popup-title-h2' style='color:",ActionSelection()$Color,";'>", ActionSelection()$SubAction,"</span>",
                      "</div>",
                    "</div>",
                    "<div class='popup-body'>",
                      "<span class='popup-title-h2 pu-h2-adj'><b>Project:</b>", ActionSelection()$ProjectName,"</span>",
                      "<span class='popup-line'>",
                        "<b>Status:</b> ",  Data_Test_V1$Status[row],"",  
                        "<b style='margin-left:10px'>Started:</b> ", ActionSelection()$Year, "",
                        "<b style='margin-left:10px'>Completed:</b> ", ActionSelection()$YearComplete, "<br>",
                       "</span>",
                       "<span class='popup-line' style='line-height:15px; display:inline-block; overflow:hidden; text-overflow: ellipsis; max-height: 36px;'><b>Description:</b>",ActionSelection()$ShortDescription,
                        "</span>",
                        "<span class='popup-line '><b>",ActionSelection()$KeyMetric1,":</b> ",ActionSelection()$Value1,"</span>",
                        "<span class='popup-line popup-line-adj'><b>",ActionSelection()$KeyMetric2,":</b> ",ActionSelection()$Value2,"</span>",
                        "<span class='popup-line'>", 
                            "<b>More Info:</b><a href='",ActionSelection()$Url,"'>",ActionSelection()$Url,"</a>",
                        "</span>",
                        "<span class='popup-line' style='text-overflow: ellipsis'>", 
                        "<span class='popup-line'><b>LOCATION:</b>",ActionSelection()$LocationName,"</span>",
                        "<span class='popup-line popup-line-adj'><b>LAT:</b>",ActionSelection()$LAT,"&nbsp; <b>LONG:</b>",ActionSelection()$LONG,"</span>",
                         
                        "</span>",
                    "</div>",
                    "</div>",
                   "</div>"
                )
        )
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
