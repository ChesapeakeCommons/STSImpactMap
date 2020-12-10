# Save the Sound's 2020 Action Map
# Documentation https://docs.google.com/document/d/14kDLOjaSYhNqzDJXG9Zn2SWk5T-k_RPIiF2jXS9h_zA/edit?usp=sharing
# Github: https://github.com/ChesapeakeCommons/R-Shiny
# Created by Gabe Watson for The Commons 
# Created 7.20.2020 
# Last edited: 11.21.2020 
# Naming Convention  
#   - Major_Components_Name minor_componentsName, MajorVariable minorvariable 
# Sections 
#   #Display Components
#   #Data importing 
#   #Data processing 
#   #Filtering 
#   #Map Rendering

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
library(stringr)
###########  UI Display Script ############
ui <- fluidPage(theme = "styler.css",

 
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
          ),
          div( class ="compass-overlay-container",
               div( class="compass-overlay")
          )
      )
      
  # Side Panel Display with Save the Sound Info
  ),
  div(id = "side-panel",
      HTML("<a href='http://www.savethesound.org' style='color:#ffffff'>"),
      div(  id = "title"
            
      #Descriptive Text 
      ),
      HTML("</>"),
     
      div( id = "side-panel-wrapper",
           div(class = "side-panel-description side-panel-goback",
             HTML("<a href='http://www.savethesound.org' style='color:#ffffff'>Go Back!</a>")
           ),
           div(  class = "side-panel-description",
                  textOutput("DescriptionParagraph")
           ),
           div(  class = "side-panel-description",
                 
                 #Text output for Action Count (See output$ActionCount Render Block in Server Side)
                 textOutput("ActionCount"),
                 textOutput("DescriptionSentence")
           ),
           #Filter output block - see output$Filters for rendering code
           div(
            class='filters',
            uiOutput("Filters")
           ),
      )
  )
)
########## Server Side Script ############
server <- function(input, output, session) {
  
  ##### DATASET IMPORT ####
  # this lets us bypass the Authorization step from gsheet4 if they are ok with just having the googlesheet be public - I can hook it up to be private though after the fact.
  gs4_deauth()
  
  #Raw Data
  Import <- read_sheet("https://docs.google.com/spreadsheets/d/1Hf6V-pWunsaA1Vt3tvvf12D0G5MwFKCe61SIr1sVAfg/edit#gid=0")
  
  #Zoom Extent
  ZoomExtent_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1viLwGCnhsdhfgsgIHjYYj6INNu7YqG_h8srlQsCNf6Y/edit#gid=0")
  
  #Symbology
  Symbology_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1R5wLQGimKxDGNhMm5NSZLcHM36qFwtdiPzvNmyq2C60/edit#gid=0")
  
  #Descriptive Text 
  Text_Input_V1 <- read_sheet("https://docs.google.com/spreadsheets/d/1zOnGKfYbfOH7C6Dp1BjkpVOd6WenxViqk490DlLbcuI/edit#gid=0")
  
  #Boat geoJSON
  STS_Boat <- rgdal::readOGR("www/STS_Boat_v6.geojson")
  

 
         #### END IMPORT ####
  ## ****************************# 
  #### IMPORT DATA MOTIFICATIONS #### 

  ## Merging input dataset with Symbology datatest
  Import <- merge(Import,Symbology_V1, all.x = TRUE)
  
  #### Block for calculating year span for uncompleted projects #### 
  Import$Status <- ""
  Import$Span <- 1
  
  #Loops through set and checks to see if data contains both start and end year 
  #If dif between start and end year are greater than 1, calculates delta between years 
  for (row in 1:nrow(Import))
  {
    if(Import$YearComplete[row] == "x")
    {
      Import$Status[row] <- "On Going"
    }
  else
    {
     Import$Span[row] <- (as.numeric(Import$YearComplete[row]) - as.numeric(Import$Year[row])) + 1
     Import$Status[row] <- "Complete"
    }
  }

  #Converts year variables to numeric
  Import$YearComplete <- as.numeric(Import$YearComplete)
  Import$Year <- as.numeric(Import$Year)
  
  
  #Loops throug and adds additional layers
  MapData <- Import[rep(row.names(Import), Import$Span), 1:22]
              
  #Changes the start year 
  MapData$Count <- ave(MapData$Year, MapData$ProjectName, FUN = seq_along)
  
  #Corrects the math
  MapData$Year <- MapData$Year - 1 + MapData$Count

  
  #Checks to see if image link is NA - if so puts in placeholder image sourced from the symbology sheet 
  for(row in 1:nrow(MapData))
  {
   if(is.na(MapData$Image[row]))
     {
       MapData$Image[row] <- Symbology_V1$Marker[9]
    }
  }


   #Converting all characters to factors, and setting NAs to blank
   MapData <- MapData %>%
   mutate_if(is.character, as.factor)
  
   MapData[is.na(MapData)] = ""
   MapData[is.null(MapData)] = ""
  
  
  #Dynamic Text Paragraph
  output$DescriptionParagraph <- renderText({
    Text_Input_V1$DescriptionParagraph
  })
  
  #Dynamic Text Action Count
  output$ActionCount <- renderText({
    Text_Input_V1$ActionCount
  })
  
  #Dynamic Text
  output$DescriptionSentence <- renderText({
    Text_Input_V1$DescriptionSentence
  })
  
 #### END IMPORT DATA MOTIFICATIONS ####
    ## ****************************## 
          #### FILTERS #### 
  
  #Original Render for filters 
  output$Filters <- renderUI({
    req(MapData)
    req(ZoomExtent_V1)
    tagList(
      #Action Selection
      selectizeInput("inActionSelector", "Search by Action:",
                     choices = MapData$Action, multiple = TRUE, options = list(placeholder = 'All Actions')),
      
      #Sub Action Selection
      selectizeInput("inSubActionSelector", "Search by Sub Action:",
                     choices = MapData$SubAction, multiple = TRUE, options = list(placeholder = 'All Sub Actions')),
      
      # Year
      selectizeInput("inYearSelector", "Search by Year:",
                     choices = MapData$Year, multiple = TRUE, selected = 2020),
      
      # Zoom
      selectizeInput("inZoomSelector", "Zoom to:",
                     choices = ZoomExtent_V1$Extent, multiple = FALSE)
    )
  })
  
  #### FILTER INPUT DATA HANDLING ###
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
      Layers <- MapData
      updateSelectizeInput(session, "inActionSelector",
                           choices = sort(Layers$Action))
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = sort(Layers$SubAction))
      updateSelectizeInput(session, "inYearSelector",
                           choices = sort(Layers$Year, decreasing = TRUE))
      return(Layers)
    }
    #Updates Subaction and Year selection when only Action is chosen
    if (length(x) > 0 && length(y) == 0 && length(z) == 0 )
    {
      Layers <- filter(MapData, Action %in% x)  
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = sort(Layers$SubAction))
      updateSelectizeInput(session, "inYearSelector",
                           choices = sort(Layers$Year, decreasing = TRUE))
      return(Layers)
    }
    # Updates Year when Action and SubAction are selected
    if (length(x) > 0 && length(y) > 0 && length(z) == 0)
    {
      Layers <- filter(MapData, Action %in% x)
      Layers <- filter(Layers, SubAction %in% y)
      updateSelectizeInput(session, "inYearSelector",
                           choices = sort(Layers$Year, decreasing = TRUE))
      return(Layers)
    }
    # Updates Action and Year when SubAction is Selected
   if (length(x) == 0 && length(y) > 0 && length(z) == 0)
    {
     Layers <- filter(MapData, SubAction %in% y) 
     updateSelectizeInput(session, "inActionSelector",
                          choices = sort(Layers$Action))
     updateSelectizeInput(session, "inYearSelector",
                          choices = sort(Layers$Year, decreasing = TRUE))
     return(Layers)
    }
    #Updates Action when SubAction and Year are selected
    if (length(x) == 0 && length(y) > 0 && length(z) > 0)
    {
      Layers <- filter(MapData, SubAction %in% y)
      Layers <- filter(Layers, Year %in% z)
      updateSelectizeInput(session, "inActionSelector",
                           choices = sort(Layers$Action))
      return(Layers)
    }
    #Updates Action and Subaction when Year is selected
    if (length(x) == 0 && length(y) == 0 && length(z) > 0)
    {
      Layers <- filter(MapData, Year %in% z)
      updateSelectizeInput(session, "inActionSelector",
                           choices = sort(Layers$Action))
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = sort(Layers$SubAction))
      return(Layers)
    }
    #Updates Subaction when Action and Year are selected
    if (length(x) > 0 && length(y) == 0 && length(z) > 0)
    {
      Layers <- filter(MapData, Action %in% x)
      Layers <- filter(Layers, Year %in% z)

      updateSelectizeInput(session, "inSubActionSelector",
                           choices = sort(Layers$SubAction))
      return(Layers)
    }
    #No update, just filters when all are selected 
    if (length(x) > 0 && length(y) > 0 && length(z) > 0)
    {
      Layers <- filter(MapData, Action %in% x)
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
  

  
        #### END FILTERS ####
  ## ****************************## 
      #### MAP RENDERING  #### 
  
  
  ##### MAP ####
  #This creates the original drawing of the map, observeEvents below update only the dataframe and zoom extent such that the map does not completely re render
  output$leafmap <- renderLeaflet({
     leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
       addPolygons(data = STS_Boat, fill = FALSE, weight = 2, dashArray = "5", color = "grey") %>%
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
    w <- str_remove_all(ActionSelection()$Width, "[px]")
    h <- str_remove_all(ActionSelection()$Height, "[px]")
    
    mapIcon <- makeIcon(
      iconUrl = url,
      iconWidth = w, iconHeight = h)

      # Creating Popup Image
      PopupImage <- ActionSelection()$Image
      
     #Marker creation
     leafletProxy("leafmap") %>%
       #Clears markers and marker clusters for re render
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addMarkers(data = ActionSelection(),
                lng = ~LONG, lat = ~LAT,
                icon = mapIcon,
                #Label
                label = ActionSelection()$ProjectName,
                #Marker Cluster Options
                clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                  zoomToBoundsOnClick = TRUE, 
                                  spiderfyOnMaxZoom = 2,
                                  removeOutsideVisibleBounds = TRUE,
                                  spiderLegPolylineOptions = list(weight = 2, color = "#222", opacity = 0.5), 
                                  freezeAtZoom = 10,
                                  maxClusterRadius = .05,
                                  weight=3,
                                  color="#33CC33", opacity=1, fillColor="#FF9900", 
                                  fillOpacity=0.8) ,
                #Popup Code
                popup = paste(
                  "<div class='popup-wrapper'>",
                    "<div class='popup-image' style='border: 0px solid red;",
                    "background-image: url(\"",ActionSelection()$Image,"\")'>",
                    # "<img class='pu-img' src='", ActionSelection()$Image ,"'>",
                    "</div>",
                    "<div class='popup-text'>",
                      "<div class='popup-title'>",
                      "<div class='popup-title-marker",
                        (ifelse(ActionSelection()$Override == TRUE,
                                paste("marker-resize-01"),
                                paste("marker-resize-02")
                          )) ,
                              "' style='margin-right:6px;", ActionSelection()$Height,"; width:",ActionSelection()$Width,"; background-image:url(\"",ActionSelection()$Marker,"\");'>",
                        "</div>",
                          "<div class='popup-title-text'>",
                            "<span class='popup-title-h1' style=''>", ActionSelection()$Action, "</span>",
                            "<span class='popup-title-h2' style='color:",ActionSelection()$Color,";'>", ActionSelection()$SubAction,"</span>",
                          "</div>",
                        "</div>",
                        "<div class='popup-body'>",
                          "<span class='popup-title-h2 pu-h2-adj'><b>Project:</b>", ActionSelection()$ProjectName,"</span>",
                          "<span class='popup-line adj'>",
                            "<b>Status:</b> ",  ActionSelection()$Status,"",  
                            "<b style='margin-left:10px;'>Started:</b> ", ActionSelection()$Year, "",
                            "<b class='no-left-margin' style='margin-left:10px;'>Completed:</b> ", ActionSelection()$YearComplete, "<br>",
                           "</span>",
                           "<span class='popup-line popup-line-description",
                                  (ifelse((ActionSelection()$ShortDescription == 'NULL')|(is.na(ActionSelection()$ShortDescription)),
                                            paste("hide"),
                                            paste("")
                                    )) , 
                              "'><b>Description:</b>",ActionSelection()$ShortDescription,
                            "</span>",
                            "<span class='popup-line", 
                                  (ifelse(ActionSelection()$Value1 == 'NULL' |(is.na(ActionSelection()$Value1)),
                                                               paste("hide"),
                                                               paste("")
                                   )) , 
                                "'><b>",ActionSelection()$KeyMetric1,":</b> ",ActionSelection()$Value1,"</span>",
                            "<span class='popup-line popup-line-adj",
                                   (ifelse(ActionSelection()$Value2 == 'NULL' |(is.na(ActionSelection()$Value2)),
                                       paste("hide"),
                                       paste("")
                                )) , 
                              "'><b>",ActionSelection()$KeyMetric2,":</b> ",ActionSelection()$Value2,"</span>",
                            "<span class='popup-line",
                                  (ifelse((ActionSelection()$Url == '')|(is.na(ActionSelection()$Url)),
                                          paste("hide"),
                                          paste("")
                                  )) , 
                            "'>", 
                                "<b>More Info:</b><a href='",ActionSelection()$Url,"'>",ActionSelection()$Url,"</a>",
                            "</span>",
                            "<span class='popup-line' style='text-overflow: ellipsis'>", 
                              "<span class='popup-line",
                                   (ifelse(ActionSelection()$LocationName == 'NULL'|(is.na(ActionSelection()$LocationName)),
                                          paste("hide"),
                                          paste("")
                                  )) , 
                              "'><b>Location:</b>",ActionSelection()$LocationName,"</span>",
                              "<span class='popup-line popup-line-adj'><b>Lat:</b>",ActionSelection()$LAT,"&nbsp; <b>Long:</b>",ActionSelection()$LONG,"</span>",
                               
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

  
  
 }

  



# Run the application 
shinyApp(ui = ui, server = server)
