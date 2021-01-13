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
library(readr)
library(tidyr)
library(splitstackshape)
library(reshape2)
###########  UI Display Script ############
ui <- fluidPage(theme = "styler.css",

 
  ### DISPLAY COMPONENTS ###
  div(id = "wrapper",
      #Map
      div(id = "main-panel",
          
          leafletOutput("leafmap"),
         # div( class= "key-overlay-container",
            #  div(class = "key-overlay",
             #     div(class="key-text",
                    #  HTML("Key")),
               #   div(class="key-image")
             # )
          #),
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
            h5("Select an Action:"),
            uiOutput("KeyBar"),
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
  
  #Function which trys the Gsheets read_sheet and if Resource Exhause Limit is reached, uses a saved CSV.
  readsheet <- function(x, y)
  {
    tryCatch(
      expr = {
        read_sheet(x)
      },
      error = function(e)
      {
        df <- read_csv(y)
      }
    )
  }

  #Raw Data
  Import <- readsheet("https://docs.google.com/spreadsheets/d/10VMsQ57EL25gDjb7bAEjOZDI2mEWiOkIoHwHWNW0MOE/edit#gid=0", "www/TestData/Input.csv")

  #Zoom Extent
  ZoomExtent_V1 <- readsheet("https://docs.google.com/spreadsheets/d/1viLwGCnhsdhfgsgIHjYYj6INNu7YqG_h8srlQsCNf6Y/edit#gid=0", "www/TestData/Zoom.csv")

  #Symbology
  Symbology_V1 <- readsheet("https://docs.google.com/spreadsheets/d/1R5wLQGimKxDGNhMm5NSZLcHM36qFwtdiPzvNmyq2C60/edit#gid=0", "www/TestData/Symbology.csv")

  #Descriptive Text
  Text_Input_V1 <- readsheet("https://docs.google.com/spreadsheets/d/1zOnGKfYbfOH7C6Dp1BjkpVOd6WenxViqk490DlLbcuI/edit#gid=0", "www/TestData/Text.csv")

  
  #Boat geoJSON
  STS_Boat <- rgdal::readOGR("www/STS_Boat_v6.geojson")
  
  #EJ Map Layer 
  EJLayer <- rgdal::readOGR("www/EJLayer/WGS84_LISej_NYCexpansion.shp")
  

 
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
  MapData <- Import[rep(row.names(Import), Import$Span), 1:ncol(Import)]
              
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
  
   i <- sapply(MapData, is.factor)
   MapData[i] <- lapply(MapData[i], as.character)
  
   MapData[is.na(MapData)] = ""
   MapData[is.null(MapData)] = ""
   
   ##### CREATING TAGSLIST FOR SEARCH FUNCTION #####
   
   #Pulls out list of Tags from MapDataFinal static frame 
   TagsList <- data.frame(MapData$Tags, stringsAsFactors = FALSE) %>%
     separate_rows(MapData.Tags, sep = ",") %>%
     unique()

   #Splits texts to columns for the Tags and creating MapDataFinal 
   
   MapDataFinal <- cSplit(MapData, "Tags", ",")
   
   ## END DATA SETUP 
   
   
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
      #Tag Word Search 
      selectizeInput("inTagsSearch" , "Search", choices = sort(c(TagsList$MapData.Tags, MapData$Action, MapData$SubAction, MapData$ProjectName), decreasing = FALSE), multiple = TRUE,  options = list(placeholder = 'What are you looking for?')),
      
      #Sub Action Selection
      selectizeInput("inSubActionSelector", "Search By Project Type",
                     choices = MapData$SubAction, multiple = TRUE, options = list(placeholder = 'Select a Project Type!')),
      
      # Year
      selectizeInput("inYearSelector", "Filter By Year",
                     choices = MapData$Year, multiple = TRUE),
      
      # Zoom
      selectizeInput("inZoomSelector", "Map Location:",
                     choices = ZoomExtent_V1$Extent, multiple = FALSE),
      
      actionButton("ResetAll", label = "Reset All")
    )
  })
  
  output$KeyBar <- renderUI({
    tagList( 
      div(id='action-button-container',
        HTML("<div class='button-row'>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Climate", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Climate_and_Resiliency.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Climate\n</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Healthy", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
                background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Healthy_Water.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Healthy Waters</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Lands", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Protected_Lands.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Protected Lands</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Eco", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Ecological_Restoration.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Ecological Restoration</div></div>"),
       
        HTML("
          </div>
          "),
        HTML("<div class='button-row'>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Justice", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Environmental_Justice.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Environment Justice</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Monitoring", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Water_Monitoring.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Water Monitoring</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("Cleanups", label = "", style = "width: 34px; height: 34px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Cleanups.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label'>Cleanups</div></div>"),
          HTML("<div class='button-wrapper'>"),
            HTML("<div class='button'>"),
              actionButton("SoundKeeper", label = "", style = "width: 60px; height: 29px; margin: 0 auto;
              background: url('https://www.savethesound.org/wp-content/uploads/2020/11/Icon_Soundkeeper.png');  background-size: cover; background-position: center;"),
            HTML("</div><div class='button_label' style='margin-left:7px'>Soundkeeper</div></div>"),
          HTML("</div>")
        )
    
    )
    
  })
  
  # #### FILTER INPUT DATA HANDLING ###
  
  #ReactiveVal for handling MapData
  MapDataReactive <- reactiveValues(df = data.frame())
  MapDataReactive$df <- as.data.frame(MapDataFinal)
  
  ## Function for handling Action Button Clicks
  buttonUpdate <- function(y)
  {
    
    #Logic Block
    #If action selected is in dataframe and dataframe isn't the default 
        #If theres only one action selected - do nothing 
        #Else, remove the selected action from dataframe 
      #Else, add selected action to dataframe 
    if(y %in% MapDataReactive$df$Action && nrow(MapDataReactive$df) != nrow(MapDataFinal))
    {
      if((nrow(data.frame(unique(MapDataReactive$df$Action)))) == 1)
      {
      MapDataReactive$df <- MapDataReactive$df
      }
      else
      {
      MapDataReactive$df <- filter(MapDataReactive$df, Action != y)
      }
    }
    else
    {
      MapDataReactive$df <- unique(rbind(MapDataReactive$df, filter(MapDataFinal, Action == y)))
    }
    
    #If entire dataset is present, only select the chosen action 
    if(nrow(MapDataReactive$df) == nrow(MapDataFinal))
    {
       MapDataReactive$df <- filter(MapDataFinal, Action == y)
    }
     # print(data.frame(unique(MapDataReactive$df$Action)))
      updateSelectizeInput(session, "inYearSelector",
                           choices = sort(MapDataReactive$df$Year, decreasing = TRUE))
      updateSelectizeInput(session, "inSubActionSelector",
                           choices = sort(MapDataReactive$df$SubAction, decreasing = TRUE))
  }
  
  #Reset
  observeEvent(input$ResetAll, {
    MapDataReactive$df <- as.data.frame(MapDataFinal)
    
    updateSelectizeInput(session, "inTagsSearch",
                         choices = sort(c(TagsList$MapData.Tags, MapData$Action, MapData$SubAction, MapData$ProjectName), decreasing = FALSE), options = list(placeholder = 'What are you looking for?'))
    
    updateSelectizeInput(session, "inYearSelector",
                         choices = sort(MapDataReactive$df$Year, decreasing = TRUE))
    updateSelectizeInput(session, "inSubActionSelector",
                         choices = sort(MapDataReactive$df$SubAction, decreasing = TRUE))
  })
  
  #ButtonUpdate Function wrapped in an observe Event, ooooh its so much cleaner than before!
  observeEvent(input$Cleanups,{buttonUpdate("Cleanups")})
  observeEvent(input$Climate,{buttonUpdate("Climate & Resiliency")})
  observeEvent(input$Eco,{buttonUpdate("Ecological Restoration")})
  observeEvent(input$Justice,{buttonUpdate("Legal")})
  observeEvent(input$Healthy,{buttonUpdate("Healthy Waters")})
  observeEvent(input$Lands,{buttonUpdate("Protected Lands")})
  observeEvent(input$Monitoring,{buttonUpdate("Water Monitoring")})
  observeEvent(input$SoundKeeper,{buttonUpdate("SoundKeeper")})

  #SubAction Update
  observeEvent(input$inSubActionSelector, {
    MapDataReactive$df <- filter(MapDataReactive$df, SubAction %in% input$inSubActionSelector)
    updateSelectizeInput(session, "inSubActionSelector",
                         choices = sort(MapDataReactive$df$SubAction, decreasing = TRUE))
    updateSelectizeInput(session, "inYearSelector",
                         choices = sort(MapDataReactive$df$Year, decreasing = TRUE))
  })

  #Year Update
  observeEvent(input$inYearSelector, {
    MapDataReactive$df <- filter(MapDataReactive$df, Year %in% input$inYearSelector)
    updateSelectizeInput(session, "inYearSelector",
                         choices = sort(MapDataReactive$df$Year, decreasing = TRUE))
    updateSelectizeInput(session, "inSubActionSelector",
                         choices = sort(MapDataReactive$df$SubAction, decreasing = TRUE))
  })
  
  #Tags Update
  observeEvent(input$inTagsSearch, {
    
    MapDataReactive$df <- MapDataFinal %>% 
                          filter_all(any_vars(. %in% input$inTagsSearch))
    updateSelectizeInput(session, "inYearSelector",
                         choices = sort(MapDataReactive$df$Year, decreasing = TRUE))
    updateSelectizeInput(session, "inSubActionSelector",
                         choices = sort(MapDataReactive$df$SubAction, decreasing = TRUE))
  })
  
  ###### END FILTER INPUT HANDLING 
  
  
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
       addPolygons(data = EJLayer, color = "#b57edc", weight = 1, group = "Environmental Justice Areas") %>%
       addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets") %>%
       addProviderTiles("Esri.WorldTopoMap", group = "Terrain")%>%
       addProviderTiles("GeoportailFrance.orthos", group = "Satellite")%>%
       addLayersControl(overlayGroups = c("Environmental Justice Areas"),
                        baseGroups = c("Streets", "Terrain", "Satellite"),
                        options = layersControlOptions(collapsed = FALSE,  position = 'bottomright')) %>%
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
  observeEvent(MapDataReactive$df,
    {
  #  print(MapDataReactive())
    Layers <- MapDataReactive$df
    #Creating Map Markers with URL (Will likely store this information in an Action only sheet and then join to layers in program after data importing)
    url <- as.character(Layers$Marker)
    w <- str_remove_all(Layers$Width, "[px]")
    h <- str_remove_all(Layers$Height, "[px]")
    
    mapIcon <- makeIcon(
      iconUrl = url,
      iconWidth = w, iconHeight = h)

      # Creating Popup Image
      PopupImage <- Layers$Image
      
     #Marker creation
     leafletProxy("leafmap") %>%
       #Clears markers and marker clusters for re render
        clearMarkers()%>%
        clearMarkerClusters()%>%
        addMarkers(data = Layers,
                lng = ~LONG, lat = ~LAT,
                icon = mapIcon,
                #Label
                label = Layers$ProjectName,
                #Marker Cluster Options
                clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                  zoomToBoundsOnClick = TRUE, 
                                  spiderfyOnMaxZoom = 5,
                                  removeOutsideVisibleBounds = TRUE,
                                  spiderLegPolylineOptions = list(weight = 2, color = "#222", opacity = 0.5), 
                                  freezeAtZoom = 15,
                                  maxClusterRadius = 1,
                                  weight=3,
                                  color="#33CC33", opacity=1, fillColor="#FF9900", 
                                  fillOpacity=0.8) ,
                #Popup Code
                popup = paste(
                  "<div class='popup-wrapper'>",
                    "<div class='popup-image' style='border: 0px solid red;",
                    "background-image: url(\"",Layers$Image,"\")'>",
                    # "<img class='pu-img' src='", Layers$Image ,"'>",
                    "</div>",
                    "<div class='popup-text'>",
                      "<div class='popup-title'>",
                      "<div class='popup-title-marker",
                        (ifelse(Layers$Override == TRUE,
                                paste("marker-resize-01"),
                                paste("marker-resize-02")
                          )) ,
                              "' style='margin-right:6px;", Layers$Height,"; width:",Layers$Width,"; background-image:url(\"",Layers$Marker,"\");'>",
                        "</div>",
                          "<div class='popup-title-text'>",
                            "<span class='popup-title-h1' style=''>", Layers$Action, "</span>",
                            "<span class='popup-title-h2' style='color:",Layers$Color,";'>", Layers$SubAction,"</span>",
                          "</div>",
                        "</div>",
                        "<div class='popup-body'>",
                          "<span class='popup-title-h2 pu-h2-adj'><b>Project:</b>", Layers$ProjectName,"</span>",
                          "<span class='popup-line adj'>",
                            "<b>Status:</b> ",  Layers$Status,"",  
                            "<b style='margin-left:10px;'>Started:</b> ", Layers$Year, "",
                            "<b class='no-left-margin' style='margin-left:10px;'>Completed:</b> ", Layers$YearComplete, "<br>",
                           "</span>",
                           "<span class='popup-line popup-line-description",
                                  (ifelse((Layers$ShortDescription == 'NULL')|(is.na(Layers$ShortDescription)),
                                            paste("hide"),
                                            paste("")
                                    )) , 
                              "'><b>Description:</b>",Layers$ShortDescription,
                            "</span>",
                            "<span class='popup-line", 
                                  (ifelse(Layers$Value1 == 'NULL' |(is.na(Layers$Value1)),
                                                               paste("hide"),
                                                               paste("")
                                   )) , 
                                "'><b>",Layers$KeyMetric1,":</b> ",Layers$Value1,"</span>",
                            "<span class='popup-line popup-line-adj",
                                   (ifelse(Layers$Value2 == 'NULL' |(is.na(Layers$Value2)),
                                       paste("hide"),
                                       paste("")
                                )) , 
                              "'><b>",Layers$KeyMetric2,":</b> ",Layers$Value2,"</span>",
                            "<span class='popup-line",
                                  (ifelse((Layers$Url == '')|(is.na(Layers$Url)),
                                          paste("hide"),
                                          paste("")
                                  )) , 
                            "'>", 
                                "<b>More Info:</b><a href='",Layers$Url,"'>",Layers$Url,"</a>",
                            "</span>",
                            "<span class='popup-line' style='text-overflow: ellipsis'>", 
                              "<span class='popup-line",
                                   (ifelse(Layers$LocationName == 'NULL'|(is.na(Layers$LocationName)),
                                          paste("hide"),
                                          paste("")
                                  )) , 
                              "'><b>Location:</b>",Layers$LocationName,"</span>",
                              "<span class='popup-line popup-line-adj'><b>Lat:</b>",Layers$LAT,"&nbsp; <b>Long:</b>",Layers$LONG,"</span>",
                               
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
