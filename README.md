Save The Sound 2020 Impact Map
---------------

About
---------------
The Save The Sound 2020 Impact Map was built for the [Save The Sound](https://www.savethesound.org/) orginization by [The Commons](https://www.ourcommoncode.org/) to communicate and catalog their various efforts to improve the greater Long Island Sound. Below you will find general information about the Save the Sound Impact Map and documention for the database control sheets. App documentation can be found inline in App.R and Styler.css. Please disseminate to anyone who will be editing or entering input data for the application.

Quick Links
---------------

[Application](https://www.savethesound.org/about-us/impact-map/)

[Data](https://drive.google.com/drive/u/0/folders/1TvIs_COOCuC0LRyBo31iS5_vm2kZqpFo)

[Images](https://docs.google.com/spreadsheets/d/1lPJKhxGUZ4C4RSy0zVy4wzC4VAKcKAc257Co_pMy-eM/edit#gid=0)

[Git](https://github.com/ChesapeakeCommons/STSImpactMap)

Version Control and Access 
---------------
You can download the application from the [git repository.](https://github.com/ChesapeakeCommons/STSImpactMap) After downloading, please fork a new branch, and name it your last name, and date. e.g. Watson_07_08. Make any changes you'd like to this branch - it is yours. If there is a bug or problem with the Master branch, please perform a pull request to make changes to the core application and to be subsequently approved by the Commons team.

Application Publishing
---------------

To publish the application, use the whirlpool icon in Rstudio to publish via [Shinyapps.io](https://www.shinyapps.io/). You can see a step by step process of this [here](https://www.r-bloggers.com/2021/05/push-button-publishing-for-shiny-apps/).

Google Sheets Data Management
---------------

#### Data for the map is sourced from Google Sheets, the folder containing all files can be found [here](https://drive.google.com/drive/u/0/folders/1TvIs_COOCuC0LRyBo31iS5_vm2kZqpFo). **A few best management suggestions and important notes:**

* Limit access to the **Symbology, Zoom_Extent, Text_Input, and Production_Input** sheets. These sheets feed directly into the program and changing formatting and incorrect inputs can result in app failure.  
* Utilize version history and the permissions settings to manage access and editing to the input sheets.
* Only edit one sheet at a time before refreshing and checking changes. In the event of app failure due to incorrect inputs, it will be easier to identify the problem. 
* Enter and edit data following the instructions below. 

## **Production_Input and Dev_Input**
The **Production_Input** sheet contains the raw Action level data to be ingested by the application. **Production_Input** is ideally updated by a limited number of people to mitigate possible data entry issues. Use **Dev_Input** to first enter data into, and then append the data to **Production_Input** after data entry quality assurance and control. Small edits can be made directly to **Production_Input. Note, fields marked with a ‘*’ are required.**
### Fields 
   * Year 
      *  Start year of project 
   * YearComplete
      * Completion year of project. **If completed the same year as start, indicate  as such. If project is ongoing, mark with an ‘x’**
   * Action 
      * Must match Actions listed in **Symbology** verbatim.
   * SubSction 
      * Character limit of 30
   * ProjectName 
      * Character limit of 63
   * Image 
      * Can only accept links from Save the Sound wordpress website.
      * Ideally 3:1 aspect ratio, will crop and resize photos to scale.
   * LocationName 
      * Character limit of 47
   * City 
   * State 
      * State initials 
   * LAT
      * Decimal Degrees
   * LONG
      * Decimal Degrees
   * ShortDescription
      * No character limit 
   * KeyMetric1
      * Character limit of 47
   * Value1
      * No character limit
   * KeyMetric2
      * Character limit of 47
   * Value2
      * No character limit
   * URL 
   * Tags 
     * Enter comma delimited words to show up in the search box 
   
## **Symbology**
The **Symbology** sheet contains the master list of actions and their Icons. It also contains placeholder image for when no image is provided in **Production_Input**. 
You can add, remove, and alter actions so long as consistent with actions in **Production_Input and Dev_Input**.
### Fields 
  * Action 
    * Must match Production_Input verbatim 
  * Marker 
    * Link for Marker icon from Save The Sound wordpress site 
  * Width 
    * Width in pixels, include ‘px’
  * Height 
    * Height in pixels, include 'px’
  * Override 
    * Set to TRUE for all markers except SoundKeeper
    
## **Text_Input**
The **Text_Input** sheet controls the descriptive text and links in the sidebar. 
### Fields 
   * DescriptionParagraph
   * ActionCount
   * DescriptionSentence
   * Link1
   * Link2
   * Link3
  
## **Zoom_Extent**
The **Zoom_Extent** sheet controls the zoom levels and locations for the extent selection
### Fields 
   * Extent
      * Name of region ie, 'greater connecticut'
   * Latitude
      * Decimal Degrees
   * Longitude
      * Decimal Degrees 
   * Zoom
      * Higher number = more zoomed in.
      
 ## Images
 Images for the actions, symbology, application title, key, and compass rose, are hosted on the Save the Sound website. 
 Image links can be found [here](https://docs.google.com/spreadsheets/d/1lPJKhxGUZ4C4RSy0zVy4wzC4VAKcKAc257Co_pMy-eM/edit#gid=0). **Note, application images like the key and title, need to maintain URLs and dimensions when updating.**


















































