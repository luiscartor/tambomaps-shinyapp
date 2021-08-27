# tambomap_app.R
# by Luis Carrasco
# on March 2021
# tambomap_app.R includes the Shiny IU and SERVER to run the tambomaps shiny application

##############################################################################################################
# Load packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(raster)) install.packages("raster", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("EColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(leafem)) install.packages("leafem", repos = "http://cran.us.r-project.org")
if(!require(mapview)) install.packages("mapview", repos = "http://cran.us.r-project.org")
if(!require(leafpop)) install.packages("leafpop", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

##############################################################################################################
# 1. DATA INPUTS
options(max.print=100000)

# INPUT Shapefiles
# Shapefile of the GADM data for Japanese prefectures (does not include Okinawa)
prefsshapefile <- readOGR(dsn='inputdata', layer='gadm36_JPN_1_noOkinawa_simp') 
# Shapefile of the GADM data for Japanese sub-prefectures (does not include Okinawa)
subprefsshapefile <- readOGR(dsn='inputdata', layer='gadm36_JPN_2_noOkinawa_simp') 

# INPUT Data tables
# Data on aggregated rice area at the prefecture and subprefecture levels
prefsdata <- read.csv(file='inputdata/riceareabyprefecture.csv', header =TRUE)
subprefsdata <- read.csv(file='inputdata/riceareabysubprefecture.csv', header =TRUE)


# 2. DATA PRE-PROCESSING

# Merge shapefiles with area change table: (function merge fails to keep polygon order to I use match function here)
prefsshapefile@data = data.frame(prefsshapefile@data, prefsdata[match(prefsshapefile@data$NAME_1, prefsdata$NAME_1),])
subprefsshapefile@data = data.frame(subprefsshapefile@data, subprefsdata[match(subprefsshapefile@data$GID_2, subprefsdata$GID_2),])

# There are NA values for subN level percentage change (0 initial rice area), so:
subprefsshapefile@data$percchange <- as.numeric(subprefsshapefile@data$percchange)

# Area columns names
areacolumns <- c("area198589","area199094","area199599","area200004",
                 "area200509","area201014","area201519")   

# Value ranges to use in color scales
prefsarearange <- c(min(prefsshapefile@data[,areacolumns],na.rm = T),max(prefsshapefile@data[,areacolumns],na.rm = T))
subprefsarearange <- c(min(subprefsshapefile@data[,areacolumns],na.rm = T),max(subprefsshapefile@data[,areacolumns],na.rm = T))

# Period names
periods <- c("1985-89","1990-94","1995-99","2000-04","2005-09","2010-14","2015-19","Total Change")


##############################################################################################################

# 3. LAYERS CREATION

# Colour pallets
# Create Diverging palette for percentage change in prefectures
# vector of colors for values smaller than 0 (80 colors)
rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(60)
## vector of colors for values larger than 0 (20 colors)
rc2 <- colorRampPalette(colors = c("white", "blue"), space = "Lab")(10)
## Combine the two color palettes
prefdivcols <- c(rc1, rc2)

# Create Diverging palette for percentage change in subprefectures
rc3 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(100)
## vector of colors for values larger than 0 (20 colors)
rc4 <- colorRampPalette(colors = c("white", "blue"), space = "Lab")(250)
## Combine the two color palettes
subprefdivcols <- c(rc3, rc4)


# CREATE BASEMAP
basemap <- leaflet() %>% setView(lng = 138, lat = 38, zoom = 6) %>% 
  
          # Adds order to layers (first background, then rasters, shapefiles, and lables)
          addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
          addMapPane("rastermaps", zIndex = 420) %>%  
          addMapPane("prefslayer", zIndex = 430) %>%
          addMapPane("subprefslayer", zIndex = 440) %>%
          addMapPane("cartolabels", zIndex = 450) %>%

          # Adds carto DB maps (labels separated so that the are not overlapped by rasters/shapefiles)
          addProviderTiles("CartoDB.PositronNoLabels") %>%
          #addProviderTiles("CartoDB.PositronOnlyLabels", 
           #          options = leafletOptions(pane = "cartolabels"),
            #         group = "cartolabels") %>%
          
          # Adds Ersri World Imagery map
          addProviderTiles(provider = "Esri.WorldImagery", group = "Esri World Imagery", #) %>%
                           options = pathOptions(pane = "background_map"))  %>%
          # Adds coords
          addMouseCoordinates()       %>% 
  
          # Adds background maps layer control
          addLayersControl(
              baseGroups = c("Carto", "Esri World Imagery"),
              options = layersControlOptions(collapsed = FALSE)) %>%
  
          # These widgets add title to LayersControl and put the base layer to the background
          htmlwidgets::onRender("
          function() {
            $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Background Map</label>');
          }
          function(el, x) {
               this.on('baselayerchange', function(e) {
                 e.layer.bringToBack();
               })
             }
            ")
          
##############################################################################################################


### 4. SHINY UI 
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">TAMBO Project</a>'), id="nav",
             windowTitle = "TAMBO Project",
             
             # FIRST PANNEL: MAPS
             tabPanel("Rice Mapping",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 350, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        sliderTextInput("periods","Select Period or Total Change" , 
                                                        choices = periods,
                                                        selected = periods[1],
                                                        grid = TRUE), #values which will be selected by default
                                        selectInput("mapselection", "Select Map",
                                                   choices = list("Background only","Rice map","Rice paddy area (Prefecture-level)","Rice paddy area (County-level)"),
                                                   selected = "Rice map"),
                                        # selectInput("colors", "Color Scheme for Area Maps",
                                        #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                        checkboxInput("legend", "Show legend", TRUE),
                                        tags$br(),
                                        "Total Change represents lost rice fields, estimated from the difference
                                        between the period 1985-89 and 2015-19."
                          )
                      )
             ),
             
             # SECOND PANEL: DATA AND PLOTS
             tabPanel("Data and Plots",
                      sidebarPanel(
                        
                        span(tags$h4("Visualize and download data"), style="color:#045a8d"),
                        pickerInput("selectpref", "Select Prefecture",
                                    choices = c(prefsdata$NAME_1),
                                    selected = "All Japan"),
                        pickerInput("selectcounty", "Select County" ,choices = NULL),
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Plots", 
                                   br(), plotOutput("plot")),
                          tabPanel("Tables", style = "overflow-y:scroll; max-height: 600px",
                                   br(), verbatimTextOutput("table"),
                                  downloadButton("downloadCsv", "Download as CSV")
                                  )
                        )
                      )
                      
             ),
             
             # THIRD PANEL: ABOUT
             tabPanel("About",
                      tags$div(
                        tags$h4("Tambo Project"),
                        "The Tambo Project aims to map rice fields in Japan since the 80's.
                        These data can be used for ecological or agricultural research,
                        conservation, management and planning, etc. We suggest users to check the associated documentation and to understand the 
                        uncertainties and limitations of this dataset.",
                        
                        tags$br(),tags$br(),tags$h4("Raster layers"),
                        "The rice raster layers were created by classfying landsat images, combining image temporal aggregation and phenology
                        of rice fields in Google Earth Engine. Each layer represents an averaged representation of the distribution of rice fields over a period of 5 years.
                        Methods for creating the rice layers are fully described here:", tags$br(),
                        "Carrasco et al. 2022. Historical mapping of rice fields in Japan using phenology andtemporally aggregated Landsat images in Google Earth Engine. Under review.", tags$br(),
                        "Original rice layers have a spatial resolution of 30m. In this app, rice layers are rendered using leaflet tiles and
                        the resolution depends on the zoom level. Original rasters can be downloaded here:",tags$br(),
                        "Extra post-processing was conducted for the presented rices layers: water bodies masking, and masking out rice pixels for Hokkaido cities were rice was never recorded in the last decades.",
                        
                        tags$br(), tags$br(),tags$h4("Rice paddy area aggregations"),
                        "Polygon layers represent aggregated rice area based on the raster layers, at the prefecture and sub-prefectural levels.",
                        
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data used to generate this Shiny application are available on ",tags$a(href="https://github.com/luiscartor/rice-mapping-app/", "Github."),
                        
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "Luis Carrasco Tornero",tags$br(),
                        "Laboratory of Biodiversity Sciences",tags$br(),
                        "Graduate School of Agricultural and Life Sciences",tags$br(),
                        "The University of Tokyo",tags$br(),
                        "luiscartor@gmail.com"
                        
                      )
                      
             )
  )
)

##############################################################################################################

### 5. SHINY SERVER

server <- function(input, output, session) {

  # Reactive expression to select which layer to render
  renderlayer <- reactive({
    input$mapselection
  })
  
  # This reactive expression represents the path to the rice map for certain period
  # The folder with tiles must be inside www folder in shiny project (only this solution works)
  # If working in web server (shinyapps), tiles are stored in personal website (luiscartor.github.io)
  ricemappath <- reactive({
    # Reads tiles locally (impossible to upload to shinyapps.io)
    #paste("tambo",input$periods,"tiles/{z}/{x}/{y}.png",sep="")
    # Reads tiles from my github website
    paste("https://luiscartor.github.io/tiles/tanboproject/tambo",input$periods,"tiles/{z}/{x}/{y}.png",sep="")
  })
 
  # Reactive expression to select period
  areayear <- reactive({
    if(input$periods == "Total Change"){
      "percchange"
    } else
        paste("area",gsub("-", "", input$periods),sep="")
  })
  
  # Render basemap
  output$map <- renderLeaflet({
      basemap 
  })
  

  # Background only option
  observe({
    # Render when "background only"
    if(renderlayer() == "Background only"){
      
      leafletProxy("map") %>%
        # Delete all layers
        clearGroup('rastermaps') %>%
        clearGroup('prefslayer') %>%
        clearGroup('subprefslayer')
        
    } 
  })  
  
  # Rice maps
  observe({
    # Render when "background only" option is not selected
    if(renderlayer() == "Rice map"){
    
    leafletProxy("map") %>%
      # Adds rice maps 
      # Only render rice maps if "background only" not selected
      clearGroup('rastermaps') %>%
      clearGroup('prefslayer') %>%
      clearGroup('subprefslayer') %>%
        
      addTiles(urlTemplate = ricemappath(),
       option = c(tileOptions(tms = T, minZoom = 5, maxZoom = 12),
                  # This option fixes these tiles on top always
                  pathOptions(pane = "rastermaps")), 
       group = "rastermaps")
    } 
  })

  # Prefectures area maps
  observe({
    if(renderlayer() == "Rice paddy area (Prefecture-level)"){
      
    # Polygon popup information (Prefecture and rice area)
    if(input$periods == "Total Change"){
      
      # Pallets for area change
      #pal <- colorBin("viridis", prefsshapefile@data$area198589, bins=round(seq(0,max(subprefsarearange),length.out=10),1), na.color = "#bdbdbd")
      pal <- colorNumeric(palette = prefdivcols, domain = c(-60,10), na.color = "transparent")
      # Popup for change
      popup <- paste0("<strong>Prefecture: </strong>", 
                      prefsshapefile@data$NAME_1,
                      "<br><strong>Rice area change: </strong>", 
                      round(prefsshapefile@data$areachange,2), " km<sup>2</sup>",
                      "<br><strong>Rice area percentage change: </strong>", 
                      round(prefsshapefile@data$percchange,2), " %") 

    }
    else{
      
      # Palettes for area polygon maps
      #pal <- colorNumeric( palette="YlOrRd",domain = prefsshapefile@data[,areacolumns], na.color="transparent")
      pal <- colorBin("viridis", prefsarearange, bins=8, na.color = "#bdbdbd",pretty = FALSE)
    
      # Pop-up for area
      popup <- paste0("<strong>Prefecture: </strong>", 
                      prefsshapefile@data$NAME_1, 
                      "<br><strong>Rice area: </strong>", 
                      prefsshapefile@data[,areayear()], " km<sup>2</sup>")
    }
      
    leafletProxy("map", data = prefsshapefile) %>%
      # Adds prefectural rice-area maps 
      clearGroup('rastermaps') %>%
      clearGroup('prefslayer') %>%
      clearGroup('subprefslayer') %>%
      
      # For practical reasons, we need to re-render the rice map when we select area layers
      addTiles(urlTemplate = ricemappath(),
               option = c(tileOptions(tms = T, minZoom = 6, maxZoom = 12),
                          # This option fixes these tiles on top always
                          pathOptions(pane = "rastermaps")), 
               group = "rastermaps") %>%
    
      addPolygons(data = prefsshapefile, fillColor = ~pal(prefsshapefile@data[,areayear()]), fillOpacity = 0.7,
                  color = "white", weight = 2, group = "prefslayer",
                  option = pathOptions(pane = "prefslayer"),
                  layerId = ~NAME_1, popup = popup)  
    
    }
  })
  
  # SUB-Prefectures area maps
  observe({
    
    if(renderlayer() == "Rice paddy area (County-level)"){
      
      # Polygon popup information (Sub prefecture and rice area)
      if(input$periods == "Total Change"){
        
        # Pallets for area change
        #pal <- colorBin("YlOrRd", subprefsshapefile@data[,"areachange"], bins=10, na.color = "#bdbdbd")
        pal <- colorNumeric(palette = subprefdivcols, domain = c(-100,250), na.color = "transparent")
        
        
        popup <- paste0("<strong>Prefecture: </strong>", 
                        subprefsshapefile@data$NAME_1,
                        "<br><strong>County: </strong>",
                        subprefsshapefile@data$NAME_2,
                        "<br><strong>Rice area change: </strong>", 
                        round(subprefsshapefile@data$areachange,2), " km<sup>2</sup>",
                        "<br><strong>Rice area percentage change: </strong>",
                        #if(is.numeric(subprefsshapefile@data$percchange)){
                        round(subprefsshapefile@data$percchange,2), " %") 
                        #} else
                         # subprefsshapefile@data$percchange, " %") 
          } 
      else{
        
        # Pallets for area polygon maps
        #pal <- colorBin("viridis", subprefsarearange, bins=10, na.color = "#bdbdbd")
        pal <- colorBin("viridis", subprefsarearange, bins = round(seq(0, sqrt(max(subprefsarearange)), length.out = 10)^2, 1), na.color = "#bdbdbd",pretty = FALSE) 
      
        popup <- paste0("<strong>Prefecture: </strong>", 
                        subprefsshapefile@data$NAME_1, 
                       "<br><strong>County: </strong>",
                       subprefsshapefile@data$NAME_2,
                       "<br><strong>Rice area: </strong>", 
                       subprefsshapefile@data[,areayear()], " km<sup>2</sup>")
      }
      
      leafletProxy("map", data = subprefsshapefile) %>%
        # Adds subprefectural rice-area maps 
        clearGroup('rastermaps') %>%
        clearGroup('subprefslayer') %>%
        clearGroup('prefslayer') %>%
        
        # For practical reasons, we need to re-render the rice map when we select area layers
        addTiles(urlTemplate = ricemappath(),
                 option = c(tileOptions(tms = T, minZoom = 6, maxZoom = 12),
                            # This option fixes these tiles on top always
                            pathOptions(pane = "rastermaps")), 
                 group = "rastermaps") %>%
        
        addPolygons(data = subprefsshapefile, fillColor = ~pal(subprefsshapefile@data[,areayear()]), fillOpacity = 0.7,
                    color = "white", weight = 2, group = "subprefslayer",      # adding layerId messes up the map
                    option = pathOptions(pane = "subprefslayer"), popup = popup)
      
    }
  })
  
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = prefsshapefile)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend & renderlayer() == "Rice paddy area (Prefecture-level)") {
      if(input$periods == "Total Change"){
        pal <- colorNumeric(palette = prefdivcols, domain = c(-60,10), na.color = "transparent")
        proxy %>% addLegend(position = "bottomright", title = "Rice paddy </br>area change (%)",
                            pal = pal, values = ~c(-60,10))
        
      } else{
        #pal <- colorNumeric( palette="viridis", domain=prefsshapefile@data[,areacolumns], na.color="transparent")
        pal <- colorBin("viridis", prefsarearange, bins=round(seq(0,max(subprefsarearange),length.out=10),1), na.color = "#bdbdbd",pretty = FALSE)
        proxy %>% addLegend(position = "bottomright", title = "Rice paddy </br>area (km<sup>2</sup>)",
                            pal = pal, values = ~prefsarearange)
      }
    }

    # Legend for county-level area
    if (input$legend & renderlayer() == "Rice paddy area (County-level)") {
      if(input$periods == "Total Change"){
        pal <- colorNumeric(palette = subprefdivcols, domain = c(-100,250), na.color = "transparent")
        proxy %>% addLegend(position = "bottomright", title = "Rice paddy </br>area change (%)",
                            pal = pal, values = ~c(-100,250))
      } else{
        #pal <- colorNumeric( palette="viridis", domain=subprefsshapefile@data[,areacolumns], na.color="transparent")
        pal <- colorBin("viridis", subprefsarearange, bins = round(seq(0, sqrt(max(subprefsarearange)), length.out = 10)^2, 1), na.color = "#bdbdbd",pretty = FALSE) 
        proxy %>% addLegend(position = "bottomright", title = "Rice paddy </br>area (km<sup>2</sup>)",
                            pal = pal, values = ~subprefsarearange)
      }
    }
  })

  # This observeEvent is needed in order to create a conditional seletInput (subprefecture level) for plot and data
  observeEvent(input$selectpref,{
    updatePickerInput(session,'selectcounty',
                      choices=as.character(c("No selection","All counties", subprefsshapefile@data[subprefsshapefile@data$NAME_1==input$selectpref,"NAME_2"])))
  })
  
  # PLOT ANT TABLE OUTPUTS
  # Reactive expression to select which prefecture
  prefselection <- reactive({
    input$selectpref
  })
  
  subprefselection <- reactive({
    if (identical(input$selectcounty, "NULL"))
      NULL
    else 
      input$selectcounty
  })
  
 # PLOT OUTPUT
  output$plot <- renderPlot({
    # Prefectural plots
     if(is.null(subprefselection())){
       df <- data.frame(period = periods[1:7],area = as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),
                                                                          c("area198589","area199094","area199599","area200004","area200509","area201014","area201519")]))
       areachange <- as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),"areachange"])
       percchange <- as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),"percchange"])
       ggplot(data = df,aes(x= period, y = area)) +
         scale_color_manual("blue") +
         geom_line(aes(group = 1),color='lightblue')+ geom_point(color='lightblue')+ ylab("Area "~(km^2))+ xlab("") +
         ggtitle(paste0("Prefecture: ",prefselection(),"\nTotal rice field area change: ", round(areachange,2)," km²",
                        "\nChange percentage: ",round(percchange,2),"%")) +
         theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=16))
                                                                             
       
     } else if(subprefselection()=="All counties" | subprefselection()=="No selection"){
       df <- data.frame(period = periods[1:7],area = as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),
                                                                          c("area198589","area199094","area199599","area200004","area200509","area201014","area201519")]))
       areachange <- as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),"areachange"])
       percchange <- as.numeric(prefsdata[prefsdata$NAME_1==prefselection(),"percchange"])
       ggplot(data = df,aes(x= period, y = area)) +
         scale_color_manual("blue") +
         geom_line(aes(group = 1),color='lightblue')+ geom_point(color='lightblue')+ ylab("Area "~(km^2))+ xlab("") +
         ggtitle(paste0("Prefecture: ",prefselection(),"\nTotal rice field area change: ", round(areachange,2)," km²",
                        "\nChange percentage: ",round(percchange,2),"%")) +
         theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=16))
     }
    
    else{
       df <- data.frame(period = periods[1:7],area = as.numeric(subprefsdata[subprefsdata$Prefecture==prefselection() & subprefsdata$Subprefecture==subprefselection(),
                                               c("area198589","area199094","area199599","area200004","area200509","area201014","area201519")]))
      areachange <- as.numeric(subprefsdata[subprefsdata$Prefecture==prefselection() & subprefsdata$Subprefecture==subprefselection(),"areachange"])
      percchange <- as.numeric(subprefsdata[subprefsdata$Prefecture==prefselection() & subprefsdata$Subprefecture==subprefselection(),"percchange"])
      ggplot(data = df,aes(x= period, y = area)) +
        scale_color_manual("blue") +
        geom_line(aes(group = 1),color='lightblue')+ geom_point(color='lightblue')+ ylab("Area "~(km^2))+ xlab("") +
        ggtitle(paste0("Prefecture: ",prefselection(),"\nTotal rice field area change: ", round(areachange,2)," km²",
                       "\nChange percentage: ",round(percchange,2),"%")) +
        theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=16))
  
      } 
    
  })
  
  # TABLE OUTPUT
  preftablenames <- c("Prefecture","area1985-89","area1990-94","area1995-99","area2000-04",
                      "area2005-09","area201-014","area2015-19","areachange","percentchange")
  subpreftablenames <- c("Prefecture","County","area1985-89","area1990-94","area1995-99","area2000-04",
                         "area2005-09","area201-014","area2015-19","areachange","percentchange")
  
  datasetInput <- reactive({
    if(is.null(subprefselection())){
      if(prefselection()=="All Japan"){
        mytable <- prefsdata
        names(mytable) <- preftablenames
      } else 
        mytable <- prefsdata[prefsdata$NAME_1==prefselection(),]
        names(mytable) <- preftablenames
      
    } else if(prefselection()=="All Japan" & subprefselection()=="No selection" ){
      mytable <- prefsdata
      names(mytable) <- preftablenames
      
    } else if(subprefselection()=="No selection"){
      mytable <- prefsdata[prefsdata$NAME_1==prefselection(),]
      names(mytable) <- preftablenames
      
    } else if(subprefselection()=="All counties"){
      if(prefselection()=="All Japan"){
        mytable <- subprefsdata[,-3]
      } else 
        mytable <- subprefsdata[subprefsdata$Prefecture==prefselection(),-3]
      
    } else {
      mytable <- subprefsdata[subprefsdata$Prefecture==prefselection() & subprefsdata$Subprefecture==subprefselection(),-3]
      
    }
    print(mytable, row.names = FALSE)

  })
  
  # RENDER output table
  output$table <- renderPrint({
    datasetInput()
  })
  
  # DOWNLOAD table
  # Downloadable csv of selected dataset ----
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("TAMBOproject_riceareachanges", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

##############################################################################################################

shinyApp(ui, server)
