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
if(!require(leafem)) install.packages("rgdal", repos = "http://cran.us.r-project.org")



basemap <- leaflet() %>% setView(lng = 138, lat = 38, zoom = 6) %>% 
  
          # Adds order to layers (first background, then rasters, then shapefiles)
          addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
          addMapPane("rastermaps", zIndex = 420) %>%  
          addMapPane("prefslayer", zIndex = 430) %>%
          addMapPane("subprefslayer", zIndex = 440) %>%
          addMapPane("cartolabels", zIndex = 450) %>%

          #addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
          #addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png', group = "Carto") %>% 
          
          # Adds carto DB maps (labels separated so that the are not overlapped by rasters/shapefiles)
          addProviderTiles("CartoDB.PositronNoLabels") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels", 
                     options = leafletOptions(pane = "cartolabels"),
                     group = "cartolabels") %>%
          
          # Adds Ersri World Imagery map
          addProviderTiles(provider = "Esri.WorldImagery", group = "Esri World Imagery", #) %>%
                           options = pathOptions(pane = "background_map"))  %>%
          # Adds coords
          addMouseCoordinates()       %>% 
  
          # The folder with tiles must be inside www folder in shiny project (only this solution works)
           # addTiles(urlTemplate = "tambo1519tiles_leafletprojnas/{z}/{x}/{y}.png",
           #          option = tileOptions(tms = T, minZoom = 6, maxZoom = 10)) %>%
            addLayersControl(
              baseGroups = c("Carto", "Esri World Imagery"),
              #overlayGroups = c("Rice map", "Rice paddy area (Prefecture-level)","Rice paddy area (County-level)" ),
              options = layersControlOptions(collapsed = FALSE)) %>%
          # These widgets add title to LayersControl and puts the base layer to the background
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
          

# Prefectures shapefile
#prefsshapefile <- sf::st_read('inputdata/areachangeprefs_shiny.shp')
prefsshapefile <- readOGR(dsn='inputdata', layer='areachangeprefs_shiny') 
subprefsshapefile <- readOGR(dsn='inputdata', layer='areachangesubprefs_shiny') 


# Area value ranges
areacolumns <- c("area198589","area199094","area199599","area200004",
                 "area200509","area201014","area201519")                              

prefsarearange <- c(min(prefsshapefile@data[,areacolumns],na.rm = T),max(prefsshapefile@data[,areacolumns],na.rm = T))
subprefsarearange <- c(min(subprefsshapefile@data[,areacolumns],na.rm = T),max(subprefsshapefile@data[,areacolumns],na.rm = T))


# Period names
periods <- c("1985-89","1990-94","1995-99","2000-04","2005-09","2010-14","2015-19","Total Change")
  
### SHINY UI 
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">TANBO Project</a>'), id="nav",
             windowTitle = "TANBO Project",
             
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
                                        selectInput("colors", "Color Scheme for Area Maps",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                        checkboxInput("legend", "Show legend", TRUE)
                          )
                      )
             ),
             tabPanel("Prefecture plots"
             ),
             tabPanel("Data"
                      
             ),
             tabPanel("About"
                      
             )
  )
)


### SHINY SERVER ###

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # Reactive expression to select which layer to render
  renderlayer <- reactive({
    input$mapselection
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  # This reactive expression represents the path to the rice map for certain period
  # The folder with tiles must be inside www folder in shiny project (only this solution works)
  ricemappath <- reactive({
    paste("tambo",input$periods,"tiles/{z}/{x}/{y}.png",sep="")
  })
  
  areayear <- reactive({
    if(input$periods == "Total Change"){
      "percchange"
    } else
        paste("area",gsub("-", "", input$periods),sep="")
  })
  
  #addResourcePath("mytiles", "/Users/luis/Documents/research/jsps/ricemapping/shinyapp/firstapp/inputdata/tambo1519tiles_leafletprojnas")
  #addResourcePath("mytiles", "/Users/luis/Documents/research/jsps/ricemapping/shinyapp/firstapp/inputdata/tambo1519tiles_leafletprojnas")
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    basemap #%>%
      #addTiles(urlTemplate = "tambo1519tiles_leafletprojnas/{z}/{x}/{y}.png",
      #         option = tileOptions(tms = T, minZoom = 6, maxZoom = 10))
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
       option = c(tileOptions(tms = T, minZoom = 6, maxZoom = 12),
                  # This option fixes these tiles on top always
                  pathOptions(pane = "rastermaps")), 
       group = "rastermaps")
    } 
  })

  # Prefectures area maps
  observe({
    # Render when "background only" option is not selected
    if(renderlayer() == "Rice paddy area (Prefecture-level)"){
      
    # Pallets for area polygon maps
    pal <- colorNumeric( palette="viridis",domain = prefsshapefile@data[,areacolumns], na.color="transparent")
    #pal <- colorBin("YlOrRd", prefsshapefile@data$area198589, bins=10, na.color = "#bdbdbd")
    
    # Polygon popup information (Prefecture and rice area)
    if(input$periods == "Total Change"){
    popup <- paste0("<strong>Prefecture: </strong>", 
                    prefsshapefile@data$prefs, 
                    "<br><strong>Rice area change: </strong>", 
                    prefsshapefile@data[,"areachange"], " km<sup>2</sup>",
                    "<br><strong>Percentage change: </strong>", 
                    prefsshapefile@data[,areayear()], " %")} 
    else
      popup <- paste0("<strong>Prefecture: </strong>", 
                      prefsshapefile@data$prefs, 
                      "<br><strong>Rice area: </strong>", 
                      prefsshapefile@data[,areayear()], " km<sup>2</sup>")
    
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
                  layerId = ~area198589, popup = popup)
    
    }
  })
  
  
  # SUB-Prefectures area maps
  observe({
    # Render when "background only" option is not selected
    if(renderlayer() == "Rice paddy area (County-level)"){
      
      # Pallets for area polygon maps
      #pal <- colorNumeric( palette="viridis", domain=subprefsshapefile@data[,areacolumns], na.color="transparent")
      pal <- colorBin("YlOrRd", subprefsshapefile@data[,"area198589"], bins=10, na.color = "#bdbdbd")
      
      # Polygon popup information (Sub prefecture and rice area)
      if(input$periods == "Total Change"){
        popup <- paste0("<strong>Prefecture: </strong>", 
                        subprefsshapefile@data$NAME_1, 
                        "<br><strong>County: </strong>",
                        subprefsshapefile@data$NAME_2,
                        "<br><strong>Rice area change: </strong>", 
                        subprefsshapefile@data[,"areachange"], " km<sup>2</sup>",
                        "<br><strong>Percentage change: </strong>", 
                        subprefsshapefile@data[,areayear()], " %")} 
      else
        popup <- paste0("<strong>Prefecture: </strong>", 
                        subprefsshapefile@data$NAME_1, 
                       "<br><strong>County: </strong>",
                       subprefsshapefile@data$NAME_2,
                       "<br><strong>Rice area: </strong>", 
                       subprefsshapefile@data[,areayear()], " km<sup>2</sup>")
      
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
                    color = "white", weight = 2, group = "subprefslayer", layerId = subprefsshapefile@data[,areayear()],
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
      pal <- colorNumeric( palette="viridis", domain=prefsshapefile@data[,areacolumns], na.color="transparent")
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~prefsshapefile@data[,areayear()])
    }
    
    # Legend for county-level area
    if (input$legend & renderlayer() == "Rice paddy area (County-level)") {
      pal <- colorNumeric( palette="viridis", domain=subprefsshapefile@data[,areacolumns], na.color="transparent")
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~subprefsshapefile@data[,areayear()])
    }
  })
  
  
   
}

shinyApp(ui, server)


### SHINY SERVER ###

