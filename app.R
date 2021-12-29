library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sf)
library(shinyjs)
library(magrittr)

msa <- readOGR("./data", layer = "select_msa", GDAL1_integer64_policy = TRUE)

msa_proj <- spTransform(msa, "+proj=longlat +datum=WGS84")

spp_list <- unique(msa_proj$SPECIES)

ui <- bootstrapPage(
  useShinyjs(),
  titlePanel("binder test"),
  leafletOutput("map", height = 500),
  br(),
  
  fluidRow(column(width = 5, offset = 0,
                  div(style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:5px',
                      id = "selection",
                      selectInput("spp", "Please select a spp: ", choices = spp_list)))),
  
  hidden(
    div(id = "dldiv", style='padding-left:10px; padding-right:1px; padding-top:5px; padding-bottom:5px',
        fluidRow(column(width = 5, offset = 0,
                        downloadButton("dlshp", "Download Shapefile"))))
  )
)

server <- function(input, output, session) {
  
  msa_filter <- reactive({
    msa_proj[msa_proj$SPECIES == input$spp, ]
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron) %>% 
      fitBounds(-59.41, 47.72, -55.48, 52.13) %>% 
      addDrawToolbar(polylineOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     circleOptions = FALSE,
                     rectangleOptions = FALSE,
                     markerOptions = FALSE,
                     singleFeature = TRUE)
  })
  
  observeEvent(input$map_draw_new_feature, {
    show("dldiv")
  })
  
  observe({
    msa_sub <- msa_proj[msa_proj$SPECIES == input$spp, ]
    leafletProxy("map", data = msa_filter()) %>%
      clearShapes() %>%
      addPolygons(weight = 1, color = "blue",
                  popup = ~paste("No. of surveyees: ", msa_sub$COUNT_Surv))
  })
  
  output$dlshp <- downloadHandler(
    # filename = function(){
    # 	# msa_sub <- msa_proj[msa_proj$SPECIES == input$spp,]
    # 	# spp_name <- msa_sub$SPECIES
    # 	paste("shapefile", "zip", sep = ".")
    # },
    filename = "shapefile.zip", 
    content = function(file){
      temp_shp <- tempdir()
      geo = input$map_draw_new_feature$geometry$coordinates[[1]]
      lng = map_dbl(geo, "[[", 1)
      lat = map_dbl(geo, "[[", 2)
      shp = st_as_sf(tibble(lon = lng, lat = lat),
                     coords = c("lon", "lat"), crs = 4326) %>% 
        summarise(geometry = st_combine(geometry)) %>% 
        st_cast("POLYGON")
      
      shp_files <- list.files(temp_shp, "shapefile*",
                              full.names = T)
      if(length(shp_files) != 0){
        file.remove(shp_files)
      }
      
      st_write(shp, paste(temp_shp, "shapefile.shp", sep = "\\"))
      
      # copy zip file to the file argument
      shp_files <- list.files(temp_shp, "shapefile*",
                              full.names = T)
      zip(zipfile = file, files = shp_files, flags = "-j")
      file.remove(shp_files)
      
    }
  )
  
  
  
}

shinyApp(ui, server)