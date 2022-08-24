library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(sf)
library(shinyjs)
library(tidyverse)


# save shp file as a RDA file to read into r? is this possible? 
# https://www.r-bloggers.com/2021/12/how-to-read-rda-file-in-r-with-example/

msa <- readOGR("./data", layer = "all_spp_msa", GDAL1_integer64_policy = TRUE)

msa$Species <- gsub("[[:punct:]]", " ", msa$Species) # remove special characters

msa$countCol <- ifelse(msa$COUNT_ <= 4, "grey",
                       ifelse(msa$COUNT_ >= 10, "cyan", "blue"))
msa$countCol <- ifelse(msa$COUNT_ == 500, "green", msa$countCol)

msa$parts <- ifelse(msa$COUNT_ == 500, "ICZM Atlas of Sig. Coastal & Marine Areas", msa$COUNT_)

msa_proj <- spTransform(msa, "+proj=longlat +datum=WGS84")

spp_list <- unique(msa_proj$Species)

# commercial fisheries
comFishSpp <- c("Capelin", "Cod", "Crab", "Halibut", "Herring", "Lobster", "Mackerel", "Redfish", 
                "Shrimp", "Smelt", "Snow Crab", "Tuna", "Turbot")


ui <- bootstrapPage(
  tags$style(HTML("
    body{background-color: #366677;}
    .tabbable > .nav > li > a {background-color: #366677; color: white; border-bottom: 1px solid white}
    .tabbable > .nav > li[active] > a {font-weight: bold;}
    "
  )),
  
  useShinyjs(),
  
  div(id = "main", style = 'padding-left:10px',
      
      h3("EAC Dashboard"),
      
      tags$hr(style = "border-top: 3px solid #ccc;"),
      
      sidebarLayout(
        
        sidebarPanel(
          style = "background-color: #366677", 
          tabsetPanel(type = "tabs",
                      tabPanel("Circle & ID",
                               br(),
                               actionButton("comFishButton", label = "Commercial Fisheries"),
                               shinyjs::hidden(
                                 div(id = "comFishDiv",
                                     checkboxGroupInput("comFishCheck", NULL, choices = comFishSpp))
                               ),
                               
                               actionButton("clearAll", "Clear All Displayed Layers")
                               ),
                      # tabPanel("spp",
                      #          br(),
                      #          fluidRow(
                      #            column(width = 3,
                      #              actionButton("sppButton", label = "spp"),
                      #              shinyjs::hidden(
                      #                div(id = "sppDiv",
                      #                    checkboxGroupInput("sppCheck", NULL, choices = spp_list))
                      #              )
                      #            ),
                      #            column(width = 3, offset = 2,
                      #                   actionButton("clearSpp", label = "clear all spp")
                      #              
                      #            )
                      #            
                      #          )),
                               # actionButton("sppButton", label = "spp"),
                               # shinyjs::hidden(
                               #   div(id = "sppDiv",
                               #       checkboxGroupInput("sppCheck", NULL, choices = spp_list))
                               # )),
                      tabPanel("msa",
                               h2("msa")),
                      tabPanel("tcc"),
                      tabPanel("misc."))
        ),
        
        mainPanel(
          leafletOutput("map")
        )
      )
      
      # sidebarLayout(
      #   sidebarPanel("2nd"),
      #   mainPanel()
      # )
      
      )
      
)

# ui <- bootstrapPage(
#   useShinyjs(),
#   fluidRow(column(width = 5, offset = 0,
#                   div(style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:5px',
#                       h3("EAC - MSA")))),
#   leafletOutput("map", height = 500),
#   br(),
#   
#   fluidRow(column(width = 5, offset = 0,
#                   div(style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:5px',
#                       id = "usr",
#                       #textInput("usr", "Please input your participant number: "),
#                       numericInput("usr", "Please select your participant number: ", "1", min = 1, max = 30, step = 1)))), 
#   
#   fluidRow(column(width = 5, offset = 0,
#                   div(style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:5px',
#                       id = "selection",
#                       selectInput("spp", "Please select a spp: ", choices = spp_list))),
#            column(width = 5, offset = 2,
#                   checkboxGroupInput("sppCheck", "Please select all that apply: ", choices = spp_list))),
#   
#   hidden(
#     div(id = "dldiv", style='padding-left:10px; padding-right:1px; padding-top:5px; padding-bottom:5px',
#         fluidRow(column(width = 5, offset = 0,
#                         downloadButton("dlshp", "Download Shapefile"))))
#   )
# )

# ui <- bootstrapPage(
#   useShinyjs(),
#   titlePanel("EAC Dashboard"),
#   sidebarLayout(
#     sidebarPanel(
#       fluidRow(column(width = 10,
#       
#       div(id = "side", style = 'padding-left:10px',
#           selectInput("spp", "Please select a spp: ", choices = spp_list),
#           br(),
#           checkboxGroupInput("sppCheck", "Please select all that app: ", choices = spp_list),
#           
#           numericInput("usr", "Please select your participant number: ", "1", min = 1, max = 30, step = 1),
#           downloadButton("dlshp", "Download polygons")) ))
# 
#       
#     ),
#     mainPanel(leafletOutput("map"))
#   )
# )


server <- function(input, output, session) {
  
  # msa_filter <- reactive({
  #   msa_proj[msa_proj$Species == input$spp, ]
  # })
  # 
  # msa_cfilter <- reactive({
  #   msa_proj[msa_proj$Species %in% input$sppCheck, ]
  # })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron) %>% 
      fitBounds(-59.41, 47.72, -55.48, 52.13) %>% 
      addDrawToolbar(polylineOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     circleOptions = FALSE,
                     rectangleOptions = FALSE,
                     markerOptions = FALSE,
                     editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
  })
  
  observeEvent(input$comFishButton,{
    shinyjs::toggle(id = "comFishDiv")
  })
  
  
  observe({
    comFish_csub <- msa_proj[msa_proj$Species %in% input$comFishCheck, ]
    
    leafletProxy("map") %>% 
      
      clearShapes() %>% 
      
      addPolygons(data = comFish_csub, weight = 1, color = "grey", smoothFactor = 0.5, 
                  fillColor = comFish_csub$countCol,
                  popup = ~paste("Species: ", comFish_csub$Species, "<br/>",
                                 "No. of Participants: ", comFish_csub$parts))
    
  })
  
  # observe({
  #   msa_sub <- msa_proj[msa_proj$Species == input$spp, ]
  #   leafletProxy("map", data = msa_filter()) %>% 
  #     clearShapes() %>% 
  #     addPolygons(weight = 1, color = "blue", popup = ~paste("No. of Surveyees: ", msa_sub$COUNT_Surv))
  # })
  # 
  # 
  # observe({
  #   msa_csub <- msa_proj[msa_proj$Species %in% input$sppCheck, ]
  #   leafletProxy("map", data =  msa_cfilter()) %>%
  #     clearShapes() %>%
  #     addPolygons(weight = 1, fillColor = msa_csub$countCol,
  #                 popup = ~paste("No. of Surveyees: ", msa_csub$COUNT_Surv))
  # })
  
  observeEvent(input$map_draw_new_feature, {
    show("dldiv")
  })
  
  observeEvent(input$clearAll, {
    updateCheckboxGroupInput(session, "comFishCheck", choices = comFishSpp, selected = NULL)
  })
  
  
  output$dlshp <- downloadHandler(
    filename = function() {
      msa_sub <- msa_proj[msa_proj$SPECIES == input$spp, ]
      lyrName <- unique(msa_sub$SPECIES)
      participant <- paste0(input$usr)
      paste("P", participant, "_", lyrName, ".zip", sep = "")
      
    },
    content = function(file) {
      temp_shp <- tempdir()
      geo = input$map_draw_new_feature$geometry$coordinates[[1]]
      lng = map_dbl(geo, `[[`, 1)
      lat = map_dbl(geo, `[[`, 2)
      shp = st_as_sf(tibble(lon = lng, lat = lat), 
                     coords = c("lon", "lat"),
                     crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      
      shp_files <- list.files(temp_shp, "shapefile*", 
                              full.names = TRUE)
      if(length(shp_files) != 0) {
        file.remove(shp_files)
      }
      st_write(shp, paste(temp_shp, "shapefile.shp", sep = "\\"))
      
      shp_files <- list.files(temp_shp, "shapefile*", 
                              full.names = TRUE)
      zip(zipfile = file, files = shp_files, flags = "-j")
      file.remove(shp_files)
    }
  )
  
  
}


shinyApp(ui, server)