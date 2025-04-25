
# v2 changes
# split the dataset between landlord data (which should be smaller) and house data, so that way future information is easier to add on. 
# renamed certain things to be more intuitive (ex// removed parcel_2 as an un-intuitive name)

# regex to remove comments: #[A-Za-z()\- !]+
# but just be careful about hex codes! 

# loading in libraries
{
  library(shiny)
  vwr = dialogViewer('app_V2', width = 1000, height = 1200)
  runGadget(shinyAppDir(appDir = './'), viewer = vwr)
} # Run app in presized window
# library(shiny)
library(bslib)
library(rsconnect)
library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(tigris)
library(tidygeocoder)


options(tigris_use_cache = TRUE)

# UI creation
ui <- page_navbar(
  # overall dashboard
  shinyWidgets::useShinydashboard(),
  bg = '#91cbef',
  title = "Known housing locations of top 12 worst landlords in Broome County",
  
  # sidebar information
  sidebar = sidebar(
    title = "Landlord Information",
    helpText("These landlords have sued the most tenants for eviction in Binghamton since the Emergency Eviction Moratorium ended in January 2022 (33 months)"),
    tags$style(
      HTML(
        ".box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#8FBC8F}

        div.a{
        font-size: 13px;
        font-style: itaLIC;
        font-color: 'gray';
        }"
      )
    ),
  
  # address selection
  selectizeInput(
    'addselect',
    label = "Search for an Address" ,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = "Enter an Address")
  ),
  
  # display box for landlord information
  box(
    solidHeader = TRUE,
    width = 12,
    htmlOutput("landlord_text")
  )
  ),
  
  # first panel--the map
  nav_panel(
    title = "Map",
    icon = icon("location-dot"),
    
    # display address card
    card(
      card_title("Selected Address: "),
      card_body(htmlOutput("address_text1")),
      fill = F
    ),
    
    # leaflet map card
    card(leafletOutput(outputId = "parcel")),
    div(
      style = "text-align: center; padding-top:10px;, width = 100%",
      actionButton(
        "action",
        " Zoom to Full Map View",
        icon = icon("search"),
        style = "color: 'gray'; background-color: #91cbef; border-color: 'gray', padding-top=100px;"
      )
    )
  ),
  
  # second panel--housing information
  nav_panel(
    title = "Housing Information",
    icon = icon("house"),
    
    # display address card
    card(
      card_title("Selected Address: "),
      card_body(htmlOutput("address_text2")),
      fill = F
    ),
    
    # display image of the house card
    card(uiOutput("image"))
  )
)


server <- function(input, output, session) {
  
  # load in the data
  llord = read.csv("files/landlord-dat.csv")
  hdat = read.csv("files/housing-dat.csv") %>% filter(Latitude < 43)
  
  # update the address selection
  updateSelectizeInput(
    session,
    'addselect',
    choices = c(" " = "", hdat$actaddress),
    selected = "",
    server = TRUE
  )
  
  # default values for these sections
  RV <-
    reactiveValues(llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>",
                   add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>")
  
  # loading in the base map
  counties <-
    counties(cb = TRUE, year = 2023, class = "sf") %>% st_transform(4326)
  st_crs(counties) = 4326
  broome = counties %>% filter(STATE_NAME == "New York" &
                                 NAMELSAD == "Broome County")
  broome_bbox <- st_bbox(broome)
  sf_parcel = st_as_sf(hdat,
                       coords = c('Longitude', 'Latitude'),
                       crs = 4326)
  broome_streets <-
    st_read("files/broome_streets/broome_streets.shp")
  
  # leaflet time!
  output$parcel <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = broome,
        fillColor = "#e5e5e5",
        color = "#333333",
        weight = 4,
        opacity = 1
      ) %>%
      addMarkers(
        data = sf_parcel,
        icon = list(iconUrl = "files/blue-loc.png",
                    iconSize = c(25, 25))
      ) %>%
      addPolylines(data = broome_streets,
                   weight = 2,
                   color = "#333333") %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  # observe inputs into address selector
  observeEvent(input$addselect, {
    req(input$addselect)
    selection <- hdat %>% filter(actaddress == input$addselect)
    jlat <- selection$Latitude
    jlon <- selection$Longitude
    val = which(selection$Longitude == jlon & selection$Latitude == jlat)
    leafletProxy("parcel") %>% clearGroup("SearchResult") %>%
      addMarkers(
        data = selection,
        group = "SearchResult",
        icon = list(iconUrl = "files/red-loc.png",
                    iconSize = c(25, 25))
      ) %>%
      setView(lng = jlon,
              lat = jlat,
              zoom = 16)
    
    # landlord information string to display (in HTML)
    RV$llord_click <- paste0(
      "<b>Landlord:</b> ",
      selection$Landlord_Group,
      "<br><br> <div class = 'a'>",
      "Other known aliases: ",
      paste0("<br>- ", paste0(
        str_split(llord$aliases[which(llord$landlord == selection$Landlord_Group)], ", ")[[1]], collapse = "<br>- "
      )), 
      "<hr></div>",
      "<b>Ranking: </b>",
      llord$ranking[which(llord$landlord == selection$Landlord_Group)],
      " on our Worst Evictors List",
      "<br>",
      "<b>Average Evictions Filed per Year: </b>",
      llord$yearlyfilings[which(llord$landlord == selection$Landlord_Group)],
      "<br>",
      "<b>Total Code Violations for Landlord: </b>",
      llord$code_violations[which(llord$landlord == selection$Landlord_Group)],
      "<br>"
    )
    
    RV$add = selection$actaddress
    
    RV$img = ""
    
  })
  
  # observe if an individual clicked on a marker on the map
  observeEvent(input$parcel_marker_click, {
    click <- input$parcel_marker_click
    clat <- click$lat
    clon <- click$lng
    
    val = which(hdat$Longitude == clon &
                  hdat$Latitude == clat)
    
    # update the corresponding address to be in address selectos (and run the observeEvent() for it as well)
    updateSelectizeInput(session, "addselect", selected = hdat$actaddress[val])
  })
  
  observeEvent(input$action, {
    leafletProxy("parcel") %>%
      fitBounds(
        lng1 = -76.06,
        lat1 = 42.05,
        lng2 = -75.45,
        lat2 = 42.35
      )
  })
  
  output$landlord_text <- renderText({
    print(RV$llord_click)
  })
  
  output$address_text1 = output$address_text2 <- renderText({
    print(RV$add)
  })
  
  output$image = renderUI({
    tags$img(src = RV$img)
  })
  
}


shinyApp(ui = ui, server = server)
