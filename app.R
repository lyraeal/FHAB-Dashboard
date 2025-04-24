
library(shiny)
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
ui <- page_navbar(
  shinyWidgets::useShinydashboard(),
  bg = '#91cbef',
  title = "Known housing locations of top 12 worst landlords in Broome County",
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
  selectizeInput(
    'foo',
    label = "Search for an Address" ,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = "Enter an Address")
  ),
  box(
    solidHeader = TRUE,
    width = 12,
    htmlOutput("ui_text")
  )
  ),
  nav_panel(
    title = "Map",
    icon = icon("location-dot"),
    card(
      card_title("Selected Address: "),
      card_body(htmlOutput("address_text1")),
      fill = F
    ),
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
  nav_panel(
    title = "Housing Information",
    icon = icon("house"),
    card(
      card_title("Selected Address: "),
      card_body(htmlOutput("address_text2")),
      fill = F
    ),
    card(uiOutput("image"))
  )
)


server <- function(input, output, session) {
  parcel_2 = read.csv("files/dashboard-dat.csv") %>% filter(Latitude < 43)
  updateSelectizeInput(
    session,
    'foo',
    choices = c(" " = "", parcel_2$actaddress),
    selected = "",
    server = TRUE
  )
  
  RV <-
    reactiveValues(Clicks = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>",
                   add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>")
  counties <-
    counties(cb = TRUE, year = 2023, class = "sf") %>% st_transform(4326)
  st_crs(counties) = 4326
  broome = counties %>% filter(STATE_NAME == "New York" &
                                 NAMELSAD == "Broome County")
  broome_bbox <- st_bbox(broome)
  sf_parcel = st_as_sf(parcel_2,
                       coords = c('Longitude', 'Latitude'),
                       crs = 4326)
  broome_streets <-
    st_read("files/broome_streets/broome_streets.shp")
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
  
  observeEvent(input$foo, {
    req(input$foo)
    row <- parcel_2 %>% filter(actaddress == input$foo)
    jlat <- row$Latitude
    jlon <- row$Longitude
    val = which(row$Longitude == jlon & row$Latitude == jlat)
    leafletProxy("parcel") %>% clearGroup("SearchResult") %>%
      addMarkers(
        data = row,
        group = "SearchResult",
        icon = list(iconUrl = "files/red-loc.png",
                    iconSize = c(25, 25))
      ) %>%
      setView(lng = jlon,
              lat = jlat,
              zoom = 16)
    RV$Clicks <- paste0(
      "<b>Landlord:</b> ",
      row$Landlord_Group,
      "<br><br> <div class = 'a'>",
      "Other known aliases: ",
      paste0("<br>- ", paste0(
        str_split(row$Aliases_of_Group, ", ")[[1]], collapse = "<br>- "
      )), 
      "<hr></div>",
      "<b>Ranking: </b>",
      row$Ranking,
      " on our Worst Evictors List",
      "<br>",
      "<b>Average Evictions Filed per Year: </b>",
      row$Yearly_Filings,
      "<br>",
      "<b>Total Code Violations for Landlord: </b>",
      row$Total.Code.Violations.for.Landlord.Group,
      "<br>"
    )
    
    RV$add = row$actaddress
    
    RV$img = ""
    
  })
  observeEvent(input$parcel_marker_click, {
    click <- input$parcel_marker_click
    clat <- click$lat
    clon <- click$lng
    
    val = which(parcel_2$Longitude == clon &
                  parcel_2$Latitude == clat)
    updateSelectizeInput(session, "foo", selected = parcel_2$actaddress[val])
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
  
  output$ui_text <- renderText({
    print(RV$Clicks)
  })
  
  output$address_text1 = output$address_text2 <- renderText({
    print(RV$add)
  })
  
  output$image = renderUI({
    tags$img(src = RV$img)
  })
  
}


shinyApp(ui = ui, server = server)
