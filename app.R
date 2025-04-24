
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
  bg = '#8FBC8F',
  title = "Known housing location of worst landlords",
  sidebar = sidebar(
    helpText("You are looking at the top 12 worst landlords in Broome County, and their known properties"),
    tags$style(
      HTML(
        ".box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#8FBC8F}")),
  selectizeInput(
    'foo', label = "Search for an Address" , choices = NULL,selected = NULL,
    options = list(placeholder = "Enter an Address")
  ),
  box(
    solidHeader = TRUE,
    title = "Housing Information",
    status = "primary",
    width = 10,
    collapsed = T,
    htmlOutput("ui_text"))),
  nav_panel(
    title = "The Map",
    card(leafletOutput(outputId = "parcel"))))


server <- function(input, output, session) {
  parcel_2 = read.csv("files/dashboard-dat.csv")%>% filter(Latitude < 43)
  updateSelectizeInput(session, 'foo', choices = parcel_2$actaddress, selected = NULL, server = TRUE)
  RV <- reactiveValues(Clicks = list())
  counties <- counties(cb = TRUE, year = 2023, class = "sf") %>% st_transform(4326)
  st_crs(counties) = 4326
  broome = counties %>% filter(STATE_NAME == "New York" & NAMELSAD == "Broome County")
  broome_bbox <- st_bbox(broome)
  sf_parcel = st_as_sf(parcel_2, coords = c('Longitude', 'Latitude'), crs = 4326)
  parcel_2$town = str_match(parcel_2$Municipal, "^[A-Z,a-z]+ [A-Z,a-z]+ ([A-Z,a-z, ]+)")[ ,2]
  broome_streets <- st_read("files/broome_streets/broome_streets.shp")
  output$parcel <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = broome,
        fillColor = "#e5e5e5",
        color = "#333333",
        weight = 4,
        opacity = 1) %>%
      addMarkers(
        data = sf_parcel,
        icon = list(iconUrl = "https://www.iconpacks.net/icons/2/free-location-icon-2955-thumb.png",
                    iconSize = c(25, 25))
      ) %>%
      addPolylines(data = broome_streets, weight = 2, color = "#333333") %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observeEvent(
    input$foo,{
      row <- parcel_2 %>% filter(actaddress == input$foo)
      
      print(row)
      jlat <- row$Latitude
      jlon <- row$Longitude
      val = which(row$Longitude == jlon & row$Latitude == jlat)
      leafletProxy("parcel") %>% clearGroup("SearchResult") %>%
        addMarkers(
          data = row, group = "SearchResult",icon = list(iconUrl = "https://img.icons8.com/?size=100&id=7880&format=png&color=FA5252",
                                                         iconSize = c(25, 25))
        )
      
      RV$Clicks <- paste0(
        "<b>Address:</b> ", 
        row$actaddress,
        "<br>",
        "<b>Owner:</b> ", row$Landlord_Group, "<br>",
        "<b>Ranking: </b>", row$Ranking, " on our Worst Evictors List", "<br>",
        "<b>Average Evictions Filed per Year: </b>", row$Yearly_Filings, "<br>",
        "<b>Total Code Violations: </b>", row$Total_Violations, "<br>"
      )
      
    })
  observeEvent(
    input$parcel_marker_click, {
      click <- input$parcel_marker_click
      clat <- click$lat
      clon <- click$lng
      
      print(clat)
      print(clon)
      val = which(parcel_2$Longitude == clon & parcel_2$Latitude == clat)
      updateSelectInput(session,"foo",selected = c(parcel_2$actaddress[val], RV$Clicks))
    })
  
  output$ui_text <- renderText({print(RV$Clicks)})
  
}

shinyApp(ui = ui, server = server)
