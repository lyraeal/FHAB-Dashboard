
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
  
  
  bg = '#91cbef',
  title = "Top 12 worst evictors in Broome County",
  
  
  sidebar = sidebar(
    width = 320,
    
    helpText("These landlords have sued the most tenants for eviction in Binghamton since the Emergency Eviction Moratorium ended in January 2022 (33 months). Here we have their known housing locations."),
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
    'addselect',
    label = "Search for an Address" ,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = "Enter an Address")
  ),
  
  
  selectizeInput(
    'llordselect',
    label = "Filter by landlord" ,
    choices = NULL,
    selected = NULL,
    options = list(placeholder = "Enter a landlord")
  ),
  

  box(
    title = "Landlord Information",
    solidHeader = TRUE,
    width = 12,
    htmlOutput("landlord_text")
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
    

    card
    (uiOutput("image"),
      fill = F)
  ),
  
  

  

  nav_panel(
    title = "About Us",
    icon = icon("circle-info"),
    
    card(
      card_title("FHAB Worst Landlords Project"),
      card_body(htmlOutput("bginfo")),
      fill = F
    )
  )
  
)




server <- function(input, output, session) {
  

  llord = read.csv("files/landlord-dat.csv")
  hdat = read.csv("files/housing-dat.csv") %>% filter(Latitude < 43)
  


  updateSelectizeInput(
    session,
    'addselect',
    choices = c(" " = "", hdat$actaddress),
    selected = "",
    server = TRUE
  )
  
  updateSelectizeInput(
    session,
    'llordselect',
    choices = c("All" = "All", llord$landlord),
    selected = "All",
    server = TRUE
  )
  

  RV <-
    reactiveValues(llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>",
                   add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>")
  

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
  

  output$parcel <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = broome,
        fillColor = "#e5e5e5",
        color = "#333333",
        weight = 4,
        opacity = 1
      ) %>%
      addPolylines(data = broome_streets,
                   weight = 2,
                   color = "#333333") %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  

  

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
      "<br>",
      "<br> There are ", length(which(hdat$Landlord_Group == selection$Landlord_Group)), " known properties associated with this landlord."
    )
    
    RV$add = selection$actaddress
    


    RV$img = selection$pic
    
  })
  
  

  


  observeEvent(input$llordselect, {
    req(input$llordselect)
    
    if (input$llordselect == "All"){
      
      leafletProxy("parcel") %>% clearMarkers() %>%
        addMarkers(
          data = hdat,
          icon = list(iconUrl = "files/blue-loc.png",
                      iconSize = c(25, 25))
        )
      
      updateSelectizeInput(session, "addselect", selected = list(placeholder = "Enter an Address"))
      
      RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
      RV$img = ""
      
    } else {
      llordgroup <- hdat %>% filter(Landlord_Group == input$llordselect)
      leafletProxy("parcel") %>% clearMarkers() %>%
        addMarkers(
          data = llordgroup,
          icon = list(iconUrl = "files/blue-loc.png",
                      iconSize = c(25, 25))
        )
      
      RV$llord_click <- paste0(
        "<b>Landlord:</b> ",
        input$llordselect,
        "<br><br> <div class = 'a'>",
        "Other known aliases: ",
        paste0("<br>- ", paste0(
          str_split(llord$aliases[which(llord$landlord == input$llordselect)], ", ")[[1]], collapse = "<br>- "
        )), 
        "<hr></div>",
        "<b>Ranking: </b>",
        llord$ranking[which(llord$landlord == input$llordselect)],
        " on our Worst Evictors List",
        "<br>",
        "<b>Average Evictions Filed per Year: </b>",
        llord$yearlyfilings[which(llord$landlord == input$llordselect)],
        "<br>",
        "<b>Total Code Violations for Landlord: </b>", "<br>", 
        llord$code_violations[which(llord$landlord == input$llordselect)],
        "<br>", 
        "<br>There are ", length(which(hdat$Landlord_Group == input$llordselect)), " known properties associated with this landlord."
      )
      
      updateSelectizeInput(session, "addselect", selected = list(placeholder = "Enter an Address"))
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
      RV$img = ""
      
    }
    
  })
  
  
  

  

  observeEvent(input$parcel_marker_click, {
    click <- input$parcel_marker_click
    clat <- click$lat
    clon <- click$lng
    
    val = which(hdat$Longitude == clon &
                  hdat$Latitude == clat)
    

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
    print(paste0(RV$add, "<!--Designed by Lyra Lu -->"))
  })
  
  output$image = renderUI({
    tags$img(src = RV$img)
  })
  
  output$bginfo = renderText({
    print(paste0(
      '<li style="display:inline-block;">In 2019, the NYS Housing Stability & Tenant Protections Act significantly increased the rights and protections of renter households against landlord profiteering and exploitation. While these expansions were hard fought, tenants throughout the City of Binghamton continue to face discrimination - SOI, disability status, eviction history, household composition, etc. - when attempting to rent available units. Illegal practices are similarly pervasive in the form of self-help evictions as local landlords do everything they can to avoid submitting to legal court proceedings in order to carry out more immediate displacement. 
      <br><br>
      Fair Housing Advisory Board members have identified low enforcement rates of RPAPL §768, related to unlawful eviction attempts, by Binghamton Police and Broome County Sheriff Departments and inconsistent enforcement of the otherwise expanded protections within Landlord Tenant proceedings at City Court, indicating that local tenants cannot simply rely on the implementation of the 2019 changes when seeking to obtain or maintain rental housing. Similarly, while New York State offers a Statewide Landlord Tenant Eviction Dashboard, the accessible data only represents aggregate eviction filings and offers neither information that can be used to assist tenants in making informed decisions regarding their residential opportunities nor resources should they be subject to potential violations by their landlord.
      <br><hr>
      This project was created in collaboration with the Fair Housing Advisory Board (FHAB) at Broome County. Please check out more information related to the FHAB at their website <a href="https://www.binghamtonhousingjustice.com/">here</a>.
      <br><br>
      The source code for this project can be found <a href="https://github.com/lyraeal/FHAB-Dashboard">here</a>. All data used in this project was obtained from FOIL requests, and properties were found by searching <a href="https://gis.broomecountyny.gov/website/apps/parcel_mapper/viewer.html">here</a>.
      <br><br>
      This dashboard currently has ', nrow(llord), ' landlords and ', nrow(hdat), ' known housing locations.'
    ))
  })
  
}



shinyApp(ui = ui, server = server)
