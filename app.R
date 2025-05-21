

# libraries
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


# ui creation -------------------------------------------------------------

ui <- page_navbar(
  bg = '#91cbef',
  title = "Top 12 worst evictors in Binghamton",
  
  
  
  
  sidebar = sidebar(
    width = 340,
    
    helpText(
      "These landlords have sued the most tenants for eviction in Binghamton since the Emergency Eviction Moratorium ended in January 2022 (as of 9/30/2024). Here we have their known housing locations."
    ),
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
    'locselect',
    label = "Location",
    choices = NULL,
    selected = NULL
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
    title = "About Us",
    icon = icon("circle-info"),
    
    card(
      card_title("Binghamton Housing Justice: Worst Evictors Project"),
      card_body(htmlOutput("bginfo")),
      fill = F
    )
  )
  
)


# server function ---------------------------------------------------------

server <- function(input, output, session) {
  llord = read.csv("files/landlord-dat.csv")
  hdat = read.csv("files/housing-dat.csv") %>% filter(Latitude < 43)
  
  
  
  
  updateSelectizeInput(
    session,
    'locselect',
    choices = c("Binghamton" = "Binghamton", "Broome County" = "Broome_County"),
    selected = "Binghamton",
    server = TRUE
  )
  
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
  
  
  places = places(
    state = "NY",
    cb = T,
    year = 2023,
    class = "sf"
  ) %>%
    st_transform(4326)
  st_crs(places) = 4326
  bing = places %>% filter(NAME == "Binghamton")
  bing_bbox <- st_bbox(bing)
  
  
  hdat = st_as_sf(hdat,
                  coords = c('Longitude', 'Latitude'),
                  crs = 4326)
  hdat$bing = hdat %>%
    st_within(bing) %>%
    cbind() %>%
    as.data.frame()
  
  broome_streets <-
    st_read("files/broome_streets/broome_streets.shp")
  
  
  output$parcel <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = bing,
        fillColor = "#e5e5e5",
        color = "#333333",
        weight = 4,
        opacity = 1
      ) %>%
      addPolylines(
        data = st_intersection(broome_streets, bing),
        weight = 2,
        color = "#333333"
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  
  
  
  
  observeEvent(input$addselect, {
    req(input$addselect)
    selection <- hdat %>% filter(actaddress == input$addselect)
    jlat <- selection$Latitude
    jlon <- selection$Longitude
    val = which(selection$Longitude == jlon &
                  selection$Latitude == jlat)
    leafletProxy("parcel") %>% clearGroup("SearchResult") %>%
      addMarkers(
        data = selection,
        group = "SearchResult",
        icon = list(iconUrl = "files/red-loc.png",
                    iconSize = c(25, 25))
      ) %>%
      setView(
        lng = st_coordinates(selection$geometry)[1],
        lat = st_coordinates(selection$geometry)[2],
        zoom = 16
      )
    
    
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
      "<br> There are ",
      length(which(
        hdat$Landlord_Group == selection$Landlord_Group
      )),
      " known properties associated with this landlord in Broome County."
    )
    
    RV$add = selection$actaddress
    
    
    
    RV$img = selection$pic
    
  })
  
  
  
  
  observeEvent(input$locselect, {
    req(input$locselect)
    
    if (input$locselect == "Binghamton") {
      disp_dat = hdat %>% filter(bing == 1)
      
      leafletProxy("parcel") %>% clearMarkers() %>%
        addMarkers(
          data = disp_dat,
          icon = list(iconUrl = "files/blue-loc.png",
                      iconSize = c(25, 25))
        ) %>%
        clearShapes() %>%
        addPolygons(
          data = bing,
          fillColor = "#e5e5e5",
          color = "#333333",
          weight = 4,
          opacity = 1
        )  %>%
        addPolylines(
          data = st_intersection(broome_streets, bing),
          weight = 2,
          color = "#333333"
        )
      
      bbox = bing_bbox
      
      RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
      RV$img = ""
      
    } else {
      disp_dat = hdat
      
      leafletProxy("parcel") %>% clearMarkers() %>%
        addMarkers(
          data = disp_dat,
          icon = list(iconUrl = "files/blue-loc.png",
                      iconSize = c(25, 25))
        ) %>%
        clearShapes() %>%
        addPolygons(
          data = broome,
          fillColor = "#e5e5e5",
          color = "#333333",
          weight = 4,
          opacity = 1
        )  %>%
        addPolylines(data = broome_streets,
                     weight = 2,
                     color = "#333333")
      
      bbox = broome_bbox
      
      updateSelectizeInput(session,
                           "addselect",
                           selected = list(placeholder = "Enter an Address"))
      
      RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
      RV$img = ""
      
    }
    
    
    observeEvent(input$llordselect, {
      req(input$llordselect)
      
      if (input$llordselect == "All") {
        leafletProxy("parcel") %>% clearMarkers() %>%
          addMarkers(
            data = disp_dat,
            icon = list(iconUrl = "files/blue-loc.png",
                        iconSize = c(25, 25))
          )
        
        
        updateSelectizeInput(
          session,
          'addselect',
          choices = c(" " = "", disp_dat$actaddress),
          selected = list(placeholder = "Enter an Address"),
          server = TRUE
        )
        
        RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
        
        RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
        
        RV$img = ""
        
      } else {
        llordgroup <-
          disp_dat %>% filter(Landlord_Group == input$llordselect)
        leafletProxy("parcel") %>% clearMarkers() %>%
          addMarkers(
            data = llordgroup,
            icon = list(iconUrl = "files/blue-loc.png",
                        iconSize = c(25, 25))
          )
        
        
        updateSelectizeInput(
          session,
          'addselect',
          choices = c(" " = "", llordgroup$actaddress),
          selected = list(placeholder = "Enter an Address"),
          server = TRUE
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
          "<b>Total Code Violations for Landlord: </b>",
          "<br>",
          llord$code_violations[which(llord$landlord == input$llordselect)],
          "<br>",
          "<br>There are ",
          length(which(
            hdat$Landlord_Group == input$llordselect
          )),
          " known properties associated with this landlord in Broome County."
        )
        
        updateSelectizeInput(session,
                             "addselect",
                             selected = list(placeholder = "Enter an Address"))
        
        RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
        
        RV$img = ""
        
      }
      
    })
    
    
    
    observeEvent(input$action, {
      leafletProxy("parcel") %>%
        flyToBounds(
          lng1 = bbox[1] %>% unname(),
          lat1 = bbox[2] %>% unname(),
          lng2 = bbox[3] %>% unname(),
          lat2 = bbox[4] %>% unname(),
          options = list(duration = 0.25)
        )
    })
    
  })
  
  
  
  
  
  
  
  observeEvent(input$parcel_marker_click, {
    click <- input$parcel_marker_click
    
    c_coord = data.frame("lat" = click$lat,
                         "lng" = click$lng)
    
    hdat$geometry %>% str()
    
    val = which(hdat$geometry == st_as_sf(c_coord, coords = c("lng", "lat"))$geometry)
    
    updateSelectizeInput(session, "addselect", selected = hdat$actaddress[val])
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
    print(
      paste0(
        '<li style="display:inline-block;">
      In 2019, the NYS Housing Stability & Tenant Protections Act significantly increased the rights and protections of renter households against landlord profiteering and exploitation. While these expansions were hard fought, tenants throughout the City of Binghamton continue to face discrimination - source of income, disability status, eviction history, household composition, etc. - when attempting to rent available units. Illegal practices are also pervasive in the form of “self-help evictions” as local landlords do everything they can to avoid submitting to legal court proceedings in order to carry out more immediate displacement.
      <br><br>    
      Housing justice advocates in Binghamton have identified low enforcement rates of RPAPL §768, related to unlawful eviction attempts, by Binghamton Police and Broome County Sheriff Departments. Similarly, enforcement of the expanded protections within Landlord Tenant proceedings at City Court is often inconsistent. Local tenants cannot simply rely on the implementation of the 2019 changes when seeking to obtain or maintain rental housing. 
      <br><br>
      While New York State offers a Statewide Landlord Tenant Eviction Dashboard, the accessible data only represents aggregate eviction filings and offers neither information that can be used to assist tenants in making informed decisions regarding their residential opportunities nor resources should they be subject to potential violations by their landlord.
      <br><br>
      This dashboard is intended to fill that gap and will be updated + expanded annually to reflect landlords within the City who most frequently sue their tenants for eviction - a practice that, when serial or chronic, is commonly associated with poor property conditions and tenants rights violations.  The 12 landlords currently represented on this dashboard are responsible for 26% of all eviction filings in Binghamton post-COVID (1/15/2022-9/30/2024) and own an estimated 394 properties throughout Broome County.
      <br><hr>
      This project was completed in collaboration with the Fair Housing Advisory Board of Broome County, Binghamton Tenants Union, and the Digital and Data Studies program at Binghamton University. More information on local housing justice efforts may be found <a href="https://www.binghamtonhousingjustice.com/">here</a>.
      <br><br>
      The source code for this project can be found <a href="https://github.com/lyraeal/FHAB-Dashboard">here</a>. All eviction filing and Code violation data used in this project was obtained via Freedom of Information Law (FOIL) requests, with an identified date range between January 15, 2022 and September 30, 2024. Affiliated properties were found by searching <a href="https://gis.broomecountyny.gov/website/apps/parcel_mapper/viewer.html">here</a>.'
      )
    )
  })
  
}

# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
