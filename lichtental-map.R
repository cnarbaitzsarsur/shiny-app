#load libraries

install.packages("maps")
install.packages("rgdal")
install.packages("geojsonsf")
install.packages("leaflet.extras")
install.packages("shinydashboard")
install.packages("shinycssloaders")
install.packages("bslib")
install.packages("shinyWidgets")
install.packages("fresh")
install.packages("shinyBS")


#library(maps) #free data
library(leaflet)
library(dplyr)
library(shiny)
library(sf) # to Convert the SHP file to a GeoJSON format
library(geojsonsf) #to read GeoJSON file
library(leaflet.extras)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(bslib)
library(shinyWidgets)
library(fresh)

# DATA  -------------------
  # Read the GEOjson file
  
        # Base map --------------

        gebaude <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/03-infrastructure/gebaude/gebaude.geojson")
        
        # Enviroment --------------
        parksg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/01-environment/park-grunanlage/park-grunanlage-sg.geojson")
        baumensg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/01-environment/baumkataster/baumkataster-sg.geojson")
        kuhleheisseortesg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/01-environment/kuhle-heibe-orte/kuhle-heisse-orte.geojson")
        
        # Mobility --------------
        fahrradparksg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/fahrradpark.geojson")
        durchgangsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/durchgang-sg.geojson")
        eingangsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/eingang-sg.geojson")
        tramstopsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/tramstop-sg.geojson")
        busstopsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/bus-stop-sg.geojson")
        publictransportstop <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/public-transport-stops.geojson")
        publictransportnetwork <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/public-transport-network.geojson")
        strassensg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/strassengraph-sg.geojson")
        parkplatzesg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/parkplatze-sg.geojson")
        parkplatzesg <- st_cast(parkplatzesg, "POINT")
        menschenwegabendsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/citizens-way-abends.geojson")
        menschenwegtagsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/00-mobility/citizens-way-tags.geojson")
        
        # Infraestructure --------------
        lbwohnensg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/03-infrastructure/gebaude-sg/leistbares-wohnen-sg.geojson")
        parkletsg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/03-infrastructure/parklet-sg.geojson")
        erdgeschosssg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/03-infrastructure/erdgeschoss-sg.geojson")
        canalizationssg <- geojson_sf("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/data/03-infrastructure/canalizations-sg.geojson")
      
        color_palette <- function(Temperatur) {
          # Define your custom color palette based on the value range
          # You can modify or replace this function based on your specific requirements
          ifelse(Temperatur <= "heisse orte", "blue", "red")
        }
        
        
        
----------# to plot a map we need to create a map input on ui
ui <- fluidPage(
      includeCSS("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/thesis/shiny-app/shinny-app/www/style.css"),
      theme = bs_theme(version = 5, bootswatch = "minty"),
        fluidRow(
          column(width = 2,
             # Place your filters, title, and other content for the menu section here
             h1("Tune Our Block"), 
             h1("Lichtental"),
             div(),
             div(class="d-grid gap-2",
             actionButton("aboutlichtental", "About Lichtental", type="button", class="btn btn-default"),
            
             actionButton("","About Superblocks", type="button", class="btn btn-default")
             ),
             
             h2("Environment"),
               h3(prettyCheckbox("showTrees", "Trees", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                  prettyCheckbox("showGreenSpaces", "Green Spaces", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                  prettyCheckbox("showTemperature", "Cold and hot places", value = FALSE, shape = "round", thick = TRUE, status = "primary")),
          
             h2("Mobility"),
               h3(prettyCheckbox("showParkingLots", "Parking Lots", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showBikeParking", "Bike Parking", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showPedestrianSC", "Pedestrian shortcuts", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showPublicTransport", "Public Transport", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showEntries", "Parking entries", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showFlowDay", "Day pedestrian flow", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
                 prettyCheckbox("showFlowEvening", "Evening pedestrian flow", value = FALSE, shape = "round", thick = TRUE, status = "primary")),
          
             
             h2("Infrastructure"),
             h3(prettyCheckbox("showParklets", "Parklets", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
             prettyCheckbox("showGFUses", "Ground floor uses", value = FALSE, shape = "round", thick = TRUE, status = "primary"),
             prettyCheckbox("showCanalizations", "Canalizations", value = FALSE, shape = "round", thick = TRUE, status = "primary")),
             
          ),
      
      bsModal(id="aboutlichtental", title = "About Lichtental", trigger= "aboutlichtental", size = "large"),
      
            
      column(width = 10,
                  withSpinner(leafletOutput("mymap", height = "100vh"), type = 5, color = "grey")
                   # The map content goes here
            )
          ))
  
  # and stablish the "back" part in the server
  # mymap is the id of the ui that we can use in the server
  # and inside render leaflet we are gonna add the code for the map
  
server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
      
      # Access the dynamically updated geoJSON
      
          # Base map --------------
          gebaudeJSON <- reactive({gebaude})
          
          # Enviroment --------------
          baumensgJSON <- reactive({baumensg})
          parksgJSON <- reactive({parksg})
          kuhleheisseortesgJSON <- reactive({kuhleheisseortesg})
          
          # Mobility --------------
          fahrradparksgJSON <- reactive({fahrradparksg})
          durchgangsgJSON <- reactive({durchgangsg})
          eingangsgJSON <- reactive({eingangsg})
          publictransportnetworkJSON <- reactive({publictransportnetwork})
          publictransportstopJSON <- reactive({publictransportstop})
          strassensgJSON <- reactive({strassensg})
          parkplatzesgJSON <- reactive({parkplatzesg})
          menschenwegabendsgJSON <- reactive({menschenwegabendsg})
          menschenwegtagsgJSON <- reactive({menschenwegtagsg})
          
          # Infraestructure --------------
          lbwohnensgJSON <- reactive({lbwohnensg})
          parkletsgJSON <- reactive({parkletsg})
          erdgeschosssgJSON <- reactive({erdgeschosssg})
          canalizationssgJSON <- reactive({canalizationssg})
          
      
    # Add layers
      
        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE, opacity = 0.5 )) %>%
        setView(lng = 16.3581, lat = 48.22721, zoom = 17) %>% # Set the initial map view to Lichtental
          
        # Base map --------------
            
          addPolygons(data = gebaudeJSON(), color = "grey", weight = 1) %>%
            
        # Enviroment --------------
            
          addPolygons(data = parksgJSON(), color = "#92C584", weight = 1, group = "parksg", fillOpacity = 0.9) %>%
            
          addPolylines(data = kuhleheisseortesgJSON(), stroke=TRUE, 
                       color = case_when(kuhleheisseortesgJSON()$Temperatur == "kuhle orte" ~ "#DA3832",
                                                                         TRUE ~ "#464D9B"), 
                       weight = 6, opacity = 0.8,
                       fill = FALSE, fillColor = case_when(kuhleheisseortesgJSON()$Temperatur == "kuhle orte" ~ "#DA3832",
                                                      TRUE ~ "#464D9B"),
                       fillOpacity = 1, 
                       group = "kuhleheisseortesg") %>%
          
          addCircleMarkers(data = baumensgJSON(), stroke= TRUE, weight = 1, color = "#379237", radius = ~KRONENDURC * 2,fill = TRUE, fillOpacity = 0.9,
                           popup = ~paste("Tree ID:", GATTUNG_AR, "<br>",
                                          "Crown Size:", KRONENDURC,"<br>",
                                          "Height:", BAUMHOEHE_),
                           group = "baumensg"
                           #clusterOptions = markerClusterOptions()
          ) %>%
        # Mobility --------------
          
            addCircleMarkers(data=fahrradparksgJSON(), color = "#FBE34A", radius = 1, opacity = 1,
                           popup= ~paste("Capacity:", capacity),
                           group = "fahrradparksg")%>%
            

            addPolylines(data = durchgangsgJSON(),
                         color = "blue",
                         weight = 1,
                         dashArray = ifelse(durchgangsgJSON()$state == "Opened", "5", "0"),
                         group = "durchgangsg" ) %>%
            
            

            addCircleMarkers(data=eingangsgJSON(), color = "#DA3832", radius = 1, opacity = 1,
                             group = "eingangsg")%>%
            
            addCircleMarkers(data = publictransportstopJSON(), stroke = TRUE, color = "black", weight = 2, opacity = 1, fillColor = "#DA3832", radius = 6, fillOpacity = 1,
                             group = "publictransport")%>%
            
            #addPolylines(data = publictransportnetworkJSON(), color = "blue", weight = 1,
             #            group = "publictransport")%>%
            
            addCircleMarkers(data=parkplatzesg, color = "#464D9B", radius = 0.3, opacity = 1,
                             group = "parkplatzesg")%>%
          
            addPolylines(data = menschenwegtagsgJSON(), color = "#464D9B", weight = ~lines*1, opacity = 1, dashArray = "10, 10", group = "menschenwegtagsg") %>%
            
            addPolylines(data = menschenwegabendsgJSON(), color = "#DA3832", weight = ~lines*1, opacity = 1, dashArray = "10, 10", group = "menschenwegabendsg") %>%
          
          
        # Infraestructure --------------
            
            #addPolygons(data = lbwohnensgJSON(), color = "#4c4c4b", weight = 1,
                     # popup = ~paste("Social Housing Name:", HOFNAME, "<br>",
                      #               "No. of Apartments:", WOHNUNGSAN,"<br>",
                      #               "Construction year:", BAUJAHR),
                    #  group = "lbwohnensg")%>%
          
            addPolygons(data = parkletsgJSON(), color = "#FBE34A", group = "parkletsg") %>%
            
            addPolygons(data = erdgeschosssgJSON(), 
                        fillColor = case_when(erdgeschosssgJSON()$use == "permeable active" ~ "#E27245",
                                          erdgeschosssgJSON()$use == "permeable non active" ~ "#AFAEAF",
                                          erdgeschosssgJSON()$use == "public" ~ "#FBE34A",
                                          erdgeschosssgJSON()$use == "private non permeable" ~ "#5B5C5F",
                                          TRUE ~ "#FB2576"),
                        fillOpacity = 1,
                        color = case_when(erdgeschosssgJSON()$use == "permeable active" ~ "#5A5B5E",
                                          erdgeschosssgJSON()$use == "permeable non active" ~ "black",
                                          erdgeschosssgJSON()$use == "public" ~ "#5A5B5E",
                                          erdgeschosssgJSON()$use == "private non permeable" ~ "#5B5C5F",
                                          TRUE ~ "#FB2576"),
                        dashArray = case_when(erdgeschosssgJSON()$use == "permeable active" ~ "5, 5",
                                              erdgeschosssgJSON()$use == "permeable non active" ~ "5, 5",
                                              erdgeschosssgJSON()$use == "public" ~ "5, 5",
                                              erdgeschosssgJSON()$use == "private non permeable" ~ "NULL",
                                              TRUE ~ "#FB2576"),
                        weight = 1,
                        opacity = 1,
                        group = "erdgeschosssg")%>%
            
            addPolygons(data = canalizationssgJSON(), color = "black", weight = 0.5, group = "canalizations")

        })
    
    
    # Turn off and on layers
    
        # Enviroment --------------
        
            observe({
              if (input$showTrees) { leafletProxy("mymap", session) %>% showGroup("baumensg")
              } else { leafletProxy("mymap", session) %>% hideGroup("baumensg")}
              })
            
            observe({
              if (input$showGreenSpaces) { leafletProxy("mymap", session) %>% showGroup("parksg")
              } else { leafletProxy("mymap", session) %>% hideGroup("parksg")}
            })
            
            observe({
              if (input$showTemperature) { leafletProxy("mymap", session) %>% showGroup("kuhleheisseortesg")
              } else { leafletProxy("mymap", session) %>% hideGroup("kuhleheisseortesg")}
            })
        
        # Mobility --------------
            
            observe({
              if (input$showParkingLots) { leafletProxy("mymap", session) %>% showGroup("parkplatzesg")
              } else { leafletProxy("mymap", session) %>% hideGroup("parkplatzesg")}
            })
            
            observe({
              if (input$showBikeParking) { leafletProxy("mymap", session) %>% showGroup("fahrradparksg")
              } else { leafletProxy("mymap", session) %>% hideGroup("fahrradparksg")}
            })
            
            observe({
              if (input$showPedestrianSC) { leafletProxy("mymap", session) %>% showGroup("durchgangsg")
              } else { leafletProxy("mymap", session) %>% hideGroup("durchgangsg")}
            })
            
            observe({
              if (input$showPublicTransport) { leafletProxy("mymap", session) %>% showGroup("publictransport")
              } else { leafletProxy("mymap", session) %>% hideGroup("publictransport")}
            })
            
            observe({
              if (input$showEntries) { leafletProxy("mymap", session) %>% showGroup("eingangsg")
              } else { leafletProxy("mymap", session) %>% hideGroup("eingangsg")}
            })
            
            observe({
              if (input$showFlowDay) { leafletProxy("mymap", session) %>% showGroup("menschenwegtagsg")
              } else { leafletProxy("mymap", session) %>% hideGroup("menschenwegtagsg")}
            })
            
            observe({
              if (input$showFlowEvening) { leafletProxy("mymap", session) %>% showGroup("menschenwegabendsg")
              } else { leafletProxy("mymap", session) %>% hideGroup("menschenwegabendsg")}
            })
            
            
        # Infraestructure --------------
            
            observe({
              if (input$showParklets) { leafletProxy("mymap", session) %>% showGroup("parkletsg")
              } else { leafletProxy("mymap", session) %>% hideGroup("parkletsg")}
            })
            
            observe({
              if (input$showGFUses) { leafletProxy("mymap", session) %>% showGroup("erdgeschosssg")
              } else { leafletProxy("mymap", session) %>% hideGroup("erdgeschosssg")}
            })
            
            observe({
              if (input$showCanalizations) { leafletProxy("mymap", session) %>% showGroup("canalizations")
              } else { leafletProxy("mymap", session) %>% hideGroup("canalizations")}
            })
            
       # Pop ups
            
            # About Lichtental --------------
            
            'observeEvent(input$aboutlichtental, {
              showModal(modalDialog(
                title = "Important message",
                "This is an important message!",
                easyClose = TRUE
              ))})'
          
    
  }
  
shinyApp(ui, server)


