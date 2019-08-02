library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see


# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -118.24, lat = 34, zoom = 10)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    selected_year <- input$year
    crimes_by_year <- crimes_year_region[crimes_year_region$Year == selected_year, ]
    
    radius <- ((crimes_by_year$num_of_crimes / max(crimes_year_region$num_of_crimes)) / 0.5)^3 * 2000

    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    colorData <- crimes_by_year$num_of_crimes
    pal <- colorBin("viridis", colorData, 5, pretty = FALSE)

    leafletProxy("map", data = crimes_by_year) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius=radius, layerId=~Area,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>% addLegend("bottomleft", pal=pal, values=colorData, title="Num of Crimes",
        layerId="Area")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(area, lat, lng) {
    #selectedZip <- allzips[allzips$zipcode == zipcode,]
    num_of_crimes <- crimes_year_region[crimes_year_region$Year == input$year 
                                        & crimes_year_region$Area == area, ]$num_of_crimes
    
    content <- as.character(tagList(
      # TODO: display rank as h4
      tags$h4(area),
      sprintf("Year: %s", input$year),
      tags$br(),
      sprintf("Number of Crimes: %s", num_of_crimes)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = area)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
}
