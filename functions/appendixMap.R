# map of apartments and catchments for working paper appendix (based on dashboard.Rmd plot)

appendixMap <- function(area,
                        catchment,
                        selected.year.display.apartments,
                        other.year.display.apartments) {

  m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    # other tiling options
    # addProviderTiles(providers$CartoDB.Positron) %>%
    # addProviderTiles(providers$CartoDB.Voyager) %>%
    
    # polygons - full weight
    # addPolygons(data = area, color = "black", weight = 3, opacity = 1,
    #             fillColor = "transparent") %>%
    # addPolygons(data = catchment, color = "#e7298a", weight = 2, opacity = 1,
    #             fillColor = "#e7298a", fillOpacity = 0.1) %>%
    
    # polygons - reduced weight for lower resolution
    addPolygons(data = area, color = "black", weight = 2, opacity = 1,
                fillColor = "transparent") %>%
    addPolygons(data = catchment, color = "#e7298a", weight = 1, opacity = 1,
                fillColor = "#e7298a", fillOpacity = 0.1) %>%
    
    addCircleMarkers(
      data = other.year.display.apartments,
      # radius = ~sqrt(hi_dens_dwel) / 2,  # for full size
      radius = ~sqrt(hi_dens_dwel) / 6,  # for lower resolution
      color = "#2c7bb6",
      # weight = 2,  # for full size
      weight = 1,  # for lower resolution
      opacity = 1,
      fillColor = "#2c7bb6",
      fillOpacity = 0.65) %>%
    addCircleMarkers(
      data = selected.year.display.apartments,
      # radius = ~sqrt(hi_dens_dwel) / 2,  # for full size
      radius = ~sqrt(hi_dens_dwel) / 6,  # for lower resolution
      color = "#0000ff",
      # weight = 2,  # for full size
      weight = 1,  # for lower resolution
      opacity = 1,
      fillColor = "#0000ff",
      fillOpacity = 0.65) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))

  return(m)

}
