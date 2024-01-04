# function for output map

# For Greater Melbourne and LGAs, 'area of analysis' is the GCCSA or LGA
# boundary, while 'walkable catchments' is the catchments of the PT stops
# within 800m of the apartments within the area of analysis (and so the 
# 'walkable catchments' can extend outside the 'area of analysis').  

# For corridors, the 'area of analysis' consists of walkable catchments, 
# truncated at the Melbourne LGA cordon - and so no 'walkable catchments' 
# are shown on the map.

output.map <- function(geog, data, map.zoom, selection.area, 
                       selected.catchments, apartments, geog.text) {
  
  if (geog %in% c("corridors_all_services", "corridors_own_services")) {
    ggplot() +
      annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
      geom_sf(data = selection.area, aes(color = "Area of analysis"), linewidth = 1, fill = NA) +
      geom_sf(data = st_filter(st_centroid(apartments),
                               selection.area,
                               predicate = st_intersects), aes(color = "Apartments"), linewidth = 1) +
      scale_color_manual(values = c("Area of analysis" = "red", 
                                    "Apartments" = "blue"), 
                         labels = c("Apartments", "Area of analysis")) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      guides(color = guide_legend(override.aes = list(
        linetype = c(0, 1),
        shape = c(16, NA)
      ))) +
      labs(title = geog.text,   
           color = NULL) 
    
  } else if (geog %in% c("corridors_all_services_with_apts",
                         "corridors_own_services_with_apts")) {
    ggplot() +
      annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
      geom_sf(data = selected.catchments, aes(color = "Area of analysis"), linewidth = 1, fill = NA) +
      geom_sf(data = st_filter(st_centroid(apartments),
                               selection.area,
                               predicate = st_intersects), aes(color = "Apartments"), linewidth = 1) +
      scale_color_manual(values = c("Area of analysis" = "red", 
                                    "Apartments" = "blue"), 
                         labels = c("Apartments", "Area of analysis")) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      guides(color = guide_legend(override.aes = list(
        linetype = c(0, 1),
        shape = c(16, NA)
      ))) +
      labs(title = geog.text,   
           color = NULL) 
    
  } else {
  ggplot() +
    annotation_map_tile(type = "osm", zoom = map.zoom, alpha = 0.8) +
    geom_sf(data = selection.area, aes(color = "Area of analysis"), linewidth = 1, fill = NA) +
    geom_sf(data = selected.catchments, aes(color = "Walkable catchments"), linewidth = 1, fill = NA) +
    geom_sf(data = st_filter(st_centroid(apartments),
                             selection.area,
                             predicate = st_intersects), aes(color = "Apartments"), linewidth = 1) +
    scale_color_manual(values = c("Area of analysis" = "black", 
                                  "Walkable catchments" = "red", 
                                  "Apartments" = "blue"), 
                       labels = c("Apartments", "Area of analysis", "Walkable catchments")) +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    guides(color = guide_legend(override.aes = list(
      linetype = c(0, 1, 1),
      shape = c(16, NA, NA)
    ))) +
    labs(title = geog.text,   
         color = NULL)
  }
}

