# an attempt at an expanded version of 'appendixMap.R', which tries to deal with
# the fact that leaflet, when used with html, wants to print its outputs in 4:3
# aspect ratio

# The script works by padding the bounding box of the data by 5% at the top and
# 30% at the sides, allowing for 25% to be trimmed in the move from 4:3 to 1:1 
# aspect ratio.  Then, the map is saved as an .html, read back as a .png, and
# cropped to a new smaller .png.

# It all falls down with the scale bar and attribution.  They get cropped as 
# well.  You can't set them at 'bottom centre', they have to be left or right.
# ChatGPT tried some options that involved re-writing lines of HTML, but
# they didn't work.


appendixMap <- function(area,
                        catchment,
                        selected.year.display.apartments,
                        other.year.display.apartments) {
  
  # Calculate the bounding box
  if (nrow(area) > 0 & nrow(catchment) > 0) {
    bbox <- sf::st_bbox(st_union(area, catchment))
  } else if (nrow(area) > 0) {
    bbox <- sf::st_bbox(area) 
  } else {
    bbox <- sf::st_bbox(catchment)
  }
  
  # Calculate the center of the bounding box
  center_lon <- (bbox$xmin + bbox$xmax) / 2
  center_lat <- (bbox$ymin + bbox$ymax) / 2
  
  # Calculate the width and height of the bounding box in degrees
  width_deg <- abs(bbox$xmax - bbox$xmin)
  height_deg <- abs(bbox$ymax - bbox$ymin)
  
  # Find the larger dimension
  larger_dimension <- max(width_deg, height_deg)
  
  # Calculate the padding for width and height
  height_padding <- 0.05 * larger_dimension  # 5% of the larger dimension
  width_padding <- 0.3 * larger_dimension   # 30% of the larger dimension (because about 25% will be cropped)
  
  # Add the padding to the height and width
  width_deg <- width_deg + width_padding
  height_deg <- height_deg + height_padding
  
  # Calculate the new bounding box with equal dimensions
  new_bbox <- list(
    west = center_lon - width_deg / 2,
    east = center_lon + width_deg / 2,
    south = center_lat - height_deg / 2,
    north = center_lat + height_deg / 2
  )
  
  # Create the map
  # Create the map
  m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    fitBounds(
      lng1 = unname(new_bbox$west), lat1 = unname(new_bbox$south),
      lng2 = unname(new_bbox$east), lat2 = unname(new_bbox$north)) %>%
    addPolygons(data = area, color = "black", weight = 3, opacity = 1,
                fillColor = "transparent") %>%
    addPolygons(data = catchment, color = "#e7298a", weight = 2, opacity = 1,
                fillColor = "#e7298a", fillOpacity = 0.1) %>%
    addCircleMarkers(
      data = other.year.display.apartments,
      radius = ~sqrt(hi_dens_dwel) / 2,
      color = "#2c7bb6",
      weight = 2,
      opacity = 1,
      fillColor = "#2c7bb6",
      fillOpacity = 0.65) %>%
    addCircleMarkers(
      data = selected.year.display.apartments,
      radius = ~sqrt(hi_dens_dwel) / 2,
      color = "#0000ff",
      weight = 2,
      opacity = 1,
      fillColor = "#0000ff",
      fillOpacity = 0.65) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
  
  # Save the map as an HTML file
  saveWidget(m, file = "./temp.html")
  
  # The image gets saved in 4:3 aspect ratio, and we want it to be a square,
  # so it needs to be cropped (will lose 25% at the sides)
  
  # Read the HTML file
  html_content <- readLines("./temp.html")
  
  # Find the line containing the scale bar CSS and replace it with the new CSS for bottom center positioning
  for (i in seq_along(html_content)) {
    if (grepl("<style>", html_content[i])) {
      html_content[i + 1] <- gsub("bottom: 0px;", "bottom: 0px; left: 50%; transform: translateX(-50%);", html_content[i + 1])
      break
    }
  }
  
  # Write the modified HTML content back to the file
  writeLines(html_content, "./temp.html")
  
  # Capture the HTML file as a PNG with larger dimensions
  webshot("./temp.html", file = "temp_large.png")
  
  # Read the captured PNG image
  img <- readPNG("temp_large.png")
  
  # Calculate the coordinates for cropping to a square aspect ratio
  crop_dim <- min(dim(img)[1], dim(img)[2])
  crop_x <- (dim(img)[1] - crop_dim) / 2
  crop_y <- (dim(img)[2] - crop_dim) / 2
  
  # Crop the image to a square aspect ratio
  img_cropped <- img[crop_x:(crop_x + crop_dim), crop_y:(crop_y + crop_dim),]
  
  return(img_cropped)
}

# Then, once the img_cropped is returned back to the main script, write it to file:
# library(png)
# writePNG(map.tt, paste0("../Appendix/maps/tt/", type_i, "_", location_i, ".png"))
