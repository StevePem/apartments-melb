# script to generate outputs for working paper, similar to dashboard:
# plot, table and map

library(dplyr)
library(sf)
library(fs)
library(plotly)
library(orca)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(openxlsx)

dir_walk(path = "./functions/", source, recurse = T, type = "file")


# 1 Data ----
# -----------------------------------------------------------------------------#
# load data as for dashboard
all.data <- readRDS("./dashboard/data.rds")

areas <- readRDS("./dashboard/areas.rds")
walkable.catchments <- readRDS("./dashboard/walkable catchments.rds")
apartments <- readRDS("./dashboard/apartments.rds")

# table of types and locations
location.table <- all.data %>%
  dplyr::select(type, location) %>%
  distinct() %>%
  # Greater Melbourne at the top
  arrange(desc(type == "Greater Melbourne"))


# 2 Plot ----
# -----------------------------------------------------------------------------#
# setup for running orca
# First, need to install orca from https://github.com/plotly/orca/releases
# Right-clicking 'properties' on the desktop shorthub, find the path
# Set the path here in the environmental variables
Sys.setenv(PATH = paste0("C:/Users/steve/AppData/Local/Programs/orca/", ";", Sys.getenv("PATH")))

# ensure folders are ready
if(!dir.exists("../Appendix/plots/tt")) dir_create("../Appendix/plots/tt")
if(!dir.exists("../Appendix/plots/ttb")) dir_create("../Appendix/plots/ttb")

# conversion factor for px to cm (assumes device resolution of 96 PPI; vary if different
px_cm <- 37.8

# loop to process each location
for (i in 1:nrow(location.table)) {
# for (i in 1:40) {

  type_i <- location.table$type[i]
  location_i <- location.table$location[i]
  data <- all.data %>%
    filter(location == location_i)
  
  # table for train and tram, 2004 to 2022
  if (!type_i == "Bus") {
    data.tt <- data %>%
      # sum of train and tram, base and capadj
      mutate(service = Train + Tram,
             service.capadj = Train.capadj + Tram.capadj)
    
    # plot, and save [suppress warnings about orca deprecation for kaleido]
    plot.tt <- appendixPlot(data.tt)
    suppressWarnings(orca(plot.tt, 
                          paste0("../Appendix/plots/tt/", 
                                 type_i, "_", location_i, ".svg"),
                          width = 14.5 * px_cm, height = 14.5 * px_cm))
  }
 
  # data for train, tram and bus, 2016 to 2022
  data.ttb <- data %>%
    # from year 2016
    filter(Year >= 2016) %>%
    # sum of train, tram and bus, base and capadj
    mutate(service = Train + Tram + Bus,
           service.capadj = Train.capadj + Tram.capadj + Bus.capadj)
  
  # plot, and save [suppress warnings about orca deprecation for kaleido]
  plot.ttb <- appendixPlot(data.ttb)
  suppressWarnings(orca(plot.ttb, 
                        paste0("../Appendix/plots/ttb/", 
                               type_i, "_", location_i, ".svg"),
                        width = 14.5 * px_cm, height = 14.5 * px_cm))

}  


# 3 Table ----
# -----------------------------------------------------------------------------#
# empty dataframes to hold output
table.tt <- data.frame(matrix(ncol = 0, nrow = 4))
table.ttb <- data.frame(matrix(ncol = 0, nrow = 4))

# loop to process each location
for (i in 1:nrow(location.table)) {
  # for (i in 1:40) {
  
  type_i <- location.table$type[i]
  location_i <- location.table$location[i]
  data <- all.data %>%
    filter(location == location_i)
  
  # table for train and tram, 2004 to 2022
  if (!type_i == "Bus") {
    data.tt <- data %>%
      # sum of train and tram, base and capadj
      mutate(service = Train + Tram,
             service.capadj = Train.capadj + Tram.capadj)
    
    # output column, and add to table
    column.tt <- appendixTable(location_i, data.tt)
    table.tt <- bind_cols(table.tt, column.tt)
    
  }
  
  # table for train, tram and bus, 2016 to 2022
  data.ttb <- data %>%
    # from year 2016
    filter(Year >= 2016) %>%
    # sum of train, tram and bus, base and capadj
    mutate(service = Train + Tram + Bus,
           service.capadj = Train.capadj + Tram.capadj + Bus.capadj)
  
  # output column, and add to table
  column.ttb <- appendixTable(location_i, data.ttb)
  table.ttb <- cbind(table.ttb, column.ttb)
  
}

# convert NaN to dash, then save
table.tt[is.na(table.tt)] <- "-"
table.ttb[is.na(table.ttb)] <- "-"

# save outputs
write.xlsx(table.tt, "../Appendix/table_tt.xlsx", rowNames = TRUE)
write.xlsx(table.ttb, "../Appendix/table_ttb.xlsx", rowNames = TRUE)


# 4 Map ----
# -----------------------------------------------------------------------------#
# ensure folders are ready
if(!dir.exists("../Appendix/maps/tt")) dir_create("../Appendix/maps/tt")
if(!dir.exists("../Appendix/maps/ttb")) dir_create("../Appendix/maps/ttb")

# loop to process each location
for (i in 1:nrow(location.table)) {
# for (i in 1:20) {
  
  type_i <- location.table$type[i]
  location_i <- location.table$location[i]
  
  # get polygons to display for 'area' and 'catchment'
  area <- areas %>% filter(name == location_i)  # returns empty row for train, tram & bus
  catchment <- walkable.catchments %>% filter(name == location_i)
  
  # get apartments to display
  if (type_i %in% c("Greater Melbourne", "LGA")) {
    display.apartments <- apartments %>%
      st_filter(area, .predicate = st_intersects)
  } else {
    display.apartments <- apartments %>%
      st_filter(catchment, .predicate = st_intersects)
  }

  # map for use with train and tram, 2004 to 2022
  if (!type_i == "Bus") {
    map.tt <- appendixMap(area,
                          catchment,
                          selected.year.display.apartments = display.apartments,
                          other.year.display.apartments = display.apartments %>%
                            filter(year_comp < 2000))  # no 'other' year
    
    # save the map as html, then take a screenshot of it
    saveWidget(map.tt, file = "./temp.html")
    webshot("./temp.html", file = paste0("../Appendix/maps/tt/",
                                         type_i, "_", location_i, ".png"))
   }
  
  # map for use with train, tram and bus, 2016 to 2022
  # nb - difference from tt is that pre-2016 apartments are in different colour)
  map.ttb <- appendixMap(area,
                         catchment,
                         selected.year.display.apartments = display.apartments %>%
                           filter(year_comp >= 2016),
                         other.year.display.apartments = display.apartments %>%
                           filter(year_comp < 2016))  # to display in diff colour
  
  # save the map as html, then take a screenshot of it
  saveWidget(map.ttb, file = "./temp.html")
  webshot("./temp.html", file = paste0("../Appendix/maps/ttb/",
                                       type_i, "_", location_i, ".png"))

  }
  
# clean up temp file used while saving
file.remove("./temp.html")
