# Script to create corridors based on train and tram lines


# Required libraries are in Part B Option 3 of 'analysis.R', which invokes
# this script

# 1 Create corridors ----
# -----------------------------------------------------------------------------#

## 1.1 Catchments and cordon ----
## ------------------------------------#
# walkable 800m catchments
stop.catchments <- st_read("../GIS/stop catchments.sqlite")

# load 'cordon' (120m buffer around Melbourne LGA, being wide enough to cover 
# Richmond station)
cordon <-  read_zipped_GIS(zipfile = "../Data/LGA.zip",
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN") %>%
  filter(LGA_NAME == "MELBOURNE") %>%
  st_buffer(., 120)

# ggplot() + geom_sf(data = cordon)

## 1.2 Train corridors ----
## ------------------------------------#
# find all train stops (there will be some duplicates, because of VLine stations,
# with either identical or similar locations; but these can be ignored as they'll
# all be merged together anyway)
train.stops <- 
  # read in rail stops - based on current (Sep 2022) stations [so not 'Union']
  # see section 3 of 'train cleaning checks.R' for reconciliation of GTFS and DOT
  # station names, and steps required to align
  read_gtfs("../Data/gtfs_20220929.zip") %>%
  gtfs_as_sf(., crs = 4326) %>%
  .$stops %>%   
  st_transform(7899) %>%
  # filter to rows containing 'Railway Station' and not '/' (used for bus or tram stops at stations) 
  filter(grepl("Railway Station", stop_name) & !grepl("/", stop_name)) %>%
  # replace the pattern 'space + Railway + any number of other characters' with nothing
  mutate(station_name = gsub(" Railway.*","", stop_name)) %>%
  # fix name for Jolimont
  mutate(station_name = if_else(station_name == "Jolimont-MCG", 
                                "Jolimont", station_name)) %>%
  # join station/line details that were calculated using 'stationLineDetails' in
  # section 4 of 'trains.R' (to calculate station percentages) %>%
  left_join(., read.csv("../Tables/station percentages.csv") %>%
              dplyr::select(Station, Routes),
            by = c("station_name" = "Station"))

train.routes <- c("Alamein", "Belgrave", "Craigieburn", "Cranbourne", "Frankston",
                  "Glen Waverley", "Hurstbridge", "Lilydale", "Mernda", "Pakenham",
                  "Sandringham", "Stony Point", "Sunbury", "Upfield", "Werribee",
                  "Williamstown")

# empty dataframe to hold corridors
train.corridors <- data.frame()

for (i in 1:length(train.routes)) {
  # find the stops (as points) on the route that are outside the cordon
  corridor.stops <- train.stops %>%
    # confine to relevant routes (note 'Routes' for eg 'Belgrave' includes
    # 'Belgrave', 'Belgrave, Lilydale', 'Alamein, Belgrave, Lilydale', etc - 
    # this finds all of them)
    filter(grepl(train.routes[i], Routes)) %>%
    # select only if outside cordon
    filter(!st_intersects(., cordon, sparse = FALSE))
  
  # find the catchments for the stops
  corridor.catchment <- stop.catchments %>%
    filter(station_name %in% corridor.stops$station_name) %>%
    st_union() %>% 
    # exclude areas within cordon
    st_difference(., cordon) %>%
    st_as_sf(sf_column_name = "x") %>%
    mutate(corridor = train.routes[i],
           stations = paste(corridor.stops$station_name %>% 
                           unique() %>%
                           sort(), 
                         collapse = ", "),
           routes = NA) %>%
    rename(geometry = x)
  
  # add the corridor to the dataframe
  train.corridors <- rbind(train.corridors,
                           corridor.catchment)
}

# ggplot() +
#   annotation_map_tile(type = "osm", zoom = 13, alpha = 0.8) +
#   geom_sf(data = train.corridors[1,], colour = "red", linewidth = 2, fill = NA)


## 1.3 Tram corridors ----
## ------------------------------------#
# find list of current tram routes as at June 2022
tram.voltable.current.routes <- 
  # Service volumes supplied by DoT, 23 Jan 2023
  readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                    sheet = "YarraMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Tram)`) %>%
  # add date column (single so rows can be selected by > or <)
  mutate(date = (as.Date(paste0("01", Month, Year), format = "%d%B%Y"))) %>%
  # filter to June 2022
  filter(date == as.Date("2022-06-01")) %>%
  # select just the route numbers
  .$Route

# add 3a and 5s, as these are in the stoptable, and 30 (which is 'city shuttle'
# in the voltable) (produces 26 routes, including 3a, 5s and both 30 and 
# city shuttle, but not 35)
tram.routes.current <- c(tram.voltable.current.routes, "3a", "5s", "30") %>%
  sort()

# find current tram stops by route, filtered to current route (produces 25
# routes - same as 'tram.routes.current' but without city shuttle (which is 30))
tram.stoptable.current <- read.csv("../Tables/tram stops routes.csv") %>%
  # '1' means that the stop is current in Jun_2022 for the relevant route
  # (but not necessarily that trams actually run on that route currently)
  filter(Jun_2022 == 1) %>%
  dplyr::select(stop_id = stop, route) %>%
  # filter to current routes
  filter(route %in% tram.routes.current)

tram.stops <- st_read("../GIS/tram stop list.sqlite") %>%
  mutate(stop_id = as.numeric(stop_id)) %>%
  # add routes to the stops
  left_join(., tram.stoptable.current, by = "stop_id") %>%
  # omit where there is no route - these are the former route 58 (Domain Rd) 
  # stops, plus one in CBD
  filter(!is.na(route)) %>%
  # merge 3/3a and 5/5s (resulting in 23 routes)
  mutate(route = case_when(
    route == "3a" ~ "3",
    route == "5s" ~ "5",
    TRUE ~ route))

tram.routes <- tram.stops$route %>% unique() %>% sort()

# empty dataframe to hold corridors (there will be 23 corridors - all routes
# except 30, which is wholly within cordon)
tram.corridors <- data.frame()

for (i in 1:length(tram.routes)) {
  # find the stops (as points) on the route that are outside the cordon
  corridor.stops <- tram.stops %>%
    # confine to relevant routes
    filter(route == tram.routes[i]) %>%
    # select only if outside cordon
    filter(!st_intersects(., cordon, sparse = FALSE))
  
  # find the catchments for the stops (excluding any such as route 30
  # which have no stops)
  if (nrow(corridor.stops) > 0) {
    corridor.catchment <- stop.catchments %>%
      filter(stop_id %in% corridor.stops$stop_id) %>%
      st_union() %>% 
      # exclude areas within cordon
      st_difference(., cordon) %>%
      st_as_sf(sf_column_name = "x") %>%
      mutate(corridor = paste("Route", tram.routes[i]),
             stations = NA) %>%
      rename(geometry = x)
    
    # add the corridor to the dataframe
    tram.corridors <- rbind(tram.corridors,
                            corridor.catchment)
  }
}

# find those that are multipolygons
multipart.corridors <- tram.corridors %>%
  filter(st_geometry_type(.) == "MULTIPOLYGON")

# check multiparts
multipart.corridors$corridor
# [1] "Route 1"   "Route 109" "Route 12"  "Route 58"  "Route 6"   "Route 96" 

# make empty dataframe to hold replacement corridors
replacement.corridors <- data.frame()

# find replacement partial corridors for the multipart corridors
for (i in 1:nrow(multipart.corridors)) {
  corridor <- multipart.corridors[i,] %>%
    # split into single parts
    st_cast(., "POLYGON")
  
  # route 58 has a small parts to be disregarded near Domain interchange, 
  # just outside cordon
  if (multipart.corridors[i, "corridor"][[1]] == "Route 58") {
    corridor <- corridor %>%
      mutate(area = st_area(geometry)) %>%
      # omit smallest
      filter(area != min(area)) %>%
      # and drop area column
      dplyr::select(-area)
  }
  
  # update route number, based on location compared to central Melb
  # routes 1, 12, 58, 6 and 96 run largely north/south
  if (multipart.corridors[i, "corridor"][[1]] %in% c("Route 1", "Route 12", "Route 6",
                                                     "Route 58",  "Route 96")) {
    # update for north/south
    for (j in 1:nrow(corridor)) {
      if (st_coordinates(st_centroid(corridor[j, ]))[1, "Y"]  >
          st_coordinates(st_centroid(cordon))[1, "Y"]) {
        corridor[j, "corridor"][[1]] <- paste(corridor[j, "corridor"][[1]], "north")
      } else {
        corridor[j, "corridor"][[1]] <- paste(corridor[j, "corridor"][[1]], "south")
      }
    } 
  } else if (multipart.corridors[i, "corridor"][[1]] %in% c("Route 109")) {
    # update for east/west
    for (j in 1:nrow(corridor)) {
      if (st_coordinates(st_centroid(corridor[j, ]))[1, "X"]  >
          st_coordinates(st_centroid(cordon))[1, "X"]) {
        corridor[j, "corridor"][[1]] <- paste(corridor[j, "corridor"][[1]], "east")
      } else {
        corridor[j, "corridor"][[1]] <- paste(corridor[j, "corridor"][[1]], "west")
      }
    }
  }
  

  # add to replacement corridors
  replacement.corridors <- rbind(replacement.corridors, 
                                 corridor) 
}      

# remove multipart corridors from tram.corridors, and bind in the replacements
tram.corridors <- tram.corridors %>%
  filter(!(corridor %in% multipart.corridors$corridor)) %>%
  rbind(., replacement.corridors) %>%
  arrange(corridor)

# ggplot()  +
#   annotation_map_tile(type = "osm", zoom = 13, alpha = 0.8)+ 
#   geom_sf(data = cordon, colour = "blue", fill = NA) +
#   geom_sf(data = tram.corridors[28,], colour = "red", linewidth = 2, fill = NA)

# add routes, which are from the 'corridor' column plus manual adjustments
# for predecessor and associated (3a/5s) routes
tram.corridors <- tram.corridors %>%
  # main route number
  mutate(routes = gsub("\\D", "", corridor)) %>%
  # manual adjustments
  mutate(routes = case_when(
    corridor == "Route 11"       ~ paste(routes, "112", sep = ", "),
    corridor == "Route 12 north" ~ paste(routes, "112", sep = ", "),
    corridor == "Route 12 south" ~ paste(routes, "112", sep = ", "),
    corridor == "Route 16"       ~ paste(routes, "69", sep = ", "),
    corridor == "Route 3"        ~ paste(routes, "3a", sep = ", "),
    corridor == "Route 5"        ~ paste(routes, "5s", sep = ", "),
    corridor == "Route 58 north" ~ paste(routes, "55", "68", sep = ", "),
    corridor == "Route 58 south" ~ paste(routes, "8", sep = ", "),
    corridor == "Route 6 north"  ~ paste(routes, "8", "22", sep = ", "),
    corridor == "Route 78"       ~ paste(routes, "79", sep = ", "),
    TRUE                         ~ routes
  ))


## 1.4 Combine corridors ----
## ------------------------------------#
corridors <- rbind(
  train.corridors %>%
    mutate(type = "train"),
  tram.corridors %>% 
    mutate(type = "tram")
)

# reset the rownumbers (some tram numbers have become eg 1, 1.1 from st_cast)
rownames(corridors) <- NULL