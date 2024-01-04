# Script to create corridors based on train and tram lines


# Required libraries are in Part B of 'analysis.R', where section 3 invokes
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
            by = c("station_name" = "Station")) %>%
  
  # convert route groups to corridor names (note that these names take account of 
  # the later removal of stops within cordon, eg 'Jewell to Upfield' rather than
  # 'Macaulay to Upfield')
  mutate(corridor = case_when(
    Routes == "Alamein"       ~ "Alamein (Riversdale to Alamein)",
    Routes == "Belgrave"      ~ "Belgrave (Heathmont to Belgrave)",
    Routes == "Craigieburn"   ~ "Craigieburn (Ascot Vale to Craigieburn)",
    Routes == "Cranbourne"    ~ "Cranbourne (Lynbrook to Cranbourne)",
    Routes == "Frankston"     ~ "Frankston (Glenhuntly to Frankston)",
    Routes == "Glen Waverley" ~ "Glen Waverley (Heyington to Glen Waverley)",
    Routes == "Hurstbridge"   ~ "Hurstbridge (Westgarth to Hurstbridge)",
    Routes == "Lilydale"      ~ "Lilydale (East Ringwood to Lilydale)",
    Routes == "Mernda"        ~ "Mernda (Rushall to Mernda)",
    Routes == "Pakenham"      ~ "Pakenham (Hallam to Pakenham)",
    Routes == "Sandringham"   ~ "Sandringham (Prahran to Sandringham)",
    Routes == "Stony Point"   ~ "Stony Point (Leawarra to Stony Point)",
    Routes == "Sunbury"       ~ "Sunbury (Middle Footscray to Sunbury)",
    Routes == "Upfield"       ~ "Upfield (Jewell to Upfield)",
    Routes == "Werribee"      ~ "Werribee (Seaholme to Werribee)",
    Routes == "Williamstown"  ~ "Williamstown (North Williamstown to Williamstown)",
    
    Routes == "Alamein, Belgrave, Craigieburn, Cranbourne, Frankston, Glen Waverley, Hurstbridge, Lilydale, Mernda, Pakenham, Sandringham, Sunbury, Upfield, Werribee, Williamstown"
    ~ "City",
    
    Routes == "Alamein, Belgrave, Cranbourne, Frankston, Glen Waverley, Lilydale, Pakenham, Sandringham"
    ~ "Richmond",
    
    Routes == "Alamein, Belgrave, Glen Waverley, Lilydale" 
    ~ "Burnley (East Richmond, Burnley)",
    
    Routes == "Alamein, Belgrave, Lilydale"
    ~ "Camberwell (Hawthorn to Camberwell)",
    
    Routes == "Belgrave, Lilydale"
    ~ "Ringwood (East Camberwell to Ringwood)",
    
    Routes == "Craigieburn, Sunbury, Upfield, Werribee, Williamstown"
    ~ "North Melbourne",
    
    Routes == "Cranbourne, Frankston, Pakenham" 
    ~ "Caulfield (Hawksburn to Caulfield)",
    
    Routes == "Cranbourne, Frankston, Pakenham, Sandringham" 
    ~ "South Yarra",
    
    Routes == "Cranbourne, Pakenham"
    ~ "Dandenong (Carnegie to Dandenong)",
    
    Routes == "Frankston, Stony Point"
    ~ "Frankston (Glenhuntly to Frankston)",  # Frankston is treated as Frankston line only
    
    Routes == "Hurstbridge, Mernda"
    ~ "Clifton Hill (Collingwood to Clifton Hill)",
    
    Routes == "Sunbury, Werribee, Williamstown"
    ~ "Footscray",
    
    Routes == "Werribee, Williamstown" 
    ~ "Newport (Seddon to Newport)"
  ))

train.routes <- train.stops$corridor %>% 
  unique() %>% 
  sort()

# empty dataframe to hold corridors
train.corridors <- data.frame()

for (i in 1:length(train.routes)) {
  # find the stops (as points) on the route that are outside the cordon
  corridor.stops <- train.stops %>%
    # confine to relevant corridor
    filter(corridor == train.routes[i]) %>%
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
           stops = paste(corridor.stops$station_name %>% 
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
             stops = NA) %>%
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


## 1.4 Bus corridors ----
## ------------------------------------#
# routes for which corridors are required 
# possible version for all apparent PPTN routes (not used)
# bus.routes <- c(901, 902, 903, 905, 906, 907, 908, 900, 703,  # SmartBus
#                 170, 180, 190, 495,  # Werribee
#                 411, 412, 472, 460, 420, 426, 216, 220, 410, 223, 406,
#                 402, 467, 506,  # Footscray, Sunshine, Moonee Ponds
#                 301, 250, 350, 386, 251, 548,  # La Trobe Uni & north
#                 293, 302, 304, 200, 270, 732,  # Eastern suburbs
#                 234, 246,  # Inner city
#                 630, 601, 822, 828, 791, 841) # South-east

bus.routes <- c(901, 902, 903, 905, 906, 907, 908, 900, 703)  # SmartBus

# set June 2022 (final analysis month) as the month to determine stops
# associated with routes, and get its daily route file names
route.stop.file.paths <- dir_ls("../Tables/bus daily stops and routes/") %>%
  as.character(.)

month.file.paths <- route.stop.file.paths[str_detect(route.stop.file.paths, "2022-06")]

# assemble a table of distinct stop/route combinations for the month
stop.routes <- c()

# create a temp directory (so that it can be cleared of the temp uncompressed
# files  - otherwise leads to crash) [not sure if this is needed]
temp_dir <- tempdir()

for (i in 1:length(month.file.paths)) {
  # read the day's details (distinct stops and routes only)
  daily.details <- read_csv(month.file.paths[i], show_col_types = FALSE) %>%
    distinct(stop_id, route_short_name)
  
  # add to stop.routes
  stop.routes <- rbind(stop.routes, daily.details)

}

# limit stop.routes to distinct combinations
stop.routes <- stop.routes %>%
  distinct()

# remove temporary files created for the month (to avoid crash) [not sure if needed]
unlink(list.files(temp_dir, full.names = T)) 

# read in bus stops (ignore warning 'NAs introduced by coercion - one
# of the stops has a non-numeric number that doesn't match anything in stop.routes)
bus.stops <- st_read("../GIS/bus stop list.sqlite") %>%
  mutate(stop_id = as.numeric(stop_id))  

# empty dataframe to hold corridors
bus.corridors <- data.frame()

# find the corridors for the routes
for (i in 1:length(bus.routes)) {
  # find stops that corresond to the route
  route.stops <- stop.routes %>%
    filter(route_short_name == bus.routes[i]) %>%
    .$stop_id
  
  # find the stops (as points) on the route that are outside the cordon
  corridor.stops <- bus.stops %>%
    # confine to relevant stops
    filter(stop_id %in% route.stops) %>%
    # select only if outside cordon
    filter(!st_intersects(., cordon, sparse = FALSE))
  
  # find the catchments for the stops
  if (nrow(corridor.stops) > 0) {
    corridor.catchment <- stop.catchments %>%
      filter(stop_id %in% corridor.stops$stop_id) %>%
      st_union() %>% 
      # exclude areas within cordon
      st_difference(., cordon) %>%
      st_as_sf(sf_column_name = "x") %>%
      mutate(corridor = paste("Route", bus.routes[i]),
             stops = paste(corridor.stops$stop_id %>% 
                                unique() %>%
                                sort(), 
                              collapse = ", "),
             routes = NA) %>%
      rename(geometry = x)
    
    # for DART routes, omit the small disconnected section at the Hoddle St end
    if (bus.routes[i] %in% c(905, 906, 907, 908)) {
      # find new geometry which excludes the disconnected section
      corridor.catchment.geom <- corridor.catchment %>%
        dplyr::select(geometry) %>%
        # split into single parts
        st_cast(., "POLYGON") %>%
        # omit the western-most part: first find the centroid x-coordinate
        rowwise() %>%
        mutate(x = st_coordinates(st_centroid(geometry))[1]) %>%
        ungroup() %>%
        # omit lowest x (which is western-most)
        filter(x != min(x)) %>%
        # and unify
        st_union() %>%  
        st_as_sf() %>%  # note the geometry column is now called 'x' (diff from x coordinate above)
        rename(geometry = x)
      
      # set the new geometry as the geometry of the corridor
      corridor.catchment <- corridor.catchment %>%
        st_set_geometry(., corridor.catchment.geom$geometry)
    }

    # add the corridor to the dataframe
    bus.corridors <- rbind(bus.corridors,
                            corridor.catchment)
  }
}

# ggplot()  +
#   annotation_map_tile(type = "osm", zoom = 13, alpha = 0.8)+
#   geom_sf(data = cordon, colour = "blue", fill = NA) +
#   geom_sf(data = bus.corridors[9, ], colour = "red", linewidth = 2, fill = NA)


## 1.5 Combine corridors ----
## ------------------------------------#
corridors <- rbind(
  train.corridors %>%
    mutate(type = "train"),
  tram.corridors %>% 
    mutate(type = "tram"),
  bus.corridors %>%
    mutate(type = "bus")
)

# reset the rownumbers (some tram numbers have become eg 1, 1.1 from st_cast)
rownames(corridors) <- NULL
