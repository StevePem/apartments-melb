# Former section 2.4 of 'apartments', which added stops and routes
# to completed apartments

# But was only based on current stops and routes

# Required update to find also stops used by former tram and bus routes,
# and not to locate routes, as they vary over time

# Also, decided to use just the walking distance version



# 2.4 Add stops and routes to completed projects ----
# -------------------------------------#
# Note - two versions provided below, for 800m 'crow flies' and 'walking' distance.  
# Select appropriate version below.  'Crowflies' is 800m euclidean distance from
# perimeter of feature; 'walking' is 800m walking distance from centroid.
# The walking distance version requires at least 6GB of memory.  Make sure as few
# R objects are loaded, and as few other programs running, as possible.
# If still fails due to memory shortage, may need to restructure code so that 'roads' and 'stops' aren't
# loaded at same time - that is, find stop no's first, and then remove 'roads' and find stops.

# select one - 
# version <- "crowflies"
version <- "walking"

completed.projects <- st_read("../GIS/completedProjects.sqlite") 


# re-load and clear file already populated with stops/routes
# completed.projects <- st_read("../GIS/completedProjects(crowflies).sqlite") %>%
#   dplyr::select(-c(rail_stn, tram_bus_stop, tram_routes, bus_routes))
#   

# test <- st_read("../GIS/completedProjects.sqlite") %>%
#   .[1:10,]
# test <- completed.projects[1:10,]

# 'roads' only required for version with walking distances
## note - for possible future enhancement, consider whether OSM would provide better walking routes than Vicmap Roads
if (version == "walking") {
  roads <- read_zipped_GIS("../Data/TR_ROAD.zip",
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMTRANS") %>%
    filter(!(CLASS_CODE %in% c(0, 13, 14))) %>% # 0-Freeway, 13-Paper Road, 14-Ferry Route - see http://services.land.vic.gov.au/catalogue/metadata?anzlicId=ANZVI0803002595&publicId=guest&extractionProviderId=1#tab2
    # filter to two fields (no attributes are actually needed at all, and better to reduce size;  
    # but 'points2network' Approach 2 only works if you have some attributes for which data is provided, 
    # and seems to require at least two, and it's easiest to pick numeric ones)
    dplyr::select(PFI, UFI)
}


# divide 'stops' into rail and tram/bus
# rail
rail.stops <- stops %>%
  # filter to rows containing 'Railway Station' and not '/' (used for bus or tram stops at stations) 
  filter(grepl("Railway Station", stop_name) & !grepl("/", stop_name)) %>%
  # replace the pattern 'space + Railway + any number of other characters' with nothing
  mutate(station_name = gsub(" Railway.*","", stop_name))

tram.bus.stops <- stops %>%
  filter(!(stop_id %in% rail.stops$stop_id))

for (i in 1:nrow(completed.projects)) {
  # for (i in 1:nrow(test)) {
  
  if (version == "crowflies") {
    #============version with crow-flies distances
    # extract the relevant project and buffer to 800m
    # (alternatively, could buffer centroid)
    project <- completed.projects[i,]
    project.buffer <- st_buffer(project, 800)
    
    # find stops that intersect the buffer - produces a list of intersecting rows;
    # extract the row numbers from the list with [[1]]; subset stops to the listed rows
    project.rail.stops <- rail.stops[st_intersects(project.buffer, rail.stops)[[1]], ]
    project.rail.names <- project.rail.stops$station_name %>% unique(.) %>% sort(.)
    
    project.tram.bus.stops <- tram.bus.stops[st_intersects(project.buffer,tram.bus.stops)[[1]], ]
    project.tram.bus.stop.nos <- project.tram.bus.stops$stop_id %>% unique(.) %>% sort(.)
    
    # note - could be more direct to do "rail.stops %>% filter(st_intersects(GEOMETRY, project.buffer, sparse = FALSE))"
    # but it took 50% longer
    #============ end version with crow-flies distances
    
  } else if (version == "walking") {
    #============version with walking distances
    project <- completed.projects[i,] %>%
      st_centroid(.)
    project.buffer <- st_buffer(project, BUFFDIST)
    
    # find roads which intersect buffer
    project.roads <- roads[st_intersects(project.buffer, roads)[[1]],] %>%
      # convert to spatial lines dataframe object (as required by shp2graph)
      as_Spatial()
    
    # checking the connectivity (using shp2graph)
    # conn <- nt.connect(project.roads)
    ## not totally happy about number of disconnected nodes, eg in project #1 - some of the missing ones are actually walkable
    ## another reason for possibly using OSM instead
    
    # find stops which intersect buffer
    candidate.stops <- stops[st_intersects(project.buffer, stops)[[1]], ]
    
    # find which candidate stops are within 800m walking distance
    if (nrow(candidate.stops) > 0) {
      project.stop.nos <- findWalkableStops(project.roads, project, candidate.stops, BUFFDIST)
    } else {
      project.stop.nos <- NULL
    }
    
    # rail stops are those which are in 'rail.stops'
    project.rail.stop.nos <- project.stop.nos[project.stop.nos %in% rail.stops$stop_id]
    
    # find names of the rail stops
    if (length(project.rail.stop.nos) > 0) {
      project.rail.names <- candidate.stops %>%
        filter(stop_id %in% project.rail.stop.nos) %>%
        # replace the pattern 'space + Railway + any number of other characters' with nothing
        mutate(station_name = gsub(" Railway.*","", stop_name)) %>%
        .$station_name %>%
        unique(.) %>%
        sort(.)
    } else {
      project.rail.names <- NULL
    }
    
    # tram/bus stops are all others (ie not in 'rail.stops')
    project.tram.bus.stop.nos <- project.stop.nos[!(project.stop.nos %in% project.rail.stop.nos)]
    #============ end version with walking distances
    
  }
  
  
  # vectors to hold lists of tram and bus routes 
  project.tram.routes <- c()
  project.bus.routes <- c()
  
  # for tram and bus stops, find the routes that use them
  if (length(project.tram.bus.stop.nos > 0)) {
    for (j in 1:length(project.tram.bus.stop.nos)) {
      stop <- project.tram.bus.stop.nos[j]
      
      # join to trips and routes ('routes' contains the 'route_short_name', ie route no.)
      stops.with.routes <- stop_times %>%
        filter(stop_id == stop) %>%
        left_join(., trips, by = "trip_id") %>%
        left_join(., routes, by = "route_id")
      
      # extract tram routes, and add to the project vector
      tram.routes <- stops.with.routes %>%
        filter(agency_id == "3") %>%  # 3 is tram
        .$route_short_name
      
      project.tram.routes <- c(project.tram.routes, tram.routes)
      
      # extract bus routes, and add to the project vector
      bus.routes <- stops.with.routes %>%
        filter(agency_id == "4") %>% # 4 is metropolitan bus
        .$route_short_name
      
      project.bus.routes <- c(project.bus.routes, bus.routes)
    }
  }
  
  # get unique tram/bus routes for the project, and sort
  project.tram.routes <- project.tram.routes %>%
    unique(.) %>%
    sort(.)
  
  project.bus.routes <- project.bus.routes %>%
    unique(.) %>%
    sort(.)
  
  # add the details (as comma-separated strings)
  completed.projects[i, "rail_stn"] <- toString(project.rail.names)
  completed.projects[i, "tram_bus_stop"] <- toString(project.tram.bus.stop.nos)
  completed.projects[i, "tram_routes"] <- toString(project.tram.routes)
  completed.projects[i, "bus_routes"] <- toString(project.bus.routes)
  # test[i, "rail_stn"] <- toString(project.rail.names)
  # test[i, "tram_bus_stop"] <- toString(project.tram.bus.stop.nos)
  # test[i, "tram_routes"] <- toString(project.tram.routes)
  # test[i, "bus_routes"] <- toString(project.bus.routes)
  
  # progress reporting
  if (i %% 50 == 0) {
    print(paste("Completed", i, "of", nrow(completed.projects), "projects at", Sys.time()))
  }
}
## About 6 hrs for crow-flies distance; 18 hrs for walking distance

# See note in 'findWalkableStops.R' regarding 'In showSRID(SRS_string, format = "PROJ"...' error message

# write output to file
if (version == "crowflies") {
  st_write(completed.projects, "../GIS/completedProjects_crowflies.sqlite", delete_layer = TRUE)
  write.csv(completed.projects %>% st_drop_geometry(), "../GIS/completedProjects_crowflies.csv", row.names = FALSE)
  
} else if (version == "walking") {
  st_write(completed.projects, "../GIS/completedProjects_walking.sqlite", delete_layer = TRUE)
  write.csv(completed.projects %>% st_drop_geometry(), "../GIS/completedProjects_walking.csv", row.names = FALSE)
  
}

