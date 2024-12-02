# function, based on the AToM 'NetworkGenerator.R' from https://github.com/matsim-melbourne/network/
# (dev branch as at 23-Nov-24), to create small networks for clusters of apartments
# where trip generation surveys have been carried out

# Preliminary steps: 
# 1 Copy the 'functions' folder from https://github.com/matsim-melbourne/network/
#   into the working directory, and re-name it as 'functions AToM'
# 2 In 'densifyNetwork.R', change line 33 so that both cycling and walking
#   paths will be densified as a result of 'densifyBikeways':
#     filter(length > minimum_length & (is_cycle == 1 | is_walk == 1))
# 3 In 'getOsmExtract.R', change line 56 so that the only layers extracted
#   are lines and points (speeds up processing, as the others are not used):
#     layers <- c("lines", "points")

# Other notes:
# 1 Note that 'removeDangles' and 'makeEdgesOneway' are commented out


makeNetwork<-function(city){

  # Parameters --------------------------------------------------------------
  
  # CITY PARAMETERS
  # City locations and data
  tripgenDataLocation <- "../Tables/trip generation locations.csv"
  data <- read.csv(tripgenDataLocation)
  cities <- unique(data$Group)
  if (!city %in% cities) {
    print(paste0("Not configured for city '", city, "'; exiting"))
    return()
  } 
  
  # OSM extract location (already downloaded)
  if (city %in% c("Ballina", "Bowral", "Central Coast 1", "Central Coast 2",
                  "Central Coast 3", "Coffs Harbour", "Gold Coast 1", "Gold Coast 2",
                  "Lismore", "Melbourne", "Murwillimbah", "Newcastle", "Orange",
                  "Port Macquarie", "Sydney 1", "Sydney 2", "Sydney 3", "Sydney 4",
                  "Sydney 5", "Tahmoor", "Tweed Heads", "Wagga", "Wollongong" )) {
    fullExtractLocation <- "../trip generation/data/geofabrik_australia-latest.osm.pbf"
  } else if (city %in% c("Auckland 1", "Auckland 2", "Auckland 3", "Christchurch 1",
                         "Christchurch 2", "Dunedin", "Palmerston North")) {
    fullExtractLocation <- "../trip generation/data/new-zealand-latest.osm.pbf"
  } else {
   print(paste0("OSM extract for city '", city, "' has not been set; exiting"))
    return()
  }
  
  # Processing locations
  outputDirectory <- "../trip generation/networks/"  # location where processed network will be located
  if (!dir.exists(outputDirectory)) dir.create(outputDirectory)
  region <- paste0(outputDirectory, "region.sqlite")  # intermediate file storage location
  osmGpkg <- paste0(outputDirectory, "osm.gpkg")  # intermediate file storage location
  
  # CRS: desired coordinate system for output network
  if (city %in% c("Melbourne", "Orange", "Wagga")) {
    outputCrs <- 7855
  } else if (city %in% c("Ballina", "Bowral", "Central Coast 1", "Central Coast 2",
                         "Central Coast 3", "Coffs Harbour", "Gold Coast 1", "Gold Coast 2",
                         "Lismore", "Murwillimbah", "Newcastle", 
                         "Port Macquarie", "Sydney 1", "Sydney 2", "Sydney 3", "Sydney 4",
                         "Sydney 5", "Tahmoor", "Tweed Heads", "Wollongong" )) {
    outputCrs <- 7856
  } else if (city %in% c("Christchurch 1", "Christchurch 2", "Dunedin")) {
    outputCrs <- 2134
  } else if (city %in% c("Auckland 1", "Auckland 2", "Auckland 3", "Palmerston North")) {
    outputCrs <- 2135
  } else {
    print(paste0("CRS for city '", city, "' has not been set; exiting"))
    return()
  }
  
  # Distance to buffer apartments when creating region and its network
  apartmentBufferDist <- 5000  # 5 km
  
  # Unused parameters
  unconfiguredSqlite <- ""  # intermediate file storage location
  cropAreaPoly = ""  # for cropping to test area
  demFile = ""   # for adding elevation
  ndviFile = ""  # for adding NDVI 
  gtfs_feed = ""  # for adding public transport links or destinations
  outputDir <- ""  # output for unused writing options
  
  # REGION BUFFER DISTANCE
  # Distance to buffer region when getting osm extract, destinations or gtfs routes
  regionBufferDist=0
  
  # EXTRACT OSM for REGION
  # A flag for whether to make an OSM extract for the region, either by downloading
  # an extract in .osm.pbf format and clipping it to the region, or by clipping
  # an existing extract in .osm.pbf format (if not, and if network needs to be 
  # processed, then must already have osmGpkg file)
  extractOsm=T
  useFullExtractHeld=T  # Whether to use an existing OSM extract, instead of downloading
  retainDownload=T  # Whether to retain downloaded file after region extracted
  
  # NETWORK FROM OSM 
  # A flag for whether to build unconfigured network from osm extract (if not,
  # must already have unconfigured sqlite)
  networkFromOsm=T
  saveUnconfigured=F

  # SIMPLIFICATION
  shortLinkLength=20
  minDangleLinkLengh=500
  crop2Area=F

  # DENSIFICATION
  densificationMaxLength=50   # rather than default of 500m
  densifyBikeways=T  # see note above - change to 'densifyNetwork.R' means this densifies walkways too

  # CAPACITY ADJUSTMENT
  # A flag for whether to multiply capacity of links shorter than 100m by 2 or not
  # In some cases such as when building network for simulation of small samples (e.g. <1%) it might be desired
  adjustCapacity=F

  # ELEVATION
  # A flag for whether to add elevation or not
  addElevation=F
  ElevationMultiplier=1
  
  # DESTINATIONS
  # A flag for whether to add a destinations layer (drawn from OSM, and GTFS for PT) or not
  addDestinationLayer=F

  # NDVI
  # A flag for whether to add NDVI or not
  addNDVI=F
  # Buffer distance for finding average NDVI for links
  ndviBuffDist=30

  # GTFS
  # A flag for whether to add a network based on GTFS or not
  addGtfs=F
  # Select an analysis date, eg a midweek day that's not a public or school holiday
  analysis_date=as.Date("2023-11-15","%Y-%m-%d")
  onroadBus=T  # whether to route buses on roads (rather than create separate pseudo links)

  # # Outputs
  # # outputSubdirectory=format(Sys.time(),"%d%b%y_%H%M") # date_hour, eg. "17Aug21_1308"
  # if(exists("outputSubdirectory")){
  #   outputSubdirectory=outputSubdirectory
  # } else {outputSubdirectory="generated_network"}
  writeXml=F
  writeShp=F
  writeSqlite=F 

  # Packages ----------------------------------------------------------------

  library(sf)
  library(fs)
  library(tidyverse)
  library(data.table)
  library(igraph)
  library(terra)
  library(lwgeom)
  library(tidytransit)
  library(hablar)
  library(hms)
  library(osmextract)
  library(doSNOW)
  library(parallel)
  library(foreach)
  library(nngeo)
  library(igraph)

  # # Building the output folder structure ------------------------------------
  # outputDir <- paste0("output/",outputSubdirectory)
  # if(outputSubdirectory != "" & dir.exists(outputDir)) dir_delete(outputDir)
  # dir_create(paste0('./',outputDir))
  # sink(paste0('./',outputDir,'/makeMatsimNetwork.log'), append=FALSE, split=TRUE)
  # # if (addGtfs) dir_create(paste0(outputDir,"/gtfs"))

  #  Functions --------------------------------------------------------------

  dir_walk(path="./functions AToM/",source, recurse=T, type = "file")

  # Network processing-------------------------------------------------------
  echo("========================================================\n")
  echo("                **Network Generation Setting**          \n")
  echo("--------------------------------------------------------\n")
  echo(paste0("- Getting  OSM extract:                           ", extractOsm,"\n"))
  echo(paste0("- Processing the OSM extract:                     ", networkFromOsm,"\n"))
  echo(paste0("- Cropping to a test area:                        ", crop2Area,"\n"))
  echo(paste0("- Shortest link length in network simplification: ", shortLinkLength,"\n"))
  echo(paste0("- Adding elevation:                               ", addElevation,"\n"))
  echo(paste0("- Adding destination layer:                       ", addDestinationLayer,"\n"))
  echo(paste0("- Adding NDVI:                                    ", addNDVI,"\n"))
  echo(paste0("- Adding PT from GTFS:                            ", addGtfs,"\n"))
  echo(paste0("- Writing outputs in SQLite format:               ", writeSqlite,"\n"))
  echo(paste0("- Writing outputs in ShapeFile format:            ", writeShp,"\n"))
  echo(paste0("- Writing outputs in MATSim XML format:           ", writeXml,"\n"))
  echo("========================================================\n")
  echo("                **Launching Network Generation**        \n")
  echo("--------------------------------------------------------\n")
  
  # Creating the region (apartments for the city, buffered)
  buffered.apartments <- data %>%
    filter(Group == city) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), 
             crs = 4326) %>%
    st_transform(outputCrs) %>%
    st_buffer(apartmentBufferDist) %>%
    dplyr::select() %>%
    summarise()
  st_write(buffered.apartments, region)
  
  # Extracting OSM
  if (extractOsm) {
    echo(paste0("Extracting OSM for ", city, "\n"))
    getOsmExtract(region, outputCrs, regionBufferDist, osmGpkg, retainDownload,
                  useFullExtractHeld, fullExtractLocation)
  }
  
  # Processing OSM, or loading existing layers if not required
  if(networkFromOsm) {
    echo(paste0("Starting to process osm extract file, ", osmGpkg, "\n"))
    networkUnconfiguredOutputs <- processOsm(osmGpkg, outputCrs)
    
    if (saveUnconfigured) {
      if (file_exists(unconfiguredSqlite)) st_delete(unconfiguredSqlite)
      st_write(networkUnconfiguredOutputs[[1]], unconfiguredSqlite, layer = "nodes")
      st_write(networkUnconfiguredOutputs[[2]], unconfiguredSqlite, layer = "edges")
      st_write(networkUnconfiguredOutputs[[3]], unconfiguredSqlite, layer = "osm_metadata")
    }
    
    networkUnconfigured <- list(networkUnconfiguredOutputs[[1]],
                                networkUnconfiguredOutputs[[2]])
    osm_metadata <- networkUnconfiguredOutputs[[3]]

  } else {
    
    if (file_exists(unconfiguredSqlite)) {
      echo(paste("Reading in existing unconfigured network,", unconfiguredSqlite, "\n"))
      networkUnconfigured <- 
        list(st_read(unconfiguredSqlite, layer = "nodes") %>% st_set_geometry("geom"),
             st_read(unconfiguredSqlite, layer = "edges") %>% st_set_geometry("geom"))
      if (st_crs(networkUnconfigured[[1]])$epsg != outputCrs) {
        networkUnconfigured[[1]] <- st_transform(networkUnconfigured[[1]], outputCrs)
      }
      if(st_crs(networkUnconfigured[[2]])$epsg != outputCrs) {
        networkUnconfigured[[2]] <- st_transform(networkUnconfigured[[2]], outputCrs)
      }
      osm_metadata <- st_read(unconfiguredSqlite, layer = "osm_metadata") %>%
        filter(osm_id %in% networkUnconfigured[[2]]$osm_id)
      
    } else {
      echo(paste("Unconfigured network file", unconfiguredSqlite, "not found; unable to proceed\n"))
      return()
    }
  }
  
  # crop to test area if required
  if(crop2Area)system.time(networkUnconfigured <- crop2Poly(networkUnconfigured,
                                                            cropAreaPoly,
                                                            outputCrs))
  # process OSM metadata
  echo("processing OSM meta data\n")
  echo("Building default OSM attribute tables\n")
  defaults_df <- buildDefaultsDF()
  highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
  echo("Processing OSM tags and joining with defaults\n")
  system.time( osmAttributes <- processOsmTags(osm_metadata,defaults_df))
  
  edgesAttributed <- networkUnconfigured[[2]] %>%
    inner_join(osmAttributes, by="osm_id") %>%
    dplyr::select(-highway, highway_order)
  
  # keep only the largest connected component
  largestComponent <- largestConnectedComponent(networkUnconfigured[[1]], edgesAttributed)
  
  # simplify intersections while preserving attributes and original geometry.
  system.time(intersectionsSimplified <- simplifyIntersections(largestComponent[[1]],
                                                               largestComponent[[2]],
                                                               shortLinkLength,
                                                               outputCrs))
  
  # Merge edges going between the same two nodes, picking the shortest geometry.
  # * One-way edges going in the same direction will be merged
  # * Pairs of one-way edges in opposite directions will be merged into a two-way edge.
  # * Two-way edges will be merged regardless of direction.
  # * One-way edges will NOT be merged with two-way edges.
  # * Non-car edges do NOT count towards the merged lane count (permlanes)
  system.time(edgesCombined <- combineRedundantEdges(intersectionsSimplified[[1]],
                                                     intersectionsSimplified[[2]],
                                                     outputCrs))
  
  # Merge one-way and two-way edges going between the same two nodes. In these 
  # cases, the merged attributes will be two-way.
  # This guarantees that there will only be a single edge between any two nodes.
  system.time(combinedUndirectedAndDirected <- 
                combineUndirectedAndDirectedEdges(edgesCombined[[1]],
                                                  edgesCombined[[2]],
                                                  outputCrs))
  
  # If there is a chain of edges between intersections, merge them together
  system.time(edgesSimplified <- simplifyLines(combinedUndirectedAndDirected[[1]],
                                               combinedUndirectedAndDirected[[2]]))
  
  # Remove dangles
  # system.time(noDangles <- removeDangles(edgesSimplified[[1]],edgesSimplified[[2]],
  #                                        minDangleLinkLengh))
  noDangles <- edgesSimplified  # used instead of removing dangles
  
  # Do a second round of simplification. # but not for Christchurch 2, where it's failing
  if (city == "Christchurch 2") {
    edgesCombined3 <- edgesSimplified
  } else {
    system.time(edgesCombined2 <- combineRedundantEdges(noDangles[[1]],
                                                        noDangles[[2]],
                                                        outputCrs))
    system.time(combinedUndirectedAndDirected2 <- 
                  combineUndirectedAndDirectedEdges(edgesCombined2[[1]],
                                                    edgesCombined2[[2]],
                                                    outputCrs))
    
    system.time(edgesSimplified2 <- simplifyLines(combinedUndirectedAndDirected2[[1]],
                                                  combinedUndirectedAndDirected2[[2]]))
    system.time(edgesCombined3 <- combineRedundantEdges(edgesSimplified2[[1]],
                                                        edgesSimplified2[[2]],
                                                        outputCrs))
  }
  
  networkMode <- addMode(edgesCombined3)
  
  # ensure transport is a directed routeable graph for each mode (i.e., connected
  # subgraph). The first function ensures a connected directed subgraph and the
  # second function ensures a connected subgraph but doesn't consider directionality.
  # We car and bike modes are directed, but walk is undirected.
  networkNonDisconnected <- largestDirectedNetworkSubgraph(networkMode,'car,bike')
  networkConnected <- largestNetworkSubgraph(networkNonDisconnected,'walk')
  
  # densify the network so that no residential streets are longer than 500m
  if (addElevation==T & densifyBikeways==F) message("Consider changing densifyBikeways to true when addElevation is true to get a more accurate slope esimation for bikeways")
  networkDensified <- densifyNetwork(networkConnected,densificationMaxLength,
                                     densifyBikeways)
  
  # Adding NDVI to links
  if(addNDVI) {
    system.time(networkDensified[[2]] <- addNDVI2Links(networkDensified[[2]],
                                                       ndviFile,
                                                       ndviBuffDist,
                                                       outputCrs))
  }
  
  # adding destinations layer
  if (addDestinationLayer) {
    destinations <- addDestinations(networkDensified[[1]],
                                    networkDensified[[2]],
                                    osmGpkg,
                                    city,
                                    gtfs_feed,
                                    outputCrs,
                                    region,
                                    regionBufferDist)
  }

  # simplify geometry so all edges are straight lines
  system.time(networkDirect <-
                makeEdgesDirect(networkDensified[[1]],
                                networkDensified[[2]],
                                outputCrs))
  
  # add mode to edges, add type to nodes, change cycleway from numbers to text
  networkRestructured <- restructureData(networkDirect, highway_lookup,
                                         defaults_df)
  
  # Doubling capacity for small road segments to avoid bottlenecks
  # Set adjustCapacity to True if this adjustment is desired
  if(adjustCapacity) {
    networkRestructured[[2]] <- networkRestructured[[2]] %>% 
      mutate(capacity = ifelse(length<100 , capacity*2, capacity))
  }
  
  # Adding elevation to nodes and gradient to links
  if(addElevation){ 
    networkRestructured[[1]] <- addElevation2Nodes(networkRestructured[[1]], 
                                                   demFile,
                                                   ElevationMultiplier,
                                                   outputCrs)
    networkRestructured[[2]] <- addElevation2Links(networkRestructured)
  }
  
  # Make network oneway (required because cycling impedances such as level of 
  # traffic stress and slope may be different in each direction)
  # echo("Making all links one way\n")
  # networkOneway <- makeEdgesOneway(networkRestructured[[1]], 
  #                                  networkRestructured[[2]],
  #                                  defaults_df)
  networkOneway <- networkRestructured  # used instead of making edges oneway
  
  # Adding PT pseudo-network based on GTFS
  # Adjust your analysis date and gtfs feed name above
  if (addGtfs) {
    # Adjust these parameters based on your GTFS file
    if (file.exists(region)) {
      # read in the study region boundary 
      echo("Using Region file for GTFS processing\n")
      region.poly <- st_read(region)
      if (st_crs(region.poly)$epsg != outputCrs) {
        region.poly <- st_transform(region.poly, outputCrs)
      }
      studyRegion <- st_buffer(region.poly, regionBufferDist)  %>%
        st_snap_to_grid(1)
    } else {
      echo("Region file was not found, skipping\n")
      studyRegion = NA
    }
    system.time(
      networkOneway[[2]] <- addGtfsLinks(outputLocation = paste0(outputDir,"/pt/"),
                                         nodes = networkOneway[[1]], 
                                         links = networkOneway[[2]],
                                         gtfs_feed,
                                         analysis_date,
                                         studyRegion,
                                         outputCrs,
                                         onroadBus,
                                         city)) 
  }
  
  networkFinal <- networkOneway
  
  if (addDestinationLayer) {
    networkFinal[[3]] <- destinations
  }
  
  # writing outputs
  echo("========================================================\n")
  echo("|               **Launching Output Writing**           |\n")
  echo("--------------------------------------------------------\n")
  
  if(writeSqlite) system.time(exportSQlite(networkFinal, outputDir, outputCrs))
  if(writeShp) system.time(exportShp(networkFinal, outputDir, outputCrs))
  if(writeXml) system.time(exportXML(networkFinal, outputDir)) 
  
  # writing to file named for city
  st_write(networkFinal[[1]], paste0(outputDirectory, city, ".sqlite"), 
           layer = "nodes", delete_layer = T)
  st_write(networkFinal[[2]], paste0(outputDirectory, city, ".sqlite"), 
           layer = "links", delete_layer = T)
  
  # clean up
  unlink(osmGpkg)
  unlink(region)
  
  # # end logging
  # sink()
}

# making all cities in a loop
cities <- unique(read.csv("../Tables/trip generation locations.csv")$Group)
for (i in seq_along(cities)) {
  makeNetwork(city = cities[i])
}

# making cities individually
makeNetwork(city = "Auckland 1")
makeNetwork(city = "Auckland 2")
makeNetwork(city = "Auckland 3")
makeNetwork(city = "Ballina")
makeNetwork(city = "Bowral")
makeNetwork(city = "Central Coast 1")
makeNetwork(city = "Central Coast 2")
makeNetwork(city = "Central Coast 3")
makeNetwork(city = "Christchurch 1")
makeNetwork(city = "Christchurch 2")
makeNetwork(city = "Coffs Harbour")
makeNetwork(city = "Dunedin")
makeNetwork(city = "Gold Coast 1")
makeNetwork(city = "Gold Coast 2")
makeNetwork(city = "Lismore")
makeNetwork(city = "Melbourne")
makeNetwork(city = "Murwillimbah")
makeNetwork(city = "Newcastle")
makeNetwork(city = "Orange")
makeNetwork(city = "Palmerston North")
makeNetwork(city = "Port Macquarie")
makeNetwork(city = "Sydney 1")
makeNetwork(city = "Sydney 2")
makeNetwork(city = "Sydney 3")
makeNetwork(city = "Sydney 4")
makeNetwork(city = "Sydney 5")
makeNetwork(city = "Tahmoor")
makeNetwork(city = "Tweed Heads")
makeNetwork(city = "Wagga")
makeNetwork(city = "Wollongong")
