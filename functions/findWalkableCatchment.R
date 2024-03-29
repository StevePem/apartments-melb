# function to find walkable area (as a polygon) within a walkable distance
# ('WALKDIST' - say 800m) of a given  location

# uses similar techniques to 'findWalkableStops.R'


findWalkableCatchment <- function(location, roads, WALKDIST) {
  # location = stops %>% filter(stop_id == 20691)
  # roads = roads
  # WALKDIST = 800
  
  # note crs (only needed for optional ggplot display below - because crs is 
  # lost when 'nt.connect' is used in the alternative 'no selected stops' loop
  # below, and so 'roadnet.crs is written into the ggplot code) 
  road.crs <- st_crs(roads)
  
  # buffer location to WALKDIST
  location.centroid <- location %>%
    st_centroid(.)
  location.buffer <- st_buffer(location.centroid, WALKDIST)
  
  # filter roads to WALKDIST buffer around location
  location.roads <- roads[st_intersects(location.buffer, roads)[[1]],]
  
  # poor results may be returned if the location node snaps to a small disconnected
  # segment, so first create basic the location graph and find out whether the 
  # location node is in a cluster with fewer than 10 members that is not the largest

  # convert roads, plus point representing location, to edges.node network, with length (using shp2graph)
  # note that shp2graph has two main functions for converting spatial lines dataframe object to network:
  # 'nel2igraph', which converts the object to an igraph object, and 
  # 'points2network', which also lets you add extra points (and produces a group of lists)
  basic.location.net <- points2network(ntdata = location.roads %>% as_Spatial(),
                                pointsxy = st_coordinates(location.centroid),
                                # approach = 1, # 'mapping each point to the nearest node in the network/graph
                                approach = 2, # 'mapping each point to the nearest point in the network/graph and creating new node
                                ELComputed = TRUE,
                                ea.prop = rep(0, ncol(location.roads %>% as_Spatial())))  # attributes not used - but 'approach 2' requires them to be there
  
  # convert network to igraph
  basic.location.graph <- nel2igraph(nodelist = basic.location.net[[1]],
                              edgelist = basic.location.net[[2]],
                              weight = basic.location.net[[8]],  # 8 is the weights calculated by 'ELComputed'
                              Directed = FALSE)  # not required to be directed for walking
  
  # extract node number (which is in location.net[[3]]) for the location
  basic.location.node <- unlist(basic.location.net[[3]])[1]
  
  # poor results may be returned if the location node has snapped to a small
  # disconnected segment; if the location node is not in the largest cluster, 
  # and if the cluster that contains the location node has fewer than 10 members, 
  # then re-run using the largest cluster
  components <- clusters(basic.location.graph)
  largest.cluster.id <- which.max(components$csize)
  largest.cluster.members <- V(basic.location.graph)[components$membership == largest.cluster.id]
  location.node.cluster <- components$membership[basic.location.node]
  location.node.cluster.size <- components$csize[location.node.cluster]
  
  # 'needs largest' will be true if fewer than 10 and not largest cluster
  needs.largest <- !(basic.location.node %in% largest.cluster.members) & 
    location.node.cluster.size < 10
  
  # re-create network, densified
  location.roads.densified <- location.roads %>%
    # densify to 10m (so that the maximum walkable length of roads can be found)
    sf::st_segmentize(., dfMaxLength = 10) %>%
    nngeo::st_segments(.) %>%
    # convert to spatial lines dataframe object (as required by shp2graph)
    as_Spatial()
  
  # filter to largest connected component if needed
  if (needs.largest) {
    location.roads.densified <- nt.connect(location.roads.densified)
  }
  
  # repeat graph creation process on densified network
  location.net <- points2network(ntdata = location.roads.densified,
                                pointsxy = st_coordinates(location.centroid),
                                # approach = 1, # 'mapping each point to the nearest node in the network/graph
                                approach = 2, # 'mapping each point to the nearest point in the network/graph and creating new node
                                ELComputed = TRUE,
                                ea.prop = rep(0, ncol(location.roads.densified)))  # attributes not used - but 'approach 2' requires them to be there
  
  # convert network to igraph
  location.graph <- nel2igraph(nodelist = location.net[[1]],
                              edgelist = location.net[[2]],
                              weight = location.net[[8]],  # 8 is the weights calculated by 'ELComputed'
                              Directed = FALSE)  # not required to be directed for walking
  
  # extract node number (which is in location.net[[3]]) for the location
  location.node <- unlist(location.net[[3]])[1]
  
  # find all nodes within WALKDIST from the location.node
  walkable.nodes <- V(location.graph)[distances(graph = location.graph,
                                               v = as.character(location.node),
                                               mode = "out") <= WALKDIST]

  # filter the graph to the walkable nodes, and produce as data frame with coordinates
  walkable.points <- as_data_frame(location.graph, what = "vertices") %>%
    mutate(id = row_number()) %>%
    filter(id %in% walkable.nodes) %>%
    st_as_sf(., coords = c("x", "y"), crs = road.crs)
  
  # use convex hull for catchment 
  walkable.catchment <- st_convex_hull(st_union(walkable.points)) %>%
    # convert from sfc to sf object
    st_as_sf(.) %>%
    rename(geometry = x) %>%
    # and bind with original location attributes
    cbind(location %>% st_drop_geometry)
  
  # examine outputs
  # ggplot() +
  #   geom_sf(data = location.buffer, colour = "black") +
  #   geom_sf(data = walkable.catchment, colour = "red", linewidth = 2) +
  #   geom_sf(data = location.roads %>% st_as_sf %>% st_set_crs(road.crs)) +
  #   geom_sf(data = location, colour = "red", fill = "red", size = 4) +
  #   geom_sf(data = walkable.points, colour = "blue", size = 1)

  
  return(walkable.catchment)

 }

 