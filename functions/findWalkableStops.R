# function to find train/tram/bus stops within a walkable distance
# ('BUFFDIST' - say 800m) of a given project location

## testing
# roadnet = project.roads
# BUFFDIST = WALKDIST


# Note re error messages "In showSRID(SRS_string, format = "PROJ", multiline = "NO",  ... :
# Discarded datum Geocentric Datum of Australia 2020 in Proj4 definition" - see
# https://psfaculty.plantsciences.ucdavis.edu/plant/additionaltopics_datumwarning.pdf and
# https://stackoverflow.com/questions/63727886/proj4-to-proj6-upgrade-and-discarded-datum-warnings
# Don't appear particularly problematic - but first reference suggests they could be avoided
# if "We simply assign the UTM projection to the sf object before converting it to an sp object"
# (sp is effectively used in shp2graph, but I thought this had already been done)


findWalkableStops <- function(roadnet, project, candidate.stops,
                              BUFFDIST) {
  
  # note crs (only needed for optional ggplot display below - because crs is 
  # lost when 'nt.connect' is used in the alternative 'no selected stops' loop
  # below, and so 'roadnet.crs is written into the ggplot code) 
  roadnet.crs <- st_crs(roadnet)
  
  # convert roads, plus points representing project and stops, to edges.node network, with length (using shp2graph)
  # note that shp2gaph has two main functions for converting spatial lines dataframe object to network:
  # 'nel2igraph', which converts the object to an igraph object, and 
  # 'points2network', which also lets you add extra points (and produces a group of lists)
  project.net <- points2network(ntdata = roadnet,
                                pointsxy = rbind(st_coordinates(project),
                                                 st_coordinates(candidate.stops)),
                                # approach = 1, # 'mapping each point to the nearest node in the network/graph
                                approach = 2, # 'mapping each point to the nearest point in the network/graph and creating new node
                                ELComputed = TRUE,
                                ea.prop = rep(0, ncol(roadnet)))  # attributes not used - but 'approach 2' requires them to be there
  
  # extract node numbers (which are in project.net[[3]]) for the project
  project.node <- unlist(project.net[[3]])[1]
  # extract node numbers (which are in project.net[[3]]) for the stops, make a table showing corresponding stop_ids and node
  candidate.stops.nodes <- cbind(candidate.stops,
                                 node = unlist(project.net[[3]][-1]))
  # and a list of unique nodes ('distances' in igraph doesn't allow duplicates)
  stops.nodes <- candidate.stops.nodes$node %>% unique
  
  # convert network to igraph
  project.graph <- nel2igraph(nodelist = project.net[[1]],
                              edgelist = project.net[[2]],
                              weight = project.net[[8]],  # 8 is the weights calculated by 'ELComputed'
                              Directed = FALSE)  # not required to be directed for walking
  
  # plot(project.graph, vertex.label = NA, vertex.size = 2, vertex.size2 = 2, mark.co = "green")
  # plot(project.graph, vertex.size = 2, vertex.size2 = 2, mark.co = "green")
  
  # find the nodes that are reachable within 800m walking distance (using 'distances' from igraph)
  selected.nodes <- stops.nodes[distances(project.graph, project.node, stops.nodes) <= BUFFDIST]

    
  # if there are no selected stops, it may be because the project node has snapped
  # to a small disconnected segment; if so then try again with the largest connected part
  
  if (length(selected.nodes) < 1) {
    # find whether project.node has snapped to largest network
    # largest cluster https://stackoverflow.com/questions/64344845/getting-the-biggest-connected-component-in-r-igraph
    components <- clusters(project.graph)
    largest.cluster.id <- which.max(components$csize)
    largest.cluster.members <- V(project.graph)[components$membership == largest.cluster.id]
    project.node.cluster <- components$membership[project.node]
    project.node.cluster.size <- components$csize[project.node.cluster]
    
    # if the project node is not in the largest cluster, and if the cluster that 
    # contains the project node has less than 10 members, then re-run using the
    # largest cluster
    if (!(project.node %in% largest.cluster.members) & 
        project.node.cluster.size < 10) {
      # find and return largest connected part, using nt.connect (and preserve its
      # crs - really only needed for optional ggplot display below) 
      roadnet.crs <- st_crs(roadnet)
      roadnet <- nt.connect(roadnet)
      
      # and run the same process again on the largest connected part (code repeated from above)
      project.net <- points2network(ntdata = roadnet,
                                    pointsxy = rbind(st_coordinates(project),
                                                     st_coordinates(candidate.stops)),
                                    # approach = 1, # 'mapping each point to the nearest node in the network/graph
                                    approach = 2, # 'mapping each point to the nearest point in the network/graph and creating new node
                                    ELComputed = TRUE,
                                    ea.prop = rep(0, ncol(roadnet)))  # attributes not used - but 'approach 2' requires them to be there
      
      project.node <- unlist(project.net[[3]])[1]
      candidate.stops.nodes <- cbind(candidate.stops,
                                     node = unlist(project.net[[3]][-1]))
      stops.nodes <- candidate.stops.nodes$node %>% unique
      
      project.graph <- nel2igraph(nodelist = project.net[[1]],
                                  edgelist = project.net[[2]],
                                  weight = project.net[[8]],  # 8 is the weights calculated by 'ELComputed'
                                  Directed = FALSE)  # not required to be directed for walking
      
      selected.nodes <- stops.nodes[distances(project.graph, project.node, stops.nodes) <= BUFFDIST]
    }
  }

  # find the selected stops
  selected.stops <- candidate.stops.nodes %>%
    filter(node %in% selected.nodes) %>%
    .$stop_id %>%
    unique(.) %>%
    sort(.)
  

  # examine outputs
  # ggplot() +
  #   geom_sf(data = project.roads %>% st_as_sf %>% st_set_crs(roadnet.crs)) +
  #   geom_sf(data = project, colour = "red", size = 4) +
  #   geom_sf(data = candidate.stops, colour = "blue", size = 4) +
  #   geom_sf(data = candidate.stops %>% filter(stop_id %in% selected.stops),
  #           colour = "green", size = 2)


  
  return(selected.stops)
  
}

