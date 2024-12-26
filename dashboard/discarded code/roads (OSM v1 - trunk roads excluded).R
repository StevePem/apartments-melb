# create network from OSM extract (based on matsim_melbourne)


# 1 Download OSM extract ----
# -----------------------------------------------------------------------------#
# Download from https://www.interline.io/osm/extracts/ 

## Downloaded - melbourne_australia.osm.pbf


# 2 Convert to osm format ----
# -----------------------------------------------------------------------------#
# If OSM file has been downloaded in .pbf format, needs conversion to .osm

# Download OSM converter from https://wiki.openstreetmap.org/wiki/Osmconvert, and
# place in same directory as downloaded .pbf file

## Downloaded - osmconvert64-0.8.8p.exe

# Complete names of converter file, input .pbf and output .osm here:
OSMCONVERTER <- "osmconvert64-0.8.8p.exe"

INPUTFILE <- "../Data/melbourne_australia.osm.pbf"

OUTPUTNAME <- "../Data/melbourne_australia.osm"

# Convert .pbf file to .osm (adds new file with OUTPUTNAME
# to current directory - file will be much larger than .pbf input)
system(paste(OSMCONVERTER,
             INPUTFILE,
             "--out-osm",
             paste0("-o=", OUTPUTNAME)))

# Or run in command line tool, eg
# osmconvert64-0.8.8p.exe melbourne_australia.osm.pbf --out-osm -o=mebourne_australia.osm


# 3 Make network of edges and nodes ----
# -----------------------------------------------------------------------------#
# Creates network of edges and nodes, using files processOSM.sh and network.sql
# Output has 3 layers: edges, nodes, osm_metadata
# Adapted from https://github.com/matsim-melbourne/network

# Requires GDAL/OGR command line tools and Postgres to be installed
# GDAL/OGR - use OSGeo4W Network Installer from https://qgis.org/en/site/forusers/download.html
#  (see https://www.youtube.com/watch?v=CDN9MRuuf9k for instructions) - download
#  both GDAL/OGR command line tools and Spatialite library support
# Postgres - see https://www.postgresql.org/
# Ensure that running 'which ogr2ogr' in the terminal window returns the path
#  to the version with Spatialite library support; if not, then alter the order
#  of the environmental path variables

## 3.1 Parameters ----
## ------------------------------------#
# Input OSM extract to be converted to network
OSMEXTRACT <- "../data/melbourne_australia.osm"

# Desired coordinate system for network
OUTPUTCRS <- 7899  # for Melbourne

# Name of output file
OUTPUTSQLITE <- "../data/melbourne_network_unconfigured.sqlite"

# Password for postgres installation
PWORD <- "postgres"


## 3.2 Convert to unconfigured network ----
## ------------------------------------#
# For linux (not tested)
# system(paste("./processOSM.sh", OSMEXTRACT, OUTPUTCRS, OUTPUTSQLITE, PWORD))

# For Windows
command <- paste("./processOSM.sh", OSMEXTRACT, OUTPUTCRS, OUTPUTSQLITE, PWORD)
wincommand <- gsub("/", "\\\\", command)  # replacing "/" with "\\"
shell(wincommand, intern = TRUE)

# Or run in command line tool, eg
# ./processOSM.sh ./melbourne_australia.osm 28355 ./melbourne_network_unconfigured.sqlite postgres




# 4 Configure network  ----
# -----------------------------------------------------------------------------#
# Configure the network created under previous step by:
# - ensuring geometry column is 'geom' not 'GEOMETRY'
# - processing OSM tags and adding defaults for speed etc based on highway type
# - discarding all except largest connected component

# Adapted from https://github.com/matsim-melbourne/network

## 4.1 Parameters/inputs ----
## ------------------------------------#
# Complete and check all parameters / inputs in this section before running function

# Network file created under section 3
INPUTSQLITE <- "../Data/melbourne_network_unconfigured.sqlite"

# Country (for defaults)
COUNTRY <- "Australia"


# Output network file
OUTPUTNETWORK <- "../GIS/melbourne_network.sqlite"


## 4.2 Function: configureNetwork ----
## ------------------------------------#

configureNetwork <- function(INPUTSQLITE,
                             COUNTRY,
                             OUTPUTNETWORK) {
  
  
  ## Libraries ----
  ## ------------------------------------#
  library(sf)
  library(fs)  ## 'dir_walk
  library(dplyr)
  library(data.table) ## 'like' (in 'processOsmTags.R')
  library(igraph)  ## in 'largestConnectedComponent.R'
  # library(raster)
  # library(lwgeom)  ## st_snap_to_grid
  
  
  ## Functions ----
  ## ------------------------------------#
  dir_walk(path="./functions/",source, recurse=T, type = "file")
  
  
  ## Configure network ----
  ## ------------------------------------#
  # Read in nodes and edges
  networkInput <- list(st_read(INPUTSQLITE, layer = "nodes", quiet = T),
                       st_read(INPUTSQLITE, layer = "edges", quiet = T))
  
  networkCRS <- st_crs(networkInput[[1]])
  
  
  # Ensure geometry column is 'geom' instead of 'GEOMETRY'
  if ('GEOMETRY' %in% colnames(networkInput[[1]])) {
    networkInput[[1]]<-networkInput[[1]] %>% rename(geom = GEOMETRY)
  }
  
  if ('GEOMETRY' %in% colnames(networkInput[[2]])) {
    networkInput[[2]]<-networkInput[[2]] %>% rename(geom = GEOMETRY)
  }
  
  
  # Check structure of nodes and edges
  cat(paste0("Network input, nodes:\n"))
  str(networkInput[[1]])
  cat(paste0("\nNetwork input, edges:\n"))
  str(networkInput[[2]])
  cat(paste0("\n"))
  
  
  # Process OSM metadata and build defaults
  echo("processing OSM metadata\n")
  osm_metadata <- st_read(INPUTSQLITE, layer="osm_metadata", quiet = T) %>%
    filter(osm_id %in% networkInput[[2]]$osm_id)
  
  echo("Building default OSM attribute tables\n")
  defaults_df <- buildDefaultsDF(COUNTRY)
  highway_lookup <- defaults_df %>% dplyr::select(highway, highway_order)
  
  echo("Processing OSM tags and joining with defaults\n")
  system.time(osmAttributes <- processOsmTags(osm_metadata, defaults_df))
  
  
  # Join attributes to edges
  networkInput[[2]] <- networkInput[[2]] %>%
    inner_join(osmAttributes, by = "osm_id") %>%
    # dplyr::select(-osm_id,highway,highway_order)
    dplyr::select(-highway, highway_order)
  
  
  # Keep only the largest connected component
  largestComponent <- largestConnectedComponent(networkInput[[1]], networkInput[[2]])
  
  
  # Check structure of nodes and edges of largest component
  cat(paste0("largestComponent, nodes:\n"))
  str(largestComponent[[1]])
  cat(paste0("\nlargestComponent, edges:\n"))
  str(largestComponent[[2]])
  cat(paste0("\n"))

  
  # Add car, bike and walk modes (used in checking routable graphs for each mode )
  networkMode <- addMode(largestComponent)
  
  
  # Ensure transport is a directed routeable graph for each mode (i.e., connected
  # subgraph). Functions are in 'cleanNetwork.R'. First function ensures a 
  # connected directed subgraph and second  ensures a connected subgraph without
  #  considering directionality. Car and bike modes are directed; walk is undirected.
  echo("Ensuring networks for each mode are routeable\n")
  networkNonDisconnected <- largestDirectedNetworkSubgraph(networkMode, "car,bike")
  networkConnected <- largestNetworkSubgraph(networkNonDisconnected, "walk")
  
  
  ## Write output ----
  ## ------------------------------------#
  # Final network for writing as output
  networkFinal <- networkConnected
  
  
  # Write output
  echo(paste0("Writing output to ", OUTPUTNETWORK, "\n"))
  st_write(networkFinal[[1]], OUTPUTNETWORK, layer = "nodes", 
           driver = "SQLite", delete_layer = T)
  st_write(networkFinal[[2]], OUTPUTNETWORK, layer = "links",  
           driver = "SQLite", delete_layer = T)
  
}

## 4.3 Run the function ----
## ------------------------------------#
configureNetwork(INPUTSQLITE,
                 COUNTRY,
                 OUTPUTNETWORK)



# 5 (Optional) delete temporary files  ----
# -----------------------------------------------------------------------------#
library(fs)

# Insert names of files to be deleted as required

# delete .osm created from .pbf in section 2
# CARE - don't delete the .osm unless .pbf is saved - uncomment to use
# file_delete("../Data/melbourne_australia.osm")

# delete the unconfigured output sqlite created in section 3
file_delete("../Data/melbourne_network_unconfigured.sqlite")

