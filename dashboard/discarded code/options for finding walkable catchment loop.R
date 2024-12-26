### 2.4.6 find walkable catchments around PT stops ----
### -------------------------------------#
# find stops that are walkable from completed projects (that is, which were
# selected in section 2.4.3 and added to 'completed.projects')
completed.projects <- st_read("../GIS/completed_projects_with_stops.sqlite")

walkable.train.stops <- stopList(completed.projects$rail_stn) 
walkable.tram.stops <- stopList(completed.projects$tram_stop)
walkable.bus.stops <- stopList(completed.projects$bus_stop)

walkable.stops <- stops %>%
  filter(station_name %in% walkable.train.stops |
           stop_id %in% walkable.tram.stops |
           stop_id %in% walkable.bus.stops)

# make dataframe to hold outputs
stop.catchment.outputs <- data.frame(matrix(0, ncol = ncol(stops), nrow = nrow(stops)))

# make directory to hold temporary outputs
dir.create("./catchments")


# setup for parallel processing - detect no of available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# set up progress reporting [not working for some reason]
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = nrow(walkable.stops), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# report
print(paste(Sys.time(), "| Finding walkable catchments for", nrow(walkable.stops), "PT stops; parallel processing with", cores, "cores"))

# loop to find monthly volumes
system.time(
  output <-
    # foreach(i = 1:nrow(walkable.stops),
    foreach(i = 1:10,
            .combine = rbind,
            .packages = c("dplyr", "sf", "shp2graph", "igraph", "nngeo"),
            .verbose = TRUE,  # using this as progress reporting not working
            .options.snow = opts) %dopar% {
              
              #======= works but slow and freezes
              # find and return the walkable catchment
              # return(findWalkableCatchment(walkable.stops[i, ], roads, WALKDIST))
              #=======
              
              # ==========testing
              # catchment <- findWalkableCatchment(walkable.stops[i, ], roads, WALKDIST)
              # stop.catchment.outputs[i, ] <- catchment
              
              # ==========
              
              # ======more testing
              catchment <- findWalkableCatchment(walkable.stops[i, ], roads, WALKDIST)
              saveRDS(catchment, paste0("./catchments/stop_", i, ".rds"))
              
              # ===========
              
              
            }
)
# close the progress bar and cluster
close(pb)
stopCluster(cluster)

# vector to hold all stop number variables
stop.nos <- c()

# read in all the files in 'catchments' folder, with each given the name of the
# file minus .rds (that is, 'stop_1.rds' read in as 'stop_1', etc)
for (i in 1:length(list.files("./catchments"))) {
  # create the stop number variable (eg 'stop_1.rds' is 'stop_1')
  var.name <- gsub(".rds", "", list.files("./catchments")[i])
  # read in the rds file and assign it to the variable
  assign(var.name,
         readRDS(paste0("./catchments/", list.files("./catchments")[i])))
  # add the variable name to the vector
  stop.nos <- c(stop.nos, var.name)
}

# combine all the 'stop_' files
# begin with the first variable (stop_1)
stop.catchments <- get(stop.nos[1])
# then add the others
for (i in 2:length(stop.nos)) {
  stop.catchments <- rbind(stop.catchments, get(stop.nos[i]))
}



# limit first to those that are in projects

### CHECK WHAT NAMES NEED TO CHANGE, MAYBE JOLIMONT; ALSO CHECK scc, ffs

### 2.4.7 write 'walkable catchment' outputs ----
### -------------------------------------#
# # uncomment to use
# st_write(stop.catchments, "./GIS/stop catchments.sqlite", delete_layer = TRUE)

# # delete the 'catchments' folder once outputs are written
# dir_delete("./catchments")
