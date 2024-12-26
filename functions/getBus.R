# Function to find total number of bus services, by month, for a given set of stops,
# based on GTFS data, converted to daily files
# Has options to include separate totals for each route, as well as overall totals

get.bus.vol <- function(route.stop.location, # folder containing daily route and stop .gz files
                        filtered.stops = F,  # either F (for all stops) or a vector of stops, eg c("941", "942")
                        filtered.route = F,  # either F (for all routes) or a numerical route number, eg 901
                        start.month,  # YYYYmm format, eg "201507" for July 2015, must not be before start of route.stop.location
                        end.month) {  # YYYYmm format, eg "201507" for July 2015, must not be after end of route.stop.location
  
  #   route.stop.location = "../Tables/bus daily stops and routes/"
  #   filtered.stops = selected.bus.stops
  #   start.month = "201507"
  #   end.month = "202206"
  
  # ## testing
  # filtered.stops = c("941", "942", "943")   #<< parameter
  # end.month = "201509"
  
  
  ## 1 Get list of stop & route file names/dates 
  ## -------------------------------------#
  # get the file paths from route.stop.location
  route_stop_file_paths <- dir_ls(route.stop.location) %>%
    as.character(.)
  # # extract the text after the final "/" (that is, the file names)
  # route_stop_file_names <- sapply(strsplit(route_stop_file_paths, 
  #                                          split = "/", 
  #                                          fixed = TRUE), 
  #                                 tail, 1L)
  
  
  ## 2 For each month, find the daily volumes and combine into monthly volumes
  ## -------------------------------------#
  # first day of 'start.month'
  start.date <- as.Date(paste0(start.month, "01"), "%Y%m%d")
  # last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
  end.date <- as.Date(paste0(end.month, "01"), "%Y%m%d") %m+% months(1) %m-% days(1)
  # 'dates' are the first day of every month from start to end
  bus.months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
    strftime(., format = "%Y-%m")
  
  # setup for parallel processing - detect no of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # report
  print(paste(Sys.time(), "| Finding bus volumes for", length(bus.months), "months; parallel processing with", cores, "cores"))
  
  # set up progress reporting
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = length(bus.months), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # loop to find daily volumes for each month, and combine as monthly volumes
  output.list <- 
    foreach(j = 1:length(bus.months),
            .packages = c("dplyr", "stringr", "readr", "tidyr"),
            # .combine = list, # resulted in multiple lists being combined - don't use!
            # .verbose = TRUE,
            .options.snow = opts) %dopar% {
              # create a temp directory (so that it can be cleared of the temp uncompressed
              # files on each iteration - otherwise leads to crash)
              temp_dir <- tempdir()
              
              # year and month (for display)
              year.month <- strftime(as.Date(paste0(bus.months[j], "-01", "%Y-%m-%d")), format = "%Y %B")
              
              # paths to daily details for the month
              month.file.paths <- route_stop_file_paths[str_detect(route_stop_file_paths, bus.months[j])]
              
              # empty dataframe to hold monthly volumes
              monthly.volumes <- data.frame()
              
              # find volumes for each day
              for (k in 1:length(month.file.paths)) {
                # read in the daily details
                daily.details <- read_csv(month.file.paths[k], show_col_types = FALSE)
                
                # filter to required stops, unless filtered.stops=F
                if (!isFALSE(filtered.stops)) {
                  daily.details <- daily.details %>%
                    filter(stop_id %in% filtered.stops)
                }
                
                # filter to required route, unless filtered.route=F
                if (!isFALSE(filtered.route)) {
                  daily.details <- daily.details %>%
                    filter(route_short_name == filtered.route)
                }
                
                # find daily volumes by route
                daily.volumes <- daily.details %>%
                  group_by(route_short_name) %>%
                  summarise(count = n_distinct(trip_id)) %>%
                  ungroup() %>%
                  pivot_wider(names_from = route_short_name,
                              names_prefix = "route_",
                              values_from = count) %>%
                  # add total_services column
                  mutate(total_services = sum(across(.cols = everything()), na.rm = T))
                
                # add to monthly volumes
                monthly.volumes <- bind_rows(monthly.volumes,
                                             daily.volumes)
              }
              
              # summarise daily totals for month
              monthly.totals <- monthly.volumes %>%
                # collapse into total for month
                summarise_all(sum, na.rm = T) %>%
                # add month and year %>%
                cbind(month = bus.months[j], .)
              
              # remove temporary files created for the month (to avoid crash)
              unlink(list.files(temp_dir, full.names = T))
              
              # return monthly total
              return(monthly.totals)         
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  
  ## 3 Prepare for display and return
  ## -------------------------------------#
  # convert parallel processing output list into a single table (can't be done
  # by rbind option within the parallel processing loop, because the output
  # rows have different columns where routes have changed)
  output.table <- data.frame()
  
  for (j in 1:length(output.list)) {
    output.table <- bind_rows(output.table,
                              output.list[[j]])
  }
  
  # reorder columns for display
  output.table <- output.table %>%
    # order alphabetically ('period', then 'routes' (numerically), then 'total_services')
    select(order(colnames(output.table)))
  
  return(output.table)
}
