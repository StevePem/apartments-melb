# function to determine indicators for developments with trip generation surveys:
# distance to nearest stop (of each mode), and number of services between
# 7am to 7pm within 800m of site

indicators <- function() {
  
  # 1 libraries ----
  # -----------------------------------#
  
  library(tidyverse)
  library(sf)
  library(tidytransit)
  library(igraph)
  library(openxlsx)
  
  
  # 2 load development survey data ----
  # -----------------------------------#
  
  data <- read.csv("../Tables/trip generation locations.csv")
  cities <- unique(data$Group) %>% sort(na.last = TRUE)
  
  
  # 3 output dataframe ----
  # -----------------------------------#
  outputs <- c()
  
  
  # 4 loop to process cities ----
  # -----------------------------------#
  # 
  for (i in seq_along(cities)) {
  # for (i in 19:20) {
    
    city <- cities[i]
    print(paste(Sys.time(), "| Beginning processing for", city))
    
    ## 4.1 pedestrian network for city, and graph ----
    links <- st_read(paste0("../trip generation/networks/", city, ".sqlite"),
                              layer = "links", quiet = TRUE) %>%
      filter(is_walk == 1)
    nodes <- st_read(paste0("../trip generation/networks/", city, ".sqlite"),
                     layer = "nodes", quiet = TRUE) %>%
      filter(id %in% links$from_id | id %in% links$to_id)
    network.crs <- st_crs(links)
    
    g <- graph_from_data_frame(links %>%
                                 st_drop_geometry %>%
                                 dplyr::select(from_id, to_id, weight = length),
                               directed = FALSE)
    
    ## 4.2 developments in the city, with nearest nodes ----
    # note -  manual adjustment(s) to move coordinates, to avoid poor snapping
    # or where there are significant mismatches from street addresses
    developments <- data %>%
      filter(Group == city) %>%
      
      # lat/long adjustments
      mutate(Longitude = case_when(
        Site.ID == "NZ-465"         ~ Longitude + 0.0001,  # ~10m to east
        Site.ID == "HDR-PHD-2014-3" ~ Longitude + 0.0002,  # ~20m to east
        Site.ID == "AU-222"         ~ Longitude - 0.0001,  # ~10m to west
        Site.ID == "HDR-2017-8"     ~ Longitude + 0.0003,  # ~30m to east
        Site.ID == "AU-237 & LDR3"  ~ Longitude - 0.001,  # ~100m to west
        Site.ID == "HDR-2017-1"     ~ Longitude - 0.0001,  # ~10m to west
        Site.ID == "AU-80 & SH9"    ~ Longitude - 0.0005,  # ~50m to west
        Site.ID == "AU-221"         ~ Longitude - 0.0001,  # ~10m to west
        Site.ID == "AU-245 & LDR11" ~ Longitude + 0.0004,  # ~40m to east
        Site.ID == "LDR-2022-11"    ~ 150.9818,  # fixing location
        TRUE                        ~ Longitude
      ),
      Latitude = case_when(
        Site.ID == "HDR-PHD-2014-3" ~ Latitude - 0.0002,  # ~20m to south
        Site.ID == "HDR-2017-8"     ~ Latitude - 0.002,  # ~200m to south
        Site.ID == "AU-217"         ~ Latitude - 0.0001,  # ~10m to south
        Site.ID == "HDR-2017-16"    ~ Latitude - 0.0003,  # ~30m to south
        Site.ID == "MDR-2013-7"     ~ Latitude - 0.002,  # ~200m to south
        Site.ID == "MDR-2013-10"    ~ Latitude - 0.001,  # ~100m to south
        Site.ID == "LDR-2022-11"    ~ -33.8312,  # fixing location
        TRUE                        ~ Latitude
      )) %>%
      
      # date correction
      mutate(Date.s = case_when(
        Site.ID == "NZ-722" & Day == "Sat" ~ "2/11/2013",
        TRUE                               ~ Date.s
      )) %>%
      
      # sf object with nearest nodes
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      st_transform(network.crs) %>%
      mutate(node = nodes[st_nearest_feature(., nodes), ] %>% 
               st_drop_geometry() %>%
               .$id)
      
    ## 4.3 region buffer ----
    ## note: this is the developments for the city, buffered to 5km; no stops will 
    ## be found outside this 5km distance (which matches the network extent)
    region <- developments %>%
      st_buffer(5000) %>%
      summarise()
    
    ## 4.4 loop to process GTFS (except Palmerston North) ----
    if (!city == "Palmerston North") {
      gtfs.dates <- unique(developments$GTFS)  %>% sort(na.last = TRUE)
      
      for (j in seq_along(gtfs.dates)) {
        gtfs.date <- gtfs.dates[j]
        print(paste(Sys.time(), "| Processing GTFS", gtfs.date, "for", city))
        
        ### 4.4.1 load GTFS ---- 
        gtfs <- read_gtfs(paste0("../trip generation/data/gtfs_", gtfs.date, ".zip")) %>%
          gtfs_as_sf(., crs = 4326)
        
        # convert dates in calendar and calendar_dates to integers (note: dates
        # are originally chr 'yyyymmdd'; tidytransit should convert to dates 
        # 'yyyy-mm-dd'; but doesn't work for Melbourne because of GTFS duplicate stop faults)
        calendar <- gtfs$calendar %>%
          mutate(start_date = as.numeric(gsub("-", "", as.character(start_date))),
                 end_date = as.numeric(gsub("-", "", as.character(end_date))))
        calendar_dates <- gtfs$calendar_dates %>%
          mutate(date = as.numeric(gsub("-", "", as.character(date))))
        
        ### 4.4.2 stops with modes and nearest nodes ----
        stop.modes <- gtfs$stop_times %>%
          
          # join trips and routes to find the route types
          left_join(gtfs$trips, by = "trip_id") %>%
          left_join(gtfs$routes, by = "route_id") %>%
          
          # keep only distinct stop_id and route_type combinations
          dplyr::select(stop_id, route_type) %>%
          distinct()
        
        stops <- gtfs$stops %>%
          st_transform(network.crs) %>%
          
          # filter to region
          st_filter(region, predicate = st_intersects) %>%
          
          # add modes
          left_join(stop.modes, by = "stop_id") %>%
          
          # omit where mode is NA (these can be deleted as they don't appear in stop_times) %>%
          filter(!is.na(route_type)) %>%
          
          # classify mode - standard route_type codes are from 
          # https://developers.google.com/transit/gtfs/reference:
          #   0-tram, 1-metro, 2-train, 3-bus, 4-ferry, 5-cable tram, 6-cable car, 
          #   7-funicular, 11-trolleybus, 12-monorail
          # and others have been determined by inspection
          mutate(mode = case_when(
            # standard
            route_type == 0  ~ "tram",
            route_type == 1  ~ "train",
            route_type == 2  ~ "train",
            route_type == 3  ~ "bus",
            route_type == 4  ~ "ferry",
            # additional for nsw_20220528
            route_type %in% c(106, 401)           ~ "train",
            route_type %in% c(204, 700, 712, 714) ~ "bus",
            route_type == 900                     ~ "tram"
          )) %>%
          
          # find nearest node
          mutate(node = nodes[st_nearest_feature(., nodes), ] %>% 
                   st_drop_geometry() %>%
                   .$id)
        
        stop.nodes <- as.character(stops$node) %>% unique()
        
        ### 4.4.3 developments for the gtfs date ----
        developments.date <- developments %>% 
          filter(GTFS == gtfs.date)
        
        ### 4.4.4 loop to process developments ----
        for (k in 1:nrow(developments.date)) {
          development <- developments.date[k, ]
          
          print(paste(Sys.time(), "| Processing development", development$Site.ID, "in", city))
          
          # only calculate distances if there are stops
          if (nrow(stops) > 0) {
            
            development.node <- as.character(development$node)
            
            #### 4.4.4.1 distance matrix from development to stops ----
            dist <- distances(g, v = development.node, to = stop.nodes) %>%
              as.data.frame()
            
            #### 4.4.4.2 distance to closest stop of each mode ----
            train.nodes <- stops %>% filter(mode == "train") %>% .$node %>% as.character()
            train.dist <- dist %>% dplyr::select(any_of(train.nodes))
            
            tram.nodes <- stops %>% filter(mode == "tram") %>% .$node %>% as.character()
            tram.dist <- dist %>% dplyr::select(any_of(tram.nodes))
            
            ferry.nodes <- stops %>% filter(mode == "ferry") %>% .$node %>% as.character()
            ferry.dist <- dist %>% dplyr::select(any_of(ferry.nodes))
            
            bus.nodes <- stops %>% filter(mode == "bus") %>% .$node %>% as.character()
            bus.dist <- dist %>% dplyr::select(any_of(bus.nodes))
            
            # closest for each mode (capped at 5km, otherwise NA)
            if (length(train.dist) > 0) {
              closest.train <- train.dist %>% t() %>% min()
              if (closest.train > 5000) closest.train <- NA
            } else {
              closest.train <- NA
            }
            
            if (length(tram.dist) > 0) {
              closest.tram <- tram.dist %>% t() %>% min()
              if (closest.tram > 5000) closest.tram <- NA
            } else {
              closest.tram <- NA
            }
            
            if (length(ferry.dist) > 0) {
              closest.ferry <- ferry.dist %>% t() %>% min()
              if (closest.ferry > 5000) closest.ferry <- NA
            } else {
              closest.ferry <- NA
            }
            
            if (length(bus.dist) > 0) {
              closest.bus <- bus.dist %>% t() %>% min()
              if (closest.bus > 5000) closest.bus <- NA
            } else {
              closest.bus <- NA
            }
            
            #### 4.4.4.3 stops within 800m ----
            # filter distance matrix to nodes within 800m
            nodes.800m <- dist %>% 
              t() %>%
              as.data.frame() %>%
              rename_at(1, ~"distance") %>%
              mutate(node = as.numeric(row.names(.))) %>%
              filter(distance <= 800)
            
            # keep only the stops that match the nodes within 800m
            stops.800m <- stops %>%
              st_drop_geometry() %>%
              left_join(nodes.800m, by = "node") %>%
              filter(!is.na(distance))
            
            # only calculate services if there are stops within 800m
            if (nrow(stops.800m) > 0) {
              #### 4.4.4.4 survey date(s) ----
              # date is original date, or substitute date if there is no exact GTFS date match
              survey.dates.base <- ifelse(development$Date_match == "Yes",
                                          development$Date.s,
                                          development$Substitute_dates)
              
              # convert dates to integers (could be single date, comma-separated, or dash-separated range)
              if (grepl("-", survey.dates.base)) {
                
                # split the range into start and end dates
                range.split <- strsplit(survey.dates.base, "\\s*-\\s*")[[1]]
                start.date <- as.Date(range.split[1], format = "%d/%m/%Y")
                end.date <- as.Date(range.split[2], format = "%d/%m/%Y")
                
                # generate the sequence of dates
                dates.parsed <- seq(from = start.date, to = end.date, by = "day")
                survey.dates <- as.integer(format(dates.parsed, "%Y%m%d"))
                
              } else {
                
                # handle single dates or multiple comma-separated dates
                dates.split <- strsplit(survey.dates.base, ",\\s*")[[1]]
                dates.parsed <- as.Date(dates.split, format = "%d/%m/%Y")
                survey.dates <- as.integer(format(dates.parsed, "%Y%m%d"))
              }
              
              #### 4.4.4.5 loop to process dates ----
              counts <- c()
              
              for (l in seq_along(survey.dates)) {
                survey.date <- survey.dates[l]
                survey.day <- tolower(weekdays(as.Date(as.character(survey.date), format = "%Y%m%d")))
                
                ##### 4.4.4.5.1 service id's for the relevant date ----
                services <- calendar %>%
                  filter(start_date <= survey.date & end_date >= survey.date &
                           !!sym(survey.day) == 1) %>%
                  .$service_id
                
                ##### 4.4.4.5.2 adjust for exceptions in calendar_dates ----
                for (m in 1:nrow(calendar_dates)) {
                  if (as.numeric(calendar_dates[m, "date"]) == survey.date) {
                    if (as.numeric(calendar_dates[m, "exception_type"]) == 1) {
                      services <- c(services, as.character(calendar_dates[m, "service_id"]))
                    }
                    if (as.numeric(calendar_dates[m, "exception_type"]) == 2) {
                      services <- services[!services %in% as.character(calendar_dates[m, "service_id"])]
                    }
                  }
                }
                
                ##### 4.4.4.5.3 trips for the relevant services ----
                trips <- gtfs$trips %>%
                  filter(service_id %in% services) %>%
                  .$trip_id
                
                ##### 4.4.4.5.4 stop times for the relevant trips and stops ----
                stop.times <- gtfs$stop_times %>%
                  # omit final stops (we only want departures)
                  group_by(trip_id) %>%
                  filter(stop_sequence < max(stop_sequence)) %>%
                  ungroup() %>%
                  # filter to relevant trips and stops
                  filter(trip_id %in% trips) %>%
                  filter(stop_id %in% stops.800m$stop_id)
                
                ##### 4.4.4.5.5 tally trips for each mode ----
                
                # only proceed if there are stop times within 800m
                if (nrow(stop.times) > 0) {
                  trip.tally <- stop.times %>%
                    # filter to 7am to 7pm (represented as hhmmss integers)
                    mutate(arrival_time = as.numeric(gsub(":", "", arrival_time)),
                           departure_time = as.numeric(gsub(":", "", departure_time))) %>%
                    filter(departure_time >= 70000 & arrival_time <= 190000) %>%
                    
                    # join modes
                    left_join(gtfs$trips %>% 
                                dplyr::select(trip_id, route_id) %>% 
                                distinct(),
                              by = "trip_id") %>%
                    left_join(gtfs$routes %>%
                                dplyr::select(route_id, route_type, route_short_name) %>%
                                distinct(),
                              by = "route_id") %>%
                    
                    # classify modes - see note above 
                    mutate(mode = case_when(
                      # standard
                      route_type == 0  ~ "tram",
                      route_type == 1  ~ "train",
                      route_type == 2  ~ "train",
                      route_type == 3  ~ "bus",
                      route_type == 4  ~ "ferry",
                      # additional for nsw_20220528
                      route_type %in% c(106, 401)           ~ "train",
                      route_type %in% c(204, 700, 712, 714) ~ "bus",
                      route_type == 900                     ~ "tram"
                    )) %>%
                    
                    # tally trips by mode
                    dplyr::select(trip_id, mode) %>%
                    distinct() %>%
                    group_by(mode) %>%
                    summarise(n = n())
                  
                  count.train <- ifelse("train" %in% trip.tally$mode,
                                        trip.tally %>% filter(mode == "train") %>% pull(n), 
                                        0)
                  count.tram <- ifelse("tram" %in% trip.tally$mode,
                                       trip.tally %>% filter(mode == "tram") %>% pull(n), 
                                       0)
                  count.ferry <- ifelse("ferry" %in% trip.tally$mode,
                                        trip.tally %>% filter(mode == "ferry") %>% pull(n), 
                                        0)
                  count.bus <- ifelse("bus" %in% trip.tally$mode,
                                      trip.tally %>% filter(mode == "bus") %>% pull(n), 
                                      0)
                  
                } else {
                  count.train <- 0
                  count.tram <- 0
                  count.ferry <- 0
                  count.bus <- 0
                }
                
                ## counts for the date
                count.row <- cbind(count_train = count.train, 
                                   count_tram = count.tram, 
                                   count_ferry = count.ferry, 
                                   count_bus = count.bus) %>%
                  as.data.frame()
                
                counts <- bind_rows(counts, count.row)
                
              }
              
              #### 4.4.4.6 average counts where multiple dates ---- 
              average.counts <- counts %>%
                colMeans()
              no.train <- average.counts["count_train"]
              no.tram <- average.counts["count_tram"]
              no.ferry <- average.counts["count_ferry"]
              no.bus <- average.counts["count_bus"]
              
            } else {
              no.train <- 0
              no.tram <- 0
              no.ferry <- 0
              no.bus <- 0
              
            }
            
          } else {
            closest.train <- NA
            closest.tram <- NA
            closest.ferry <- NA
            closest.bus <- NA
            no.train <- 0
            no.tram <- 0
            no.ferry <- 0
            no.bus <- 0
          }
          
          #### 4.4.4.7 add results to output ----
          output.row <- development %>%
            st_drop_geometry() %>%
            dplyr::select(Site.ID, Date.s, Substitute_dates) %>% 
            cbind(closest_train = closest.train,
                  closest_tram = closest.tram,
                  closest_ferry = closest.ferry,
                  closest_bus = closest.bus,
                  no_train = no.train,
                  no_tram = no.tram,
                  no_ferry = no.ferry,
                  no_bus = no.bus)
          
          outputs <- bind_rows(outputs, output.row)
          
        }
        
      }

    } else if (city == "Palmerston North") {
      
      ## 4.5 Palmerston North ----
      
      ### 4.5.1 stops with modes and nearest nodes ----
      bus.stops <- read.csv("../trip generation/data/PNCC_Bus_Stops.csv") %>%
        st_as_sf(coords = c("X", "Y"), crs = 2193) %>%   # CRS is NZGD2000 / New Zealand Transverse Mercator 2000 (determined by trial and error)
        st_transform(network.crs) %>%
        mutate(mode = "bus")
      
      train.stop <- tibble(station = "Palmerston North", X = 380612, Y = 5533094) %>%  # Coordinates from inspecting station location in OSM, using QGIS
        st_as_sf(coords = c("X", "Y"), crs = 2135) %>% 
        mutate(mode = "train")
      
      stops = bind_rows(bus.stops, train.stop) %>%
        mutate(node = nodes[st_nearest_feature(., nodes), ] %>% 
                 st_drop_geometry() %>%
                 .$id)
      
      stop.nodes <- as.character(stops$node) %>% unique()
      
      ### 4.5.2 loop to process developments ----
      
      # dataframe to hold stop outputs
      pn.stops <- c()
      
      for (j in 1:nrow(developments)) {
        
        development <- developments[j, ]
        print(paste(Sys.time(), "| Processing development", development$Site.ID, "in", city))
        
        #### 4.4.2.1 distance matrix from development to stops ----
        development.node <- as.character(development$node)
        
        dist <- distances(g, v = development.node, to = stop.nodes) %>%
          as.data.frame()
        
        #### 4.5.2.2 distance to closest stop of each mode ----
        train.nodes <- stops %>% filter(mode == "train") %>% .$node %>% as.character()
        train.dist <- dist %>% dplyr::select(any_of(train.nodes))
        
        bus.nodes <- stops %>% filter(mode == "bus") %>% .$node %>% as.character()
        bus.dist <- dist %>% dplyr::select(any_of(bus.nodes))
        
        # closest for each mode (capped at 5km, otherwise NA)
        if (length(train.dist) > 0) {
          closest.train <- train.dist %>% t() %>% min()
          if (closest.train > 5000) closest.train <- NA
        } else {
          closest.train <- NA
        }
        
        closest.tram <- NA

        closest.ferry <- NA

        if (length(bus.dist) > 0) {
          closest.bus <- bus.dist %>% t() %>% min()
          if (closest.bus > 5000) closest.bus <- NA
        } else {
          closest.bus <- NA
        }
        
        #### 4.5.2.3 stops within 800m ----
        # filter distance matrix to nodes within 800m
        nodes.800m <- dist %>% 
          t() %>%
          as.data.frame() %>%
          rename_at(1, ~"distance") %>%
          mutate(node = as.numeric(row.names(.))) %>%
          filter(distance <= 800)
        
        # keep only the stops that match the nodes within 800m (they are all bus stops)
        stops.800m <- stops %>%
          st_drop_geometry() %>%
          left_join(nodes.800m, by = "node") %>%
          filter(!is.na(distance)) %>%
          # add column for development 
          mutate(Site.ID = development$Site.ID, .before = 1) %>%
          # order by route
          arrange(ROUTE, distance)
        
        # add to the dataframe
        pn.stops <- bind_rows(pn.stops, stops.800m)
        
        #### 4.5.2.4 add results to output ----
        output.row <- development %>%
          st_drop_geometry() %>%
          dplyr::select(Site.ID, Date.s, Substitute_dates) %>% 
          cbind(closest_train = closest.train,
                closest_tram = closest.tram,
                closest_ferry = closest.ferry,
                closest_bus = closest.bus,
                no_train = 0,
                no_tram = 0,
                no_ferry = 0,
                no_bus = 0)  # to be supplemented below, with  manual results
        
        outputs <- bind_rows(outputs, output.row)
        
      }
      
      ### 4.5.3 save stop locations for manual processing ----
      
      write.csv(pn.stops, "../Tables/Palmerston North development bus stops.csv")
      
      
      ### 4.5.4 add manually processed counts and substitute dates ----
      if (file.exists("../Tables/Palmerston North development bus counts.csv")) {
        pn.counts <- read.csv("../Tables/Palmerston North development bus counts.csv")
        outputs[which(outputs$Site.ID == "NZ-691"), "no_bus"] <- pn.counts[which(pn.counts$Site.ID == "NZ-691"), "no_bus"]
        outputs[which(outputs$Site.ID == "NZ-741"), "no_bus"] <- pn.counts[which(pn.counts$Site.ID == "NZ-741"), "no_bus"]
        
      }
 
    }
    
  }
  
  # 5 outputs ----
  # -----------------------------------#
  
  ## 5.1 write outputs to excel file ----
  
  ## first, prepare the data in the required format
  
  # read in excel table, without col names
  table <- read.xlsx("../Tables/Residential Trip Generation Data - Cut down version for SP - 240924.xlsx", 
                     sheet = "Dataset", colNames = FALSE)
  
  # col names for PT headings
  PT.head <- table[1:2, c("X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17")]
  
  # join output data
  table.joined <- table %>%
    # re-convert dates to correct formula (excel reads them in as numbers, origin 1899-12-30)
    # (ignore 'NAs introduced by coercion' - it applies to the non-date headings)
    mutate(X7 = case_when(
      str_detect(X7, ",") ~ X7,
      str_detect(X7, "-") ~ X7,
      X1 == "NZ-692" & X7 == "02/2007" ~ "01/02/2007",
      X1 == "NZ-722" & X7 == "11/2013" ~ "02/11/2013",
      TRUE ~ format(as.Date(as.numeric(X7), origin = "1899-12-30"), "%d/%m/%Y")
    )) %>%
    # remove the empty PT columns
    dplyr::select(-c(X10:X17)) %>%
    # join outputs, with dates also formatted as "%d/%m%Y"
    left_join(outputs %>%
                mutate(Date.s = case_when(
                  str_detect(Date.s, ",") ~ Date.s,
                  str_detect(Date.s, "-") ~ Date.s,
                  TRUE ~ format(as.Date(Date.s, format = "%d/%m/%Y"), "%d/%m/%Y")
                )), 
              by = c("X1" = "Site.ID", "X7" = "Date.s")) %>%
    # move 'Substitute_dates' to end
    relocate(Substitute_dates, .after = no_bus)
  
  # add the PT and substitute date column headings
  table.joined[1:2, 10:17] <- PT.head
  table.joined[1, 18] <- "Substitute_dates"

  ## second, writing output, with original styling
  
  # load original file as a workbook
  wb <- loadWorkbook("../Tables/Residential Trip Generation Data - Cut down version for SP - 240924.xlsx")
  
  # substitute the data
  writeData(wb, sheet = "Dataset", table.joined, colNames = FALSE)
  
  # save the output with new name
  saveWorkbook(wb, 
               paste0("../Tables/Residential Trip Generation Data with PT ", Sys.Date(), ".xlsx"), 
               overwrite = TRUE)

  ## 5.2 return table of outputs for checking ----
  joined.outputs <- data %>% 
    left_join(outputs %>% dplyr::select(-Substitute_dates), 
              by = c("Site.ID", "Date.s"))
  
  return(joined.outputs)
  
}

output.data <- indicators()

