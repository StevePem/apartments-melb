# bus cleaning checks

# set up environment
library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(readxl)
library(openxlsx)  # writing to excel
library(tidyr) # pivoting
library(ggplot2)


# 1. Investigate changed routes ---- 
# -----------------------------------------------------------------------------#
# Reviews successive quarterly archives of gtfs data, from 2015 (earliest
# available) to 2021, to detect changes in stops for bus routes

# NOTE - was run when only quarterly archives had been saved, before downloading 
# additional monthly archives (see section 2.2)

# Covers only changes to routes, not new or removed routes (as they will simply
# be detected by appearing, or not appearing, in the timetable)

# Creates a table of added and removed stops by route and archive
# But there are too many for manual processing, so decided instead
# to use a programmatic approach which is now in section 3 of buses.R

## 1.1 get list of GTFS archive names/dates  ----
## -------------------------------------#
GTFS.archive.location <- "../Data/gtfs archive/"

# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


## 1.2 read in stop_times, tables and routes for each archive  ----
## -------------------------------------#
for (i in 1:length(gtfs_file_paths)) {
  
  # get the 'name' from the file, in the form gtfs_yymm
  name <- str_sub(gtfs_file_names[i], start = 1, end = 11)
  
  # read in each archive file
  assign(name,
         read_gtfs(gtfs_file_paths[i]) %>%
           gtfs_as_sf(., crs = 4326))
  
  # open the required elements
  # assign(paste(name, "_calendar"), get(name)$calendar)
  # assign(paste(name, "_calendar_dates"), get(name)$calendar_dates)
  # assign(paste(name, "_stop_times"), get(name)$stop_times)
  # assign(paste(name, "_trips"), get(name)$trips)
  # assign(paste(name, "_routes"), get(name)$routes)
  # assign(paste(name, "_stops"), get(name)$stops %>%
  #          st_transform(7899))
  # assign(paste(name, "_shapes"), get(name)$shapes %>%
  #          st_transform(7899))
  # assign(paste(name, "_agency"), get(name)$agency)
  
  # collect the stops for each bus route
  assign(paste0(name, "_stop_table"), get(name)$stop_times %>%
           # filter to bus, using trip id - 
           filter(str_detect(trip_id, "\\.4-.*-mjp")) %>% # [pattern '.4-', then anything, then '-mjp']]
           # join trips and routes
           left_join(get(name)$trips, by = "trip_id") %>%
           left_join(get(name)$routes, by = "route_id") %>%
           # collect list of stop_ids for each route
           group_by(route_short_name) %>%
           summarise(stops = list(unique(stop_id))))
}

# result is a 'stop table' for each archive, eg gtfs_201503_stop_table


## 1.3 detect changes  ----
## -------------------------------------#
# compare each successive pair of archives, and compile into table

# empty table to hold outputs
bus.route.changes <- data.frame()


# for each pair of successive archives, see which routes have changed stop numbers
for (i in 1:(length(gtfs_file_paths) - 1)) {
  old_table_name <- str_sub(gtfs_file_names[i], start = 1, end = 11)  # eg "gtfs_201503"
  new_table_name <- str_sub(gtfs_file_names[i + 1], start = 1, end = 11)  # eg "gtfs_201506"
  
  # join the old and new tables (inner join: only routes that are in both)
  compare_table <- get(paste0(old_table_name, "_stop_table")) %>%
    inner_join(get(paste0(new_table_name, "_stop_table")), by = "route_short_name") %>%
    # find stops added to or removed from row
    rowwise() %>%
    mutate(stops_added = list(setdiff(stops.y, stops.x)),
           stops_removed = list(setdiff(stops.x, stops.y))) %>%
    # keep just the rows with results
    filter(length(stops_added) > 0 | length(stops_removed > 0)) %>%
    
    ## testing
    mutate(
      stops_added_names = list(
        get(new_table_name)$stops %>% 
          filter(stop_id %in% stops_added) %>%
          .$stop_name),
      stops_removed_names = list(
        get(old_table_name)$stops %>% 
          filter(stop_id %in% stops_removed) %>%
          .$stop_name)) %>%
    ungroup() %>%
    # add archive details
    mutate(old_table = old_table_name,
           new_table = new_table_name) %>%
    # select required fields
    dplyr::select(old_table, new_table, route_short_name, 
                  stops_added, stops_added_names,
                  stops_removed, stops_removed_names)
  
  bus.route.changes <- rbind(bus.route.changes,
                             compare_table)
}

## 1.4 complete and save output table  ----
## -------------------------------------#
# finalise table
bus.route.changes <- bus.route.changes %>%  
  # convert lists to simple strings
  rowwise() %>%
  mutate(stops_added = paste(stops_added, collapse = ", "),
         stops_added_names = paste(stops_added_names, collapse = ", "),
         stops_removed = paste(stops_removed, collapse = ", "),
         stops_removed_names = paste(stops_removed_names, collapse = ", ")) %>%
  # order by route no
  arrange(route_short_name)

# write output
write.csv(bus.route.changes, "../Tables/bus route changes for checking.csv", row.names = FALSE)


tr_replace_all(str, ", ", "', '")  # insert within first two quotes


# 2. See how far out each GTFS archive extends ---- 
# -----------------------------------------------------------------------------#
# Appears to be less than a quarter ...

library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(lubridate) # %m+%

GTFS.archive.location <- "../Data/gtfs archive/"


## 2.1 First test - monthly to Jun 2016, then quarterly ----
## -------------------------------------#

# empty dataframe to hold results
archive.coverage <- data.frame()


### 2.1.1 get list of GTFS archive names/dates ----
### -------------------------------------#
# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


### 2.1.2 for each archive, extract details and find dates of next quarter  ----
### -------------------------------------#
for (i in 1:length(gtfs_file_names)) {
  
  # read in each archive
  cat(as.character(Sys.time()), "|", "Reading in archive file", gtfs_file_names[i], "\n")
  gtfs <- read_gtfs(gtfs_file_paths[i]) #%>%
  #gtfs_as_sf(., crs = 4326)
  
  calendar.bus <- gtfs$calendar %>%
    filter(str_detect(service_id, "^4_"))
  calendar_dates.bus <- gtfs$calendar_dates %>%
    filter(str_detect(service_id, "^4_"))
  # stop_times <- gtfs$stop_times
  # trips <- gtfs$trips
  # routes <- gtfs$routes
  # stops <- gtfs$stops #%>%
  #   st_transform(7899)
  # shapes <- gtfs$shapes #%>%
  #   st_transform(7899)
  # agency <- gtfs$agency
  
  
  # date from archive file name
  archive.date <- str_extract(gtfs_file_names[i], "[[:digit:]]+") %>%
    as.Date(., format = "%Y%m%d")
  
  # for each date in each quarter, find its bus services 
  # for each quarter following the archive date, find its dates
  for (j in 1:3) {
    # start date is j months after first date of the archive month
    start.date <- as.Date(format(archive.date, "%Y-%m-01")) %m+% months(j)
    # end date is start date plus 1 month minus 1 day (eg 1 April -> 30 April)
    end.date <- start.date %m+% months(1) %m-% days(1)
    
    # date sequence
    dates <- seq.Date(from = start.date, to = end.date, by = "day")
    
    # month & year (for display)
    month.year <- paste(strftime(start.date, format = "%B"), 
                        strftime(start.date, format = "%Y"))
    cat(as.character(Sys.time()), "|", "Finding bus services for", month.year, "\n")
    
    ### 2.1.3 for each date, find its bus service id's ----
    ### ---------------------------------------------#
    for (k in 1:length(dates)) {
      # date and day of week
      date <- dates[k]
      weekday <- strftime(date, format = "%A") %>% 
        tolower()
      
      # services
      services <- calendar.bus %>%
        # correct day within time window
        filter(get(weekday) == "1") %>%
        filter(strptime(start_date, format = "%Y-%m-%d") <= date) %>%
        filter(strptime(end_date, format = "%Y-%m-%d") >= date) %>%
        # service list
        .$service_id
      
      # update service list for exceptions
      if (nrow(calendar_dates.bus) > 0) {
        for (l in 1:nrow(calendar_dates.bus)) {
          if (strptime(calendar_dates.bus[l, "date"][[1]], format = "%Y-%m-%d") == date) {
            if (calendar_dates.bus[l, "exception_type"] == "1") {
              services <- c(services, as.character(calendar_dates.bus[l, 1]))
            }
            if (calendar_dates.bus[l, "exception_type"] == "2") {
              services <- services[!services %in% calendar_dates.bus[l, 1]]
            }
          }
        }
       }
          
      
      ### 2.1.4 add services to output file ----
      ### ---------------------------------------------#
      archive.coverage <- rbind(archive.coverage, 
                                cbind(archive_date = gtfs_file_names[i],
                                      date = strftime(date, format = "%d %B %Y"),
                                      no_services = length(services),
                                      services = toString(services, sep = ", ")))
      
    }
  }
}

### 2.1.5 write result ----
### ---------------------------------------------#
write.csv(archive.coverage,
          "../Tables/archive coverage monthly to June 2016 then quarterly.csv", row.names = FALSE)

# Outcome: Apr 2015, May 2015 and Apr 2016 don't even cover full month; all after that 
# (quarterly) cover at least one month, but often not more.  Will need monthly for whole period.


## 2.2 Second test - monthly, with extra rules for months with inadequate coverage ----
## -------------------------------------#

# empty dataframe to hold results
archive.coverage <- data.frame()


### 2.2.1 get list of GTFS archive names/dates ----
### -------------------------------------#
# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


### 2.2.2 for each archive, extract details and find dates of next quarter  ----
### -------------------------------------#
for (i in 1:length(gtfs_file_names)) {
  # read in each archive
  cat(as.character(Sys.time()),
      "|",
      "Reading in archive file",
      gtfs_file_names[i],
      "\n")
  gtfs <- read_gtfs(gtfs_file_paths[i]) #%>%
  #gtfs_as_sf(., crs = 4326)
  
  calendar.bus <- gtfs$calendar %>%
    filter(str_detect(service_id, "^4_"))
  calendar_dates.bus <- gtfs$calendar_dates %>%
    filter(str_detect(service_id, "^4_"))
  # stop_times <- gtfs$stop_times
  # trips <- gtfs$trips
  # routes <- gtfs$routes
  # stops <- gtfs$stops #%>%
  #   st_transform(7899)
  # shapes <- gtfs$shapes #%>%
  #   st_transform(7899)
  # agency <- gtfs$agency
  
  
  # date from archive file name
  archive.date <-
    str_extract(gtfs_file_names[i], "[[:digit:]]+") %>%
    as.Date(., format = "%Y%m%d")
  
  # for each date in each month, find its bus services
  # General rule: full month following archive date
  # start date is 1 month after first date of the archive month
  start.date <-
    as.Date(format(archive.date, "%Y-%m-01")) %m+% months(1)
  # end date is start date plus 1 month minus 1 day (eg 1 April -> 30 April)
  end.date <- start.date %m+% months(1) %m-% days(1)
  
  # Exceptions for archives with missing or inadequate coverage
  # May 2015: use archive dated from 1 May 2015 instead of last April archive
  if (gtfs_file_names[i] == "gtfs_20150501.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-01"))
    end.date <- start.date %m+% months(1) %m-% days(1)
  }
  # Nov 2015: supplement with archives from 12 Nov 2015 and 26 Nov 2015
  # (and so last archive of Nov 2015 runs from 27 Nov to end of Dec)
  if (gtfs_file_names[i] == "gtfs_20151029.zip") {
    end.date <- as.Date("20151111", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20151112.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <- as.Date("20151126", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20151126.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d")) %m+% days(1)
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(2) %m-% days(1)
  }
  # May 2016: supplement with archive from 26 May 2016
  # (and so last archive of May 2016 runs from 27 May to end of Jun)
  if (gtfs_file_names[i] == "gtfs_20160422.zip") {
    end.date <- as.Date("20160526", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20160526.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d")) %m+% days(1)
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(2) %m-% days(1)
  }
  # Jan 2018: supplement with archive from 12 Jan 2018
  if (gtfs_file_names[i] == "gtfs_20171231.zip") {
    end.date <- as.Date("20180111", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180112.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Apr 2018: supplement with archive from 13 Apr 2018
  if (gtfs_file_names[i] == "gtfs_20180331.zip") {
    end.date <- as.Date("20180412", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180413.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # May 2018: supplement with archive from 14 May 2018
  if (gtfs_file_names[i] == "gtfs_20180427.zip") {
    end.date <- as.Date("20180513", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180514.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Jun 2018: supplement with archive from 15 Jun 2018
  if (gtfs_file_names[i] == "gtfs_20180524.zip") {
    end.date <- as.Date("20180614", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180615.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Jul 2018: supplement with archive from 19 Jul 2018 (18 Jul archive
  # is missing services from 18 Jul itself)
  if (gtfs_file_names[i] == "gtfs_20180625.zip") {
    end.date <- as.Date("20180718", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180718.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d")) %m+% days(1)
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Sep 2018: supplement with archive from 14 Sep 2018
  if (gtfs_file_names[i] == "gtfs_20180827.zip") {
    end.date <- as.Date("20180913", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20180914.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Dec 2018: supplement with archive from 14 Dec 2018
  if (gtfs_file_names[i] == "gtfs_20181121.zip") {
    end.date <- as.Date("20181213", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20181214.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Jul 2019: supplement with archive from 12 Jul 2019
  if (gtfs_file_names[i] == "gtfs_20190621.zip") {
    end.date <- as.Date("20190711", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20190712.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Sep 2020: supplement with archive from 18 Sep 2020
  if (gtfs_file_names[i] == "gtfs_20200826.zip") {
    end.date <- as.Date("20200917", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20200918.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Apr 2022: use archive dated from 1 April 2022 instead of last March archive, 
  # and supplement with archive from 14 Apr 2022
  if (gtfs_file_names[i] == "gtfs_20220401.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-01"))
    end.date <- as.Date("20220413", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20220414.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  # Jun 2022: supplement with archive from 17 Jun 2022
  if (gtfs_file_names[i] == "gtfs_20220527.zip") {
    end.date <- as.Date("20220616", format = "%Y%m%d")
  }
  if (gtfs_file_names[i] == "gtfs_20220617.zip") {
    start.date <- as.Date(format(archive.date, "%Y-%m-%d"))
    end.date <-
      as.Date(format(start.date, "%Y-%m-01")) %m+% months(1) %m-% days(1)
  }
  
  # date sequence
  dates <- seq.Date(from = start.date, to = end.date, by = "day")
  
  # month & year (for display)  # doesn't work exactly for exception archives
  month.year <- paste(strftime(start.date, format = "%B"),
                      strftime(start.date, format = "%Y"))
  cat(as.character(Sys.time()),
      "|",
      "Finding bus services from",
      month.year,
      "\n")
  
  ### 2.2.3 for each date, find its bus service id's ----
  ### ---------------------------------------------#
  for (k in 1:length(dates)) {
    # date and day of week
    date <- dates[k]
    weekday <- strftime(date, format = "%A") %>%
      tolower()
    
    # services
    services <- calendar.bus %>%
      # correct day within time window
      filter(get(weekday) == "1") %>%
      filter(strptime(start_date, format = "%Y-%m-%d") <= date) %>%
      filter(strptime(end_date, format = "%Y-%m-%d") >= date) %>%
      # service list
      .$service_id
    
    # update service list for exceptions
    if (nrow(calendar_dates.bus) > 0) {
      for (l in 1:nrow(calendar_dates.bus)) {
        if (strptime(calendar_dates.bus[l, "date"][[1]], format = "%Y-%m-%d") == date) {
          if (calendar_dates.bus[l, "exception_type"] == "1") {
            services <- c(services, as.character(calendar_dates.bus[l, 1]))
          }
          if (calendar_dates.bus[l, "exception_type"] == "2") {
            services <- services[!services %in% calendar_dates.bus[l, 1]]
          }
        }
      }
    }
    
    
    ### 2.2.4 add services to output file ----
    ### ---------------------------------------------#
    archive.coverage <- rbind(
      archive.coverage,
      cbind(
        archive_date = gtfs_file_names[i],
        date = strftime(date, format = "%d %B %Y"),
        no_services = length(services),
        services = toString(services, sep = ", ")
      )
    )
    
  }
}

### 2.2.5 result checks ----
### ---------------------------------------------#
# dates from 1 April 2015 to 31 Dec 2022
all.dates <- seq.Date(from = as.Date("20150401", format = "%Y%m%d"),
                      to = as.Date("20221231", format = "%Y%m%d"),
                      by = "day")

coverage.dates <- as.Date(archive.coverage$date, format = "%d %B %Y")

# any missing dates from archive coverage?
missing.dates <- all.dates[!(all.dates %in% coverage.dates)]  # none

# any duplicate dates?
coverage.dates[duplicated(coverage.dates)]  # none

# any zeros (ie no services found)?
zeros <- archive.coverage %>%
  filter(no_services == 0 | length(services) < 1)
zeros$date  # none


### 2.2.6 write result ----
### ---------------------------------------------#
write.csv(archive.coverage,
          "../Tables/archive coverage monthly with specific exceptions.csv", row.names = FALSE)

# read in again
archive.coverage <- read.csv("../Tables/archive coverage monthly with specific exceptions.csv")


# 3. Check whether bus stop ids are unique identifiers ---- 
# -----------------------------------------------------------------------------#
## 3.1 first check - distinct on all elements  ----
## -------------------------------------#
# First approach - created master list of all stops, where
# all elements (stop_id, stop_name, stop_lat, stop_lon) are distinct
# Has 69342 entries

## 3.1 Coding for the first approach ----
## ------------------------------------#
### 3.1.1 Set up environment ----
### ------------------------------------#
library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(sf)
# library(lubridate) # %m+%
# library(readr)  # reading & writing .gz files (read_csv, write_csv)

GTFS.archive.location <- "../Data/gtfs archive/"


### 3.1.2 Get list of GTFS archive names/dates ----
### -------------------------------------#
# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


### 3.1.3 Combine stops from all archives  ----
### -------------------------------------#
# dataframe to hold results
stop.list <- data.frame()

# for each archive, add its stops to the stop.list
for (i in 1:length(gtfs_file_names)) {
  # for (i in 7:11) {
  
  # read in each archive
  cat(as.character(Sys.time()),
      "|",
      "Finding stops in archive file",
      gtfs_file_names[i],
      "\n")
  gtfs <- read_gtfs(gtfs_file_paths[i])
  
  # get list of distinct stops
  stops.bus <- gtfs$trips %>%
    # filter to bus services only
    filter(str_detect(service_id, "^4_")) %>%
    # join stop_times and stops
    left_join(gtfs$stop_times, by = "trip_id") %>%
    left_join(gtfs$stops, by = "stop_id") %>%
    dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>%
    distinct()
  
  # add the stops to the master stop.list
  stop.list <- rbind(stop.list,
                     stops.bus) %>%
    # and eliminate duplicates
    distinct()
  
}

### 3.1.4 Save master stop.list  ----
### -------------------------------------#
# Save as .csv (for checking - see bus cleaning checks section 3)
write.csv(stop.list, "../Tables/bus stop list.csv", row.names = FALSE)



## 3.2 Testing the first approach  ----
## -------------------------------------#
# read in stop list, and convert geom to X & Y
stop.list <- st_read("../GIS/bus stop list.sqlite") %>%
  mutate(X = st_coordinates(.)[, 1],
         Y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# note that stop list is already 'distinct' - that is, there are no entries
# where stop_id, stop_name, stop_lat and stop_lon are all the same

### 3.2.1 Checking for duplicates  ----
### -------------------------------------#
# find any duplicate stop id's
stop.id.duplicates <- stop.list %>%
  group_by(stop_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# there are 18721 duplicates (that is, stops where >1 entry for the stop_id)

duplicated.stops <- stop.list %>%
  filter(stop_id %in% stop.id.duplicates$stop_id)
# there are 67984 entries (some duplicates have many )

# from initial check, looks like many have same stop_id, stop_lat and stop_lon, 
# but changed stop_name

### 3.2.2 First check - ignore stop name  ----
### -------------------------------------#
# check how many remain if stop_name is ignored
duplicated.stops.diff.name1 <- duplicated.stops %>%
  distinct(stop_id, X, Y)

duplicated.stops.diff.name.duplicates <- duplicated.stops.diff.name1 %>%
  group_by(stop_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# there are 5343 duplicates (stops where > 1 entry for the stop_id)

duplicated.stops.diff.name2 <- duplicated.stops.diff.name1 %>%
  filter(stop_id %in% duplicated.stops.diff.name.duplicates$stop_id)
# there are 11784 entries (most have 2, but a few have more)

# from initial check, many have just small differences on X and Y

# however, there are 2 which have negative X and Y (derived from their 
# original lat/lon being 0)
duplicated.stops.diff.name3 <- duplicated.stops.diff.name2 %>%
  filter(X > 0 & Y > 0)
# now 11782 entries

### 3.2.3 Second check - rounded X/Y  ----
### -------------------------------------#
# round lat & lon to nearest 100 (100m)
duplicated.stops.rounded1 <- duplicated.stops.diff.name3 %>%
  mutate(X_round = round(X, -2),
         Y_round = round(Y, -2)) %>%
  distinct(stop_id, X_round, Y_round)

duplicated.stops.rounded.duplicates <- duplicated.stops.rounded1 %>%
  group_by(stop_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# there are 1200 duplicates (stops where rounded X/Y equal)

duplicated.stops.rounded2 <- duplicated.stops.rounded1 %>%
  filter(stop_id %in% duplicated.stops.rounded.duplicates$stop_id)
# there are 2441 entries (most have 2, a few have more)

# most look like they differ by a single digit


### 3.2.3 Third check  - examine difference from mean X/Y ----
### -------------------------------------#
# find differences from mean X/Y
duplicated.stops.mean <- duplicated.stops.diff.name3 %>%
  group_by(stop_id) %>%
  # calculate means for each group of stop id's 
  mutate(mean_X = mean(X),
         mean_Y = mean(Y)) %>%
  ungroup() %>%
  # calculate difference from means
  mutate(diff_X = abs(X - mean_X),
         diff_Y = abs(Y - mean_Y))

max(duplicated.stops.mean$diff_X) # 13665 (~13.5 km)
max(duplicated.stops.mean$diff_Y) # 1670.702 (~1.6 km)


# find stops where differences is > 100m
duplicated.stops.mean.100m <- duplicated.stops.mean %>%
  filter(diff_X > 100 | diff_Y > 100)
# 165 obs

# find stops where difference is > 250m
duplicated.stops.mean.250m <- duplicated.stops.mean %>%
  filter(diff_X > 250 | diff_Y > 250)
# 39 obs

# So - there are 39 entries where X or Y differs from the mean by more than 250m
# And 165 if looking at 100m
# Some random checks show they are either actual shifted stops (eg 
# from one side of Dorset Rd to another (12049), or - more usually -  errors
# Are there any where the stop name suggests a genuine recycling of a number
# for a completely different stop?

### 3.2.3 Fourth check  - see whether 100m + have wildly differing stop names ----
### -------------------------------------#
duplicated.stops.mean.100m.with.name <- duplicated.stops.mean.100m %>%
  left_join(stop.list, by = "stop_id") %>%
  dplyr::select(stop_id, stop_name) %>%
  distinct()

# 364 entries.  Manually checked
# Conclusion - all are recognisable groups in similar locations.
# Ssome have moved - for example, stop 51069 has 2 different close locations
# on Williamsons Rd South Morang, and another 400m away in a side street
# But numbers don't simply get recycled into completely different locations

# Conclusion: some stops do move, but numbers don’t get re-cycled to completely 
# unrelated locations.  They can be relied on as stable. Resolved on approach: 
# can rely on stop_ids being distinct; take the version from the latest archive
# where the stop appears.
# Implemented in section 5 of buses.R


# 4. Comparing GTFS and DoT volumes ----
## ----------------------------------------------------------------------------#
# Compares the volumes drawn from GTFS (as created in section 4) to the volumes
# provided by DoT

## 4.1 Load GTFS and DoT volumes ----
## -------------------------------------#
# GTFS volumes (created in section 4)
GTFS.monthly <- read.csv("../Tables/bus monthly totals all routes.csv") %>%
  
  # filter to Jul 2017 to Jun 2022 (to match available DOT volumes and 
  # analysis period)
  filter(as.Date(paste(period, "01"), "%Y %B %d") >= as.Date("2017-07-01") &
           as.Date(paste(period, "01"), "%Y %B %d") <= as.Date("2022-06-01")) %>%
  
  # convert period format from "2017 June" to "June_2017"
  rowwise() %>%
  mutate(period = rev(strsplit(period, " "))) %>%
  mutate(period = paste(period, collapse = "_")) %>%
  ungroup() %>%
  
  # transpose to match DOT, converting period to col names
  # (https://stackoverflow.com/questions/7970179/transposing-a-dataframe-maintaining-the-first-column-as-heading)
  pivot_longer(cols = c(-period), names_to = "Route") %>%
  pivot_wider(names_from = c(period)) %>%
  
  # drop 'route' from the Route column ("route_150" >> "150")
  mutate(Route = str_replace(Route, "route_", "")) %>%
  
  # omit where all (except 'Route') are NA (routes cancelled before Jul 2017 or
  # started after Jun 2022)
  filter(if_any(!Route, ~ !is.na(.))) %>%
  
  # omit 'total services'
  filter(Route != "total_services") %>%
  
  # convert to numeric
  mutate(across(!Route, ~ as.numeric(.)))


# DoT volumes
DOT.monthly <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                 sheet = "MetroBusByMonth",
                                 skip = 1) %>% # omitting top 'year' row 
  
  # restore month/year names that were omitted when omitting top 'year' row
  setNames(., c("Mode", 
                "Route", 
                seq.Date(from = as.Date("2017-07-01"), to = as.Date("2017-12-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2017",
                seq.Date(from = as.Date("2018-01-01"), to = as.Date("2018-12-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2018",
                seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-12-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2019",
                seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2020",
                seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-12-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2021",
                seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-11-01"),
                         by = "month") %>% strftime(., format = "%B_%Y"),
                "Total_2022")) %>%
  
  # omit mode, totals and months from July 2022 (so period covered is Jul 2017 
  # to Jun 2022, matching analysis period to extend of availability)
  dplyr::select(-Mode, -contains("Total"), -July_2022, -August_2022, 
                -September_2022, -October_2022, -November_2022) %>%
  
  # omit where all (except 'Route') are NA (routes started after Jun 2022)
  filter(if_any(!Route, ~ !is.na(.))) %>%
  
  # omit 'NA' (which is the total row - it is not a route number)
  filter(!is.na(Route))


## 4.2 Check routes  covered by one but not other, and align ----
## -------------------------------------#
# checks
GTFS.routes <- GTFS.monthly$Route
DOT.routes <- DOT.monthly$Route

# find GTFS not in DOT
GTFS.extra <- GTFS.routes[!GTFS.routes %in% DOT.routes]
GTFS.extra
# [1] "109"              "109.Express"      "200.207.combined" "201"              "246"              "301"             
# [7] "401"              "402"              "465"              "490"              "601"              "700"             
# [13] "787.New"          "941"              "943"              "947"              "949"              "951"             
# [19] "953"              "959"              "965"              "967"              "978"              "979"             
# [25] "981"              "982"              "NA"          
# There are some major omissions here, incl 401 (Melb Uni shuttle) and 246 
# (Hoddle St/Punt Rd).  Some of the 9xx routes are quite low volume.  
# 'NA' is a small number of unallocated buses.

# find DOT not in GTFS
DOT.extra <- DOT.routes[!DOT.routes %in% GTFS.routes]
DOT.extra
# none

# where GTFS routes are missing from DOT, add lines
for (i in 1:length(GTFS.extra)) {
  DOT.monthly <- rbind(DOT.monthly, 
                       c(GTFS.extra[i], rep(as.numeric(NA), ncol(DOT.monthly) - 1)))
}
DOT.monthly <- DOT.monthly %>% 
  arrange(Route) %>%
  # convert to numeric
  mutate(across(!Route, ~ as.numeric(.)))


## 4.3 Find differences, and export results ----
## -------------------------------------#
# convert NAs to 0 (except in 'Route' column)
DOT.monthly <- DOT.monthly %>%
  mutate(across(!Route, ~ if_else(is.na(.), 0, .)))
GTFS.monthly <- GTFS.monthly %>%
  mutate(across(!Route, ~ if_else(is.na(.), 0, .)))

# difference as a proportion (GTFS/DOT) (so 1 is a perfect match), and using 'Inf'
# where DOT figure is missing, and '-Inf' where GTFS figure is missing
diff.prop <- data.frame()
for (i in 1:nrow(GTFS.monthly)) {
  for (j in 1:ncol(GTFS.monthly)) {
    # first column is always the route no
    if (j == 1) {
      diff.prop[i, j] <- GTFS.monthly[i, 1]
    } else {
      diff.prop[i, j] <- case_when(
        GTFS.monthly[i, j] == 0 & DOT.monthly[i, j] == 0 ~ 0,
        GTFS.monthly[i, j] > 0 & DOT.monthly[i, j] == 0 ~ 999, # no DOT figure
        GTFS.monthly[i, j] == 0 & DOT.monthly[i, j] > 0 ~ -999, # no GTFS figure
        GTFS.monthly[i, j] > 0 & DOT.monthly[i, j] > 0 ~ 
          round(GTFS.monthly[i, j][[1]] / DOT.monthly[i, j][[1]], 2)
      )
    }
  }
}
colnames(diff.prop) <- colnames(GTFS.monthly)

# difference as a number (DOT-GTFS): bind the first column (which is 'Route')
# with a table showing difference for each route (so 0 is a perfect match)
diff.no <- cbind(GTFS.monthly[, 1],
                 DOT.monthly[, -1] - GTFS.monthly[, -1])


# Write results
# create blank workbook, with the required sheet names
wb <- createWorkbook()
addWorksheet(wb, sheetName = "DOT.monthly")
addWorksheet(wb, sheetName = "GTFS.monthly")
addWorksheet(wb, sheetName = "diff.prop")
addWorksheet(wb, sheetName = "diff.no")

# write the results to the worksheets
writeData(wb, sheet = "DOT.monthly", DOT.monthly)
writeData(wb, sheet = "GTFS.monthly", GTFS.monthly)
writeData(wb, sheet = "diff.prop", diff.prop)
writeData(wb, sheet = "diff.no", diff.no)

# write the workbook to  file
saveWorkbook(wb, "../Tables/bus DOT and GTFS volume comparisons.xlsx", overwrite = TRUE)

# the 'diff.prop' and 'diff.no' sheets then require some manual colour-coding
# in excel; see section 6.8 of working.docx for approach 


## 4.4 Metrics for extent of compliance ----
## -------------------------------------#
# Provide measures for differences between DOT and GTFS volumes, for those
# months and routes where volume is provided by either or both of them

### 4.4.1 Proportional differences ----
### -------------------------------------#
# This section looks at each route number/month combination, and checks how
# closely the GTFS and DOT figures align to each other for the months within our
# analysis period where both exist (that is, July 2017-Jun 2022).  There are 60 
# months and 380 distinct route numbers, making a total of 22,800 route/month 
# combinations; but some routes do not run in new months, for example new or 
# terminated routes.The result is a table which shows the number and percentage 
# of route number/month combinations that are exact matches, within 5%, etc.

# function to create table of differences
diff.prop.metrics <- function(diff.prop) {
  # convert all except 'Route' column to single vector, and omit zeros
  diffs <- diff.prop %>%
    dplyr::select(-Route) %>% 
    unlist() %>%
    as.vector(mode = "numeric") %>%
    .[.!= 0]
  
  # find numbers for each outcome (exact match, within 5 or 10%, greater diff,
  # one missing numbers)
  exact <- length(diffs[diffs == 1])
  within.5 <- length(diffs[(diffs >= 0.95 & diffs < 1) |
                             (diffs > 1 & diffs <= 1.05)])
  within.5to10 <- length(diffs[(diffs >= 0.9 & diffs < 0.95) |
                                 (diffs > 1.05 & diffs <= 1.1)])
  more.than.10.DOT.more <- length(diffs[diffs < 0.9 & diffs > -999])
  more.than.10.GTFS.more <- length(diffs[diffs > 1.1 & diffs < 999])
  no.DOT <- length(diffs[diffs == 999])
  no.GTFS <- length(diffs[diffs == -999])
  total <- length(diffs)
  
  outputs <- rbind(c("DOT and GTFS exact match",
                     exact,
                     exact / total * 100),
                   c("DOT and GTFS within 5%",
                     within.5,
                     within.5 / total * 100),
                   c("DOT and GTFS between 5 and 10% apart",
                     within.5to10,
                     within.5to10 / total * 100),
                   c("Over 10% difference (DOT more)",
                     more.than.10.DOT.more,
                     more.than.10.DOT.more / total * 100),
                   c("Over 10% difference (GTFS more)",
                     more.than.10.GTFS.more,
                     more.than.10.GTFS.more / total * 100),
                   c("No DOT figure",
                     no.DOT,
                     no.DOT / total * 100),
                   c("No GTFS figure",
                     no.GTFS,
                     no.GTFS / total * 100),
                   c("Total",
                     total,
                     total / total * 100)) %>%
    as.data.frame()
  
  colnames(outputs) <- c("Match", "No", "%")
  
  return(outputs)
}

# read in diff.prop
diff.prop <- readxl::read_xlsx("../Tables/bus DOT and GTFS volume comparisons.xlsx", 
                               sheet = "diff.prop")

diff.prop.old <- readxl::read_xlsx("../Tables/bus DOT and GTFS volume comparisons [9 Feb 2023].xlsx", 
                                   sheet = "diff.prop")

# produce outputs
metrics.current <- diff.prop.metrics(diff.prop)
metrics.old <- diff.prop.metrics(diff.prop.old)

metrics.combined <- metrics.old %>%
  left_join(., metrics.current, by = "Match", suffix = c(".Former", ".Current"))

# write output
write.csv(metrics.combined, 
          "../Tables/bus DOT and VTFS volume comparison metrics.csv",
          row.names = FALSE)

### 4.4.2 Total service numbers ----
### -------------------------------------#
# This section compares the total service volumes shown by each of DOT and GTFS 
# for each month.  DOT is consistently lower, because they omit some routes,
# see section 4.2.  Results are shown as line graphs.  The second 'output.adjusted'
# plot adjusts the DOT figures for display, by adding [5000] to each of the 
# monthly DOT figures (this figure is the total excess of the GTFS volumes over 
# the DOT volumes, for the 5 year period, divided by 60 to produce a monthly 
# figure).

# Load 'GTFS.monthly' and 'DOT.monthly' from section 4.1, if not already loaded

GTFS.monthly.totals <- colSums(GTFS.monthly %>%
                                  dplyr::select(-Route),
                               na.rm = TRUE)

DOT.monthly.totals <- colSums(DOT.monthly %>%
                                dplyr::select(-Route),
                              na.rm = TRUE)

# months <- as.Date(paste0(names(GTFS.monthly.totals), "_01"), 
#                   format = "%Y_%B_%d") %>%
#   strftime(., format = "%Y%m")

total.table <- cbind(1:length(GTFS.monthly.totals),
                     as.numeric(GTFS.monthly.totals),
                     as.numeric(DOT.monthly.totals)) %>%
  as.data.frame() %>%
  setNames(c("Month", "GTFS", "DOT"))

# adjustment figure used to add to DOT figures for display
adj <- sum(total.table$GTFS - total.table$DOT) / 60
adj # 31344.63

output.plot <- function(data, mytitle, mysubtitle, mycaption) {
  ggplot(data) +
    
    # GTFS monthly totals
    geom_line(aes(x = Month, y = GTFS, colour = "GTFS")) +
    geom_point(aes(x = Month, y = GTFS, colour = "GTFS")) +
    
    # DOT monthly totals
    geom_line(aes(x = Month, y = DOT, colour = "DOT")) +
    geom_point(aes(x = Month, y = DOT, colour = "DOT")) +
    
    scale_y_continuous(labels = scales::comma) +
    
    # CHECK https://stackoverflow.com/questions/40833809/add-legend-to-geom-line-graph-in-r
    scale_colour_manual(name = "Data source", 
                        values =  c("GTFS" = "red", "DOT" = "blue")) +
    
    labs(x = "Month from July 2017 (month 1) to June 2022 (month 60)",
         y = "Number of bus services per month",
         title = mytitle,
         subtitle = mysubtitle,
         caption = mycaption) +
    
    theme_bw() +
    
    theme(plot.caption = element_text(hjust = 0))
  
}

output.std <- output.plot(data = total.table,
                          mytitle = "Comparison of bus service volumes reported by GTFS and DOT",
                          mysubtitle = "",
                          mycaption = "")

output.adjusted <- output.plot(data = total.table %>%
                                 mutate(DOT = DOT + adj),
                               mytitle = "",
                               mysubtitle = "Adjusted DOT volumes*",
                               mycaption = paste("*DOT volumes adjusted for display by adding", round(adj), "to each (being the total excess of the GTFS volumes \n over the DOT volumes for the full 5 year period, divided by 60 to produce a monthly figure)"))

png( "../Images/bus DOT and GTFS volume comparisons std.png", width = 2500, height = 1200, res = 300)
output.std
dev.off()

png( "../Images/bus DOT and GTFS volume comparisons [DOT adjusted].png", width = 2500, height = 1200, res = 300)
output.adjusted
dev.off()

