#==============================================================================#
#   Investigation of bus data
#
#   Steve Pemberton, [December 2022]
#
# 
#   Organised as follows [review and expand as needed]
#   1  Bus route share for routes 906 & 907
#   2  Capacity factors - routes 601, 906 and 907
#   3  Find changed routes
#   4  
#
#==============================================================================#

# set up environment
library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(readxl)
library(openxlsx)  # writing to excel
library(tidyr) # pivoting
# library(lubridate)  # wday


# zip folder for PTV GTFS data (for allocation of current DART routes, section 1)
# from https://transitfeeds.com/p/ptv/497 , downloaded 3 Oct 2022
GTFS <- "../Data/gtfs_20220929.zip"

# folder containing series of gtfs archives (used in section 3)
GTFS.archive.location <- "../Data/gtfs archive/"


# 1. Bus route share factors for routes 906 & 907 ----
# -----------------------------------------------------------------------------#
# Kinetic advise as follows (emails 15-16 Dec) - 
# - artic introduces 2 in 2010, 2 more in Nov 2015, 2 more in Oct 2018
# - artics are mainly used on routes 906 & 907, and rarely on 905 & 908
#   (we have decided to assume used on 906-907 only)
# - total of about 35 buses used on those 4 routes

# Assume the first 2 artic started in Oct 2010 (based on start date of 
# DART services of 4 Oct 2010)

# Assume division between DART services (ie routes 905, 906, 907 and 908) is 
# based approximately on resources in current timetable for those routes


## 1.1 read in GTFS ----
## -------------------------------------#
gtfs <- read_gtfs(GTFS) %>%
  gtfs_as_sf(., crs = 4326)

calendar <- gtfs$calendar
calendar_dates <- gtfs$calendar_dates
stop_times <- gtfs$stop_times
trips <- gtfs$trips
routes <- gtfs$routes

# stops <- gtfs$stops %>%
#   st_transform(7899)
# shapes <- gtfs$shapes %>%
#   st_transform(7899)
# agency <- gtfs$agency
st_write(stops, "../GIS/current stops.sqlite", delete_layer = TRUE)

## 1.2 select dates and find their service IDs ----
## -------------------------------------#
# select non-holiday week: 21-27 Nov 2011

# selected dates
DAYS <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
DATES <- as.character(c(20221010:20221016))

# table to hold service list
service.table <- data.frame()

for (i in 1:length(DATES)) {
  DAY <- DAYS[i]
  DATE <- DATES[i]
  
  services <- calendar %>%
    as.data.frame(.) %>%
    filter(get(DAY) == "1") %>%
    filter(strptime(start_date, format = "%Y-%m-%d") <= strptime(DATE, format = "%Y%m%d")) %>%
    filter(strptime(end_date, format = "%Y-%m-%d") >= strptime(DATE, format = "%Y%m%d")) %>%
    .$service_id
  
  # update service list for exceptions (but, as per section 2.2, only bus so doesn't matter)
  for (i in 1:nrow(calendar_dates)) {
    if (strptime(calendar_dates[i, "date"][[1]], format = "%Y-%m-%d") == strptime(DATE, format = "%Y%m%d")) {
      if (calendar_dates[i, "exception_type"] == "1") {
        services <- c(services, as.character(calendar_dates[i, 1]))
      }
      if (calendar_dates[i, "exception_type"] == "2") {
        services <- services[!services %in% calendar_dates[i, 1]]
      }
    }
  }
  
  service.table <- rbind(service.table,
                         cbind(date = DATE,
                               service_id = services))
}

# confine to bus services (start with 4_)
bus.service.table <- service.table %>%
  filter(str_sub(service_id, 1, 2) == "4_")

## 1.3 find DART trips for selected dates, from services  ----
## -------------------------------------#
# filter trips to relevant DART bus services (906, 907)
dart.trips <- trips %>%
  left_join(routes, by = "route_id") %>%
  # filter(route_short_name %in% c("905", "906", "907", "908"))
  filter(route_short_name %in% c("906", "907"))

# join service table for selected week to dart trips
# (a trip that runs on multiple days will be selected once for each day)
service.table.dart <- service.table %>%
  inner_join(., dart.trips, by = "service_id") %>%
  dplyr::select(date, service_id, trip_id, trip_headsign, route_short_name)

# find trip time for each dart trip for the selected week
trip.times.dart <- service.table.dart %>%
  # join service.table.dart to stop_times
  inner_join(., stop_times, by = "trip_id") %>%
  
  # extract time for each trip, using first and last in stop sequence
  group_by(date, route_short_name, trip_id) %>%
  summarise(trip_time = departure_time[which.max(stop_sequence)] -
              departure_time[which.min(stop_sequence)]) %>%
  ungroup()

# find number of weekly trips, and total travel time, for each route
route.trips.dart <- trip.times.dart %>%
  group_by(route_short_name) %>%
  summarise(no_services_week = n(),
            total_time_hrs_week = as.numeric(sum(trip_time) / 3600)) %>%
  ungroup() %>%
  # add percentage of hours for each route
  mutate(pct_hrs = total_time_hrs_week / sum(total_time_hrs_week) * 100) %>%
  # calculate share of 35 buses
  mutate(share_35_buses = round(pct_hrs * 35 / 100))

## 1.4 write output  ----
## -------------------------------------#
write.csv(route.trips.dart, "../Tables/dart route share.csv", row.names = FALSE)


# 2. Capacity factors - routes 601, 906 and 907 ----
# -----------------------------------------------------------------------------#
# Based on information from DOT, Kinetic and various online materials
# (including calcuation of the dart route share in section 1):
# - route 601 - initially 5 std; then 4 std 1 artic bus from Jan 2020
# - route 906 - 17 buses, all std other than 1 artic from Oct 2010, 2 artics
#   from Nov 2015 and 3 artic from Oct 2018
# - route 907 - 18 buses, all std other than 1 artic from Oct 2010, 2 artics
#   from Nov 2015 and 3 artic from Oct 2018
# - capacity is 70 for std buses and 110 for artic buses


## 2.1 Set up months for which capacity factors are required  ----
## -------------------------------------#
# relevant months
START.MONTH <- "July 2015"
END.MONTH <- "June 2022"

# first day of 'start.month'
start.date <- as.Date(paste(START.MONTH, "01"), "%B %Y %d")
# last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
end.date <- as.Date(paste(END.MONTH, "01"), "%B %Y %d") %m+% months(1) %m-% days(1)
# 'dates' are the first day of every month from start to end
months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
  strftime(., format = "%Y-%m")


## 2.2 Table of capacity factors  ----
## -------------------------------------#
# base table
cap.factors <- data.frame()

for (i in 1:length(months)) {
  # first month
  if (i == 1) {
    cap.factor.row <- 
      data.frame(month = months[i],
                 route_601 = 1, 
                 route_906 = 1, 
                 route_907 = 1)
  } 
  
  # Nov 2015 - increase in routes 906 and 907
  else if (months[i] == "2015-11") {
    cap.factor.row <- 
      data.frame(month = months[i],
                 route_601 = cap.factors[cap.factors$month == months[i-1], "route_601"],
                 route_906 = cap.factors[cap.factors$month == months[i-1], "route_906"] *
                   (((15 * 70) + (2 * 110)) / ((16 * 70) + (1 * 110))),
                 route_907 = cap.factors[cap.factors$month == months[i-1], "route_907"] *
                   (((16 * 70) + (2 * 110)) / ((17 * 70) + (1 * 110))))
  }
  
  # Oct 2018 - further increase in routes 906 and 907
  else if (months[i] == "2018-10") {
    cap.factor.row <- 
      data.frame(month = months[i],
                 route_601 = cap.factors[cap.factors$month == months[i-1], "route_601"],
                 route_906 = cap.factors[cap.factors$month == months[i-1], "route_906"] *
                   (((14 * 70) + (3 * 110)) / ((15 * 70) + (2 * 110))),
                 route_907 = cap.factors[cap.factors$month == months[i-1], "route_907"] *
                   (((15 * 70) + (3 * 110)) / ((16 * 70) + (2 * 110))))
  }
  
  # Jan 2020 - increase in route 601
  else if (months[i] == "2020-01") {
    cap.factor.row <- 
      data.frame(month = months[i],
                 route_601 = cap.factors[cap.factors$month == months[i-1], "route_601"] *
                   (((4 * 70) + (1 * 110)) / (5 * 70)),
                 route_906 = cap.factors[cap.factors$month == months[i-1], "route_906"],
                 route_907 = cap.factors[cap.factors$month == months[i-1], "route_907"])
  }
  
  # otherwise - same as previous row
  else {
    cap.factor.row <- 
      data.frame(month = months[i],
                 route_601 = cap.factors[cap.factors$month == months[i-1], "route_601"],
                 route_906 = cap.factors[cap.factors$month == months[i-1], "route_906"],
                 route_907 = cap.factors[cap.factors$month == months[i-1], "route_907"])
  }
  
  cap.factors <- rbind(cap.factors,
                       cap.factor.row)
  
}


## 2.3 Write output  ----
## -------------------------------------#
write.csv(cap.factors, "../Tables/bus capacity factors.csv", row.names = FALSE)



# 3. Bus routes by day and stop ----
# -----------------------------------------------------------------------------#
# Script to extract the routes and stops operating on each day from a set
# of GTFS archives

## 3.1 Set up environment ----
## ------------------------------------#
library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(lubridate) # %m+%
library(readr)  # reading & writing .gz files (read_csv, write_csv)
library(tidyr) # pivot_wider
library(doSNOW)  # instead of doParallel, because allows progress reporting
library(parallel)
library(foreach)


GTFS.archive.location <- "../Data/gtfs archive/"


## 3.2 Get list of GTFS archive names/dates ----
## -------------------------------------#
# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


## 3.3 For each archive, extract details and create daily details of services  ----
## -------------------------------------#
# dataframe to hold monthly results
# monthly.details <- data.frame()  ## not currently used

for (i in 1:length(gtfs_file_names)) {
  # for (i in 7:11) {
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
  stop_times.bus <- gtfs$stop_times %>%
    left_join(gtfs$trips, by = "trip_id") %>%
    left_join(gtfs$routes, by = "route_id") %>%
    filter(str_detect(service_id, "^4_")) %>%
    dplyr::select(trip_id, stop_id, service_id, route_short_name)
  
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
  
  
  ### 3.3.1 for each date, find its bus service id's ----
  ### ---------------------------------------------#
  for (j in 1:length(dates)) {
    # date and day of week
    date <- dates[j]
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
      for (k in 1:nrow(calendar_dates.bus)) {
        if (strptime(calendar_dates.bus[k, "date"][[1]], format = "%Y-%m-%d") == date) {
          if (calendar_dates.bus[k, "exception_type"] == "1") {
            services <- c(services, as.character(calendar_dates.bus[k, 1]))
          }
          if (calendar_dates.bus[k, "exception_type"] == "2") {
            services <- services[!services %in% calendar_dates.bus[k, 1]]
          }
        }
      }
    }
    
    
    ### 3.3.2 filter stop_times.bus to find bus services for the day ----
    ### ---------------------------------------------#
    daily.details <- stop_times.bus %>%
      filter(service_id %in% services) %>%
      # mutate(date = date) %>% ### IN FILE NAME
      # dplyr::select(date, stop_id, route_short_name, trip_id)
    dplyr::select(stop_id, route_short_name, trip_id)
    
    ### 3.3.3 write daily details ----
    ### ---------------------------------------------#
    write_csv(daily.details,
              paste0("../Tables/bus daily stops and routes/",
                     as.character(date), 
                     ".csv.gz"))
    
  }
  # ### 3.3.3 [not used] add daily details to monthly results ----
  #   ### ---------------------------------------------#
  #   monthly.details <- rbind(
  #     monthly.details,
  #     daily.details
  #   )
  #   
  #   ### 3.3.4 [not used] when month complete, write monthly results ----
  #   ### ---------------------------------------------#
  #   # check whether end of month (next day is first of month)
  #   if (strftime(date %m+% days(1), format = "%d") == "01") {
  #     
  #     # write monthly results
  #     write_csv(monthly.details,
  #               paste0("../Tables/bus monthly stops and routes/",
  #                      strftime(date, format = "%Y%m"),
  #                      ".csv.gz"))
  #     
  #     # re-set monthly.details to empty
  #     monthly.details <- data.frame()
  #   }
}

# ## 3.4 Combine daily files into monthly [not used] ----
# ## -------------------------------------#
# # Not used, because the monthly files are slower to read in and process
# # than the daily ones
# # reads in the daily stops and combines into monthly tables
# route.stop.location = "../Tables/bus daily stops and routes/"
# start.month = "201504"
# end.month = "202212"
# 
# # first day of 'start.month'
# start.date <- as.Date(paste0(start.month, "01"), "%Y%m%d")
# # last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
# end.date <- as.Date(paste0(end.month, "01"), "%Y%m%d") %m+% months(1) %m-% days(1)
# # 'dates' are the first day of every month from start to end
# bus.months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
#   strftime(., format = "%Y-%m")
# 
# # get the file paths from route.stop.location
# route_stop_file_paths <- dir_ls(route.stop.location) %>%
#   as.character(.)
# # extract the text after the final "/" (that is, the file names)
# route_stop_file_names <- sapply(strsplit(route_stop_file_paths, 
#                                          split = "/", 
#                                          fixed = TRUE), 
#                                 tail, 1L)
# 
# 
# # setup for parallel processing - detect no of available cores and create cluster
# cores <- detectCores()
# cluster <- parallel::makeCluster(cores)
# doSNOW::registerDoSNOW(cluster)
# 
# # report
# print(paste(Sys.time(), "| Finding bus volumes for", length(bus.months), "months; parallel processing with", cores, "cores"))
# 
# # set up progress reporting
# # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
# pb <- txtProgressBar(max = length(bus.months), style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)
# 
# # loop to find daily volumes for each month, and combine as monthly volumes
# output.list <- 
#   foreach(j = 1:length(bus.months),
#           .packages = c("dplyr", "stringr", "readr", "tidyr"),
#           # .combine = list, # resulted in multiple lists being combined - don't use!
#           # .verbose = TRUE,
#           .options.snow = opts) %dopar% {
#             # create a temp directory (so that it can be cleared of the temp uncompressed
#             # files on each iteration - otherwise leads to crash)
#             temp_dir <- tempdir()
#             
#             # year and month (for display)
#             year.month <- strftime(as.Date(paste0(bus.months[j], "-01", "%Y-%m-%d")), format = "%Y %B")
#             
#             # paths to daily details for the month
#             month.file.paths <- route_stop_file_paths[str_detect(route_stop_file_paths, bus.months[j])]
#             
#             # empty dataframe to hold monthly details
#             monthly.details <- data.frame()
#             
#             # find volumes for each day and bind into monthly details
#             for (k in 1:length(month.file.paths)) {
#               # read in the daily details, and add date
#               daily.details <- read_csv(month.file.paths[k], 
#                                         show_col_types = FALSE) %>%
#                 # date is all digits (see https://stackoverflow.com/questions/54099535/how-to-extract-number-from-character-string)
#                 mutate(date = gsub("\\D", "", month.file.paths[k]))  
#               
#               monthly.details <- rbind(monthly.details, daily.details)
#             }
#             
#             # remove temporary files created for the month (to avoid crash)
#             unlink(list.files(temp_dir, full.names = T))
#             
#             # write the monthly details
#             write_csv(monthly.details,
#                       paste0("../Tables/bus monthly stops and routes/",
#                              as.character(bus.months[j]), 
#                              ".csv.gz"))
#             
#             # return number only (to avoid crashing by returning list
#             # of large files that have already been written to disk)
#             return(j)
#             
#           }
# 
# # close the progress bar and cluster
# close(pb)
# stopCluster(cluster)


# 4. Processing daily bus routes by day and stop ----
# -----------------------------------------------------------------------------#
# Function to find total number of services, by month, for a given set of stops
# Has options to include separate totals for each route, as well as overall totals

bus.services <- function(route.stop.location, # folder containing daily route and stop .gz files
                         filtered.stops = F,  # either F (for all stops) or a vector of stops, eg c("941", "942")
                         include.routes = T,  # if T, will include totals for individual routes; if F, grand total only
                         start.month = "201504",  # YYYYmm format, should match first month in route.stop.location, or later
                         end.month = "202212") {  # YYYYmm format, should match last month in route.stop.location, or earlier
  
  # route.stop.location = "../Tables/bus daily stops and routes/"  #<< parameter
  # filtered.stops = F #<< parameter
  # start.month = "201507"   #<< parameter
  # end.month = "201509" #<< parameter
  # include.routes = T
  # 
  # ## testing
  # # filtered.stops = "941"   #<< parameter
  # filtered.stops = c("941", "942", "943")   #<< parameter

  
  ## 1 Set up environment ----
  ## ------------------------------------#
  library(dplyr)
  library(stringr)
  library(tidytransit)  # for reading GTFS ??used??
  library(fs)  # dir_ls
  library(lubridate) # %m+%
  library(readr)  # reading & writing .gz files (write_csv)
  library(tidyr)  # pivoting
  
  # output table to be returned as results
  output.table <- data.frame()
  
  # create a temp directory (so that it can be cleared of the temp uncompressed
  # files on each iteration - otherwise leads to crash)
  temp_dir <- tempdir()
  
  
  ## 2 Get list of stop & route file names/dates ----
  ## -------------------------------------#
  # get the file paths from route.stop.location
  route_stop_file_paths <- dir_ls(route.stop.location) %>%
    as.character(.)
  # extract the text after the final "/" (that is, the file names)
  route_stop_file_names <- sapply(strsplit(route_stop_file_paths, 
                                           split = "/", 
                                           fixed = TRUE), 
                                  tail, 1L)
  
  
  ## 3 For each month, find the daily volumes ----
  ## -------------------------------------#
  # first day of 'start.month'
  start.date <- as.Date(paste0(start.month, "01"), "%Y%m%d")
  # last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
  end.date <- as.Date(paste0(end.month, "01"), "%Y%m%d") %m+% months(1) %m-% days(1)
  # 'dates' are the first day of every month from start to end
  months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
    strftime(., format = "%Y-%m")
  
  for (i in 1:length(months)) {
    # year and month (for display)
    year.month <- strftime(as.Date(paste0(months[i], "-01", "%Y-%m-%d")), format = "%Y %B")

    # show progress
    cat(as.character(Sys.time()), "|", "Finding bus services for", year.month, "\n")

    # paths to daily details for the month
    month.file.paths <- route_stop_file_paths[str_detect(route_stop_file_paths, months[i])]

    # empty dataframe to hold monthly volumes
    monthly.volumes <- data.frame()

    for (j in 1:length(month.file.paths)) {
      # read in the daily details
      daily.details <- read_csv(month.file.paths[j], show_col_types = FALSE)

      # filter to required stops, unless filtered.stops=F
      if (!isFALSE(filtered.stops)) {
        daily.details <- daily.details %>%
          filter(stop_id %in% filtered.stops)
      }


      if (include.routes) {  # where finding individual route totals

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

      } else {  # where not finding individual route totals

        # find daily volumes
        daily.volumes <- daily.details %>%
          summarise(total_services = n_distinct(trip_id))

      }

      # add to monthly volumes
      monthly.volumes <- bind_rows(monthly.volumes,
                                   daily.volumes)
    }

    ## 4 Summarise daily totals for month, and prepare for display ----
    ## -------------------------------------#
    monthly.totals <- monthly.volumes %>%
      # collapse into total for month
      summarise_all(sum, na.rm = T) %>%
      # add month and year %>%
      cbind(period = year.month, .)

    # add to output table
    output.table <- bind_rows(output.table,
                              monthly.totals)
  
    # remove temporary files created for the month (to avoid crash)
    unlink(list.files(temp_dir, full.names = T))
    }
  
  # reorder columns for display
  output.table <- output.table %>%
    # order alphabetically ('period', then 'routes' (numerically), then 'total_services')
    select(order(colnames(output.table)))
  
  return(output.table)
}



# test1 <- bus.services(route.stop.location = "../Tables/bus daily stops and routes/",
#                       filtered.stops = c("941", "942", "943"),
#                       include.routes = T,  # if T, will include totals for individual routes; if F, grand total only
#                       start.month = "201504",  # YYYY/mm format, should match first month in route.stop.location, or later
#                       end.month = "201505")
# 
# test2 <- bus.services(route.stop.location = "../Tables/bus daily stops and routes/",
#                       filtered.stops = c("941", "942", "943"),
#                       include.routes = F,  # if T, will include totals for individual routes; if F, grand total only
#                       start.month = "201504",  # YYYY/mm format, should match first month in route.stop.location, or later
#                       end.month = "201505")

monthly.totals.all.routes <- bus.services(route.stop.location = "../Tables/bus daily stops and routes/",
                                          filtered.stops = F,
                                          include.routes = T)

write_csv(monthly.totals.all.routes, "../Tables/bus monthly totals all routes.csv")



# 5. Master list of bus stops ----
# -----------------------------------------------------------------------------#
# Creates a master list of bus stops, drawn from GTFS archive

## 5.1 Set up environment ----
## ------------------------------------#
library(dplyr)
library(stringr)
library(tidytransit)  # for reading GTFS
library(fs)  # dir_ls
library(sf)

GTFS.archive.location <- "../Data/gtfs archive/"


## 5.2 Get list of GTFS archive names/dates ----
## -------------------------------------#
# get the file paths from GTFS.archive.location
gtfs_file_paths <- dir_ls(GTFS.archive.location) %>%
  as.character(.) %>%
  # only the .zip files (exclude any metadata file)
  subset(str_detect(., "zip"))
# extract the text after the final "/" (that is, the file names)
gtfs_file_names <- sapply(strsplit(gtfs_file_paths, split = "/", fixed = TRUE), tail, 1L)


## 5.3 Combine stops from all archives  ----
## -------------------------------------#
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
    distinct() %>%
    # add archive date
    mutate(archive_date = str_extract(gtfs_file_names[i], "[[:digit:]]+"))

  # add the stops to the master stop.list
  stop.list <- rbind(stop.list,
                     stops.bus)
  
}

## 5.4 Keep just the latest version of each stop  ----
## -------------------------------------#
# See section 3 of bus cleaning checks.R for examination of whether stop_ids 
# are distinct, and conclusion that they are.  Some stops do move location
# somewhat, but there is no recycling of disused stop numbers to 
# unrelated locations
# Accordingly, conclusion is that they can be relied on as distinct, and
# the most recent iteration can be kept
# There are sometimes errors in location (eg lat or lon that is completely
# wrong - hopefully these are more likely to affect earlier than later, but
# not checked)

stop.list.latest <- stop.list %>%
  group_by(stop_id) %>%
  filter(archive_date == max(archive_date))

# # checking the distribution of stop.list.latest
# stop.list.latest.dist <- stop.list.latest %>%
#   group_by(archive_date) %>%
#   summarise(n = n())
# 18649 out of 20135 are from 20221125

## 5.5 Convert to sf and save  ----
## -------------------------------------#
stop.list.sf <- st_as_sf(stop.list.latest, 
                         coords = c("stop_lon", "stop_lat"),
                         crs = 4326) %>%
  st_transform(7899)

st_write(stop.list.sf, "../GIS/bus stop list.sqlite", delete_layer = TRUE)


