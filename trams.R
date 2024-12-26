#==============================================================================#
#   Investigation of tram data
#
#   Steve Pemberton, [November 2022]
#
# 
#   Organised as follows [review and expand as needed]
#   1  Tram capacity factors
#   2  
#   3  
#   4  
#
#==============================================================================#




# 1. Tram capacity factors ----
# -----------------------------------------------------------------------------#
# set up environment
library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)  # separate, unite
library(lubridate)

fleet.file <- "../Tables/tram fleet by route.xlsx"


## 1.1 read in data and add capacities for unclear classes ----
## -------------------------------------#
# fleet allocation by route (manually compiled from Yarra Trams allocation files)
fleet.alloc <- read.xlsx(fleet.file, sheet = "fleet allocation") %>%
  # dates had been read in as integers from excel; for origin, see R Help for 'as.Date')
  mutate(start_date = as.Date(start_date, origin = "1899-12-30"))

# class capacity
class.cap <- read.xlsx(fleet.file, sheet = "class capacity") %>%
  # omit last two information rows
  slice(1:(n()-2)) %>%
  # classes to lower case
  mutate(class = tolower(class)) %>%
  # add alternative class names
  rbind(.,
        c("z3", as.numeric(.[.$class == "z", "capacity"])),
        c("e1", as.numeric(.[.$class == "e", "capacity"])))


## 1.2 adjust allocations for missing routes or data ----
## -------------------------------------#

### 1.2.1 routes  22 and 69 ----
### -----------------------------------#
# Routes 22 and 69 ran before 17/10/04 (before first fleet data), then
# merged into routes 8 and 16 respectively.  
# Route 8's first fleet allocation is 28z and 13b; split into former
# route route 8  served from Brunswick (11z + 13b) and route 22 from Malvern (100% z) 
# Route 16's first fleet allocation is 100% z; apply same to route 69

# Create new rows for 1/7/03 (start date) and 17/10/04 (reorganisation date),
# same as first fleet allocation except for affected routes
route.22.69.row1 <- fleet.alloc[1, ] %>%
  mutate(start_date = as.Date("2003-07-01"),
         mtt_ref = "added",
         route_8 = "11 z, 13 b", 
         route_22 = "z",
         route_69 = route_16)
route.22.69.row2 <- route.22.69.row1 %>%
  mutate(start_date = as.Date("2004-10-17"),
         mtt_ref = "added",
         route_8 = "28 z, 13 b",
         route_22 = NA,
         route_69 = NA)

# Bind with fleet.alloc
fleet.alloc <- bind_rows(route.22.69.row1, route.22.69.row2, fleet.alloc)


### 1.2.2 routes 24 and 48 ----
### -----------------------------------#
# DoT service volume figures treat route 24 as a child of route 48, so 
# figures need to be combined
# Route 24 is always all 'a', while route 48 is either all 'a', or 'a' and 'c1'
# For combination purposes, base spreadsheet has been completed with the number
# of 'a's specified (even though number is not usually specified for 100% one class)
fleet.alloc <- fleet.alloc %>%
  # separate out the route 48 classes
  separate(col = route_48, into = c("route_48_a", "route_48_c1"), sep = ", ") %>%
  # new column for the combined 'a' class (taking just the number before the space)
  rowwise() %>%
  mutate(combined_a = sum(as.numeric(str_split(route_24, " ")[[1]]), 
                          as.numeric(str_split(route_48_a, " ")[[1]]), 
                          na.rm = TRUE)) %>%
  mutate(combined_a = ifelse(combined_a > 0, 
                              paste(as.character(combined_a), "a"),
                              NA)) %>%
  # re-join total a's to route 48 c's
  unite(col = route_48, c("combined_a", "route_48_c1"), sep = ", ", na.rm = TRUE) %>%
  # remove the number if only one class [if split at comma produces only one result,
  # then there is only one class, so just keep the part after the space (ie the class)]
  rowwise() %>%
  mutate(route_48 = ifelse(length(unlist(str_split(route_48, ", "))) == 1,
                                 unlist(str_split(route_48, " "))[2],
                                 route_48)) %>%
  ungroup() %>%
  # remove route_24, and newly-created route_48_a (other new columns have disappeared in 'unite')
  select(-route_24, -route_48_a)


### 1.2.3 route 68 ----
### -----------------------------------#
# Route 68 ran to Aug-05 as Sunday-only version of route 55
# Apply route 55 allocation to route 68 (can apply to all times, even though did
# not run after Aug-05, as there will simply be no volumes after that month)
fleet.alloc <- fleet.alloc %>%
  mutate(route_68 = route_55)


### 1.2.4 MTT15a and MTT17a allocations ----
### -----------------------------------#
# MTT15a is missing allocations for several routes, omits c2 and e class, and 
#   contains several other anomalous results.  
# MTT17a does not distinguish between a/z and b/c1 and c2/e classes, though in
#   most cases the position is obvious from other timetables.
# Assumptions are made for the gaps in the 'MTT15a and MTT17a adjustments' tab
#   of 'tram fleet by route.xlsx', and implemented below.

# routes 1, 11, 12, 19, 30, 35, 35, 48, 70, 75, 86, 109 - MTT15a assumed same as MTT17a
selected.routes <- c(1, 11, 12, 19, 30, 35, 35, 48, 70, 75, 86, 109)
for (i in 1:length(selected.routes)) {
  route.field <- paste0("route_", selected.routes[i])
  fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 15a", route.field] <- 
    fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 17a", route.field]
}

# route 3 - MTT17a assumed to be 12 z, 5 a, 8 b
fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 17a", "route_3"] <- "12 z, 5 a, 8 b"

# route 8 - MTT assumed same as MTT11
fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 15a", "route_8"] <- 
  fleet.alloc[fleet.alloc$mtt_ref == "MR3 / 11", "route_8"]

# route 96 - MTT15a assumed to be 6 c2, 28 e; MTT17a assumed to be 6 c2, 36 e
fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 15a", "route_96"] <- "6 c2, 38 e"
fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 17a", "route_96"] <- "6 c2, 36 e"


### 1.2.5 other adjustments ----
### -----------------------------------#
# Various other allocations are missing; see 'Route operating periods' tab of 
# 'tram route changes.xlsx' for review and assumptions that are implemented below

# route 3a assumed same as route 3; route 5s assumed same as route 5
fleet.alloc <- fleet.alloc %>%
  mutate(route_3a = route_3,
         route_5s = route_5)

# routes 8 and 55 - MTT17a assumed same as MTT15a
selected.routes <- c(8, 55)
for (i in 1:length(selected.routes)) {
  route.field <- paste0("route_", selected.routes[i])
  fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 17a", route.field] <- 
    fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 15a", route.field]
}

# routes 11 and 12  - MTT11 assumed same as MTT15a
selected.routes <- c(11, 12)
for (i in 1:length(selected.routes)) {
  route.field <- paste0("route_", selected.routes[i])
  fleet.alloc[fleet.alloc$mtt_ref == "MR3 / 11", route.field] <- 
    fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 15a", route.field]
}

# route 31 - MTT27, 2, 3, 4-5 & 11 assumed same as MTT23 ('a' class)
fleet.alloc[fleet.alloc$mtt_ref %in% c("MR3 / 27", "MR3 / 2", "MR3 / 3",
                                       "MR3 / 4-5", "MR3 / 11"), "route_31"] <-
  fleet.alloc[fleet.alloc$mtt_ref == "MR3 / 23", "route_31"]

# route 35 - MTT18 & 19a assumed same as MTT17a ('w' class)
fleet.alloc[fleet.alloc$mtt_ref %in% c("MR4 / 18", "MR4 / 19a"), "route_35"] <-
  fleet.alloc[fleet.alloc$mtt_ref == "MR4 / 17a", "route_35"]

# route 95 - assumed to be same as route 86 to MTT11, and NA thereafter
fleet.alloc <- fleet.alloc %>%
  mutate(route_95 = ifelse(start_date <= as.Date("2012-04-22"),
                           route_86,
                           NA))


## 1.3 make table of average capacities for each timetable ----
## -------------------------------------#
# function to find average capacity of classes operating on a route
find.ave.cap <- function(route.classes) {
  # extract the classes from 'route classes' as a vector of no/class pairs, 
  # eg. "5 z, 63 d1, 10 d2" >> "5 z", "63 d1", "10 d2"
  classes <- str_split(route.classes, ", ")[[1]]  
  
  # find the average capacity
  if (length(classes) == 1) {
    # only one class: record its capacity from class.cap
    ave.cap <- as.numeric(class.cap[class.cap$class == classes, "capacity"])
  } else {
    # more than one class: table to calculate average capacity
    df <- data.frame()
    # loop through each class
    for (i in 1:length(classes)) {
      # number of the relevant class
      number <- as.numeric(str_split(classes[i], " ")[[1]][1])
      # class
      class <- str_split(classes[i], " ")[[1]][2]
      # total cap (number * capacity for class)
      total.cap <- number * as.numeric(class.cap[class.cap$class == class, "capacity"])
      # add to table
      df <- rbind(df,
                  c(number, class, total.cap))
      # name columns
      colnames(df) <- c("number", "class", "total.cap")
      
    }
    # average capacity is total capacity divided by number of trams
    ave.cap <- sum(as.numeric(df$total.cap)) / sum(as.numeric(df$number))
  }
  
  return(ave.cap)
}

# create capacities as a copy of fleet.alloc, but with blank cells
capacities <- fleet.alloc[, 1:2]
route.names <- names(fleet.alloc)[3:length(names(fleet.alloc))]
for (i in 1:length(route.names)) {
  capacities[, route.names[i]] <- as.numeric(NA) 
}

# for each cell in table, if not NA, then find ave cap
for (i in 1:nrow(fleet.alloc)) {
  for (j in 3:ncol(fleet.alloc)) {  # omit first 2 columns (which are not routes)
    if (!(is.na(fleet.alloc[i, j]))) {  # exclude NAs
      capacities[i, j] <- find.ave.cap(fleet.alloc[i, j])
    }
  }
}


## 1.4 make table of capacity factors for each timetable ----
## -------------------------------------#
# capacity factor is ave.cap[n] / ave.cap[1], where ave.cap[n] is the
# capacity for the relevant timetable, and ave.cap[1] is the capacity
# for the first allocation for a route

# find first capacity allocation for each route

# vector of route-name columns from fleet.alloc (route_1, route_3, etc)
route.cols <- names(fleet.alloc)[str_detect(names(fleet.alloc), "route")]

# vector of first capacity allocations
first.caps <- c()

for (i in 1:length(route.cols)) {
  # contents of the column
  cap <- capacities %>%
    # select column with the relevant name (route_1, etc)
    dplyr::select(any_of(route.cols[i])) %>%
    # omit any NAs
    na.omit() %>%
    # as vector
    .[[1]] %>%
    # first in the vector
    .[1]
  
  # add to first caps
  first.caps <- c(first.caps, cap)
}

# combine into table of first capacity figures
routes.first.caps <- data.frame(cbind(route.cols,
                                      first.caps))


# function to find capacity factor
find.cap.factor <- function(col.name, route.cap) {
  # find the first capacity figure for a given route column
  first.cap <- as.numeric(routes.first.caps[routes.first.caps$route.cols == col.name, "first.caps"])
  
  return(route.cap / first.cap)
}

capacity.factors <- capacities

# for each cell in table, if not NA, then find cap factor
for (i in 1:nrow(capacity.factors)) {
  for (j in 3:ncol(capacity.factors)) {  # omit first 2 columns (which are not routes)
    if (!(is.na(capacity.factors[i, j]))) {  # exclude NAs
      capacity.factors[i, j] <- find.cap.factor(names(capacity.factors)[j], 
                                                as.numeric(capacity.factors[i, j]))
    }
  }
}

# convert route columns to date and numeric as required 
capacity.factors <- capacity.factors %>%
  # dates had been read in as integers from excel; for origin, see R Help for 'as.Date')
  mutate(start_date = as.Date(start_date, origin = "1899-12-30")) %>%
  mutate(across(any_of(route.cols), ~ as.numeric(.x)))


## 1.5 adjust capacity factors for route replacements ----
## -------------------------------------#
# Capacity factors for routes 11 & 12 to continue from 112, and 58 from 8 & 55,
# and 71 from 31

# route 11 & 12 capacity factors continue from 112, so that '1' in first 
# year of route 11/12 is same as final route 112, and following are adjusted accordingly
final.route.112 <- unlist(capacity.factors[capacity.factors$mtt_ref == "MR3 / 11", "route_112"])
capacity.factors <- capacity.factors %>%
  mutate(route_11 = route_11 * final.route.112,
         route_12 = route_12 * final.route.112)

# route 58 capacity factor continues from average of 8 and 55, so that '1' in first
# year of route 58 is same as average of final route 8 and 55, and following
# are adjusted accordingly
final.route.8.55 <- mean(c(unlist(capacity.factors[capacity.factors$mtt_ref == "MR4 / 17a",
                                                   "route_8"]),
                           unlist(capacity.factors[capacity.factors$mtt_ref == "MR4 / 17a",
                                                   "route_55"])))
capacity.factors <- capacity.factors %>%
  mutate(route_58 = route_58 * final.route.8.55)

# route 71 capacity factor continues from 31 (from Sep 09, when route 71 temporarily
# replaced route 31), so that '1' in first year of route 71 is same as 
# Sep 09 version of route 31, and following are adjusted accordingly
final.route.31 <- unlist(capacity.factors[capacity.factors$mtt_ref == "MR3 / 27", "route_31"])
capacity.factors <- capacity.factors %>%
  mutate(route_71 = route_71 * final.route.31)


## 1.6 make table of capacity factors for each month ----
## -------------------------------------#
# relevant months
START.MONTH <- "July 2003"
END.MONTH <- "June 2022"

# first day of 'start.month'
start.date <- as.Date(paste(START.MONTH, "01"), "%B %Y %d")
# last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
end.date <- as.Date(paste(END.MONTH, "01"), "%B %Y %d") %m+% months(1) %m-% days(1)
# 'dates' are the first day of every month from start to end
months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
  strftime(., format = "%Y-%m")

# add and complete end date column in capacity factors
capacity.factors.dates <- capacity.factors %>%
  # add end_date, one day before next start_date
  mutate(end_date = lead(start_date) - 1) %>%
  # last end date is NA - complete as end date
  mutate(end_date = if_else(is.na(end_date),
                           end.date, 
                           end_date))

# data frame to hold outputs
capacity.factors.months <- data.frame()

# identify timetables for each month and number of days for which they are in effect 
for (i in 1:length(months)) {
  # find start and end dates of month
  month.start <- as.Date(paste0(months[i], "-01"))
  month.end <- month.start %m+% months(1) %m-% days(1)
  
  # if month starts and ends before first timetable, use first timetable
  # (this won't happen assuming start.date is in fact date of first timetable)
  if (month.end < unlist(capacity.factors.dates[1, "start_date"])) {
    timetables <- capacity.factors.dates[1,] %>%
      # and 'days' will be full year
      mutate(days = month.end - month.start + 1)  # if you don't add 1, it only records (say) 30 days for a 31 day month
  } else {
    # select the relevant capacity factor timetables
    timetables <- capacity.factors.dates %>%
      filter(
        # starts within month
        start_date >= month.start & start_date <= month.end |
          # ends within month
          end_date >= month.start & end_date <= month.end |
          # covers entire month
          start_date < month.start & end_date > month.end
      ) %>%
      # complete end_date as month.end if NA
      mutate(end_date = case_when(
        is.na(end_date) ~ month.end,
        TRUE            ~ end_date
      ))
    
    # change first start_date to month.start if first timetable starts during month
    # (which won't happen if start.date is in fact date of first timetable)
    if (unlist(timetables[1, "start_date"]) > month.start) {
      timetables[1, "start_date"] <- month.start
    } 
    
    # add number of days that relevant timetable is in effect
    timetables <- timetables %>%
      mutate(days = case_when(
        # starts before month and ends within month
        start_date < month.start & end_date <= month.end  ~ end_date - month.start + 1,
        # starts and ends within month
        start_date >= month.start & end_date <= month.end ~ end_date - start_date + 1,
        # starts within month and ends after month
        start_date >= month.start & end_date > month.end  ~ month.end - start_date + 1,
        # covers entire month
        start_date < month.start & end_date > month.end   ~ month.end - month.start + 1
      ))
  }
  # calculate weighted average route capacity factors
  cap.factors <- timetables %>%
    summarise(across(any_of(route.cols), ~ weighted.mean(.x, as.numeric(days), na.rm = TRUE)))
  
  # add month and organise
  cap.factors <- cap.factors %>%
    mutate(month = months[i]) %>%
    dplyr::select(month, any_of(route.cols))
  
  # add the result to the output
  capacity.factors.months <- rbind(capacity.factors.months,
                                  cap.factors)
}

# remove 'NaN' cells
capacity.factors.months[capacity.factors.months == "NaN"] <- NA
  

## 1.7 save outputs back to excel file ----
## -------------------------------------#
# see https://community.rstudio.com/t/how-to-append-a-new-sheet-to-existing-workbook/90755

# load file as 'workbook'
wb <- loadWorkbook(fleet.file)

# create the required sheet names (empty) if they doesn't already exist
if (!("timetable capacity factors" %in% getSheetNames(fleet.file))) {
  addWorksheet(wb, sheetName = "timetable capacity factors")
}

if (!("monthly capacity factors" %in% getSheetNames(fleet.file))) {
  addWorksheet(wb, sheetName = "monthly capacity factors")
}

# write the desired content of the sheets to the workbook
writeData(wb, sheet = "timetable capacity factors", capacity.factors)
writeData(wb, sheet = "monthly capacity factors", capacity.factors.months)

# write the workbook back to the file
saveWorkbook(wb, fleet.file, overwrite = TRUE)


# 2. Tram routes by stop and month, taking account of changes ----
# -----------------------------------------------------------------------------#
# Produces a table with 'stop' and 'route' columns for every stop/route
# combination, plus a column for each month, which is either 0 (if no service),
# 1 (if service for full month), or a fraction (if service for part month)

# Based on stops as of current timetable, plus extra for Toorak Rd alteration
# Changes are compiled in Tables/tram route changes.xlsx (see 'Summary' sheet)

# set up environment
library(dplyr)
library(stringr)
library(tidytransit)
library(sf)

# zip folder for PTV GTFS data
# from https://transitfeeds.com/p/ptv/497 , downloaded 3 Oct 2022
GTFS <- "../Data/gtfs_20220929.zip"


## 2.1 read in current GTFS data, and compile base list of stops/routes ----
## -------------------------------------#
gtfs <- read_gtfs(GTFS) %>%
  gtfs_as_sf(., crs = 4326)

# agency <- gtfs$agency
# calendar <- gtfs$calendar
# calendar_dates <- gtfs$calendar_dates
# routes <- gtfs$routes
# shapes <- gtfs$shapes %>%
#   st_transform(7899)
# stop_times <- gtfs$stop_times
# stops <- gtfs$stops %>%
#   st_transform(7899)
# trips <- gtfs$trips

# table of stops and routes
stops.routes.base <- gtfs$stop_times %>%
  left_join(gtfs$trips, by = "trip_id") %>%
  left_join(gtfs$routes, by = "route_id") %>%
  # confine to trams
  filter(str_detect(service_id, "^3_")) %>% 
  # keep only distinct stop_id and route no combinations
  dplyr::select(stop = stop_id, 
                route = route_short_name) %>%
  distinct()

# # one off step for initial setup - current routes -
# # write current tram stops to file for review in QGIS (once-off step)
# # (also includes current route no's for the stops - so some stops are duplicated
# # where the stop serves multiple routes
# st_write(gtfs$stops %>%
#            st_transform(7899) %>%
#            inner_join(stops.routes.base, by = c("stop_id" = "stop")) %>%
#            rename(stop = stop_id),
#          "../GIS/current tram stops.sqlite",
#          delete_layer = TRUE)
# 
# # one off step for initial setup - route 58 realignment -
# # write former tram stops to file for addition of former Toorak Rd stops
# formerGTFS <- read_gtfs("../Data/gtfs archive/gtfs_20150330.zip") %>%
#   gtfs_as_sf(., crs = 4326)
# 
# formerstops <- formerGTFS$stops %>%
#   st_transform(7899)
# 
# formerstoptable <- formerstops %>%
#   left_join(formerGTFS$stop_times, by = "stop_id") %>%
#   left_join(formerGTFS$trips, by = "trip_id") %>%
#   left_join(formerGTFS$routes, by = "route_id") %>%
#   # confine to trams
#   filter(str_detect(service_id, "^3_")) %>%
#   # keep only distinct stop_id and route no combinations
#   dplyr::select(stop = stop_id,
#                 route = route_short_name) %>%
#   # drop geometry and find distinct stops/routes only, then re-join geometry
#   # (because 'distinct' takes too long if geometry is retained)
#   st_drop_geometry() %>%
#   distinct() %>%
#   left_join(formerstops, by = c("stop" = "stop_id"))
# 
# st_write(formerstoptable,
#          "../GIS/tram stops 2015.sqlite",
#          delete_layer = TRUE)


# save stops.routes.base table (temp, can be deleted later)
# write.csv(stops.routes.base, "../Tables/tram stops routes base (temp).csv", 
#           row.names = FALSE)


## 2.2 read in tram stop details ----
## -------------------------------------#
# read in separate file containing details of groups of tram stops affected
# by route changes (compiled manually from selecting stops in QGIS)
# and 'to_date' helper function

source("./tram stop details.R")
source("./functions/dates.R")

## 2.3 add stop/route rows for non-current stops and relocated/split routes ----
## -------------------------------------#
# read in stops.routes.base (temp, can be deleted later)
# stops.routes.base <- read.csv("../Tables/tram stops routes base (temp).csv")

stops.routes <- stops.routes.base

### 2.3.1 route 3 - divided into 3 and 3a ----
# all 3/3a stops
route.3.3a.stops <- stops.routes %>%
  filter(route == "3/3a") %>%
  .$stop %>% 
  sort()

# add rows for route 3 stops
for (i in 1:length(setdiff(route.3.3a.stops, route.3a.only.stops))) {
  stops.routes <- rbind(stops.routes,
                        c(setdiff(route.3.3a.stops, route.3a.only.stops)[i], "3"))
}

# add rows for route 3a stops
for (i in 1:length(setdiff(route.3.3a.stops, route.3.only.stops))) {
  stops.routes <- rbind(stops.routes,
                        c(setdiff(route.3.3a.stops, route.3.only.stops)[i], "3a"))
}

# remove original 3/3a stops
stops.routes <- stops.routes %>%
  filter(route != "3/3a")


### 2.3.2 route 5s - evening shuttle ----
for (i in 1:length(route.5s.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.5s.stops[i], "5s"))
}


### 2.3.3 route 8 - replaced by route 6, Domain to Moreland ----
for (i in 1:length(route.8.to.route.6.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.8.to.route.6.stops[i], "8"))
}


### 2.3.4 route 8 - replaced by route 58, Domain to Toorak ----
for (i in 1:length(route.8.to.route.58.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.8.to.route.58.stops[i], "8"))
}

### 2.3.5 Flinders St overpass works temporary route 13 - city shuttle ----
for (i in 1:length(route.13.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.13.stops[i], "13"))
}


### 2.3.6 route 16 - former Acland St terminus ----
for (i in 1:length(route.16.old.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.16.old.stops[i], "16"))
}


### 2.3.7 former route 24 ----
for (i in 1:length(route.24.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.24.stops[i], "24"))
}


### 2.3.8 route 24 - flinders st overpass harbour esp extension ----
for (i in 1:length(route.24.harbour.esp.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.24.harbour.esp.stops[i], "24"))
}


### 2.3.9 route 22 - replaced by route 8, Arts Centre to Moreland ----
for (i in 1:length(route.22.to.route.8.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.22.to.route.8.stops[i], "22"))
}


### 2.3.10 route 30 - former Waterfront City terminus ----
for (i in 1:length(route.30.86.waterfront.city.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.30.86.waterfront.city.stops[i], "30"))
}


### 2.3.11 former route 31 ----
for (i in 1:length(route.31.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.31.stops[i], "31"))
}


### 2.3.12 route 48 - change from Flinders St to Collins St ----
for (i in 1:length(route.48.flinders.st.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.48.flinders.st.stops[i], "48"))
}


### 2.3.13 former route 50 ----
for (i in 1:length(route.50.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.50.stops[i], "50"))
}


### 2.3.14 Cth games temporary route 53 - East Malvern to West Coburg ----
for (i in 1:length(route.53.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.53.stops[i], "53"))
}


### 2.3.15 route 55 - replaced by route 58, Domain to West Coburg ----
for (i in 1:length(route.55.to.route.58.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.55.to.route.58.stops[i], "55"))
}


### 2.3.16 route 55 extra Cth games stops - Malvern to Domain
for (i in 1:length(route.55.cth.games.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.55.cth.games.stops[i], "55"))
}


### 2.3.17 route 58 - Toorak Rd realignment ----
for (i in 1:length(route.58.old.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.58.old.stops[i], "58"))
}


### 2.3.18 former route 68 ----
for (i in 1:length(route.68.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.68.stops[i], "68"))
}


### 2.3.19 route 69 - replaced by route 16, Kew to St Kilda (Carlisle St) ----
for (i in 1:length(route.69.to.route.16.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.69.to.route.16.stops[i], "69"))
}


### 2.3.20 route 70 - change from Spencer St to Harbour Esp ----
for (i in 1:length(route.70.75.spencer.st.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.70.75.spencer.st.stops[i], "70"))
}


### 2.3.21 former route 71 ----
for (i in 1:length(route.71.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.71.stops[i], "71"))
}


### 2.3.22 route 75 - change from Spencer St to Harbour Esp ----
for (i in 1:length(route.70.75.spencer.st.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.70.75.spencer.st.stops[i], "75"))
}


### 2.3.23 former route 79 ----
for (i in 1:length(route.79.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.79.stops[i], "79"))
}


### 2.3.24 former route 95 ----
for (i in 1:length(route.95.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.95.stops[i], "95"))
}


### 2.3.25 route 112 - replaced by routes 11 & 12 ----
for (i in 1:length(route.112.to.11.12.stops)) {
  stops.routes <- rbind(stops.routes,
                        c(route.112.to.11.12.stops[i], "112"))
}



## 2.4 remove stops not forming part of normal route ----
## -------------------------------------#
# e.g. depot diversions

### 2.4.1 route 1 - William St diversion
stops.routes <- stops.routes[!(stops.routes$route == 1 & 
                                 stops.routes$stop %in% route.1.unused.stops), ]


### 2.4.2 route 12 - depot diversion
stops.routes <- stops.routes[!(stops.routes$route == 12 & 
                                 stops.routes$stop %in% route.12.unused.stops), ]


### 2.4.3 route 57 - Flemington Rd diversion
stops.routes <- stops.routes[!(stops.routes$route == 57 & 
                                 stops.routes$stop %in% route.57.unused.stops), ]


### 2.4.4 route 75 - La Trobe St diversion
stops.routes <- stops.routes[!(stops.routes$route == 75 & 
                                 stops.routes$stop %in% route.75.unused.stops), ]



## 2.4 populate month columns, initially with 1 ----
## -------------------------------------#
# all columns will be filled with 1, unless overridden by a change in section 2.5
# note that '1' does not necessarily imply that the tram runs - the route
# may have been abandonned (in which case the volume for the route will be zero)

# sequence of dates for FY 2004 to 2021, in format Jul_2003
# to be added as columns in the stops.routes table
start.date <- as.Date("20030701", format = "%Y%m%d")
end.date <- as.Date("20220630", format = "%Y%m%d")
dates <- seq.Date(from = start.date, to = end.date, by = "month") %>%
  format(., "%b_%Y")

# add a new column for each month, populated with '1'
for (i in 1:length(dates)) {
  stops.routes[, dates[i]] <- 1
}



## 2.5 switch stops off for route changes ----
## -------------------------------------#
# switches follow the following pattern:
## - subset to rows with the given route number and stops in the given group
## - subset to columns, other than "stop" and "route", within the given date range,
## - and set the value to zero (if stop not used for the month), 
##   or a fraction (if stop used for part of the month)

# Note that two sets of stop switches have been coded below but commented out:
## - Flinders St overpass works
## - extra Cth Games stops
# This is because the DoT service volumes are not compatible with implementing
#   these changes.  In some cases, alternative coding is used (for example, where
#   additional stops have been added for one of these changes but are not used at 
#   all).
# If these 'NOT USED' sections were commented back in, the 'ALTERNATIVE' sections
#   should be commented out.


### 2.5.1 route 6 - extended Melbourne Uni to Moreland ----
# route 6 extended from Melbourne Uni to Moreland 1/5/17
stops.routes[stops.routes$route == 6 & stops.routes$stop %in% route.6.8.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("May_2017")] <- 0


### 2.5.2 route 8 - extended Melbourne Uni to Moreland ----
# route 8 extended from Melbourne Uni to Moreland 17/10/04
stops.routes[stops.routes$route == 8 & stops.routes$stop %in% route.6.8.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Oct_2004")] <- 0

stops.routes[stops.routes$route == 8 & stops.routes$stop %in% route.6.8.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Oct_2004")] <- 14/30


### 2.5.3 route 11 - truncated from Harbour Esp to Spencer St ----
# route 11 truncated from Harbour Esp to Spencer St 27/6/04 - 19/11/05
stops.routes[stops.routes$route == 11 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jun_2004")] <- 26/30

stops.routes[stops.routes$route == 11 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Jun_2004") &
               to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0

stops.routes[stops.routes$route == 11 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2005")] <- 12/31


### 2.5.4 route 11 - extended Harbour Esp to Victoria Harbour ----
# route 11 extended from Harbour Esp to Victoria Harbour 20/9/09
stops.routes[stops.routes$route == 11 & stops.routes$stop %in% route.11.31.vic.harbour.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Sep_2009")] <- 0

stops.routes[stops.routes$route == 11 & stops.routes$stop %in% route.11.31.vic.harbour.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Sep_2009")] <- 11/30


### 2.5.5 route 12 - diversions via La Trobe St ----
# route 12 diverted from Collins St to La Trobe St from 13/7/20 to 7/11/20 and
# 17/1/22 to 30/1/22
stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jul_2020")] <- 12/31

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Jul_2020") &
               to_date(names(stops.routes)) < to_date("Nov_2020")] <- 0

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2020")] <- 23/30

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2022")] <- 17/31

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.latrobe.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jul_2020")] <- 0

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.latrobe.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jul_2020")] <- 19/31

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.latrobe.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2020")] <- 7/30

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.latrobe.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Nov_2020")] <- 0

stops.routes[stops.routes$route == 12 & stops.routes$stop %in% route.12.latrobe.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2022")] <- 14/31


### 2.5.6 route 16 - extended St Kilda Beach to Kew ----
# route 16 extended St Kilda Beach to Kew, and Acland St stop removed, 17/10/04
stops.routes[stops.routes$route == 16 & stops.routes$stop %in% route.69.to.route.16.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Oct_2004")] <- 0

stops.routes[stops.routes$route == 16 & stops.routes$stop %in% route.69.to.route.16.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Oct_2004")] <- 14/30

stops.routes[stops.routes$route == 16 & stops.routes$stop %in% route.16.old.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Oct_2004")] <- 16/30

stops.routes[stops.routes$route == 16 & stops.routes$stop %in% route.16.old.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Oct_2004")] <- 0


### 2.5.7 route 24 - Flinders St overpass temporary extension to Harbour Esp ----
# [NOT USED] route 24 temporarily extended to Harbour Esp, 22/5/05 - 21/11/05
# stops.routes[stops.routes$route == 24 & stops.routes$stop %in% route.24.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) < to_date("May_2005")] <- 0
# 
# stops.routes[stops.routes$route == 24 & stops.routes$stop %in% route.24.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <- 10/31
# 
# stops.routes[stops.routes$route == 24 & stops.routes$stop %in% route.24.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 21/30
# 
# stops.routes[stops.routes$route == 24 & stops.routes$stop %in% route.24.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("Nov_2005")] <- 0

# ALTERNATIVE - Flinders St overpass change not implemented, so route 24 extension
# stops switched off at all times
stops.routes[stops.routes$route == 24 & stops.routes$stop %in% route.24.harbour.esp.stops,
             !(names(stops.routes) %in% c("stop", "route"))] <- 0


### 2.5.8 route 30 - extended to Waterfront City ----
# route 30 terminus extended from Spencer/La Trobe Sts to Waterfront City, 4/1/05
stops.routes[stops.routes$route == 30 & stops.routes$stop %in% route.30.extension.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jan_2005")] <- 0

stops.routes[stops.routes$route == 30 & stops.routes$stop %in% route.30.extension.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2005")] <- 28/31


### 2.5.9 route 30 - terminus changed from Waterfront City to Central Pier ----
# route 30 terminus changed from Waterfront City to Central Pier 27/7/08
stops.routes[stops.routes$route == 30 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jul_2008")] <- 26/31

stops.routes[stops.routes$route == 30 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Jul_2008")] <- 0


### 2.5.10 route 31 - truncated from Harbour Esp to Spencer St ----
# route 31 truncated from Harbour Esp to Spencer St 27/6/04 - 19/11/05
stops.routes[stops.routes$route == 31 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jun_2004")] <- 26/30

stops.routes[stops.routes$route == 31 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Jun_2004") &
               to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0

stops.routes[stops.routes$route == 31 & stops.routes$stop %in% route.11.31.truncation.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2005")] <- 12/31


### 2.5.11 route 31 - extended Harbour Esp to Vic Harbour & St Vincent's Plaza to Hoddle St ----
# route 31 reinstated with extension Harbour Esp to Vic Harbour & 
# St Vincent's Plaza to Hoddle St 24/10/10 (but only switched off to end 
# Sep 2010, so that all trips for reinstatement month are counted)
stops.routes[stops.routes$route == 31 & stops.routes$stop %in% route.31.extension.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Oct_2010")] <- 0


### 2.5.12 route 35 - Flinders St overpass temporary truncation omitting Spring & Flinders Sts ----
# [NOT USED] route 35 truncated omitting Spring & Flinders Sts 22/5/05 - 21/11/05
# stops.routes[stops.routes$route == 35 & stops.routes$stop %in% route.35.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <- 21/31
# 
# stops.routes[stops.routes$route == 35 & stops.routes$stop %in% route.35.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("May_2005") &
#                to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0
# 
# stops.routes[stops.routes$route == 35 & stops.routes$stop %in% route.35.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30


### 2.5.13 route 35 - extended to Waterfront City ----
# route 35 extended Docklands Stadium to Waterfront City 30/5/09
stops.routes[stops.routes$route == 35 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("May_2009")] <- 0

stops.routes[stops.routes$route == 35 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("May_2009")] <- 2/31


### 2.5.14 route 48 - extended to Waterfront City ----
# route 48 terminus extended from Docklands Stadium to Waterfront City, 4/1/05
stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jan_2005")] <- 0

stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2005")] <- 28/31


### 2.5.15 route 48 - Flinders St overpass temporary truncation to Market St ----
# [NOT USED] route 48 truncated to Market St 22/5/05 - 21/11/05
# stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <- 21/31
# 
# stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("May_2005") &
#                to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0
# 
# stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30


### 2.5.16 route 48 - change from Flinders St to Collins St ----
# route 48 changed from Flinders St to Collins St 20/9/09
stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.flinders.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Sep_2009")] <- 19/30

stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.flinders.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Sep_2009")] <- 0

stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Sep_2009")] <- 0

stops.routes[stops.routes$route == 48 & stops.routes$stop %in% route.48.collins.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Sep_2009")] <- 11/30


### 2.5.17 route 55 - extra Cth Games stops ----
# [NOT USED] route 55 extended Domain to Malvern 12/3/06 to 26/3/06
# stops.routes[stops.routes$route == 55 & stops.routes$stop %in% route.55.cth.games.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) < to_date("Mar_2006")] <- 0
# 
# stops.routes[stops.routes$route == 55 & stops.routes$stop %in% route.55.cth.games.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Mar_2006")] <- 15/31
# 
# stops.routes[stops.routes$route == 55 & stops.routes$stop %in% route.55.cth.games.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("Mar_2006")] <- 0

# ALTERNATIVE - extra Cth Games stops not implemented, so route 55 extension
# stops switched off at all times
stops.routes[stops.routes$route == 55 & stops.routes$stop %in% route.55.cth.games.stops,
             !(names(stops.routes) %in% c("stop", "route"))] <- 0


### 2.5.18 route 58 - Toorak Rd realignment ----
# route 58 runs straight along Toorak Rd from 1/7/17
stops.routes[stops.routes$route == 58 & stops.routes$stop %in% route.58.old.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
             to_date(names(stops.routes)) >= to_date("Jul_2017")] <- 0


stops.routes[stops.routes$route == 58 & stops.routes$stop %in% route.58.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jul_2017")] <- 0


### 2.5.19 route 70 - Flinders St overpass temporary truncation to Market St ----
# [NOT USED] route 70 truncated to Market St 22/5/05 - 21/11/05
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <- 21/31
# 
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("May_2005") &
#                to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0
# 
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.truncation.stops &
#                !(stops.routes$stop %in% route.70.75.spencer.st.stops),
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30


### 2.5.20 route 70 - change from Spencer St to Harbour Esp (Dockland Stadium) ----
# [NOT USED] route 70 changed from Spencer St to Harbour Esp (Dockland Stadium) 22/11/05
# [note that the route.70.75.spencer.st.stops did not operate at all in Nov 2005, 
# because they were suspended until 18 Nov by the Flinders St overpass works,
# and so those stops are allocated 0 rather than 21/30]
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.spencer.st.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 0 # see note above
# 
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.spencer.st.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("Nov_2005")] <- 0 
# 
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0 
# 
# stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.harbour.esp.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30 

# ALTERNATIVE - Flinders St overpass change not implemented, so route.70.75.spencer.st.stops
# treated as in operation until 21/11/05
stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.spencer.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2005")] <- 21/30

stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.75.spencer.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Nov_2005")] <- 0 

stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.harbour.esp.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0 

stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.70.harbour.esp.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30 


### 2.5.21 route 70 - extended Docklands Stadium to Waterfront City ----
# route 70 extended Docklands Stadium to Waterfront City 20/9/09
stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Sep_2009")] <- 0

stops.routes[stops.routes$route == 70 & stops.routes$stop %in% route.35.48.70.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Sep_2009")] <- 11/30


### 2.5.22 route 75 - Flinders St overpass temporary truncation to Market St ----
# [NOT USED] route 75 truncated to Market St 22/5/05 - 21/11/05
# stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.70.75.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <- 21/31
# 
# stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.70.75.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("May_2005") &
#                to_date(names(stops.routes)) < to_date("Nov_2005")] <- 0
# 
# stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.70.75.truncation.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <- 9/30


### 2.5.23 route 75 - Vermont South extension ----
# route 75 extended to Vermont South from 24/7/05
stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.75.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jul_2005")] <- 0

stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.75.new.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jul_2005")] <- 8/31


### 2.5.24 route 75 - change from Spencer St to Harbour Esp ----
# route 75 changed from Spencer St to Harbour Esp 26/1/14
stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.70.75.spencer.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2014")] <- 25/31

stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.70.75.spencer.st.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) > to_date("Jan_2014")] <- 0

stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.75.harbour.esp.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jan_2014")] <- 0

stops.routes[stops.routes$route == 75 & stops.routes$stop %in% route.75.harbour.esp.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jan_2014")] <- 6/31


### 2.5.25 route 86 - terminus changed from Central Pier to Waterfront City ----
# [NOT USED]route 86 terminus changed from Central Pier to Waterfront City, temporarily for
# Flinders St overpass works 22/5/05 to 21/11/05, and then permanently from 27/7/08
# stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) < to_date("May_2005")] <-  0
# 
# stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("May_2005")] <-  10/31
# 
# stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Nov_2005")] <-  21/30
# 
# stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) > to_date("Nov_2005") &
#                to_date(names(stops.routes)) < to_date("Jul_2008")] <-  0
# 
# stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
#              !(names(stops.routes) %in% c("stop", "route")) &
#                to_date(names(stops.routes)) == to_date("Jul_2008")] <- 5/31

# ALTERNATIVE - Flinders St overpass change not implemented, so route 86 extension
# stops switched off at all times until implemented permanently from 27/7/08
stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) < to_date("Jul_2008")] <-  0

stops.routes[stops.routes$route == 86 & stops.routes$stop %in% route.30.86.waterfront.city.stops,
             !(names(stops.routes) %in% c("stop", "route")) &
               to_date(names(stops.routes)) == to_date("Jul_2008")] <- 5/31



## 2.6 write output table ----
## -------------------------------------#
write.csv(stops.routes, "../Tables/tram stops routes.csv", row.names = FALSE)



# 3. Master list of tram stops ----
# -----------------------------------------------------------------------------#
# Creates a master list of tram stops, which are all from current (Sep 2009)
# GTFS plus the extra stops used in the former Domain Rd section of the
# Toorak Rd route (from Mar 2015 GTFS)

# set up environment
library(dplyr)
library(stringr)
library(tidytransit)
library(sf)

## 3.1 current and former stops ----
## -------------------------------------#
# zip folder for PTV GTFS data
# from https://transitfeeds.com/p/ptv/497 , downloaded 3 Oct 2022
currentGTFS <- "../Data/gtfs_20220929.zip"

# zip folder for former PTV GTFS data, for Domain Rd stops
# from https://transitfeeds.com/p/ptv/497 , downloaded 29 June 2021
formerGTFS <- "../Data/gtfs archive/gtfs_20150330.zip"

current.stops <- read_gtfs(currentGTFS) %>%
  gtfs_as_sf(., crs = 4326) %>%
  .$stops %>%
  st_transform(7899)

former.stops <- read_gtfs(formerGTFS) %>%
  gtfs_as_sf(., crs = 4326) %>%
  .$stops %>%
  st_transform(7899)


## 3.2 extract tram stops, based on tram stops routes.csv ----
## -------------------------------------#
stops.routes <- read.csv("../Tables/tram stops routes.csv")

tram.stop.ids <- stops.routes %>%
  .$stop %>%
  unique()

current.tram.stops <- current.stops %>%
  filter(stop_id %in% tram.stop.ids)

additional.former.tram.stops <- former.stops %>%
  filter(stop_id %in% tram.stop.ids &
           !(stop_id %in% current.tram.stops$stop_id))

# there are 6  duplicate tram stops in current tram stops, where
# X or Y coordinates differ by up to 10m
# dup <- current.tram.stops %>%
#   st_drop_geometry() %>%
#   group_by(stop_id) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# dup.stops <- current.tram.stops %>%
#   filter(stop_id %in% dup$stop_id)
#
# remove one of them https://stackoverflow.com/questions/45804730/randomly-remove-duplicated-rows-using-dplyr
current.tram.stops <- current.tram.stops %>%
  group_by(stop_id) %>%
  sample_n(1)

# combine current and former
all.stops <- rbind(current.tram.stops, additional.former.tram.stops)


## 3.3 write output ----
## -------------------------------------#
st_write(all.stops, "../GIS/tram stop list.sqlite", delete_layer = TRUE)
