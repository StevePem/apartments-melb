#==============================================================================#
#   Analysis of apartments and public transport
#
#   Steve Pemberton, [January 2023]
#
# 
#   Organised as follows [review and expand as needed]
#   1  
#   2  
#   3  
#   4  
#
#==============================================================================#



# 0. Setup ----
# -----------------------------------------------------------------------------#
## 0.1 Packages ----
## -------------------------------------#
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(readxl)
# library(reshape2)  # for melting (population pyramids, multi-y barcharts)
library(stringr) # for str_to_title
# library(readr)  # for reading .gz, read_delim
# library(tidytransit)
# library(hms)
library(lubridate)  # %m+%
# library(sp)
# library(shp2graph)
# library(igraph)
library(tidyr)  # for separate, pivot
library(fs)  # dir_ls
library(readr)  # reading & writing .gz files
library(doSNOW)  # insstead of doParallel, because allows progress reporting
library(parallel)
library(foreach)



## 0.2 Functions ----
## -------------------------------------#
source("./functions/readZippedGIS.R")  # helper function to read zipped GIS files, used in section 1
source("./functions/dates.R")  # helper function to convert dates in 'Jul_2003' format from text to date, used in section 3.2

## 0.3 Select area ----
## -------------------------------------#
# If no area is selected, the whole completed projects file will be used (which
# includes includes apartments in the 31 Melbourne LGAs plus Greater Geelong,
# Queenscliffe and Surf Coast)

# If using a selected area, select from ONE of the options below

### 0.3.1 Greater Melbourne ----
### -----------------------------------#
# # Greater Melbourne as filter area
Melb.GCCSA <- read_zipped_GIS(zipfile = "../Data/GCCSA_2021_AUST_SHP_GDA2020.zip") %>%
  st_transform(7899) %>%
  filter(GCC_NAME21 == "Greater Melbourne")

# apartment selection
selection.area <- Melb.GCCSA

# ### 0.3.2 LGAs ----
# ### -----------------------------------#
# LGAs as filter areas
LGA <- read_zipped_GIS(zipfile = "../Data/LGA.zip",
                       subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN")

# complete selected LGA as desired (they are in capitals)
SELECTED.LGA <- "BOROONDARA"

# apartment selection
selection.area <- LGA %>%
  filter(LGA_NAME == SELECTED.LGA)


# - or insert other means of finding a selection area, eg road corridor - 


## 0.4 Months ----
## -------------------------------------#
START.MONTH <- "July 2003"
END.MONTH <- "June 2022"

# first day of 'start.month'
start.date <- as.Date(paste(START.MONTH, "01"), "%B %Y %d")
# last day of 'end month', ie first day of 'end.month', plus a month, minus a day 
end.date <- as.Date(paste(END.MONTH, "01"), "%B %Y %d") %m+% months(1) %m-% days(1)
# 'dates' are the first day of every month from start to end
months <- seq.Date(from = start.date, to = end.date, by = "month") %>%
  strftime(., format = "%Y-%m")


# 1. Apartments ----
# -----------------------------------------------------------------------------#
## 1.1 Read in completed projects, filter to apartments ----
## -------------------------------------#
# read in completed projects
completed.projects <- st_read("../GIS/completed_projects_with_stops.sqlite")

# determine high density [same as in apartments.R section 4.1]
completed.projects.with.density <- completed.projects %>%
  rowwise() %>%  # required so 'sum' can be used in hi_dens_dwel
  mutate(dwel_ha = total_dwel / area_ha,
         
         hi_dens = case_when(apartments > 0 | att_4s > 0 ~ "yes",
                             round(dwel_ha, 1) >= 100 ~ "yes", # rounding to avoid floating point exclusions of exactly 100
                             TRUE ~ "no"),
         
         hi_dens_dwel = case_when(apartments > 0 | 
                                    att_4s > 0 ~ sum(apartments, att_4s, na.rm = TRUE),
                                  round(dwel_ha, 1) >= 100 ~ total_dwel)) %>%
  ungroup()

# filter to apartments
# note - 'apartments' here includes others with density >= 100 dwell/ha
apartments <- completed.projects.with.density %>%
  filter(hi_dens == "yes")


## 1.2 Allocate to financial years ----
## -------------------------------------#
# DELWP have advised that file years were financial years up to and including
#   the 2016 file, and calendar years including and from the 2017 file.
# Projects with a calendar year are to be split 50:50 between the two 
#   financial years spanned by the calendar year.
# The status of projects with a completion year spanning the change period are:
# - year_comp 2015 – completed in financial year 2015, ie Jul 2014 to Jun 2015
# - file_year 2016 and year_comp 2016 – completed in financial year 2016, 
#   ie Jul 2015 to Jun 2016 (there are only 29 of these)
# - file_year 2017 and year_comp 2016 – completed in calendar year 2016 
#   (there are 630 of these – note that if a project was listed in both the 2016
#   and 2017 file years, only the record from the 2017 file year is retained)
# - year_comp 2017 – completed in calendar year 2017
# So division of projects between the two halves of a calendar year is for:
# - projects with a file_year of 2017 and year_comp of 2016 (but not a file_year
#   of 2016 and year_comp of 2016, as those projects are known to have occurred 
#   in the 2016 financial year)
# -	projects with a year_comp of 2017 or later.

# apartments where the year of completion is already the financial year
apartments.fin.year.correct <- apartments %>%
  # apartments with financial year as year of completion
  filter(year_comp <= 2015 |
           year_comp == 2016 & file_year == 2016) %>%
  # add 'fin_year_comp' field
  mutate(fin_year_comp = year_comp)

# apartments where completion is to be split between financial years
apartments.fin.year.split <- apartments %>%
  # apartments with calendar year as year of completion
  filter(year_comp == 2016 & file_year >= 2017 |
           year_comp >= 2017 ) %>%
  # halve the number of completed apartments
  mutate(hi_dens_dwel = hi_dens_dwel / 2) %>%
  # add 'fin_year_comp' field for the first year
  mutate(fin_year_comp = year_comp)

# duplicate the 'split' years with 'fin_year_comp' for the following year
apartments.fin.year.split <- rbind(apartments.fin.year.split,
                                   apartments.fin.year.split %>%
                                     mutate(fin_year_comp = year_comp + 1))


# recombine the 'correct' and 'split' groups
apartments <- rbind(apartments.fin.year.correct,
                    apartments.fin.year.split)


## 1.3 Filter to area, if required ----
## -------------------------------------#
# filter apartments to any selected area
if (exists("selection.area")) {
  selected.apartments <- st_filter(st_centroid(apartments),
                                   selection.area,
                                   predicate = st_intersects) %>%
    st_drop_geometry()
  
} else {
  selected.apartments <- apartments %>%
    st_drop_geometry
}

## 1.4 Find train, tram and bus stops ----
## -------------------------------------#
# function to convert stop column into vector of unique elements
stop.list <- function(stop.column) {
  
  # convert column to a single string
  stops <- toString(paste0(stop.column, 
                           collapse = ','))  %>%
    
    # replace any 2 or more commas (occurs when a row is empty) with single comma
    gsub(",{2,}", ",", .) %>%
    
    # omit white space after commas (occurs when a row contains more than one stop)
    gsub(", ", ",", .) %>%  
    
    # split at commas into a list of individual strings
    strsplit(., ",") %>%
    
    # unlist, unique and sort
    unlist() %>%
    unique() %>% 
    sort()
  
  return(stops)
}

selected.train.stops <- stop.list(selected.apartments$rail_stn) #<<< THINK THIS WILL CHANGE TO train_stn
selected.tram.stops <- stop.list(selected.apartments$tram_stop)
selected.bus.stops <- stop.list(selected.apartments$bus_stop)


## 1.5 Find numbers of apartments by year ----
## -------------------------------------#
selected.apartments.year <- selected.apartments %>%
  group_by(fin_year_comp) %>%
  summarise(hi_dens_dwel = sum(hi_dens_dwel))

# complete any missing years as zero
years <- c(2004:2022)
for (i in 1:length(years)) {
  if (!(years[i] %in% selected.apartments.year$fin_year_comp)) {
    selected.apartments.year <- rbind(selected.apartments.year,
                                      c(years[i], 0))
  }
}

# order by year
selected.apartments.year <- selected.apartments.year %>%
  arrange(fin_year_comp)


## 1.6 Find baseline number of apartments for 2004 ----
## -------------------------------------#
# Baseline 2006 is the number of apartments as at 30 June 2006 (from 2006 census)

# Based on 2006 census, with number of apartments apportioned by area where 
# selection.area includes only a part of a census distruct

# load 2006 census dwelling structures (STRD) downloaded 26/1/23 from
# https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder 
# and collection districts (CD) downloaded 26/1/23 from
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument
STRD.2006 <- read.csv("../Data/STRD 2006 Vic CD.csv", skip = 9)

CD.2006 <- read_zipped_GIS(zipfile = "../Data/1259030002_cd06avic_shape.zip") %>%
  st_transform(7899)

# apartment fields (not including 'attached to a house')
apt.fields <- c("Flat..unit.or.apartment.in.a.one.or.two.storey.block",
                "Flat..unit.or.apartment.in.a.three.storey.block",
                "Flat..unit.or.apartment.in.a.four.or.more.storey.block")

baseline.apt.2006 <- CD.2006 %>%
  # calculate original area
  mutate(orig.area = st_area(geometry)) %>%
  
  # intersect with selection area, and calculate intersection area & proportion
  st_intersection(., selection.area) %>%
  mutate(isec.area = st_area(geometry),
         isec.prop = isec.area/orig.area) %>%
  st_drop_geometry() %>%
  
  # join STRD and calculate number of apts
  left_join(., STRD.2006, by = c("CD_CODE06" = "STRD.Dwelling.Structure")) %>%
  rowwise() %>%
  mutate(apts = sum(across(any_of(apt.fields))) * isec.prop) %>%
  ungroup() %>%
  
  # calculate total
  summarise(n = sum(apts)) %>%
  as.numeric(.$n)
  
 

# 3. Trams ----
# -----------------------------------------------------------------------------#

## 3.1 Load stoptable, filter to relevant stops, find routes ----
## -------------------------------------#
# The stoptable shows, for each stop and route combination, a figure which is 
# 0, 1 or a fraction, showing whether the route covers that stop for none, all
# or part of a month. (Note that it does not show whether trams actually run
# on the route that month - that is determined from the volume table.)

tram.stoptable <- read.csv("../Tables/tram stops routes.csv") %>%
  # filter to selected tram stops as identified in section 1.4
  filter(stop %in% selected.tram.stops)

tram.routes <- unique(tram.stoptable$route)


## 3.2 Load volume table from DoT, and adjust for split routes and Cth Games ----
## -------------------------------------#
### 3.2.1 Load volumes ----
### -------------------------------------## 
# Service volumes supplied by DoT, 23 Jan 2023
tram.voltable <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                   sheet = "YarraMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Tram)`) %>%
  # add date column (single so rows can be selected by > or <)
  mutate(date = (as.Date(paste0("01", Month, Year), format = "%d%B%Y")))

# adjustment factors for split routes
adj.factors <- readxl::read_xlsx("../Tables/tram route changes.xlsx",
                                            sheet = "Split routes")

### 3.2.2 Adjust for split routes ----
### -------------------------------------## 
source("./functions/tramVoltableSplitRoutes.R")

tram.voltable <- tramVoltableSplitRoutes(tram.voltable)


### 3.2.3 Adjust for Cth Games ----
### -------------------------------------## 
# adjustment for March 2006 (Cth Games)
# see tram cleaning checks.R section 1 for review - DoT's figures for March 2006
# are incomplete, so daily Feb 2006 volumes are used instead

# select volumes for Feb 2006, and multiply by 31/28
tram.vols.March.2006 <- tram.voltable %>%
  filter(Year == 2006 & Month == "February") %>%
  mutate(Month = "March",
         sched.vol = sched.vol * 31/28,
         date = as.Date("01March2006", format = "%d%B%Y"))

# substitute adjusted March 2006 figures
tram.voltable <- tram.voltable %>%
  # omit former March 2006 figures
  filter(!(Year == 2006 & Month == "March")) %>%
  # substitute adjusted figures
  rbind(., tram.vols.March.2006)


## 3.3 Load capacity table ----
## -------------------------------------#
tram.captable <- readxl::read_xlsx("../Tables/tram fleet by route.xlsx",
                                   sheet = "monthly capacity factors")


## 3.4 Calculate monthly volumes ----
## -------------------------------------#
# function to look up monthly volume in tram.voltable, for month in format
# "2003-07" (tram.voltable has "2003" and "July"), and route
get.tram.vol <- function(month, route) {
  year = strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%Y")
  month = strftime(as.Date(paste0(months[i], "-01"), "%Y-%m-%d"), format = "%B")
  
  # get volume from tram.voltable
  vol <- tram.voltable[tram.voltable$Year == year &
                         tram.voltable$Month == month &
                         tram.voltable$Route == route, 
                       "sched.vol"][[1]] 
  
  # if no volume is provided, 'vol' will be numeric(0): convert to ordinary 0
  if (identical(vol, numeric(0))) {vol <- 0}
  
  return(vol)
}

# function to look up monthly capacity in tram.captable, for month in format
# "2003-07" and route
get.tram.cap <- function(month, route) {
  route_col <- paste0("route_", route)
  
  # get capacity from tram.captable [would be NA if no volume present - this
  # should never happen as capacities should cover all routes where volume data
  # provided - see section 2 of checks/tram cleaning checks.R for confirmation]
  cap <- tram.captable[tram.captable$month == month, ## match to month
                       route_col][[1]]

  return(cap)
}



# set up monthly volume table, covering all routes in tram.stoptable
tram.monthly.volumes <- data.frame(route = tram.stoptable$route %>%
                                     unique() %>%
                                     sort())

# setup for parallel processing - detect no of available cores and create cluster
cores <- detectCores()
cluster <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cluster)

# set up progress reporting
# https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
pb <- txtProgressBar(max = length(months), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# report
print(paste(Sys.time(), "| Finding tram volumes for ", length(months), "months; parallel processing with", cores, "cores"))

# loop to find monthly volumes
output.list <- 
  foreach(i = 1:length(months),
          .packages = c("dplyr"),
          .options.snow = opts) %dopar% {
            
            # month in Jul_2003 format
            month.col <- strftime(as.Date(paste0(months[i], "-01"), "%Y-%m-%d"),
                                  format = "%b_%Y")
            
            tram.route.volumes <- tram.stoptable %>%
              # select the relevant month's column from the stoptable
              dplyr::select(stop,
                            route,
                            # month in Jul_2003 format
                            any_of(month.col))%>%
              setNames(., c("stop", "route", "coverage")) %>%
              
              # group by route, and find the maximum figure ('coverage') for the month,
              # indicating the portion of the month where the route covers the stop
              # (but, if max and min total 1 and max is neither 0.5 nor 1, then 
              # situation is probably a route change where some stops have the route
              # for one part of a month, and other stops for the remainder - so 1)
              group_by(route) %>%
              summarise(max.cov = as.double(max(coverage)),
                        min.cov = as.double(min(coverage))) %>%
              ungroup() %>%
              mutate(coverage = 
                       if_else(max.cov + min.cov == 1 & !(max.cov %in% c(0.5, 1)),
                               1,
                               max.cov)) %>%
              
              # find the route volumes, using function above
              rowwise() %>%
              mutate(routevol = coverage * get.tram.vol(months[i], route)) %>%
              
              # find the volumes multiplied by capacity, using function above (but if
              # routevol is 0, then there may also be no capacity figure, so zero)
              # mutate(routevolcap = routevol * get.tram.cap(months[i], route)) %>%
              mutate(routevolcap = ifelse(routevol > 0,
                                          routevol * get.tram.cap(months[i], route),
                                          0)) %>%
              
              # select just route, routevol and routevolcap, and complete column names
              dplyr::select(route, routevol, routevolcap) %>%
              rename_with(~ month.col, routevol) %>%
              rename_with(~ paste0(month.col, "_capadj"), routevolcap)
          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)

# join monthly volumes from parallel processin output into a single table
for (i in 1:length(output.list)) {
  tram.monthly.volumes <- left_join(tram.monthly.volumes, 
                                    output.list[[i]],
                                    by = "route")
}


## 3.5 Calculate annual volumes ----
## -------------------------------------#
# get sum of each column other than route
tram.annual.totals <- colSums(tram.monthly.volumes %>%
                                dplyr::select(-route)) %>%
  # this creates a 'named numeric vector' of the totals; convert to dataframe
  data.frame() %>%
  # convert row names into a column and set the names
  tibble::rownames_to_column() %>%
  setNames(., c("rowname", "volume")) %>%
  # split the 'rownames' column at "_" (ignore the warning message, which
  # arises because some columns intentionally don't have 'capadj')
  separate(col = "rowname", into = c("month", "year", "capadj"), sep = "_") %>%
  
  # add a fin_year column
  rowwise() %>%
  mutate(fin_year = if_else(month %in% c("Jul", "Aug", "Sep", 
                                           "Oct", "Nov", "Dec"),
                            as.numeric(year) + 1,
                            as.numeric(year))) %>%
  
  # group by fin year and sum
  group_by(fin_year, capadj) %>%
  summarise(annual.total = sum(volume)) %>%
  
  # pivot wider, to make separate volume and capadj columns
  pivot_wider(names_from = capadj, values_from = annual.total) %>%
  # rename and reorder columns
  dplyr::select(fin_year, tram.volume = "NA", tram.volume.capadj = capadj)
  
# output is a table with 3 columns: fin_year, volume, and volume_capadj


## NEED TO DEAL WITH SITUATION WHERE NO TRAMS, EG MANNINGHAM

# 4. Buses ----
# -----------------------------------------------------------------------------#
## 4.1 Calculate monthly volumes by route ----
## -------------------------------------#
# Function to find total number of services, by month, for a given set of stops,
# based on GTFS data, converted to daily files
# Has options to include separate totals for each route, as well as overall totals

bus.services <- function(route.stop.location, # folder containing daily route and stop .gz files
                         filtered.stops = F,  # either F (for all stops) or a vector of stops, eg c("941", "942")
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
  # extract the text after the final "/" (that is, the file names)
  route_stop_file_names <- sapply(strsplit(route_stop_file_paths, 
                                           split = "/", 
                                           fixed = TRUE), 
                                  tail, 1L)
  
  
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
  print(paste(Sys.time(), "| Finding bus volumes for ", length(bus.months), "months; parallel processing with", cores, "cores"))
  
  # set up progress reporting
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = length(bus.months), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # loop to find daily volumes for each month, and combine as monthly volumes
  output.list <- 
    foreach(i = 1:length(bus.months),
            .packages = c("dplyr", "stringr", "readr", "tidyr"),
            # .combine = list, # resulted in multiple lists being combined - don't use!
            # .verbose = TRUE,
            .options.snow = opts) %dopar% {
              # create a temp directory (so that it can be cleared of the temp uncompressed
              # files on each iteration - otherwise leads to crash)
              temp_dir <- tempdir()
              
              # year and month (for display)
              year.month <- strftime(as.Date(paste0(bus.months[i], "-01", "%Y-%m-%d")), format = "%Y %B")
              
              # paths to daily details for the month
              month.file.paths <- route_stop_file_paths[str_detect(route_stop_file_paths, bus.months[i])]
              
              # empty dataframe to hold monthly volumes
              monthly.volumes <- data.frame()
              
              # find volumes for each day
              for (j in 1:length(month.file.paths)) {
                # read in the daily details
                daily.details <- read_csv(month.file.paths[j], show_col_types = FALSE)
                
                # filter to required stops, unless filtered.stops=F
                if (!isFALSE(filtered.stops)) {
                  daily.details <- daily.details %>%
                    filter(stop_id %in% filtered.stops)
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
                cbind(month = bus.months[i], .)
              
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
  
  for (i in 1:length(output.list)) {
    output.table <- bind_rows(output.table,
                              output.list[[i]])
  }
  
  # reorder columns for display
  output.table <- output.table %>%
    # order alphabetically ('period', then 'routes' (numerically), then 'total_services')
    select(order(colnames(output.table)))
  
  return(output.table)
}

# Run the function
# Note - bus volumes are available from Apr 2015, but this starts from 
# Jul 2015 to provide only full financial years
# Takes about 15 mins with 8 core parallel processing
bus.monthly.volumes <- 
  bus.services(route.stop.location = "../Tables/bus daily stops and routes/",
               filtered.stops = selected.bus.stops,
               start.month = "201507",
               end.month = "202206") 


## 4.2 Calculate capacity adjusted monthly volumes ----
## -------------------------------------#
# load captable
bus.captable <- read.csv("../Tables/bus capacity factors.csv") %>%
  # add ".capfactor" to route column names
  rename_with(~ paste0(., ".capfactor"), starts_with("route"))

# find routes which have capacity adjustments
cap.adj.routes <- names(bus.captable)[str_detect(names(bus.captable), "route")] %>%
  # remove ".capfactor", leaving just the route
  gsub(".capfactor", "", .)

# set up capacity adjusted table, by joining cap table
bus.monthly.volumes.capadj <- bus.monthly.volumes %>%
  left_join(bus.captable,
            by = "month") 

# for relevant routes, calculate the adjusted capacity
for (i in 1:length(cap.adj.routes)) {
  route.col <- cap.adj.routes[i]
  route.capfactor <- paste0(route.col, ".capfactor")
  
  # only applies if the route is present
  if (route.col %in% names(bus.monthly.volumes.capadj)) {
    # multiply volume for capfactor
    bus.monthly.volumes.capadj <- bus.monthly.volumes.capadj %>%
      mutate(!!route.col := get(route.col) * get(route.capfactor))
  }
}

# finalise cap adj table
bus.monthly.volumes.capadj <- bus.monthly.volumes.capadj %>%
  # remove the ".capfactor" columns
  dplyr::select(-ends_with(".capfactor")) %>%
  # recalculate total_services column
  rowwise() %>%
  mutate(total_services = sum(across(.cols = -c("month", "total_services")), na.rm = T))


## 4.3 Calculate annual volumes ----
## -------------------------------------#
bus.annual.totals <- bus.monthly.volumes %>%
  # monthly volumes - select totals column only
  dplyr::select(month, month.vol = total_services) %>%
  # join capacity adjusted volumes
  left_join(bus.monthly.volumes.capadj %>%
              dplyr::select(month, month.vol.capadj = total_services),
            by = "month") %>%
  
  # add a fin_year column
  mutate(yr = strftime(as.Date(paste0(month, "-01", "%Y-%m-%d")), format = "%Y"),
         mth = strftime(as.Date(paste0(month, "-01", "%Y-%m-%d")), format = "%b"),
         fin_year = if_else(mth %in% c("Jul", "Aug", "Sep", 
                                         "Oct", "Nov", "Dec"),
                            as.numeric(yr) + 1,
                            as.numeric(yr))) %>%
  
  # group by fin year and sum
  group_by(fin_year) %>%
  summarise(bus.volume = sum(month.vol),
            bus.volume.capadj = sum(month.vol.capadj))
  
  


# 5. Display outputs ----
# -----------------------------------------------------------------------------#
# baseline for apartments - 2006 census figure for selected area, minus
# apartments constructed in fin years 2004, 2005 and 2006
baseline.apt <- baseline.apt.2006 - (sum(selected.apartments.year %>%
                                           filter(fin_year_comp %in% c(2004, 2005, 2006)) %>%
                                           .$hi_dens_dwel))

# data to be plotted
data <- selected.apartments.year %>%
  # apartments - cumulative sum, incl baseline
  mutate(cum_hi_dens_dwel = cumsum(hi_dens_dwel) + baseline.apt) %>%
  # trams
  left_join(tram.annual.totals, by = c("fin_year_comp" = "fin_year")) %>%
  # buses
  left_join(bus.annual.totals, by = c("fin_year_comp" = "fin_year"))

# dual axis transformation (to align baseline for the apt and tram axes in 2004)
apt.start <- baseline.apt + 
  selected.apartments.year[selected.apartments.year$fin_year_comp == 2004, "hi_dens_dwel"][[1]]
tram.start <- tram.annual.totals[tram.annual.totals$fin_year == 2004, "tram.volume"][[1]]
display.ratio <- tram.start / apt.start

# heading and caption text variables
geog.text.Melb <- "Greater Melbourne greater capital city statistical area (ABS)"
geog.text.LGA <- paste0(str_to_title(SELECTED.LGA))

year.text <- "2004-2022"

caption.text <- "Data sources: DELWP (annual apartments), ABS census 2006 (apartment baseline), DoT (annual tram services), 
Yarra Trams (tram capacities).  'Apartments' are dwellings classified by DELWP as 'attached 4 storey or more' (to 2016) or 
'apartments' (from 2017), and other developments with 100 dwellings per hectare or more. Apartments for the 2022 financial year
include July to December 2021 only.  Tram services are those operating within 800m walking distance of the relevant apartments."

# function to transform financial year from '2021' to '2020/21' format
transform.fin.yr <- function(year) {
  return(paste0(year - 1, "/", substr(as.character(year), 3, 4)))
}

# function for output plot
output.plot <- function(data, mylocation) {
  
  # create ggplot 
  ggplot(data) +
    # scale_y_continuous(labels = scales::comma) + 
    
    # trams - volume
    geom_line(aes(x = fin_year_comp, 
                  y = tram.volume / display.ratio, ## data transform for appearance
                  colour ="tram"),
              size = 2) +
    geom_point(aes(x = fin_year_comp, 
                   y = tram.volume / display.ratio, ## data transform for appearance
                   colour = "tram"),
               size = 4) +
    
    # trams - volume with capacity adjustment
    geom_line(aes(x = fin_year_comp,  
                  y = tram.volume.capadj / display.ratio, ## data transform for appearance
                  colour = "tram.cap"),
              size = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = tram.volume.capadj / display.ratio, ## data transform for appearance
                   colour = "tram.cap"),
               size = 2) +
    
    # buses - volume
    geom_line(aes(x = fin_year_comp, 
                  y = bus.volume / display.ratio, ## data transform for appearance
                  colour ="bus"),
              size = 2) +
    geom_point(aes(x = fin_year_comp, 
                   y = bus.volume / display.ratio, ## data transform for appearance
                   colour = "bus"),
               size = 4) +
    
    # buses - volume with capacity adjustment
    geom_line(aes(x = fin_year_comp,  
                  y = bus.volume.capadj / display.ratio, ## data transform for appearance
                  colour = "bus.cap"),
              size = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = bus.volume.capadj / display.ratio, ## data transform for appearance
                   colour = "bus.cap"),
               size = 2) +
    
    # apartments
    geom_line(aes(x = fin_year_comp,  
                  y = cum_hi_dens_dwel,
                  colour = "apt"),
              size = 1) +
    geom_point(aes(x = fin_year_comp, 
                   y = cum_hi_dens_dwel,
                   colour = "apt"),
               size = 2) +
    
    scale_x_continuous(breaks = seq(min(data$fin_year_comp), 
                                    max(data$fin_year_comp),
                                    by = 2),
                       labels = transform.fin.yr(seq(min(data$fin_year_comp), 
                                                     max(data$fin_year_comp),
                                                     by = 2))) +
    
    scale_y_continuous(name = "Cumulative number of apartments",
                       # # next 2 lines are to have y-axis start at 0
                       # limits = c(0, NA),  # lowest figure 0 for y axis
                       # expand = expansion(mult = c(0, 0.05)), # white space at top & 
                       # # bottom - zero for bottom, 5% (default) for top
                       labels = scales::comma,
                       sec.axis = sec_axis(trans = ~.*display.ratio,  # transformation is reverse of above
                                           name = "Annual bus and tram services",
                                           labels = scales::comma)) +
    # See https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot, but doesn't work
    scale_colour_manual("", 
                        labels = c("Apartments (LHS)",
                                   "Annual bus services (RHS)",
                                   "Annual bus services, adjusted for capacity (RHS)",
                                   "Annual tram services (RHS)",
                                   "Annual tram services, adjusted for capacity (RHS)"
                        ),
                        values = c("apt" = "black",
                                   "tram" = brewer.pal(n = 4, name = 'Paired')[1],
                                   "tram.cap" = brewer.pal(n = 4, name = 'Paired')[2],
                                   "bus" = brewer.pal(n = 4, name = 'Paired')[3],
                                   "bus.cap" = brewer.pal(n = 4, name = 'Paired')[4])) +
    
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    
    labs(x = "Financial year of completion",
         # y = "Cumulative number of apartments and other high density dwellings",
         title = "Annual change in apartment numbers and bus and tram services",
         subtitle = paste0(mylocation, ", ", year.text),
         caption = paste(caption.text)) +
    
    theme_bw() +
    
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0))
}

# See https://stackoverflow.com/questions/63722900/ggplot-shape-not-matching-legend 
# which says you should be transforming/melting data!

output.LGA <- output.plot(data, geog.text.LGA)
output.LGA
output.Greater.Melb <- output.plot(data, geog.text.Melb)
output.Greater.Melb

png( "../Images/plot.Boroondara.png", width = 2500, height = 1500, res = 300)
output.LGA
dev.off()

png( "../Images/plot.MooneeValley.png", width = 2500, height = 1500, res = 300)
output.LGA
dev.off()

png( "../Images/plot.Greater Melbourne.png", width = 2500, height = 1500, res = 300)
output.Greater.Melb
dev.off()


