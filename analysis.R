#==============================================================================#
#   Analysis of apartments and public transport
#
#   Steve Pemberton, [January 2023]
#
#   Organised as follows [review and expand as needed]
#   A Output function, to produce apartment/PT outputs
#     1 Setup
#     2 Read in data inputs, initial processing (apartments, trains, trams, 
#       buses, illustration plot details)
#     3 Process selected areas (apartments, trains, trams, buses) and
#       assemble and save output data and illustration plot (apts and PT)
#   B Select geographies, and run function
#     Opt 1 - Greater Melbourne
#     Opt 2 - LGAs in Greater Melbourne
#
#==============================================================================#


# PART A - output function ----
# =============================================================================#

# Function to combine apartment, train, tram and bus data, and produce:
# - an output table named 'Tables/output_data_[geog].csv', and
# - an output plot named 'Images/output_plot_[geog].png'.

# Note that running the function will write these outputs, overwriting any prior
analysis <- function(selection.areas, geog) {
  
  # geog = "Greater Melbourne"
  # geog = "LGAs"
  # selection.areas = LGAs
  
  # 1. Setup ----
  # -----------------------------------------------------------------------------#
  ## 1.1 Packages ----
  ## -------------------------------------#
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(RColorBrewer)
  library(readxl)
  library(stringr) # for str_to_title
  library(lubridate)  # %m+%
  library(tidyr)  # for separate, pivot
  library(fs)  # dir_ls
  library(readr)  # reading & writing .gz files
  library(doSNOW)  # instead of doParallel, because allows progress reporting
  library(parallel)
  library(foreach)
  library(ggspatial) ## map tiles
  library(tidytransit)  # for reading GTFS
  
  
  ## 1.2 Functions ----
  ## -------------------------------------#
  dir_walk(path="./functions/",source, recurse=T, type = "file")
  
  
  ## 1.3 Months ----
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
  
  
  # 2. Read in data inputs and initial processing ----
  # -----------------------------------------------------------------------------#
  # Section 2 contains fixed inputs for all geogs; section 3 loops through each geog
  
  ## 2.1 Apartments ----
  ## -------------------------------------#
  ### 2.1.1 Read in completed projects, filter to apartments ----
  ### -------------------------------------#
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
  
  # filter to apartments within walking distance of PT
  # note - 'apartments' here includes others with density >= 100 dwell/ha
  apartments <- completed.projects.with.density %>%
    # filter to high density
    filter(hi_dens == "yes") %>%
    # filter to walking distance of PT (ie at least one train, tram or bus stop)
    filter(!is.na(rail_stn) | !is.na(tram_stop) | !is.na(bus_stop))
  

  ### 2.1.2 Allocate to financial years ----
  ### -------------------------------------#
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
  
  
  ### 2.1.3 Data for baseline apartments for 2004 ----
  ### -------------------------------------#
  # load 2006 census dwelling structures (STRD) downloaded 26/1/23 from
  # https://www.abs.gov.au/statistics/microdata-tablebuilder/tablebuilder 
  # and collection districts (CD) downloaded 26/1/23 from
  # https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1259.0.30.0022006?OpenDocument
  STRD.2006 <- read.csv("../Data/STRD 2006 Vic CD.csv", skip = 9)
  
  CD.2006 <- read_zipped_GIS(zipfile = "../Data/1259030002_cd06avic_shape.zip") %>%
    st_transform(7899) %>%
    # calculate original area
    mutate(orig.area = st_area(geometry))
  
  # apartment fields (not including 'attached to a house')
  apt.fields <- c("Flat..unit.or.apartment.in.a.one.or.two.storey.block",
                  "Flat..unit.or.apartment.in.a.three.storey.block",
                  "Flat..unit.or.apartment.in.a.four.or.more.storey.block")
  
  
  ## 2.2 Trains ----
  ## -------------------------------------#
  ### 2.2.1 Read in volumes table; adjust for Cth Games ----
  ### -------------------------------------#
  #### 2.2.2.1 Read in volumes percentages; ----
  #### -------------------------------------## 
  # Service volumes supplied by DoT, 23 Jan 2023
  train.voltable <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                      sheet = "MetroMonthly") %>%
    # select relevant columns
    dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Train)`)
  
  
  #### 2.2.2.2 Adjust for Cth Games ----
  #### -------------------------------------## 
  # adjustment for March and April 2006 (Cth Games)
  # see 'train cleaning checks.R' section 1 for review - DoT's figures for March 2006
  # are incomplete, so daily Feb 2006 volumes are used instead; April figures
  # are also missing 2 days, so completed by multiplying April figures by 30/28
  
  # select volumes for Feb 2006, and multiply by 31/28
  train.vols.March.2006 <- train.voltable %>%
    filter(Year == 2006 & Month == "February") %>%
    mutate(Month = "March",
           sched.vol = sched.vol * 31/28)
  
  # select volumes for April 2006, and multiply by 30/28 to cover missing 2 days
  train.vols.April.2006 <- train.voltable %>%
    filter(Year == 2006 & Month == "April") %>%
    mutate(sched.vol = sched.vol * 30/28)
  
  # substitute adjusted March 2006 figures, and adjust April
  train.voltable <- train.voltable %>%
    # omit former March 2006 figures
    filter(!(Year == 2006 & Month %in% c("March", "April"))) %>%
    # substitute adjusted March figures
    rbind(., train.vols.March.2006, train.vols.April.2006)
  
  
  ### 2.2.2 Read in station percentages ----
  ### -------------------------------------#
  # Station percentages - created by section 4 of 'trains.R': DoT have supplied 
  # train line volumes for all years, but station volumes for 2016-22
  # only.  Determines, for each station, the median of the station.vol as a 
  # percentage of the line.vol.
  station.pct <- read.csv("../Tables/station percentages.csv")

  
  ### 2.2.3 Read in capacity table ----
  ### -------------------------------------#
  train.captable <- read.csv("../Tables/train capacity factors.csv")
  
  
  ### 2.2.4 Read in train stop locations ----
  ### -------------------------------------#
  # required for corridor analyses (section 3.1.2)
  train.stops <- 
    # read in rail stops - based on current (Sep 2022) stations [so not 'Union']
    # see section 3 of 'train cleaning checks.R' for reconciliation of GTFS and DOT
    # station names, and steps required to align
    read_gtfs("../Data/gtfs_20220929.zip") %>%
    gtfs_as_sf(., crs = 4326) %>%
    .$stops %>%   
    st_transform(7899) %>%
    # filter to rows containing 'Railway Station' and not '/' (used for bus or tram stops at stations) 
    filter(grepl("Railway Station", stop_name) & !grepl("/", stop_name)) %>%
    # replace the pattern 'space + Railway + any number of other characters' with nothing
    mutate(station_name = gsub(" Railway.*","", stop_name)) %>%
    # fix name for Jolimont
    mutate(station_name = if_else(station_name == "Jolimont-MCG", 
                                  "Jolimont", station_name))
  
  
  ## 2.3 Trams ----
  ## -------------------------------------#
  ### 2.3.1 Read in stoptable ----
  ### -------------------------------------#
  # The stoptable shows, for each stop and route combination, a figure which is 
  # 0, 1 or a fraction, showing whether the route covers that stop for none, all
  # or part of a month. (Note that it does not show whether trams actually run
  # on the route that month - that is determined from the volume table.)
  tram.stoptable <- read.csv("../Tables/tram stops routes.csv")
  
  
  ### 2.3.2 Read in volume table; adjust for split routes and Cth Games ----
  ### -------------------------------------#
  #### 2.3.2.1 Read in volumes ----
  #### -------------------------------------## 
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
  
  
  #### 2.3.2.2 Adjust for split routes ----
  #### -------------------------------------## 
  tram.voltable <- tramVoltableSplitRoutes(tram.voltable, adj.factors)
  
  
  #### 2.3.2.3 Adjust for Cth Games ----
  #### -------------------------------------## 
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
  
  
  ### 2.3.3 Read in capacity table ----
  ### -------------------------------------#
  tram.captable <- readxl::read_xlsx("../Tables/tram fleet by route.xlsx",
                                     sheet = "monthly capacity factors")
 
   
  ### 2.3.4 Read in tram stop locations ----
  ### -------------------------------------#
  # required for corridor analyses (section 3.1.2)
  tram.stops <- st_read("../GIS/tram stop list.sqlite")
  
  
  ## 2.4 Buses ----
  ## -------------------------------------#
  ### 2.4.1 Details for bus routes and stops ----
  ### -------------------------------------#
  bus.route.stop.location <- "../Tables/bus daily stops and routes/"
  
  # bus route and stop data (from GTFS) has been prepared from Apr 2015 to
  # Dec 2022; but confine to complete financial years
  BUS.START.MONTH <- "201507"
  BUS.END.MONTH <- "202206"
  
  
  ### 2.4.2 Read in capacity table; find capacity-adjusted routes ----
  ### -------------------------------------#
  # load captable
  bus.captable <- read.csv("../Tables/bus capacity factors.csv") %>%
    # add ".capfactor" to route column names
    rename_with(~ paste0(., ".capfactor"), starts_with("route"))
  
  # find routes which have capacity adjustments
  cap.adj.routes <- names(bus.captable)[str_detect(names(bus.captable), "route")] %>%
    # remove ".capfactor", leaving just the route
    gsub(".capfactor", "", .)
  
  
  ### 2.4.3 Read in bus stop locations ----
  ### -------------------------------------#
  # required for corridor analyses (section 3.1.2)
  bus.stops <- st_read("../GIS/bus stop list.sqlite")
  
  
  ## 2.5 Population  ----
  ## -------------------------------------#
  ### 2.5.1 Read in population and SEIFA (IRSAD) data  ----
  ### -------------------------------------#
  # load population data (meshblocks and counts for census years,
  # and SA2s and pop estimates for all analysis years; and SEIFA IRSAD (Index
  # of Relative Socio-economic Advantage and Disadvantage) for census years
  source("./population data.R")
  
  
  ### 2.5.1 Read in walkable catchments  ----
  ### -------------------------------------#
  # polygon for each train, tram and bus stop, showing area walkable within 
  # 800m, created in section 2.4 of 'apartments.R'
  stop.catchments <- st_read("../GIS/stop catchments.sqlite")
  
  
  ## 2.6 Illustration output plot details ----
  ## -------------------------------------#
  # heading and caption text constants
  year.text <- "2004-2022"
  
  caption.text <- "Data sources: DTP (annual apartments), ABS census 2006 (apartment baseline), DTP (train and tram services volumes, train capacities), Yarra Trams (tram capacities), 
GTFS data (bus service volumes), DTP and bus operators (bus capacities). 'Apartments' are dwellings classified by DTP as 'attached 4 storey or more' (to 2016) 
or 'apartments'  (from 2017), and other developments with 100 dwellings per hectare or more. Apartments for the 2022 financial year include July to December 2021 only.  
Bus and tram services are those operating within 800m walking distance of the relevant apartments."
  
  
  # 3. Process selected areas ----
  # -----------------------------------------------------------------------------#
  # Section 2 contains fixed inputs for all geogs; section 3 loops through each geog
  
  for (i in 1:nrow(selection.areas)) {

    # Reporting status
    if (geog == "Greater Melbourne") {
      print(paste(Sys.time(), 
                  "| Now processing apartments and public transport for", 
                  geog))
    } else if (geog == "LGAs") {
      lga.name <- str_to_title(selection.areas[i, "LGA_NAME"] %>%
                                 st_drop_geometry())
      print(paste(Sys.time(), 
                  "| Now processing apartments and public transport for LGA no", 
                  i, "of", nrow(selection.areas), ":", lga.name))
    } else if (geog %in% c("corridors_all_services", "corridors_own_services",
                           "corridors_all_services_with_apts",
                           "corridors_own_services_with_apts")) {
      corridor.name <- selection.areas$corridor[i]
      print(paste(Sys.time(), 
                  "| Now processing apartments and public transport for corridor no", 
                  i, "of", nrow(selection.areas), ":", corridor.name))
    }
    
    
    ## 3.1 Apartments ----
    ## ---------------------------------#
    ### 3.1.1 Filter to area ----
    ### -------------------------------------#
    selected.apartments <- st_filter(st_centroid(apartments),
                                     selection.areas[i, ],
                                     predicate = st_intersects) %>%
      st_drop_geometry()
    
    
    ### 3.1.2 Find train, tram and bus stops ----
    ### -------------------------------------#
    # For 'corridors_all_services' - stops that are within the corridor
    
    # For 'corridors_own_services' - stops that are of the same mode as the 
    # corridor and are within the corridor and, for trains, on same line 
    # (tram/bus will be filtered in 3.3/3.4 by route)
    
    # Note that these can include stops that are not listed for the apartments
    
    # For 'corridors_all_services_with_apts' and 'corridors_own_services_with_apts',
    # only includes the relevant corridor stops if they are also listed for the 
    # apartments
    
    # Otherwise (Greater Melb and LGAs) - all stops listed for apartments (which
    # may be outside the selection area)
    
    if (geog %in% c("corridors_all_services", 
                    "corridors_all_services_with_apts")) {
      selected.train.stops <- train.stops %>%
        st_filter(., selection.areas[i,], predicate = st_intersects) %>%
        st_drop_geometry() %>%
        .$station_name
      selected.tram.stops <- tram.stops %>%
        st_filter(., selection.areas[i,], predicate = st_intersects) %>%
        st_drop_geometry() %>%
        .$stop_id
      selected.bus.stops <- bus.stops %>%
        st_filter(., selection.areas[i,], predicate = st_intersects) %>%
        st_drop_geometry() %>%
        .$stop_id
      # further filter for 'with_apts'
      if (geog == "corridors_all_services_with_apts") {
        selected.train.stops <- 
          selected.train.stops[selected.train.stops %in% 
                                 stopList(selected.apartments$rail_stn)]
        selected.tram.stops <- 
          selected.tram.stops[selected.tram.stops %in% 
                                stopList(selected.apartments$tram_stop)]
        selected.bus.stops <- 
          selected.bus.stops[selected.bus.stops %in% 
                               stopList(selected.apartments$bus_stop)]
      }
    } else if (geog %in% c("corridors_own_services",
                           "corridors_own_services_with_apts")) {
      if (selection.areas$type[i] == "train") {
        selected.train.stops <- str_split(selection.areas$stops[i], ", ")[[1]]
        selected.tram.stops <- c()  
        selected.bus.stops <- c()
      } else if (selection.areas$type[i] == "tram") {
        selected.train.stops <- c()
        selected.tram.stops <- tram.stops %>%
          st_filter(., selection.areas[i,], predicate = st_intersects) %>%
          st_drop_geometry() %>%
          .$stop_id
        selected.bus.stops <- c()
      } else if (selection.areas$type[i] == "bus") {
        selected.train.stops <- c()
        selected.tram.stops <- c()
        selected.bus.stops <- bus.stops %>%
          st_filter(., selection.areas[i,], predicate = st_intersects) %>%
          st_drop_geometry() %>%
          .$stop_id
      }
      # further filter for 'with_apts'
      if (geog == "corridors_own_services_with_apts") {
        selected.train.stops <- 
          selected.train.stops[selected.train.stops %in% 
                                 stopList(selected.apartments$rail_stn)]
        selected.tram.stops <- 
          selected.tram.stops[selected.tram.stops %in% 
                                stopList(selected.apartments$tram_stop)]
        selected.bus.stops <- 
          selected.bus.stops[selected.bus.stops %in% 
                               stopList(selected.apartments$bus_stop)]
      }
    } else {
      selected.train.stops <- stopList(selected.apartments$rail_stn) 
      selected.tram.stops <- stopList(selected.apartments$tram_stop)
      selected.bus.stops <- stopList(selected.apartments$bus_stop)
    }
    
    
    ### 3.1.3 Find numbers of apartments by year ----
    ### -------------------------------------#
    selected.apartments.year <- selected.apartments %>%
      group_by(fin_year_comp) %>%
      summarise(hi_dens_dwel = sum(hi_dens_dwel))
    
    # complete any missing years as zero
    years <- c(2004:2022)
    for (j in 1:length(years)) {
      if (!(years[j] %in% selected.apartments.year$fin_year_comp)) {
        # selected.apartments.year <- rbind(selected.apartments.year,
        #                                   c(years[j], 0)) %>%
        selected.apartments.year <- rbind(selected.apartments.year,
                                          cbind(fin_year_comp = years[j], 
                                                hi_dens_dwel = 0)) %>%
          arrange(fin_year_comp)
      }
    }
    
    
    ### 3.1.4 Find baseline number of apartments for 2004 ----
    ### -------------------------------------#
    # Baseline 2006 is the number of apartments as at 30 June 2006 (from 2006 census)
    
    # Based on 2006 census, with number of apartments apportioned by area where 
    # selection.area includes only a part of a census distruct
    
    baseline.apt.2006 <- CD.2006 %>%
      # intersect with selection area, and calculate intersection area & proportion
      st_intersection(., selection.areas[i, ]) %>%
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
    
    
    ## 3.2 Trains ----
    ## -------------------------------------#
    # if there are no selected train stops, return empty dataframe; 
    # otherwise, proceed 
    if (length(na.omit(selected.train.stops)) == 0)  {
      train.annual.totals <- data.frame(fin_year = numeric(), 
                                        train.volume = numeric(), 
                                        train.volume.capadj = numeric())
    } else {
    
      ### 3.2.1 Calculate monthly station volumes ----
      ### -------------------------------------#
      # station volume is the sum of volumes for the lines serving the station
      # multiplied by station percentage
      
      # load station percentages and filter to relevant stations
      station.monthly.volumes <- station.pct %>%
        # filter to selected stations as identified in section 3.1.2
        filter(Station %in% selected.train.stops)
      
      # TESTING ONLY - ALL STATIONS
      # station.monthly.volumes <- station.pct
      
      # add volume columns for each month
      for (j in 1:length(months)) {
        # month and year
        Year <- as.numeric(unlist(strsplit(months[j], "-"))[1])
        Month <- month.name[as.numeric(unlist(strsplit(months[j], "-"))[2])]
        
        # route column, in format "Jul_2003"
        route.col <- 
          paste0(month.abb[as.numeric(unlist(strsplit(months[j], "-"))[2])],
                 "_", Year)
        
        # add volume column: combined line volumes for the month * station.pct
        station.monthly.volumes <- station.monthly.volumes %>%
          rowwise() %>%
          mutate(!!route.col := getLineVol(line.vol = train.voltable,
                                           year = Year,
                                           month = Month,
                                           routes = Routes) * station.pct / 100) 
      }
      

      # adjust for opened stations (see Tables/train new stations.xlsx)
      station.monthly.volumes <- newStations(station.monthly.volumes, months)

      
      ### 3.2.2 Calculate capacity adjusted monthly station volumes ----
      ### -------------------------------------#
      # pivot to long form, and add capacity adjusted column
      # (4 to 5 mins for all stations in Melbourne)
      line.monthly.volumes <- 
        pivot_longer(station.monthly.volumes,
                     cols = -c("Station", "Routes", "station.pct"),
                     names_to = "Month",
                     values_to = "train.vol") %>%
        rowwise() %>%
        mutate(train.vol.capadj = get.train.cap(train.captable,
                                                Station,
                                                Routes,
                                                Month) * train.vol)

      
      ### 3.2.3 Find highest volume, by month, for each line group ----
      ### -------------------------------------#
      # There may be more than one equal highest station, so after stations
      # are selected, 'distinct' is used to keep just the highest volume
      line.highest.vols <- line.monthly.volumes %>%
        group_by(Routes, Month) %>%
        filter(train.vol == max(train.vol)) %>%
        distinct(Routes, Month, train.vol) %>%
        ungroup()
      
      line.highest.vols.capadj <- line.monthly.volumes %>%
        group_by(Routes, Month) %>%
        filter(train.vol.capadj == max(train.vol.capadj)) %>%
        distinct(Routes, Month, train.vol.capadj) %>%
        ungroup()
      
      line.highest.vols <- line.highest.vols %>%
        left_join(line.highest.vols.capadj,
                  by = c("Routes", "Month"))
        
      
      
      ### 3.2.4 Collapse line groups into a single figure ----
      ### -------------------------------------#
      # dataframe to hold monthly volumes
      train.monthly.volumes <- data.frame()
      
      # Where lines feed into each other, keep the highest figure
      for (j in 1:length(months)) {
        # reformat month from "2003-07" to "Jul_2003" format
        month.reformatted = paste0(month.abb[as.numeric(unlist(strsplit(months[j], "-"))[2])],
                                   "_", 
                                   as.numeric(unlist(strsplit(months[j], "-"))[1]))
        
        # find group for month
        highest.vol.month <- line.highest.vols %>%
          filter(Month == month.reformatted)
        
        # find the monthly volume, with and without capacity adjustment
        vol.month <- collapseVolumes(highest.vol.month, "train.vol") %>%
          as.numeric()
        vol.month.capadj <- collapseVolumes(highest.vol.month, "train.vol.capadj") %>%
          as.numeric
        
        # add results to dataframe
        train.monthly.volumes <- rbind(train.monthly.volumes,
                                       data.frame(month = month.reformatted,
                                                  train.vol = vol.month,
                                                  train.vol.capadj = vol.month.capadj))
      }

      
      ### 3.2.5 Calculate annual volumes ----
      ### -------------------------------------#
      train.annual.totals <- train.monthly.volumes %>%
        
        # add a fin_year column
        mutate(yr = strftime(as.Date(paste0(month, "_01"), "%b_%Y_%d"), format = "%Y"),
               mth = strftime(as.Date(paste0(month, "_01"), "%b_%Y_%d"), format = "%b"),
               fin_year = if_else(mth %in% c("Jul", "Aug", "Sep", 
                                             "Oct", "Nov", "Dec"),
                                  as.numeric(yr) + 1,
                                  as.numeric(yr))) %>%
        
        # group by fin year and sum
        group_by(fin_year) %>%
        summarise(train.volume = sum(train.vol),
                  train.volume.capadj = sum(train.vol.capadj))
      
      # output is a table with 3 columns: fin_year, train.volume, train.volume.capadj
     }
    

    
    
    ## 3.3 Trams ----
    ## -------------------------------------#
    # if there are no selected tram stops, return empty dataframe; 
    # otherwise, proceed
    if (length(na.omit(selected.tram.stops)) == 0) {
      tram.annual.totals <- data.frame(fin_year = numeric(), 
                                       tram.volume = numeric(), 
                                       tram.volume.capadj = numeric())
    } else {
      
      ### 3.3.1 Filter stoptable to relevant stops ----
      ### -------------------------------------#
      tram.stoptable.selection <- tram.stoptable %>%
        # filter to selected tram stops as identified in section 3.1.2
        filter(stop %in% selected.tram.stops)
      
      # find stops used only for route in corridor (used here for own_services,
      # and also used in 5.3.1 for all corridors)
       if (geog %in% c("corridors_all_services", "corridors_own_services",
                       "corridors_all_services_with_apts", 
                       "corridors_own_services_with_apts")) {
         own.corridor.selection <- tram.stoptable.selection %>%
           filter(route %in% str_split(selection.areas$routes[i], ", ")[[1]])
       }
      
      # filter for 'corridors_own_services'
      if (geog %in% c("corridors_own_services", 
                      "corridors_own_services_with_apts")) {
        tram.stoptable.selection <- own.corridor.selection
      }
      
      
      ### 3.3.2 Calculate monthly volumes ----
      ### -------------------------------------#
      # set up monthly volume table, covering all routes in tram.stoptable.selection
      tram.monthly.volumes <- data.frame(route = tram.stoptable.selection$route %>%
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
      print(paste(Sys.time(), "| Finding tram volumes for", length(months), "months; parallel processing with", cores, "cores"))
      
      # loop to find monthly volumes
      tram.output.list <- 
        foreach(j = 1:length(months),
                .packages = c("dplyr"),
                .export = c("get.tram.vol", 
                            "get.tram.cap",
                            "tram.voltable",
                            "tram.captable"),
                .options.snow = opts) %dopar% {
                  
                  # month in Jul_2003 format
                  month.col <- strftime(as.Date(paste0(months[j], "-01"), "%Y-%m-%d"),
                                        format = "%b_%Y")
                  
                  tram.route.volumes <- tram.stoptable.selection %>%
                    # select the relevant month's column from the stoptable selection
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
                    mutate(routevol = coverage * get.tram.vol(months[j], route)) %>%
                    
                    # find the volumes multiplied by capacity, using function above (but if
                    # routevol is 0, then there may also be no capacity figure, so zero)
                    # mutate(routevolcap = routevol * get.tram.cap(months[j], route)) %>%
                    mutate(routevolcap = ifelse(routevol > 0,
                                                routevol * get.tram.cap(months[j], route),
                                                0)) %>%
                    
                    # select just route, routevol and routevolcap, and complete column names
                    dplyr::select(route, routevol, routevolcap) %>%
                    rename_with(~ month.col, routevol) %>%
                    rename_with(~ paste0(month.col, "_capadj"), routevolcap)
                }
      
      # close the progress bar and cluster
      close(pb)
      stopCluster(cluster)
      
      # join monthly volumes from parallel processing output into a single table
      for (j in 1:length(tram.output.list)) {
        tram.monthly.volumes <- left_join(tram.monthly.volumes, 
                                          tram.output.list[[j]],
                                          by = "route")
      }
      
      
      ### 3.3.3 Calculate annual volumes ----
      ### -------------------------------------#
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
      
      # output is a table with 3 columns: fin_year, tram.volume, tram.volume.capadj
      
    }
    
    
    ## 3.4 Buses ----
    ## -------------------------------------#
    # NOTE - corridor analysis not yet included for buses; would need (possibly
    # among other things) to filter buses for 'own services' as for trams
    
    # if there are no selected bus stops, return empty dataframe; 
    # otherwise, proceed
    if (length(na.omit(selected.bus.stops)) == 0)  {
      bus.annual.totals <- data.frame(fin_year = numeric(), 
                                      bus.volume = numeric(), 
                                      bus.volume.capadj = numeric())
    } else {
      
      ### 3.4.1 Calculate monthly volumes by route ----
      ### -------------------------------------#
      # run get.bus.vol function - about 15 mins with 8 core parallel processing
      # filtered to relevant route if 'own services'
      if (geog %in% c("corridors_own_services", 
                      "corridors_own_services_with_apts")) {
        bus.monthly.volumes <- 
          get.bus.vol(route.stop.location = bus.route.stop.location,
                      filtered.stops = selected.bus.stops,
                      filtered.route = as.numeric(gsub("\\D", "", corridor.name)),  # the digits from the corridor name
                      start.month = BUS.START.MONTH,
                      end.month = BUS.END.MONTH) 
        
      } else {
        bus.monthly.volumes <- 
          get.bus.vol(route.stop.location = bus.route.stop.location,
                      filtered.stops = selected.bus.stops,
                      filtered.route = F,
                      start.month = BUS.START.MONTH,
                      end.month = BUS.END.MONTH) 
        
      }
      
      
      ### 3.4.2 Calculate capacity adjusted monthly volumes ----
      ### -------------------------------------#
      # set up capacity adjusted table, by joining cap table
      bus.monthly.volumes.capadj <- bus.monthly.volumes %>%
        left_join(bus.captable,
                  by = "month") 
      
      # for relevant routes, calculate the adjusted capacity
      for (j in 1:length(cap.adj.routes)) {
        route.col <- cap.adj.routes[j]
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
      
      
      ### 3.4.3 Calculate annual volumes ----
      ### -------------------------------------#
      bus.annual.totals <- bus.monthly.volumes %>%
        # monthly volumes - select totals column only
        dplyr::select(month, month.vol = total_services) %>%
        # join capacity adjusted volumes
        left_join(bus.monthly.volumes.capadj %>%
                    dplyr::select(month, month.vol.capadj = total_services),
                  by = "month") %>%
        
        # add a fin_year column
        mutate(yr = strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%Y"),
               mth = strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%b"),
               fin_year = if_else(mth %in% c("Jul", "Aug", "Sep", 
                                             "Oct", "Nov", "Dec"),
                                  as.numeric(yr) + 1,
                                  as.numeric(yr))) %>%
        
        # group by fin year and sum
        group_by(fin_year) %>%
        summarise(bus.volume = sum(month.vol),
                  bus.volume.capadj = sum(month.vol.capadj))
      
      # output is a table with 3 columns: fin_year, bus.volume, bus.volume.capadj
      
    }  
    
    ## 3.5 Population and pop density----
    ## ------------------------------------------------------------------------#
    ### 3.5.1 Stop catchments matching apartments ----
    ### -------------------------------------#
    # For corridor analysis', this is the selection area (which is derived from
    # the stop catchments, but truncated at the Melbourne LGA cordon); or, for
    # 'with_apts', it's the stop catchments for the 'own corridor' services
    # that contain apartments, truncated at the Melbourne LGA cordon (for bus,
    # it is the catchments for the route stops as at June 2022, disregarding
    # any earlier route variations)
    
    
    # Otherwise (Greater Melbourne and LGAs)  find the stop catchments that; match
    # the selected apartments (that is, catchments for each stop within 800m
    # walking distance of an apartment)
    if (geog %in% c("corridors_all_services", "corridors_own_services")) {
      selected.catchments <- selection.areas[i, ]
      
    } else if (geog %in% c("corridors_all_services_with_apts", 
                           "corridors_own_services_with_apts")) {
      # select relevant stops for corridor type
      type <- selection.areas$type[i]
      if (type == "train") {
        own.corridor.stops <- str_split(selection.areas$stops[i], ", ")[[1]]
        selected.stop.catchments <- stop.catchments %>%
          filter(station_name %in% selected.train.stops &
                   station_name %in% own.corridor.stops)
      } else if (type == "tram") {
        own.corridor.stops <- own.corridor.selection$stop
        selected.stop.catchments <- stop.catchments %>%
          filter(stop_id %in% selected.tram.stops &
                   stop_id %in% own.corridor.stops)
      } else if (type == "bus") {  
        own.corridor.stops <- str_split(selection.areas$stops[i], ", ")[[1]]
        selected.stop.catchments <- stop.catchments %>%
          filter(stop_id %in% selected.bus.stops & 
                   stop_id %in% own.corridor.stops)
      }
      
      # selected catchments are the catchments for the selected stops
      selected.catchments <- selected.stop.catchments%>%
        st_union() %>% 
        # exclude areas within cordon
        st_difference(., cordon) %>%
        st_as_sf()%>%
        rename(geometry = x)
      
      # for tram exclude any parts outside the selection area corridor,
      # arising from former tram stops [this section isn't needed for bus,
      # as the bus stops making up the corridor are from June 2022 only ]
      if (type == "tram") {
        selected.catchments <- selected.catchments %>%
          st_intersection(., selection.areas[i, ])
      }
      
    } else {
      selected.catchments <- stop.catchments %>%
        filter(station_name %in% selected.train.stops |
                 stop_id %in% selected.tram.stops |
                 stop_id %in% selected.bus.stops) %>%
        st_union(.) %>%
        st_as_sf()
      
    }
 

    ### 3.5.2 Population and SEIFA (IRSAD) for the stop catchments ----
    ### -------------------------------------#
    # if there are no selected train, tram or bus stops, return empty dataframe; 
    # otherwise, proceed
    if (length(selected.train.stops) == 0 &
        length(selected.tram.stops) == 0 &
        length(selected.bus.stops) == 0)  {
      selected.population <- data.frame(fin_year = numeric(), 
                                      population = numeric(),
                                      pop_density = numeric(),
                                      irsad = numeric())
    } else {
      # area of selected catchments, in ha
      selected.catchments.area <- as.numeric(st_area(selected.catchments) / 10000)
      selected.pop.SEIFA <- 
        getPopulation(selected.catchments,
                      MB.2006, MB.2011, MB.2016, MB.2021, SA2.2021,
                      SEIFA.2006, SEIFA.2011, SEIFA.2016, SEIFA.2021)
      selected.population <- 
        cbind(fin_year = 2003:2022,
              population = selected.pop.SEIFA[[1]],
              irsad = selected.pop.SEIFA[[2]]) %>%
        as.data.frame() %>%
        mutate(pop_density = population / selected.catchments.area, 
               .after = population)
    }
    
    ## 3.6 Output data and illustration plot ----
    ## ------------------------------------------------------------------------#
    ### 3.6.1 Details for outputs  ----
    ### -------------------------------------#
    # geog variables
    if (geog == "Greater Melbourne") {
      output.file.name.text <- geog
      geog.text <- "Greater Melbourne greater capital city statistical area (ABS)"
      map.zoom <- 9
    } else if (geog == "LGAs") {
      output.file.name.text <- paste0("LGA_", lga.name)
      geog.text <- lga.name
      map.zoom <- 13
    } else if (geog == "corridors_all_services") {
      output.file.name.text <- paste0("corridor_all_services_", corridor.name)
      geog.text <- corridor.name
      map.zoom <- 13
    } else if (geog == "corridors_own_services") {
      output.file.name.text <- paste0("corridor_own_services_", corridor.name)
      geog.text <- corridor.name
      map.zoom <- 13
    } else if (geog == "corridors_all_services_with_apts") {
      output.file.name.text <- paste0("corridor_all_services_with_apts_", corridor.name)
      geog.text <- corridor.name
      map.zoom <- 13
    } else if (geog == "corridors_own_services_with_apts") {
      output.file.name.text <- paste0("corridor_own_services_with_apts_", corridor.name)
      geog.text <- corridor.name
      map.zoom <- 13
    } else {
      output.file.name.text = ""
      geog.gext <= ""
      map.zoom <- 13
    }
    
    # baseline for apartments - 2006 census figure for selected area, minus
    # apartments constructed in fin years 2004, 2005 and 2006
    baseline.apt <- baseline.apt.2006 - (sum(selected.apartments.year %>%
                                               filter(fin_year_comp %in% c(2004, 2005, 2006)) %>%
                                               .$hi_dens_dwel))
    
    # dual axis transformation (to align baseline for the apt and tram axes in 2004)
    apt.start <- baseline.apt + 
      selected.apartments.year[selected.apartments.year$fin_year_comp == 2004, "hi_dens_dwel"][[1]]
    
    # pick one of the public transport start values to align with apartments - 
    # tram if available; if not then train; if not then bus (2015) 
    pt.start <- ifelse (
      nrow(tram.annual.totals) > 0, tram.annual.totals[tram.annual.totals$fin_year == 
                                                         min(tram.annual.totals$fin_year),
                                                       "tram.volume"][[1]],
      ifelse (
        nrow(train.annual.totals) > 0, train.annual.totals[train.annual.totals$fin_year ==
                                                             min(train.annual.totals$fin_year),
                                                           "train.volume"][[1]],
        ifelse (
          nrow(bus.annual.totals) > 0,   bus.annual.totals[bus.annual.totals$fin_year == 
                                                             min(bus.annual.totals$fin_year),
                                                           "bus.volume"][[1]], 
          apt.start
        )))
    
    display.ratio <- pt.start / apt.start
    if (display.ratio == Inf | display.ratio == 0) { display.ratio = 1}
    
    
    ### 3.6.2 Assemble output data ----
    ### -------------------------------------#
    # data to be plotted
    data <- selected.apartments.year %>%
      # apartments - cumulative sum, incl baseline as at 30 June 2003
      mutate(cum_hi_dens_dwel = cumsum(hi_dens_dwel) + baseline.apt) %>%
      rbind(c(2003, NA, baseline.apt)) %>%
      # trains
      left_join(train.annual.totals, by = c("fin_year_comp" = "fin_year")) %>%
      # trams
      left_join(tram.annual.totals, by = c("fin_year_comp" = "fin_year")) %>%
      # buses
      left_join(bus.annual.totals, by = c("fin_year_comp" = "fin_year")) %>%
      # population
      full_join(selected.population, by = c("fin_year_comp" = "fin_year")) %>%
      arrange(fin_year_comp)
      
    
    ### 3.6.3 Create illustration plot and map----
    ### -------------------------------------#
    plot <- output.plot(data %>% filter(fin_year_comp != 2003), 
                        geog.text, year.text, caption.text, display.ratio)
    
    map <- output.map(geog, data, map.zoom, selection.areas[i, ], 
                      selected.catchments, apartments, geog.text)
      

    ### 3.6.4 Write outputs ----
    ### -------------------------------------#
    # write data table
    write.csv(data, 
              paste0("../Tables/output data/output_data_", output.file.name.text ,".csv"),
              row.names = FALSE)
    
    # # save illustration plot
    ggsave(paste0("../Images/illustration plots/plot_", output.file.name.text ,".png"),
           plot,
           width = 25, height = 15, units = "cm")
    
    # save illustration map
    if (nrow(selected.catchments) > 0) {
      # remove 'all/own services' from filename text if present (maps are the
      # same for both versions of the corridor outputs)
      output.file.name.text.map <- output.file.name.text %>%
        str_replace("all_services_", "") %>%
        str_replace("own_services_", "")
      ggsave(paste0("../Images/illustration map/map_", output.file.name.text.map ,".png"),
             map,
             width = 15, height = 12, units = "cm")
    }
    
  } # end 'selected areas' loop
  
} # end analysis function


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# PART B - select geographies and run function ----
# =============================================================================#
# setup to read in data for options
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial) ## map tiles
library(tidytransit)  # for reading GTFS
library(fs) # in corridors, for reading directory
library(stringr)  # used in corridors
library(readr)  # used in corridors

source("./functions/readZippedGIS.R")


## Option 1 -  Greater Melbourne ----
## -----------------------------------------------------------------------------#
Melb.GCCSA <-  read_zipped_GIS(zipfile = "../Data/GCCSA_2021_AUST_SHP_GDA2020.zip") %>%
  st_transform(7899) %>%
  filter(GCC_NAME21 == "Greater Melbourne")

analysis(selection.areas = Melb.GCCSA,
         geog = "Greater Melbourne")


## Option 2 -  LGAs in Greater Melbourne ----
## -----------------------------------------------------------------------------#
LGAs <- 
  read_zipped_GIS(zipfile = "../Data/LGA.zip",
                  subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN") %>%
  filter(LGA_NAME %in% c("BANYULE", "BAYSIDE", "BOROONDARA", "BRIMBANK", 
                         "CARDINIA", "CASEY", "DAREBIN", "FRANKSTON", 
                         "GLEN EIRA", "GREATER DANDENONG", "HOBSONS BAY", "HUME", 
                         "KINGSTON", "KNOX", "MANNINGHAM", "MARIBYRNONG", 
                         "MAROONDAH", "MELBOURNE", "MELTON", "MERRI-BEK",
                         "MONASH", "MOONEE VALLEY", "MORNINGTON PENINSULA", "NILLUMBIK",
                         "PORT PHILLIP", "STONNINGTON", "WHITEHORSE", "WHITTLESEA",
                         "WYNDHAM", "YARRA RANGES", "YARRA")) %>%
  arrange(LGA_NAME)

analysis(selection.areas = LGAs,
         geog = "LGAs")


## Option 3 - Corridors ----
## -----------------------------------------------------------------------------#
# corridors are train and tram routes [may later be extended to some bus]

# load corridors (if not already saved); also loads cordon
source("./corridors.R")

# save corridors, if required
st_write(corridors, "../GIS/corridors.sqlite", delete_dsn = TRUE)

# OR read in corridors, if already saved, and cordon 
corridors <- st_read("../GIS/corridors.sqlite")
cordon <-  read_zipped_GIS(zipfile = "../Data/LGA.zip",
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN") %>%
  filter(LGA_NAME == "MELBOURNE") %>%
  st_buffer(., 120)

# c1 [NOT USED]: all services within a corridor are counted
analysis(selection.areas = corridors,
         geog = "corridors_all_services")

# c2 [NOT USED[: counting only a corridor's own services (eg Alamein line, Route 1 tram)
analysis(selection.areas = corridors,
         geog = "corridors_own_services")

# c3: all services within a corridor are counted, but
# only for stations/stops with apartments in their catchment area
analysis(selection.areas = corridors,
         geog = "corridors_all_services_with_apts")

# c4: counting only a corridor's own services (eg Alamein line, Route 1 tram),
# but only for stations/stops with apartments in their catchment area
analysis(selection.areas = corridors,
         geog = "corridors_own_services_with_apts")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

# PART C - combine results  ----
# =============================================================================#
# setup to combine results
library(dplyr)
library(stringr)
library(tidyr)

source("./functions/outputTable.R")

## LGAs plus Greater Melbourne ----
## -----------------------------------------------------------------------------#
# vector of LGA and Greater Melbourne file names
LGA.files <- list.files("../Tables/output data/") %>% 
  str_subset(., "output_data_LGA_")
Greater.Melbourne.file <- "output_data_Greater Melbourne.csv"

LGA.Melb.files <- c(LGA.files, Greater.Melbourne.file)

output.table.LGAs.Melb <- outputTable(LGA.Melb.files)

# write result
write.csv(output.table.LGAs.Melb, 
          "../Tables/output_data_table_LGAs.csv", 
          row.names = FALSE)


## [NOT USED] corridors all services ----
## -----------------------------------------------------------------------------#
# for the corridors, 'outputFiles' puts the file list into correct order (train,
# then tram, then bus); then 'outputTable' assembles the table from the files
output.files <- list.files("../Tables/output data")
corridor.all.service.files.list <- 
  output.files[grepl("output_data_corridor_all_services", output.files) &
                 !(grepl("_with_apts", output.files))]

output.table.corridor.all.services <- corridor.all.service.file.list %>%
  outputFiles(.) %>%
  outputTable(.)

# write result
write.csv(output.table.corridor.all.services, 
          "../Tables/output_data_table_corridor_all_services.csv", 
          row.names = FALSE)


## [NOT USED] corridors own services ----
## -----------------------------------------------------------------------------#
output.files <- list.files("../Tables/output data")
corridor.own.service.files.list <- 
  output.files[grepl("output_data_corridor_own_services", output.files) &
                 !(grepl("_with_apts", output.files))]

output.table.corridor.own.services <- corridor.own.service.file.list %>%
  outputFiles(.) %>%
  outputTable(.)

# write result
write.csv(output.table.corridor.own.services, 
          "../Tables/output_data_table_corridor_own_services.csv", 
          row.names = FALSE)

## corridors all services with apts ----
## -----------------------------------------------------------------------------#
corridor.all.service.files.list <- list.files("../Tables/output data") %>%
  str_subset(., "output_data_corridor_all_services_with_apts")

output.table.corridor.all.services <- corridor.all.service.files.list %>%
  outputFiles(.) %>%
  outputTable(.)

# write result
write.csv(output.table.corridor.all.services, 
          "../Tables/output_data_table_corridor_all_services_with_apts.csv", 
          row.names = FALSE)


## corridors own services with apts ----
## -----------------------------------------------------------------------------#
corridor.own.service.files.list <- list.files("../Tables/output data") %>%
  str_subset(., "output_data_corridor_own_services_with_apts")

output.table.corridor.own.services <- corridor.own.service.files.list %>%
  outputFiles(.) %>%
  outputTable(.)

# write result
write.csv(output.table.corridor.own.services, 
          "../Tables/output_data_table_corridor_own_services_with_apts.csv", 
          row.names = FALSE)

