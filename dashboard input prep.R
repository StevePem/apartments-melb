#==============================================================================#
#   Prepare inputs for dashboard
#
#   Steve Pemberton, [August 2023]
#
# 
#   Organised as follows [review and expand as needed]
#   1  Process apt, pop and service data
#   2  Process spatial data for map
#   3  
#   4  
#
#==============================================================================#

# set up environment
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(fs)

dir_walk(path="./functions/",source, recurse=T, type = "file")


# 1 Process apt, pop and service  data ----
# -----------------------------------------------------------------------------#

## 1.1 Read in data ----
## -----------------------------------#

LGAs <- read.csv("../Tables/output_data_table_LGAs.csv") %>%
  # # ### fix Greater Melbourne name  # TO CHECK: appeared as '/Greater Melbourne'; 
  # # should have been fixed in analysis.R, so (if it works for updated data) can delete this
  # mutate(location = ifelse(grepl("Greater Melbourne", location), 
  #                          "Greater Melbourne", location)) %>%
  # add type field
  mutate(type = ifelse(location == "Greater Melbourne", "Greater Melbourne", "LGA"))

corridors <- read.csv("../Tables/output_data_table_corridor_own_services_with_apts.csv") %>%
  # remove "with_apts_'
  mutate(location = gsub("with_apts_", "", location)) %>%
  # # ### fix Lilydale corridor description  # TO CHECK: appeared as 'East Richmond to Lilydale';
  # # should have been fixed in corridors.R, so (if it works for updated data) can delete this
  # mutate(location = ifelse(location == "Lilydale (East Richmond to Lilydale)", 
  #                          "Lilydale (East Ringwood to Lilydale)", location)) %>%
  # add type field: train has no route no; tram route no < 200; bus route no 200+
  mutate(type = case_when(
    !grepl("Route", location)                    ~ "Train",
    grepl("Route", location) & as.numeric(gsub("\\D", "", location)) < 200 ~ "Tram",
    grepl("Route", location) & as.numeric(gsub("\\D", "", location)) >= 200 ~ "Bus",
    TRUE ~ "other"
  ))


## 1.2 Process data ----
## -----------------------------------#
data <- bind_rows(LGAs, corridors) %>%
  # select only required columns
  dplyr::select(location,
                type,
                contains("cum_hi_dens_dwel"),
                contains("population"),
                contains("train.volume"),
                contains("tram.volume"),
                contains("bus.volume")) %>%
  
  # convert to long form
  pivot_longer(!c(location, type), names_to = c("variable", "Year"), 
               names_sep = "_FY.", values_to = "value") %>%
  
  # tidy up variable names
  mutate(variable = case_when(
    variable == "cum_hi_dens_dwel"    ~ "Apartments",
    variable == "population"          ~ "Population",
    variable == "train.volume"        ~ "Train",
    variable == "train.volume.capadj" ~ "Train.capadj",
    variable == "tram.volume"         ~ "Tram",
    variable == "tram.volume.capadj"  ~ "Tram.capadj",
    variable == "bus.volume"          ~ "Bus",
    variable == "bus.volume.capadj"   ~ "Bus.capadj"
  )) %>%
  
  # tidy up year format
  # mutate(Year = gsub("\\.", "\\-", Year)) %>%
  mutate(Year = as.numeric(substring(Year, 1, 4)) + 1) %>%
  
  # convert to wide form
  pivot_wider(names_from = variable, values_from = value) %>%
  
  # replace NAs with 0
  replace(is.na(.), 0)


## 1.3 Save output ----
## -----------------------------------#
saveRDS(data, "./dashboard/data.rds")


# 2 Process outputs for maps ----
# -----------------------------------------------------------------------------#

## 2.1 Areas (Greater Melbourne and LGAs) ----
## -----------------------------------#
# Based on Part B of 'analysis.R'

Melb.GCCSA <-  read_zipped_GIS(zipfile = "../Data/GCCSA_2021_AUST_SHP_GDA2020.zip") %>%
  st_transform(7899) %>%
  filter(GCC_NAME21 == "Greater Melbourne") %>%
  dplyr::select(name = GCC_NAME21)

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
  arrange(LGA_NAME) %>%
  mutate(name = str_to_title(LGA_NAME)) %>%
  dplyr::select(name)

areas <- rbind(Melb.GCCSA, LGAs)

# -----------------------------------------------------------------------------#
## 2.2 Apartments ----
## -----------------------------------#
# Based on section 2.1.1 of 'analysis.R'
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


## 2.3 Stop catchments ----
## -----------------------------------#
# polygon for each train, tram and bus stop, showing area walkable within 
# 800m, created in section 2.4 of 'apartments.R'
stop.catchments <- st_read("../GIS/stop catchments.sqlite")

# corridors and cordon, created in Part 2 Option 3 of 'analysis.R'
corridors <- st_read("../GIS/corridors.sqlite")
cordon <-  read_zipped_GIS(zipfile = "../Data/LGA.zip",
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMADMIN") %>%
  filter(LGA_NAME == "MELBOURNE") %>%
  st_buffer(., 120)

# tram stops and bus stops, as used in sections 2.3.4 and 2.4.3 of 'analysis.R'
tram.stops <- st_read("../GIS/tram stop list.sqlite")
bus.stops <- st_read("../GIS/bus stop list.sqlite")

# tram stoptable, as used in section 2.3.1 of 'analysis.R'
tram.stoptable <- read.csv("../Tables/tram stops routes.csv")


# empty dataframe to hold catchments
walkable.catchments <- c()

# for Greater Melb and LGAs, walkable catchment is the catchments of the 
# stops that are within 800m of the apartments in the area
# based on sections 3.1 and 3.5.1 of 'analysis.R'
for (i in 1:nrow(areas)) {
  # apartments within the area (section 3.1.1)
  selected.apartments <- st_filter(st_centroid(apartments),
                                   areas[i, ],
                                   predicate = st_intersects) %>%
    st_drop_geometry()
  
  # public transport stops for those apartments (section 3.1.2)
  selected.train.stops <- stopList(selected.apartments$rail_stn) 
  selected.tram.stops <- stopList(selected.apartments$tram_stop)
  selected.bus.stops <- stopList(selected.apartments$bus_stop)
  
  # catchments for those public transport stops (section 3.5.1)
  selected.catchments <- stop.catchments %>%
    filter(station_name %in% selected.train.stops |
             stop_id %in% selected.tram.stops |
             stop_id %in% selected.bus.stops) %>%
    st_union(.) %>%
    st_as_sf() %>%
    rename(geometry = x)
  
  # complete fields
  walkable.catchment <- selected.catchments %>%
    mutate(name = areas$name[i])
  
  # add to walkable.catchments
  walkable.catchments <- rbind(walkable.catchments, 
                               walkable.catchment)
  
}

# for corridors, walkable catchment the stop catchments for the 'own corridor' 
# services that contain apartments
# based on sections 3.1, 3.3 and 3.5 of 'analysis.R'
for (i in 1:nrow(corridors)) {
  # apartments within the area (section 3.1.1)
  selected.apartments <- st_filter(st_centroid(apartments),
                                   corridors[i, ],
                                   predicate = st_intersects) %>%
    st_drop_geometry()
  
  type <- corridors$type[i]
  
  # stops that are of the same mode as the corridor and  within the corridor 
  # and, for trains, on same line, with filter to only include
  # relevant stops if also listed for apartments (section 3.1.2)
  if (type == "train") {
    selected.train.stops <- str_split(corridors$stops[i], ", ")[[1]]
    selected.train.stops <- 
      selected.train.stops[selected.train.stops %in% 
                             stopList(selected.apartments$rail_stn)]
    selected.tram.stops <- c()  
    selected.bus.stops <- c()
  } else if (type == "tram") {
    selected.train.stops <- c()
    selected.tram.stops <- tram.stops %>%
      st_filter(., corridors[i,], predicate = st_intersects) %>%
      st_drop_geometry() %>%
      .$stop_id
    selected.tram.stops <- 
      selected.tram.stops[selected.tram.stops %in% 
                            stopList(selected.apartments$tram_stop)]
    selected.bus.stops <- c()
  } else if (type == "bus") {
    selected.train.stops <- c()
    selected.tram.stops <- c()
    selected.bus.stops <- bus.stops %>%
      st_filter(., corridors[i,], predicate = st_intersects) %>%
      st_drop_geometry() %>%
      .$stop_id
    selected.bus.stops <- 
      selected.bus.stops[selected.bus.stops %in% 
                           stopList(selected.apartments$bus_stop)]
  }

  # 'own corridor selection' for trams, to find stops used only for route 
  # (section 3.3.1)
  if (type == "tram") {
    # tram stops as selected above, in stoptable (contains route/stop details)
    tram.stoptable.selection <- tram.stoptable %>%
      filter(stop %in% selected.tram.stops)
    
    # find stops used only for route in corridor
    own.corridor.selection <- tram.stoptable.selection %>%
      filter(route %in% str_split(corridors$routes[i], ", ")[[1]])
  }
  
  # relevant stops for corridor type (section 3.5.1)
  if (type == "train") {
    own.corridor.stops <- str_split(corridors$stops[i], ", ")[[1]]
    selected.stop.catchments <- stop.catchments %>%
      filter(station_name %in% selected.train.stops &
               station_name %in% own.corridor.stops)
  } else if (type == "tram") {
    own.corridor.stops <- own.corridor.selection$stop
    selected.stop.catchments <- stop.catchments %>%
      filter(stop_id %in% selected.tram.stops &
               stop_id %in% own.corridor.stops)
  } else if (type == "bus") {  
    own.corridor.stops <- str_split(corridors$stops[i], ", ")[[1]]
    selected.stop.catchments <- stop.catchments %>%
      filter(stop_id %in% selected.bus.stops & 
               stop_id %in% own.corridor.stops)
  }

  # catchments for those public transport stops (section 3.5.1)
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
      st_intersection(., corridors[i, ])
  }
  
  # complete fields
  walkable.catchment <- selected.catchments %>%
    mutate(name = corridors$corridor[i]) %>%
    dplyr::select(name)
  
  # add to walkable.catchments
  walkable.catchments <- rbind(walkable.catchments, 
                               walkable.catchment)
  
}


## 2.4 Save outputs ----
## -----------------------------------#
# Note - outputs are saved in EPSG:4326, to match Leaflet tiles

# save apartment output
# keep only centroid geometry and items needed for display)
apartment.centroids <- apartments %>%
  st_centroid() %>%
  mutate(address = paste0(street_num, " ", street_name, " ", street_type, ", ", 
                          str_to_title(suburb))) %>%
  mutate(display_name = case_when(is.na(proj_name) ~ address,
                                  proj_name == " " ~ address,
                                  TRUE             ~ str_to_title(proj_name))) %>%
  dplyr::select(display_name,hi_dens_dwel, year_comp) %>%
  st_transform(4326)

saveRDS(apartment.centroids, "./dashboard/apartments.rds")

# save walkable catchment output
# st_write(walkable.catchments, "./dashboard/walkable_catchments.sqlite")
walkable.catchment.output <- walkable.catchments %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 10) %>%
  st_transform(4326)

saveRDS(walkable.catchment.output, "./dashboard/walkable catchments.rds")


# save area output - simplified to reduce size
simplified.areas <- areas %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 25) %>%
  st_transform(4326)

# st_write(areas, "./dashboard/areas_base.sqlite")
# st_write(simplified.areas, "./dashboard/areas_simp.sqlite")
saveRDS(simplified.areas, "./dashboard/areas.rds")

# st_write(simplified.areas, "./dashboard/testareas.geojson")
