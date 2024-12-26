# code to create misc outputs (or their inputs) for publications


library(dplyr)
library(sf)
library(fs)

dir_walk(path="./functions/",source, recurse=T, type = "file")

# 1 Eric's ATRF paper ----
# -----------------------------------------------------------------------------#

## 1.1 Greater Melbourne stop catchments ----
## ------------------------------------#
# layer of stop catchments for stops within 800m of apartments,
# 'publication outputs.qgz', group 'Greater Melbourne apartments' 

### 1.1.1 All apartments ----
### ------------------------------------#
# Make apartments layer (from section 2.2 of 'dashboard input prep.R')
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


### 1.1.2 All apartment catchments  ----
### ------------------------------------#
# Catchments (adapted from section 2.3 of 'dashboard input prep.R')
# polygon for each train, tram and bus stop, showing area walkable within 
# 800m, created in section 2.4 of 'apartments.R'
stop.catchments <- st_read("../GIS/stop catchments.sqlite")

Melb.GCCSA <-  read_zipped_GIS(zipfile = "../Data/GCCSA_2021_AUST_SHP_GDA2020.zip") %>%
  st_transform(7899) %>%
  filter(GCC_NAME21 == "Greater Melbourne") %>%
  dplyr::select(name = GCC_NAME21)

# apartments in Greater Melbourne
selected.apartments <- st_filter(st_centroid(apartments),
                                 Melb.GCCSA,
                                 predicate = st_intersects) %>%
  st_drop_geometry()


# public transport stops those apartments 
selected.train.stops <- stopList(selected.apartments$rail_stn) 
selected.tram.stops <- stopList(selected.apartments$tram_stop)
selected.bus.stops <- stopList(selected.apartments$bus_stop)

# catchments for those public transport stops
selected.catchments <- stop.catchments %>%
  filter(station_name %in% selected.train.stops |
           stop_id %in% selected.tram.stops |
           stop_id %in% selected.bus.stops) %>%
  st_union(.) %>%
  st_as_sf() %>%
  rename(geometry = x)

# write output
st_write(selected.catchments, "../GIS/greater melbourne catchments.sqlite")

