#==============================================================================#
#   Investigation of apartment data
#
#   Steve Pemberton, [January 2024]
#
# 
#   Organised as follows [review and expand as needed]
#   1  Read in data and prepare basic requirements
#   2  Completed projects and their PT stops and routes
#   3  Investigate density for building types
#   4  Identify and calculate numbers of apartments and other high density
#
#==============================================================================#


# set up environment
library(dplyr)
library(sf)
library(ggplot2)
# library(readxl)
# library(reshape2)  # for melting (population pyramids, multi-y barcharts)
library(stringr)
# library(readr)  # for reading .gz, read_delim
library(tidytransit)
# library(hms)
# library(lubridate)
# library(sp)
library(shp2graph)
library(igraph)
library(doSNOW)
library(parallel)
library(foreach)
library(fs)  # dir_delete
library(nngeo)  # densification in 'findWalkableCatchment'


source("./functions/readZippedGIS.R")  # helper function to read zipped GIS files, used in section 1
source("./functions/findWalkableStops.R")  # find stops within BUFFDIST of project, used in section 2.4
source("./functions/findWalkableCatchment.R") # find walkable catchment around a location, used in section 2.4 
source("./functions/stopList.R")  # finds stops in 'completed projects'; used in section 2.4

# zip folder for PTV GTFS data (for stop locations) - no longer used
# GTFS <- "../Data/gtfs_20220929.zip"


# 1. Read in data and prepare basic requirements ----
# -----------------------------------------------------------------------------#
# 1.1 [not used] ----
# -------------------------------------#


# 1.2 read in MRS files and convert to common CRS ----
# -------------------------------------#
# 'MRS': major redevelopment site
# use CRS 7899 (GDA2020 / Vicgrid)

zipfile <- "../Data/UDP_Redev_Historic.zip"
file <- "/UDP_Redev_Historic.gpkg"
years <- c(2004:2021)

for (i in 1:length(years)) {
  # layer name is "Redev_data_" plus year
  layername <- paste0("Redev_data_", as.character(years[i]))
  # create a new variable name for each year, and assign it the relevant layer for the year
  # https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
  assign(paste0("MRS_", as.character(years[i])),
         read_zipped_GIS(zipfile, file, layer = layername) %>%
           st_transform(., crs = 7899))

}

MRS_2022 <- 
  read_zipped_GIS(zipfile = "../Data/UDP2022_MRS.zip",
                  subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/UDP")

# 1.3 read in GTFS data (no longer used) ----
# -------------------------------------#
# gtfs <- read_gtfs(GTFS) %>%
#   gtfs_as_sf(., crs = 4326)
# 
# stops <- gtfs$stops %>%   
#   st_transform(7899)
# stop_times <- gtfs$stop_times
# trips <- gtfs$trips
# routes <- gtfs$routes

# agency <- gtfs$agency
# calendar <- gtfs$calendar
# calendar_dates <- gtfs$calendar_dates
# shapes <- gtfs$shapes %>%
  # st_transform(7899)



# 2. Completed projects and their PT stops and routes ----
# -----------------------------------------------------------------------------#
# 2.1 Standardise columns ----
# -------------------------------------#
# functions for year groups with same/similar variable names
standard2004_07 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(year_comp = NA, add_misc = NA, townhouses = NA, 
                  apartments = NA, max_storeys = NA, file_year = year) %>%
           # omitting 'Project_Code', 'ID', 'Classification', 'Subregion', 'suburb_id', 
           # 'melway_ref', 'muni_id', 'newmuni_id', 'developer', 'owner', 
           # 'project_start_date', 'project_end_date', 'ann_dev_rate',
           # 'approval_status', 'planning_permit', 'building_permit', 
           # 'previous_use', 'previous_use_code', 'other_info', 'record_date',
           # 'Record_Creation_Date' (2005-07 only), 'Inner_Outer', 'Council_Previous_Use',
           # 'Data_source (2004-06 only), StyleCode', 'SLANumber', 'SLAName', 
           # 'Infill_status' (2005-07 only), 'Structure_Plan' (2006 only), 'IMOG' (2005 only), 
           # 'AA_Name' (2005-07 only)
           dplyr::select(project_id = Site_ID, status = Status, year_comp,
                         area_ha = Area_ha, proj_name = project_name, proj_part = project_stage,
                         street_num = street_number, street_name, street_type,
                         add_misc, suburb = suburb_name, 
                         detached, townhouses, apartments, unknown = other, 
                         att_1s = attOneS, att_2_3s = TwotoFourSMD, att_4s = FourSMD,
                         total_dwel = Total_Dwellings,
                         max_storeys, lga = Municipality, region = M2030_region,
                         file_year, geometry = SHAPE))
}
standard2008_11 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(year_comp = NA, proj_part = NA, add_misc = NA, townhouses = NA,
                  apartments = NA, max_storeys = NA, file_year = year) %>%
           # omitting 'Project_Code', 'Classification', 'suburb_id', 'melway_ref', 
           # 'muni_id', 'developer', project_start_date', 'project_end_date',
           # 'approval_status', 'planning_permit', 'building_permit', 'record_date',
           # 'Record_Creation_Date', 'StyleCode', 'SLANumber', 'SLAName'(2009-11 only), 'AA_Name'
           dplyr::select(project_id = Site_ID, status = Status, year_comp,
                         area_ha = Area_ha, proj_name = project_name, proj_part,
                         street_num = street_number, street_name, street_type,
                         add_misc, suburb = suburb_name, 
                         detached, townhouses, apartments, unknown = Unknown, 
                         att_1s = attOneS, att_2_3s = attached2and3storey, att_4s = FourSMD,
                         total_dwel = Total_Dwellings,
                         max_storeys, lga = Municipality, region = Region,
                         file_year, geometry = SHAPE))
}

standard2012 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(proj_part = NA, add_misc = NA, townhouses = NA, apartments = NA, 
                  max_storeys = NA, file_year = year) %>%
           # omitting 'Project_Code', 'Classification', 'suburb_id', 'melway_ref', 
           # 'muni_id', 'developer', project_start_date', 'project_end_date',
           # 'approval_status', 'planning_permit', 'building_permit', 'record_date',
           # 'Record_Creation_Date', 'StyleCode', 'SLANumber', 'SLAName', 'SA2'
           dplyr::select(project_id = Site_ID, status = Status, year_comp = Year_Completed,
                         area_ha = Area_ha, proj_name = project_name, proj_part,
                         street_num = street_number, street_name, street_type,
                         add_misc, suburb = suburb_name, 
                         detached, townhouses, apartments, unknown = Unknown, 
                         att_1s = attOneS, att_2_3s = attached2and3storey, att_4s = FourSMD,
                         total_dwel = Total_Dwellings,
                         max_storeys, lga = Municipality, region = Region,
                         file_year, geometry = SHAPE))
}

standard2013_16 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(proj_part = NA, townhouses = NA, apartments = NA, max_storeys = NA, 
                  file_year = year) %>%
           # omitting 'Project_Code', 'Classification', 'suburb_id', 'melway_ref', 
           # 'muni_id', 'developer', project_start_date', 'project_end_date',
           # 'approval_status', 'planning_permit', 'building_permit', 'record_date',
           # 'Record_Creation_Date', 'StyleCode', 'SLANumber', 'SLAName', 'ID' (2013 only)
           dplyr::select(project_id = Site_ID, status = Status, year_comp = Year_Completed,
                         area_ha = Area_ha, proj_name = project_name, proj_part,
                         street_num = street_number, street_name, street_type,
                         add_misc = Address_other, suburb = suburb_name, 
                         detached, townhouses, apartments, unknown = Unknown, 
                         att_1s = attOneS, att_2_3s = attached2and3storey, att_4s = FourSMD,
                         total_dwel = Total_Dwellings,
                         max_storeys, lga = Municipality, region = Region,
                         file_year, geometry = SHAPE))
}

standard2017_21 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(att_1s = NA, att_2_3s = NA, att_4s = NA, file_year = year) %>%
           # omitting 'SE_ANNO_CAD_DATA'
           dplyr::select(project_id = PROJECTID, status = STATUS, year_comp = YEARCOMPL,
                         area_ha = AREAHA, proj_name = PROJNAME, proj_part = PROJPART,
                         street_num = STREETNUM, street_name = STREETNAME, street_type = STREETTYPE,
                         add_misc = ADDMISC, suburb = SUBURB, 
                         detached = DETACHED, townhouses = TOWNHOUSES, apartments = APARTMENTS, unknown = UNKNOWN, 
                         att_1s, att_2_3s, att_4s, total_dwel = TOTAL_DWEL,
                         max_storeys = MAXSTOREYS, lga = LGA, region = REGION,
                         file_year, geometry = SHAPE))
}

standard2022 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(project_id = paste0("2022_", row_number()),
                  status = case_when(YEARCOMPL %in% c("2021", "2022") ~ "Completed",
                                     TRUE ~ "Uncompleted"),
                  proj_name = NA, proj_part = NA, street_num = NA, street_name = NA,
                  street_type = NA, add_misc = NA, suburb = NA, 
                  unknown = TOTAL_DWEL - (DETACHED + TOWNHOUSES + APARTMENTS),
                  att_1s = NA, att_2_3s = NA, att_4s = NA, lga = NA, region = NA,
                  file_year = year) %>%
           dplyr::select(project_id, status, year_comp = YEARCOMPL, area_ha = AREAHA,
                         proj_name, proj_part, street_num, street_name, street_type,
                         add_misc, suburb, detached = DETACHED, townhouses = TOWNHOUSES, 
                         apartments = APARTMENTS, unknown, att_1s, att_2_3s, att_4s, 
                         total_dwel = TOTAL_DWEL, max_storeys = MAXSTOREYS, 
                         lga, region, file_year, geometry))
}


standard_2004 <- standard2004_07(2004)
standard_2005 <- standard2004_07(2005)
standard_2006 <- standard2004_07(2006)
standard_2007 <- standard2004_07(2007)

standard_2008 <- standard2008_11(2008)
standard_2009 <- standard2008_11(2009)
standard_2010 <- standard2008_11(2010)
standard_2011 <- standard2008_11(2011)

standard_2012 <- standard2012(2012)

standard_2013 <- standard2013_16(2013)
standard_2014 <- standard2013_16(2014)
standard_2015 <- standard2013_16(2015)
standard_2016 <- standard2013_16(2016)

standard_2017 <- standard2017_21(2017)
standard_2019 <- standard2017_21(2019)
standard_2018 <- standard2017_21(2018)
standard_2020 <- standard2017_21(2020)
standard_2021 <- standard2017_21(2021)

standard_2022 <- standard2022(2022)


# 2.2 Compile single list of completed projects ----
# -------------------------------------#
# For background, see checks in 'apartment cleaning checks.R' sections #1 and #10
# and emails from D Matthews.  Before 2012, did not include a completion date, and
# one file could contain completions from multiple past years, so assume 
# completion occurred in first year that a project is marked complete.  From 2012,
# completion year is specified, and latest information should be relied on, as
# changes in a later file should reflect new information.  For other details, such
# as dwelling numbers, rely on the latest information (unless it is clearly
# wrong, in the case of a later file specifying zero total dwellings).

# Regarding NA and duplicate project IDs, see 'apartment cleaning checks.R, section 3:
# NAs can be disregarded as they don't concern completed projects; and there is only 
# one duplicate project ID (R15189) that affects completed projects

# For 2022, no project_id has been provided (one has been created in the form
# 2022_[row number]), so no filtering based on the project id applies

# Begin by combining standardised files
completed.projects <- rbind(standard_2004, standard_2005, standard_2006, standard_2007,
                            standard_2008, standard_2009, standard_2010, standard_2011,
                            standard_2012, standard_2013, standard_2014, standard_2015,
                            standard_2016, standard_2017, standard_2018, standard_2019,
                            standard_2020, standard_2021, standard_2022) %>%
  
  # Filter to projects marked as 'Completed' in their final file year (note - this
  # removes projects that are marked 'Completed' in one file year but then their
  # 'Completed' status is revoked in a subsequent year
  group_by(project_id) %>%
  filter(status[which.max(file_year)] == "Completed") %>%
  # ungroup() %>%
  
  # Filter to only 'Completed' rows
  filter(status == "Completed") %>% # 9923 obs to 2021; 10436 obs including 2022
  
  # Determine completion year for each project, which is:
  # - projects with all file years before 2012: assumed to be the earliest file
  #   year (these projects have no year_comp provided)
  # - projects with one or more file years from 2012: the year_comp specified
  #   in the latest file year (year_comp is provided for file years from 2012)
  # That is, if all year_comp's for the project_id are NA, then use the earliest
  #   file year; otherwise, use the year specified in the latest file year
  mutate(year_comp = if_else(all(is.na(year_comp)),
                              as.numeric(min(file_year)),
                              as.numeric(year_comp[which.max(file_year)]))) %>%
 
  # Remove all projects with zero total dwellings (with the result that:
  # - if there are multiple entries for a  project_id, the one(s) with
  #   zero total dwellings will not be retained, even if it is the latest
  #   file year), and
  # - if there is only a single entry for a project_id, it will be omitted)
  # Note that in all cases where total_dwel is zero, all individual dwelling
  #   categories are also zero (see section 10.3 of apartment cleaning checks.R)
  filter(total_dwel > 0) %>%
  
  # Retain only the latest file year for each project
  filter(file_year == max(file_year)) %>% # 4975 projects to 2021; 5488 projects including 2022
  ungroup()

# Note that while there are 4975 projects to 2022, there are only 4974 project_ids,
# because R15189 is used for two different projects
# length(unique(completed.projects$project_id))  # 4974 to 2021; 5487 including 2022


# write output to file
st_write(completed.projects, "../GIS/completedProjectsWithDuplicates.sqlite", delete_layer = TRUE)


# 2.3 Removing duplicate projects ----
# -------------------------------------#
# While it is rare for a single project ID to be used for multiple projects (See
# section 2.2, it is common for a single project to be repeated with different
# project IDs.  And duplicated 2022 files will all have different project
# ids, as none were provided in the base file and they were all created from
# row numbers.  This section 2.3 removes the duplicates)

# For more detail on this section, see 'apartment cleaning checks.R' sections #6, 7 and 8,
# which progressively test and refine the approach for removing duplicates.  This section
# is based on section 8 of that script.

# Section 2.3 is organised as follows.
# - 2.3.1 - load data for checking
# - 2.3.3 - filter to remove padded project_ids, eg R0302/R00302
# - 2.3.4 – collect groups of intersecting projects
#   - groups only included if intersection area exceeds 50% of the area of at least one of the affected projects.
# - 2.3.5 – filter to remove duplicate records from intersecting group with same street_num and street_name, where 
#           area_ha and total_dwel are within 20% of mean for the group
# – 2.3.6 - manual review of remaining intersecting groups. Results recorded in R/ isecManualChecked.csv.
# - 2.3.7 - combine manually checked discards with others, and filter completed.projects to remove all discards
                                                          
## 2.3.1 - load data for checking ----
# --------------------#
completed.projects.with.duplicates <- st_read("../GIS/completedProjectsWithDuplicates.sqlite") %>%
  # add a unique x_id field, to serve as consistent identifier throughout section
  mutate(x_id = row_number())  # 4975 obs. to 2021; 5488 obs including 2022


## 2.3.2 - filter to remove padded project_ids ----
# --------------------#
# removes duplicate padded project_ids (eg R0302/R00302) where same street name and suburb
completed.projects.first.filter <- completed.projects.with.duplicates %>%
  # make identifier consisting of unpadded project_id + street_name + suburb
  # but for 2022 file using unique project_id alone, as there are no addresses
  mutate(identifier = case_when(
    file_year == 2022 ~ project_id,
    TRUE ~ paste(as.numeric(substring(project_id, 2)),
                 tolower(street_name),
                 tolower(suburb)))
  ) %>%
  
  # where there is a group with the same identifier, take completion date as
  # assumed from earlier file year, unless specified in a file year on or after
  # 2012, in which case take the year of completion from the latest file year
  group_by(identifier) %>%
  mutate(year_comp = ifelse(max(file_year) < 2012,
                             year_comp[which.min(file_year)],
                             year_comp[which.max(file_year)])) %>%
  # and keep only the latest file year for each identifier group (with year_comp
  # changed, as above, for years before 2012)
  filter(file_year == max(file_year)) %>%
  ungroup()  # 4876 obs. to 2021; 5389 obs including 2022; removing 99


## 2.3.3 - collect groups of intersecting projects ----
# --------------------#
# uses st_intersection produces a dataframe with two extra columns: 
# - 'n.overlaps', which shows the number of overlaps for each record (and there is always at least one, itself;
# - 'origins',  which is the row numbers of the intersecting projects

# add w_id row numbers to completed.projects.second.filter, so that 'origins' can locate
completed.projects.first.filter <- completed.projects.first.filter %>%
  mutate(w_id = row_number())

intersections <- st_intersection(completed.projects.first.filter) %>%
  # omit where there is only one intersection (ie intersects itself)
  filter(n.overlaps > 1) %>%  # 1001 intersections to 2021; 1605 including 2022
  # omit where not a polygon or collection (ie points and lines)
  filter(st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>% # 496 intersections
  # add column for intersection area
  mutate(isec_area_ha = as.numeric(st_area(GEOMETRY)) / 10000)

# omit unless intersecting area is > 50% of at least one feature's area
# vector to hold row numbers of small intersections
small.intersections <- c()

# loop through intersections, and find those where area is <= 50% of each area
for (i in 1:nrow(intersections)) {
  # find id's of projects in the group
  ids <- unlist(intersections[i, "origins"][[1]])
  # find project areas (in ha)
  project.areas_ha <- c()
  for (j in 1:length(ids)) {
    project.areas_ha <- c(project.areas_ha,
                          # note that 'origins' are row numbers of the table that was intersected
                          as.numeric(st_area(completed.projects.first.filter %>%
                                               ## NB w_id here to match row numbers in table that was intersected
                                               filter(w_id == ids[j]) %>% 
                                               .$GEOMETRY)) / 10000)
  }
  # if isec.area is <= 50% of all project.areas, add to list of small intersections
  if (intersections[i, "isec_area_ha"][[1]] <= min(project.areas_ha) * 0.5) { 
    small.intersections <- c(small.intersections, i)
  } 
}

# filter intersections to those which aren't 'small'
intersections <- intersections %>%
  filter(!row_number() %in% small.intersections) # 141 intersections to 2021; 432 including 2022

# note there are 513 completions in the 2022 file, made up of 295 with year_comp=2021 
# and 218 with year_comp=2022; there are 291 new intersections arising from 2022,
# which is close to the 295 year_comp=2021; it's likely that all these most of
# the year_comp=2021 projects are genuine duplicates, with a few extra 2021
# completions not recorded until 2022

# create empty frame to hold intersecting groups, with same variables as 'completed.projects'
intersecting.groups <- completed.projects.first.filter %>%
  # add extra columns for group_id and isec_area_ha
  mutate(group_id = 0,
         isec_area_ha = 0) %>% 
  # remove all rows
  .[0, ] 

# using 'origins' in 'intersections', load groups into 'intersecting.groups', with new group_id
for (i in 1:nrow(intersections)) {
  # find id's of projects in the group
  ids <- unlist(intersections[i, "origins"][[1]])
  isec_area_ha <- intersections[i, "isec_area_ha"][[1]]
  # collect the projects
  projects <- completed.projects.first.filter %>%
    ## NB w_id here to match row numbers in table that was intersected
    filter(w_id %in% ids) %>%  
    # add group_id and isec_area_ha
    mutate(group_id = i,
           isec_area_ha = isec_area_ha)
  # add the projects to 'intersecting groups'
  intersecting.groups <- rbind(intersecting.groups,
                               projects)
} 
# 287 obs. in 141 groups to 2021; 874 obs. in 432 groups including 2022


## 2.3.4 - filter to remove duplicate records from intersecting group based on address, area and dwelling details ----
# --------------------#
# Identifies intersecting groups where both street number and street name are the same, 
# and area and total_dwel are each within 20% of group mean, and keeps only latest file year
# Not effective for 2022 as no address details are provided

group_ids <- unique(intersecting.groups$group_id)

# empty dataframe to hold outputs
intersecting.groups.flagged <- intersecting.groups %>% .[0, ]

# loop through intersecting groups, and flag as 'keep', 'discard' or 'further checking'
for (i in 1:length(group_ids)) {
  group <- intersecting.groups %>%
    # select the rows for the relevant group
    filter(group_id == group_ids[i]) %>%
    # add an identifier
    mutate(identifier = paste(street_num,
                              tolower(street_name)))
  
  # see whether or not there is more than one identifier  
  identifiers <- unique(group$identifier)
  
  # check the maximum year
  max.file.year <- max(group$file_year)
  
  # find mean area_ha and total_dwell for group, and check whether all are within tolerances
  # (that is, 20% of mean area_ha and total_dwel for the group)
  mean.area_ha <- mean(group$area_ha)
  mean.total_dwel <- mean(group$total_dwel)
  mean.year <- mean(group$year_comp)
  
  tolerances <- TRUE
  for (j in 1:nrow(group)) {
    # checking whether area/dwell are more than 20% from mean
    if(abs(group$area_ha[j] - mean.area_ha) > mean.area_ha * 0.2 |
       abs(group$total_dwel[j] - mean.total_dwel) > mean.total_dwel * 0.2) {
      tolerances <- FALSE
    }
  }
  
  # add tight area tolerance (1%) for 2021 duplicate completions where no address 
  # identifiers: area within 1% of mean and dwel identical
  tight.tolerances <- FALSE
  for (j in 1:nrow(group)) {
    if(mean.year == 2021 &
       abs(group$area_ha[j] - mean.area_ha) <= mean.area_ha * 0.01 &
       abs(group$total_dwel[j] - mean.total_dwel) == 0) {
      tight.tolerances <- TRUE
    }
  }

  
  # if only one (ie all have same identifier), AND area_ha and total_dwell are 
  # within tolerances, AND there is no 2022 file involved (because its identifiers
  # will be NA and so may make a false match), then flag the highest file year 
  # to keep and others to discard
  if (length(identifiers) == 1 & 
      tolerances == TRUE &
      max.file.year < 2022) {
    
    # if they all have the same street identifier and area & dwel are within tolerances,
    # then keep the highest file year
    group <- group %>%
      mutate(flag = if_else(file_year == max(file_year), 
                            "keep",
                            "discard"))
    
    # second check where duplicate file years - keep highest project id
    if (nrow(group %>% filter(flag == "keep")) > 1) {
      print(paste("duplicate 'keep' for group no ", group_ids[i]))
      # find highest project id of the "keep"s
      highest_id <- max(group %>% filter(flag == "keep") %>% .$project_id)
      group <- group %>%
        mutate(flag = if_else(flag == "keep" & project_id == highest_id,
                              "keep",
                              "discard"))
    }
    
  } else if (nrow(group) == 2 &
             tight.tolerances == TRUE) {
    # for 2021 completions where there are only 2 group members and
    # area is within 1% of mean and dwel is identical - 
    # keep the highest file year (2022) (note - tests of '2 group members and 
    # mean year is 2021' means either both are 2021, or - in theory - one is 
    # 2020 and one is 2022; this approach is fine for either)
    group <- group %>%
      mutate(flag = if_else(file_year == max(file_year), 
                            "keep",
                            "discard"))
    
  } else {
    # if not all same identifier, and not a 2022 file duplicate, flag for further checking
    group <- group %>%
      mutate(flag = "further checking")
  }
  
  # add the flagged group to 'intersecting groups flagged'
  intersecting.groups.flagged <- rbind(intersecting.groups.flagged,
                                       group)
}

# record 'keeps1' and 'discards1', to be combined with results of 
# manual checking of 'further checks'
keeps1 <- intersecting.groups.flagged %>%
  filter(flag == "keep") %>%
  .$x_id

discards1 <- intersecting.groups.flagged %>%
  filter(flag == "discard") %>%
  .$x_id
length(unique(discards1))  # 64 to discard to 2021; 304 including 2022

# extract 'further checking' for manual check
intersecting.groups.manual.to.check <- intersecting.groups.flagged %>%
  filter(flag == "further checking") %>%
  # add 'area_pct' column (concerting st_area to ha) to help identify extent of overlaps
  mutate(isec_pct = isec_area_ha / (as.numeric(st_area(GEOMETRY)) / 10000) * 100 ) %>%
  st_drop_geometry()
length(unique(intersecting.groups.manual.to.check$group_id))  
# 215 obs. in 106 groups to 2021; 268 obs. in 130 groups including 2022

# save output for manual check
write.csv(intersecting.groups.manual.to.check, "./isecManualToCheck.csv", row.names = FALSE)


## 2.3.5 - manual review of remaining intersecting groups  ----
## --------------------#
# items flagged for manual check as above reviewed, and 'flag' column completed
# with keep/discard

# load manually checked file
intersecting.groups.manual.checked <- read.csv("./isecManualChecked.csv")

# note that Excel has corrupted the street_num column,  by reading some street numbers
# as dates, eg 6-8 >> 6-Aug.  So don't use the file for anything other than keeps/discards as below

# verify that x_ids are the same in checked' file as 'to check' file
to.check.x_ids <- intersecting.groups.manual.to.check$x_id
checked.x_ids <- intersecting.groups.manual.checked$x_id

to.check.x_ids[!(to.check.x_ids %in% checked.x_ids)] # should return none
checked.x_ids[!(checked.x_ids %in% to.check.x_ids)]  # should return none


keeps2 <- intersecting.groups.manual.checked %>%
  filter(flag == "keep") %>%
  .$x_id

discards2 <- intersecting.groups.manual.checked %>%
  filter(flag == "discard") %>%
  .$x_id
length(unique(discards2))  # 51 to discard to 2021; 79 including 2022


## 2.3.6 - combine manually checked discards with others, and filter completed.projects to remove all discards ----
# --------------------#
keeps <- c(keeps1, keeps2)
discards <- c(discards1, discards2)

# check that no projects appear in both keeps and discards 
keeps[keeps %in% discards]  # none
discards[discards %in% keeps]  # none

# remove discards from 'final' file
completed.projects.second.filter <- completed.projects.first.filter %>%
  filter(!x_id %in% discards)  
# 4788 obs, removing 88, to 2021; 5007 obs, removing 382, including 2022 (number
# is so much higher with 2022 because lack of project ids means no projects
# in the 2022 file have been removed in previous steps based on same project id)

# where 2022 file duplicates project from 2021 file, and the 2022 file project is kept,
# use addresses from the 2021 file (but not reading them in from the manual checking file,
# because excel has corrupted street numbers) - note only includes 2022 file addresses
# where they are duplicates of 2021 file projects; others will remain blank
completed.projects.2022.addresses <- 
  add2022addresses(completed.projects.second.filter,
                   intersecting.groups.flagged,
                   intersecting.groups.manual.checked)

# write output
st_write(completed.projects.2022.addresses %>%
           # remove fields added during duplication removal process
           dplyr::select(-c(x_id, identifier, w_id)), 
         "../GIS/completedProjects.sqlite", delete_layer = TRUE)



# 2.4 Add stops to completed projects; find walkable catchments ----
# -------------------------------------#
# Finds stops within 800m walking distance of completed projects
# For tram and bus stops - also locates non-current stops where necessary
# because of historic route changes

# Requires at least 6GB of memory (possibly less now).  
# Make sure as few R objects are loaded, 
# and as few other programs running, as possible. If still fails due to memory 
# shortage, may need to restructure code so that 'roads' and 'stops' aren't
# loaded at same time - that is, find stop no's first, and then remove 'roads' 
#  and find stops. [This all seems less of a problem with OSM roads than Vicmap.]

# Also locates polygon areas within 800m walking distance of PT stops

### 2.4.1 read in completed projects and set WALKDIST ----
### -------------------------------------#
completed.projects <- st_read("../GIS/completedProjects.sqlite") 

# test <- completed.projects[1:10,]

WALKDIST <- 800

### 2.4.2 read in roads and stops ----
### -------------------------------------#
# read in roads
## old approach Vicmap Roads
# roads <- read_zipped_GIS("../Data/TR_ROAD.zip",
#                          subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMTRANS") %>%
#   filter(!(CLASS_CODE %in% c(0, 13, 14))) %>% # 0-Freeway, 13-Paper Road, 14-Ferry Route - see http://services.land.vic.gov.au/catalogue/metadata?anzlicId=ANZVI0803002595&publicId=guest&extractionProviderId=1#tab2
#   # filter to two fields (no attributes are actually needed at all, and better to reduce size;
#   # but 'points2network' Approach 2 only works if you have some attributes for which data is provided,
#   # and seems to require at least two, and it's easiest to pick numeric ones)
#   dplyr::select(PFI, UFI)

## first updated approach OSMv1 - exclude motorways and trunk roads
## (not used, because trunk roads can be important for walking, eg Warburton Hwy)
# roads <- st_read("../GIS/melbourne_network.sqlite", layer = "links") %>%
#   filter(is_walk == 1)  # note that this excludes motorways and trunk routes (though most trunk routes have separate footpaths)

## second updated approach OSMv2 - motorways are already excluded, and 
## nothing else needs to be
roads <- st_read("../GIS/melbourne_network.sqlite", layer = "links") %>%
  dplyr::select(from_id, to_id)
  # filter to two fields (no attributes are actually needed at all, and better to reduce size;
  #   # but 'points2network' Approach 2 only works if you have some attributes for which data is provided,
  #   # and seems to require at least two) 

# read in rail stops - based on current (Sep 2022) stations
# see section 3 of 'train cleaning checks.R' for reconciliation of GTFS and DOT
# station names, and steps taken to align
train.stops <- read_gtfs("../Data/gtfs_20220929.zip") %>%
  gtfs_as_sf(., crs = 4326) %>%
  .$stops %>%   
  st_transform(7899) %>%
  # filter to rows containing 'Railway Station' and not '/' (used for bus or tram stops at stations) 
  filter(grepl("Railway Station", stop_name) & !grepl("/", stop_name)) %>%
  # replace the pattern 'space + Railway + any number of other characters' with nothing
  mutate(station_name = gsub(" Railway.*","", stop_name)) %>%
  # add type
  mutate(type = "train") %>%
  # if geom column is 'GEOMETRY', rename as 'geometry' (for consistency when combining)
  rename(any_of(c(geometry = "GEOMETRY")))


# read in tram stops - created in section 3 of trams.R, comprises current stops
# (from Sep 2009 GTFS) plus additional stops in former Domain Rd section of 
# Toorak Rd route (from Mar 2015 GTFS)
tram.stops <- st_read("../GIS/tram stop list.sqlite") %>%
  # add type
  mutate(type = "tram") %>%
  # if geom column is 'GEOMETRY', rename as 'geometry' (for consistency when combining)
  rename(any_of(c(geometry = "GEOMETRY")))

# read in bus stops - created in section 5 of buses.R, comprises most recent
# location of all stops used since 2015
bus.stops <- st_read("../GIS/bus stop list.sqlite") %>%
  # add type
  mutate(type = "bus") %>%
  # if geom column is 'GEOMETRY', rename as 'geometry' (for consistency when combining)
  rename(any_of(c(geometry = "GEOMETRY")))

# combine all stops
stops <- bind_rows(train.stops, tram.stops, bus.stops)


### 2.4.3 find stops within walkable distance ----
### -------------------------------------#
# note - takes about 12 hours; could restructure for parallel processing
for (i in 1:nrow(completed.projects)) {
# for (i in 1:nrow(test)) {
# for (i in 1261:1270) {

    project <- completed.projects[i,] %>%
      st_centroid(.)
    project.buffer <- st_buffer(project, WALKDIST)
    
    # find roads which intersect buffer
    project.roads <- roads[st_intersects(project.buffer, roads)[[1]],] 
    
    # only continue if there are roads (ie if within area covered by road network)
    if (nrow(project.roads) > 0) {
      project.roads <- project.roads %>%
        # convert to spatial lines dataframe object (as required by shp2graph)
        as_Spatial()
      
      # checking the connectivity (using shp2graph)
      # conn <- nt.connect(project.roads)
      ## Sometimes there are some walkable disconnected roads,
      ##  or problems when an apartment connects to a small disconnected road
      ##  another reason for possibly using OSM instead
      ## The findWalkableStops function uses nt.connect to use largest disconnected
      ##  graph
      
      # find stops which intersect buffer
      candidate.stops <- stops[st_intersects(project.buffer, stops)[[1]], ]
      
      # find which candidate stops are within 800m walking distance
      if (nrow(candidate.stops) > 0) {
        project.stop.nos <- findWalkableStops(project.roads, 
                                              project, 
                                              candidate.stops, 
                                              WALKDIST)
      } else {
        project.stop.nos <- NULL
      }
      
      # add any located stops to the relevant fields (as comma-separated strings)
      if (length(project.stop.nos) > 0) {
        project.stops <- stops %>%
          filter(stop_id %in% project.stop.nos)
        completed.projects[i, "rail_stn"] <- toString(project.stops %>%
                                                        # test[i, "train_stn"] <- toString(project.stops %>%
                                                        filter(type == "train") %>%
                                                        .$station_name %>%
                                                        unique()%>%
                                                        sort())
        completed.projects[i, "tram_stop"] <- toString(project.stops %>%
                                                         # test[i, "tram_stop"] <- toString(project.stops %>%
                                                         filter(type == "tram") %>%
                                                         .$stop_id %>%
                                                         unique() %>%
                                                         sort())
        completed.projects[i, "bus_stop"] <- toString(project.stops %>%
                                                        # test[i, "bus_stop"] <- toString(project.stops %>%
                                                        filter(type == "bus") %>%
                                                        .$stop_id %>%
                                                        unique() %>%
                                                        sort())
        
      }
      
    }
    
 
  # progress reporting
  if (i %% 50 == 0) {
    print(paste("Completed", i, "of", nrow(completed.projects), "projects at", Sys.time()))
  }
}
## Vicmap roads: about 11 to 12 hrs (on new i7 computer, and about 12 to 13 on Asus with linux)
## OSM: about 10 hours (on new i7 computer); or 12 hours once 2022 is included

# See note in 'findWalkableStops.R' regarding 'In showSRID(SRS_string, format = "PROJ"...' error message


### 2.4.4 manual adjustments - completed projects ----
### -------------------------------------#
# # (For Vicmap roads only - uncomment to use)
# # Project R09066, Rufus St Apartments at 42 Rufus St Epping, is missing
# # Epping station.  Clearly within walking distance; omitted because Epping
# # snaps to an unhelpful node.  Add Epping (either as the only station, if none,
# # or as an addition to what's already there, otherwise); and see also the 
# # corresponding adjustment to the catchment in section 2.4.7
# completed.projects[completed.projects$project_id == "R09066", "rail_stn"] <- 
#   case_when(
#     completed.projects[completed.projects$project_id == "R09066", "rail_stn"][[1]] == ""
#       ~  "Epping",
#     TRUE 
#       ~ paste0(completed.projects[completed.projects$project_id == "R09066", "rail_stn"][[1]], ", Epping")
#   )

# Name of Jolimont station needs to be changed from 'Jolimont-MCG' to 'Jolimont',
# to match DoT volume figures.  (Flinders Street and Southern Cross also don't
# match, but they are addressed by changing the DoT names in section 4.1 of 
# 'trains.R'.)
completed.projects <- completed.projects %>%
  mutate(rail_stn = str_replace(rail_stn, "Jolimont-MCG", "Jolimont"))


### 2.4.5 write 'stops within walkable distance' output to file ----
### -------------------------------------#
st_write(completed.projects, 
         "../GIS/completed_projects_with_stops.sqlite", 
         delete_layer = TRUE)

write.csv(completed.projects %>% st_drop_geometry(), 
          "../GIS/completed_projects_with_stops.csv", 
          row.names = FALSE)


### 2.4.6 find walkable catchments around PT stops ----
### -------------------------------------#
# find stops that are walkable from completed projects (that is, which were
# selected in section 2.4.3 and added to 'completed.projects')
# completed.projects <- st_read("../GIS/completed_projects_with_stops.sqlite")
# 
# walkable.train.stops <- stopList(completed.projects$rail_stn) 
# walkable.tram.stops <- stopList(completed.projects$tram_stop)
# walkable.bus.stops <- stopList(completed.projects$bus_stop)
# 
# walkable.stops <- stops %>%
#   # change name of Jolimont (see section 2.4.4)
#   mutate(station_name = str_replace(station_name, "Jolimont-MCG", "Jolimont")) %>%
#   filter(station_name %in% walkable.train.stops |
#            stop_id %in% walkable.tram.stops |
#            stop_id %in% walkable.bus.stops) 

# finding catchments for all projects

# note - with around 22000 stops, and 8 cores, takes about 4 days to run for OSM 
# (possibly more with Vicmap roads; the intersection to find local roads takes longer
# because the Vicmap dataset covers entire State)
# writes each individual stop's details to separate file in 'catchments'

# note for Vicmap roads 2 stops (Adelaide, Murray Bridge) are outside Vic, and no catchment is found
# for OSM, many stations are outside Melb and so no catchment is found, and also 
# some bus stops around Bendigo and Eildon

walkable.stops <- stops %>%
  # change name of Jolimont (see section 2.4.4)
  mutate(station_name = str_replace(station_name, "Jolimont-MCG", "Jolimont"))

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

# loop to find walkable catchments - note that this will not actually produce
# an 'output' file, because nothing is returned by the loop - instead, it
# writes every stop to the temporary 'catchments' folder
output <-
  foreach(i = 1:nrow(walkable.stops),
  # foreach(i = 16696:nrow(walkable.stops),
  # foreach(i = 1:8,
          .combine = rbind,
          .packages = c("dplyr", "sf", "shp2graph", "igraph", "nngeo"),
          # .verbose = TRUE,  # can using this as progress reporting not working (or just look at 'catchments')
          .options.snow = opts) %dopar% {
            
            catchment <- findWalkableCatchment(walkable.stops[i, ], roads, WALKDIST)
            saveRDS(catchment, paste0("./catchments/stop_", i, ".rds"))
            
            # flush memory without removing environmental variables
            .rs.restartR()
            
          }

# close the progress bar and cluster
close(pb)
stopCluster(cluster)


# combine all the stop files saved to 'catchments' 
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
  
  # progress reporting
  if (i %% 100 == 0) {
    print(paste("Processed", i, "of", length(list.files("./catchments")), "stops"))
  }
  
}

# combine all the 'stop_' files
# begin with the first variable (stop_1)
stop.catchments <- get(stop.nos[1])
# then add the others
for (i in 2:length(stop.nos)) {
  stop.catchments <- rbind(stop.catchments, get(stop.nos[i]))
  
  # progress reporting
  if (i %% 100 == 0) {
    print(paste("Added", i, "of", length(stop.nos), "stops"))
  }
  
}


### 2.4.7 manual adjustments - catchments ----
### -------------------------------------#
# Only required for Vicmap roads, not OSM.  Uncomment to use

# # manual adjustment to Epping's catchment to take in the apartment that was
# # manually altered to include Epping in section 2.4.4 (though note that even
# # with this adjustment, Epping station is still not well connected; its 
# # catchment covers only areas to the north of the station)
# 
# # read in apartments and catchments if not already loaded
# # completed.projects <- st_read("../GIS/completedProjects.sqlite") 
# # stop.catchments <- st_read("../GIS/stop catchments.sqlite")
# 
# # extract geomtry (as points) from apartment and catchment
# epping.apt <- completed.projects %>% 
#   filter(project_id == "R09066") %>%
#   st_centroid(.) %>%
#   dplyr::select(geometry = GEOMETRY)
# 
# epping.stn <- stop.catchments %>%
#   filter(stop_name == "Epping Railway Station (Epping)") %>%
#   # extract vertices
#   st_coordinates(.) %>%
#   as.data.frame %>%
#   st_as_sf(., 
#            coords = c("X", "Y"), 
#            crs = st_crs(completed.projects)) %>%
#   dplyr::select(geometry)
#   
# epping.points <- st_union(epping.apt, epping.stn)
# 
# new.epping.catchment <- st_convex_hull(st_union(epping.points)) %>%
#   st_as_sf()
# 
# # check by plotting
# ggplot() +
#   geom_sf(data = completed.projects %>%
#             filter(project_id == "R09066") %>%
#             st_centroid(.),
#           colour = "blue") +
#   geom_sf(data = stop.catchments %>%
#             filter(stop_name == "Epping Railway Station (Epping)"),
#           colour = "red", fill = NA) +
#   geom_sf(data = new.epping.catchment,
#     colour = "green", fill = NA)
# 
# # set the geometry of Epping station catchment to match the new catchment
# st_geometry(stop.catchments[stop.catchments$stop_name == "Epping Railway Station (Epping)", ]) <-
#   st_geometry(new.epping.catchment)
#  

### 2.4.8 write 'walkable catchment' outputs ----
### -------------------------------------#
st_write(stop.catchments,
         "../GIS/stop catchments.sqlite",
         delete_layer = TRUE)

# # delete the 'catchments' folder once outputs are written
# dir_delete("./catchments")



# 3. Investigate density for building types ----
# -----------------------------------------------------------------------------#
# 3.1 Calculate and summarise densities ----
# -------------------------------------#
# note - may instead need to load 'completedProjects(crowflies).sqlite' or 
#'completedProjects(walking).sqlite, created under section 2.4
completed.projects <- st_read("../GIS/completedProjects.sqlite")

densities <- completed.projects %>%
  # calculate density (noting there are two area=0 which need to be fixed)
  mutate(area_ha = if_else(area_ha > 0, area_ha, as.numeric(st_area(GEOMETRY))),
         ## note - 'st_area' is wrong; it should be divided by 10000 to produce hectares
         ## but actually the two WILLS St are problematic duplications digitised as dots anyway
         density = total_dwel / area_ha,
  # mutate(density = total_dwel / as.numeric(st_area(GEOMETRY)) * 10000,
  type = case_when(
    apartments > 0 & townhouses == 0 & detached == 0 & unknown == 0 ~ "apartments",
    att_4s > 0 & (att_1s == 0 & att_2_3s == 0) & detached == 0 & unknown == 0 ~ "4+ storey attached (pre 2017)",
    
    apartments == 0 & townhouses > 0 & detached == 0 & unknown == 0 ~ "townhouses",
    att_4s == 0 & (att_1s > 0 | att_2_3s > 0) & detached == 0 & unknown == 0 ~ "1-3 storey attached (pre 2017)",
    
    apartments > 0 & townhouses > 0 & detached == 0 & unknown == 0 ~ "mixed apartments/townhouses",
    att_4s > 0 & (att_1s > 0 | att_2_3s > 0) & detached == 0 & unknown == 0 ~ "mixed attached (pre 2017)",
    
    (apartments == 0 & townhouses == 0 & detached > 0 & unknown == 0) |
      (att_4s == 0 & (att_1s == 0 & att_2_3s == 0) & detached > 0 & unknown == 0) ~ "detached",
    
    (apartments == 0 & townhouses == 0 & detached == 0 & unknown > 0) |
      (att_4s == 0 & (att_1s == 0 & att_2_3s == 0) & detached == 0 & unknown > 0) ~ "unknown",
    
    ((apartments > 0 | townhouses > 0)  & (detached > 0 | unknown > 0)) |
      ((att_4s > 0 | att_1s > 0 | att_2_3s > 0) & (detached > 0 | unknown > 0)) ~ "mixed attached & detached/unknown",
    
    TRUE ~ "unallocated")) %>%  # covers apartment/townhousesmix of detached & unknown, and all types zero (see 'apartment cleaning checks.R' #5.1)
  st_drop_geometry() %>%
  # re-order for display
  mutate(type = factor(type, levels = c("apartments","4+ storey attached (pre 2017)",
                                        "townhouses", "1-3 storey attached (pre 2017)",
                                        "mixed apartments/townhouses", "mixed attached (pre 2017)",
                                        "mixed attached & detached/unknown",
                                        "detached", "unknown", "unallocated")))


# summarise details
density.summaries <- densities %>%
  group_by(type) %>%
  summarise(n_projects = n(),
            n_dwellings = sum(total_dwel),
            mean = mean(density),
            max = max(density),
            min = min(density),
            sd = sd(density),
            median = median(density))

write.csv(density.summaries, "../Tables/density summaries.csv", row.names = FALSE)


# 3.2 Display as boxplot ----
# -------------------------------------#
plot <- ggplot(data = densities) +
  geom_boxplot(aes(x = type, y = density)) +
  labs(x = "Type of dwelling",
       y = "Density (dwellings / ha), log scale",
       title = "Density (dwellings / ha) of major redevelopment sites, Melbourne",
       subtitle = "Projects completed 2004-2022",
       caption = "Data provided by DELWP (Victoria), Urban Development Program
Categories for attached dwellings: 
2004-2007: attOneS, TwotoFourSMD, FourSMD; 2008-2016: attOneS, attached2and3storey, FourSMD; 2017-2021: townhouses, apartments
'Unallocated' category comprises a mix of detached and unknown (1 project); not shown are anomalous results showing zero dwellings for all categories (28 projects)") + 
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0))

# note - 'plot' will produce the following error messages
# 1: Transformation introduced infinite values in continuous y-axis 
# 2: Removed 28 rows containing non-finite values (stat_boxplot). 
# This is because the log of the 28 zero-density projects in 'unallocated' is -Inf
# The caption explains that they are omitted

png( "../Images/densities by dwelling category.png", width = 20000, height = 10000, res = 1000)
plot
dev.off()


# 3.3 Number of storeys ----
# -------------------------------------#
# Compile number of storeys for dwellings classified as 'apartments' (2017-21)
apartment.storeys <- densities %>%
  filter(apartments > 0) %>%
  mutate(storey.group = case_when(max_storeys == 0 | is.na(max_storeys) ~ "recorded as 0 or NA",
                                  max_storeys == 1 ~ "1",
                                  max_storeys == 2 ~ "2",
                                  max_storeys == 3 ~ "3",
                                  TRUE ~ "4 or more")) %>%  
  group_by(storey.group) %>%
  summarise(n_projects = n(),
            n_dwellings = sum(total_dwel))

write.csv(apartment.storeys, "../Tables/apartment storeys.csv", row.names = FALSE)         


# 3.4 Percentages of att_4s/apartments 100+ dwellings/ha ----
# -----------------------------------#
# load completed.projects
completed.projects <- st_read("../GIS/completedProjects.sqlite") 

# add density field (dwell/ha)
completed.projects <- completed.projects %>%
  mutate(dwel_ha = total_dwel / area_ha,
         dens_100plus = if_else(round(dwel_ha, 1) >= 100, "yes", "no")) # rounding to avoid floating point exclusions of exactly 100 (see 'checks/apartment cleaning checks.R' section 9.1)

# count att_4s dwellings that are / are not density 100+
att4s_100plus <- completed.projects %>%
  st_drop_geometry() %>%
  filter(att_4s > 0) %>%
  group_by(dens_100plus) %>%
  summarise(dwellings = sum(att_4s))

# calculate percentage att_4s that are density 100+
att4s_pct100plus <- att4s_100plus[att4s_100plus$dens_100plus == "yes", "dwellings"] / 
  sum(att4s_100plus[, "dwellings"]) *
  100
att4s_pct100plus  # 95.79526 up to 2021; 95.78471 with 2022 included


# count apt dwellings that are / are not density 100+
apt_100plus <- completed.projects %>%
  st_drop_geometry() %>%
  filter(apartments > 0) %>%
  group_by(dens_100plus) %>%
  summarise(dwellings = sum(apartments))

# calculate percentage apartments that are density 100+
apt_pct100plus <- apt_100plus[apt_100plus$dens_100plus == "yes", "dwellings"] / 
  sum(apt_100plus[, "dwellings"]) *
  100
apt_pct100plus  # 96.86924 up to 2021; 96.83957 with 2022 included


# 4.  Identify and calculate numbers of apartments and other high density ----
# -----------------------------------------------------------------------------#
# See section 9 of 'checks/apartment cleaning checks.R' for notes on this section

## 4.1 Identify apartments and other high density and their numbers ----
## -------------------------------------#
# select one - 
# version <- "crowflies"
version <- "walking"

if (version == "crowflies") {
  completed.projects <- st_read("../GIS/completedProjects_crowflies.sqlite")
} else if (version == "walking") {
  completed.projects <- st_read("../GIS/completedProjects_walking.sqlite")
}

completed.projects <- completed.projects %>%
  rowwise() %>%  # required so 'sum' can be used in hi_dens_dwel
  mutate(dwel_ha = total_dwel / area_ha,
         
         hi_dens = case_when(apartments > 0 | att_4s > 0 ~ "yes",
                             round(dwel_ha, 1) >= 100 ~ "yes", # rounding to avoid floating point exclusions of exactly 100
                             TRUE ~ "no"),
         
         hi_dens_dwel = case_when(apartments > 0 | att_4s > 0 ~ sum(apartments, att_4s, na.rm = TRUE),
                                  round(dwel_ha, 1) >= 100 ~ total_dwel)) %>%
  ungroup()

if (version == "crowflies") {
  st_write(completed.projects, "../GIS/completedProjects_crowflies.sqlite", delete_layer = TRUE)
  write.csv(completed.projects %>% st_drop_geometry(), "../GIS/completedProjects_crowflies.csv", row.names = FALSE)
} else if (version == "walking") {
  st_write(completed.projects, "../GIS/completedProjects_walking.sqlite", delete_layer = TRUE)
  write.csv(completed.projects %>% st_drop_geometry(), "../GIS/completedProjects_walking.csv", row.names = FALSE)
}

## 4.2 Calculate total number of apartments and other high density ----
## -------------------------------------#
# dwellings
total.hi_dens_dwel <- sum(completed.projects$hi_dens_dwel, na.rm = TRUE)
total.hi_dens_dwel  # 214,149 total high density dwellings

# projects
total.hi_dens.proj <- nrow(completed.projects %>%
                             filter(hi_dens == "yes"))
total.hi_dens.proj # 3146 total projects with high density dwellings


## 4.3 Repeat calculation of numbers of apartments and other high density numbers including 2022 ----
## -------------------------------------#
# completed projects, revised to include 2022
completed.projects <- st_read("../GIS/completedProjects.sqlite")

completed.projects <- completed.projects %>%
  rowwise() %>%  # required so 'sum' can be used in hi_dens_dwel
  mutate(dwel_ha = total_dwel / area_ha,
         
         hi_dens = case_when(apartments > 0 | att_4s > 0 ~ "yes",
                             round(dwel_ha, 1) >= 100 ~ "yes", # rounding to avoid floating point exclusions of exactly 100
                             TRUE ~ "no"),
         
         hi_dens_dwel = case_when(apartments > 0 | att_4s > 0 ~ sum(apartments, att_4s, na.rm = TRUE),
                                  round(dwel_ha, 1) >= 100 ~ total_dwel)) %>%
  ungroup()

# calculate numbers - note that these may include apartments which are not
# within walking distance of PT, whereas those apartments are excluded
# in section 2.1.1 of the calculation of outputs in 'analysis.R'

# dwellings
total.hi_dens_dwel <- sum(completed.projects$hi_dens_dwel, na.rm = TRUE)
total.hi_dens_dwel  # 223,709 total high density dwellings

# projects
total.hi_dens.proj <- nrow(completed.projects %>%
                             filter(hi_dens == "yes"))
total.hi_dens.proj # 3293 total projects with high density dwellings


# 5. Number of MRS apartments corresponding to BH review period ----
# -----------------------------------------------------------------------------#
# Background: checks of DELWP 'broadhectare' files were conducted (see
# 'BH files downloaded 2013-21.R' and 'BH file downloaded 2022.R', and
# found only two high density projects in the 'broadhectare' files for the 
# years 2013 to 2022 located within 800m of the PPTN.  Need to compare this
# with the number of projects from the 'major redevelopment site' files
# for the equivalent period)

apartments <- readRDS("./dashboard/apartments.rds")

apt.2013.2022 <- apartments %>%
  filter(year_comp >= 2013)

nrow(apt.2013.2022)  # 2258
