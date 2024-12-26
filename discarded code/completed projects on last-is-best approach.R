# Former 2.1-2.3 of apartments.R, which compiled single list of completed
# projects on basis that (1) before 2012, projects were taken to be completed
# in their file year, and (2) latest information, including latest file year
# for completion, was preferred over earlier

# Revised, following discussion with David Matthews (formerly DELWP, now DTP),
# so that (1) before 2012, projects are taken to be completed in the first
# file year where marked as such, but (2) in other respects, such as dwelling
# numbers, latest information is preferred over earlier


# 2. Completed projects and their PT stops and routes ----
# -----------------------------------------------------------------------------#
# 2.1 Standardise columns ----
# -------------------------------------#
# functions for year groups with same/similar variable names
standard2004_07 <- function(year) {
  MRS <- get(paste0("MRS_", as.character(year)))
  
  return(MRS %>%
           # adding missing columns
           mutate(add_misc = NA, townhouses = NA, apartments = NA, 
                  max_storeys = NA, file_year = year) %>%
           # fill year of completion for completed projects
           mutate(year_comp = if_else(Status == "Completed", year, 0)) %>%
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
           mutate(proj_part = NA, add_misc = NA, townhouses = NA, apartments = NA, 
                  max_storeys = NA, file_year = year) %>%
           # fill year of completion for completed projects
           mutate(year_comp = if_else(Status == "Completed", year, 0)) %>%
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


# 2.2 Compile single list of completed projects ----
# -------------------------------------#
# For background, see checks in 'apartment cleaning checks.R' section #1: from 2012,  
# each year's file also includes previous year's completions; D Matthews email 

# Regarding NA and duplicate project IDs, see 'apartment cleaning checks.R, section 3:
# NAs can be disregarded as they don't concern completed projects; and there is only 
# one duplicate project ID (R15189) that affects completed projects

# Rules for compiling single list:
# - start with earliest year's file
# - include completed projects
# - if a later year's file contains further info about the project, then:
# --- if it is still completed, substitute the later year's information,
# --- if it is now not completed, remove it from the list
# - if a later year's file simply omits the completed project (when it could have
#   been expected to be repeated in a second year), take no action - treat as still completed

years <- c(2004:2021)
duplicate.IDs <- c("R15189")

# empty data frame to hold completions repeated in consecutive years (for each affected
# project, 'duplicates' will contain both the old and new year info, and 'duplicate.counts
# will contain numbers) - for checking
duplicates <- data.frame()
duplicate.counts <- data.frame()

# empty data frame to hold completions removed by later year info (for each affected  
# project, will contain the deleted completion and the subsequent year info) - for checking
discards <- data.frame()  

# include completed projects from first year
completed.projects <- get(paste0("standard_", as.character(years[1]))) %>%
  filter(status == "Completed")

# loop through projects for subsequent years
for (i in 2:length(years)) {
  # get the file for the new year
  standard.file <- get(paste0("standard_", as.character(years[i])))
  
  # report status
  print(paste("Reviewing projects for", as.character(years[i]), "starting", Sys.time()))
  
  # set counter for 'duplicate.counts'
  duplicate.count <- 0
  
  # loop through the file for the new year
  for (j in 1:nrow(standard.file)) {
    # get the project id and status for the row
    row.project_id <- standard.file[j, "project_id"][[1]]
    row.status <- standard.file[j, "status"][[1]]
    
    # check whether the project ID already appears in 'completed.projects'
    if (row.project_id %in% completed.projects$project_id) {
      # if it does already appear, then check whether still appears as completed
      if (row.status == "Completed") {
        # if it's a known duplicate ID, then add the new completion to 'completed.projects'
        if (row.project_id %in% duplicate.IDs) {
          completed.projects <- completed.projects %>%
            rbind(., standard.file[j,])
        } else {
          # otherwise - if still completed' in the new year file, then-
          ## put the duplicate rows into 'duplicates' for checking, and increment counter
          duplicates <- rbind(
            duplicates,
            completed.projects %>% filter(project_id == row.project_id),
            standard.file[j,]
          )
          duplicate.count <- duplicate.count + 1
          
          ## substitute the new year file's data in 'completed projects'
          completed.projects <- completed.projects %>%
            # remove old row for the project id
            filter(!project_id == row.project_id) %>%
            # add new row for the project id
            rbind(., standard.file[j,])
        }
      } else {
        # if has an uncompleted status in the new year file (but only if it's
        # not a known duplicate ID), then -
        if (!row.project_id %in% duplicate.IDs) {
          ## put both old and new rows into 'discards' for checking,
          discards <- rbind(
            discards,
            completed.projects %>% filter(project_id == row.project_id),
            standard.file[j,]
          )
          ## remove the old row from 'completed projects'
          completed.projects <- completed.projects %>%
            filter(!project_id == row.project_id)
        }
      }
    } else {
      # if the project ID does not already appear in 'completed projects'
      if (row.status == "Completed") {
        # add to 'completed.projects' if it's completed (otherwise, do nothing)
        completed.projects <- completed.projects %>%
          rbind(., standard.file[j,])
      }
    }
  }
  
  # add duplicate.count to duplicate.counts - for checking
  duplicate.counts <- rbind(duplicate.counts,
                            cbind(year = years[i],
                                  completions = nrow(standard.file %>%
                                                       filter(status == "Completed")),
                                  duplicates = duplicate.count))
}

# manual adjustment to remove duplicate of one of the 'R15189' observations
completed.projects <- completed.projects %>%
  filter(!(project_id == "R15189" & proj_name == "The Reseve" & file_year == 2018))


# write output to file
st_write(completed.projects, "../GIS/completedProjects.sqlite", delete_layer = TRUE)


# 2.3 Removing duplicate projects ----
# -------------------------------------#
# While it is rare for a single project ID to be used for multiple projects (See
# section 2.2, it is common for a single project to be repeated with different
# project IDs.  This section 2.3 removes the duplicates)

# For more detail on this section, see 'apartment cleaning checks.R' sections #6, 7 and 8,
# which progressively test and refine the approach for removing duplicates.  This sectino
# is based on section 8 of that script.

# Section 2.3 is organised as follows.
# - 2.3.1 - load data for checking
# - 2.3.2 - filter to remove records where total_dwel is zero
# - 2.3.3 - filter to remove padded project_ids, eg R0302/R00302
# - 2.3.4 – collect groups of intersecting projects
#   - groups only included if intersection area exceeds 50% of the area of at least one of the affected projects.
# - 2.3.5 – filter to remove duplicate records from intersecting group with same street_num and street_name, where 
#           area_ha and total_dwel are within 20% of mean for the group
# – 2.3.6 - manual review of remaining intersecting groups. Results recorded in R/ isecManualChecked.csv.
# - 2.3.7 - combine manually checked discards with others, and filter completed.projects to remove all discards

## 2.3.1 - load data for checking ----
# --------------------#
completed.projects.with.duplicates <- st_read("../GIS/completedProjects.sqlite") %>%
  # completed.projects.with.duplicates <- st_read("../GIS/completedProjects(walking).sqlite") %>%
  # add a unique x_id field, to serve as consistent identifier throughout section
  mutate(x_id = row_number())  # 5002 obs.


## 2.3.2 - filter to remove records where total_dwel is zero ----
# --------------------#
completed.projects.first.filter <- completed.projects.with.duplicates %>%
  filter(total_dwel > 0)  # 4974 obs., removing 28


## 2.3.3 - filter to remove padded project_ids ----
# --------------------#
# removes duplicate padded project_ids (eg R0302/R00302) where same street name and suburb
completed.projects.second.filter <- completed.projects.first.filter %>%
  # make identifier consisting of unpadded project_id + street_name + suburb
  mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                            tolower(street_name),
                            tolower(suburb))) %>%
  # keep only the latest file year for each identifier group
  group_by(identifier) %>%
  filter(file_year == max(file_year)) %>%
  ungroup()  # 4875 obs., removing 99


## 2.3.4 - collect groups of intersecting projects ----
# --------------------#
# uses st_intersection produces a dataframe with two extra columns: 
# - 'n.overlaps', which shows the number of overlaps for each record (and there is always at least one, itself;
# - 'origins',  which is the row numbers of the intersecting projects

# add w_id row numbers to completed.projects.second.filter, so that 'origins' can locate
completed.projects.second.filter <- completed.projects.second.filter %>%
  mutate(w_id = row_number())

intersections <- st_intersection(completed.projects.second.filter) %>%
  # omit where there is only one intersection (ie intersects itself)
  filter(n.overlaps > 1) %>%  # 990 intersections
  # omit where not a polygon or collection (ie points and lines)
  filter(st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>% # 491 intersections
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
                          as.numeric(st_area(completed.projects.second.filter %>%
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
  filter(!row_number() %in% small.intersections) # 138 intersections


# create empty frame to hold intersecting groups, with same variables as 'completed.projects'
intersecting.groups <- completed.projects.second.filter %>%
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
  projects <- completed.projects.second.filter %>%
    ## NB w_id here to match row numbers in table that was intersected
    filter(w_id %in% ids) %>%  
    # add group_id and isec_area_ha
    mutate(group_id = i,
           isec_area_ha = isec_area_ha)
  # add the projects to 'intersecting groups'
  intersecting.groups <- rbind(intersecting.groups,
                               projects)
} 
# 281 obs. in 138 groups


## 2.3.5 - filter to remove duplicate records from intersecting group based on address, area and dwelling details ----
# --------------------#
# Identifies intersecting groups where both street number and street name are the same, 
# and area and total_dwel are each within 20% of group mean, and keeps only latest file year

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
  
  # find mean area_ha and total_dwell for group, and check whether all are within tolerances
  # (that is, 20% of mean area_ha and total_dwel for the group)
  mean.area_ha <- mean(group$area_ha)
  mean.total_dwel <- mean(group$total_dwel)
  
  tolerances <- TRUE
  for (j in 1:nrow(group)) {
    # checking whether area/dwell are more than 20% from mean
    if(abs(group[j, "area_ha"][[1]] - mean.area_ha) > mean.area_ha * 0.2 |
       abs(group[j, "total_dwel"][[1]] - mean.total_dwel) > mean.total_dwel * 0.2) {
      tolerances <- FALSE
    }
  }
  
  # if only one (ie all have same identifier), AND area_ha and total_dwell are 
  # within tolerances, then flag the highest file year to keep and others to discard
  if (length(identifiers) == 1 & 
      tolerances == TRUE) {
    
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
  } else {
    # if not all same identifier, flag for further checking
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
length(unique(discards1))  # 37 to discard

# extract 'further checking' for manual check
intersecting.groups.manual.to.check <- intersecting.groups.flagged %>%
  filter(flag == "further checking") %>%
  # add 'area_pct' column (concerting st_area to ha) to help identify extent of overlaps
  mutate(isec_pct = isec_area_ha / (as.numeric(st_area(GEOMETRY)) / 10000) * 100 ) %>%
  st_drop_geometry()
length(unique(intersecting.groups.manual.to.check$group_id))  # 209 obs. in 103 groups

# save output for manual check
write.csv(intersecting.groups.manual.to.check, "./isecManualToCheck.csv", row.names = FALSE)


## 2.3.6 - manual review of remaining intersecting groups  ----
# --------------------#
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
length(unique(discards2))  # 50 to discard


## 2.3.7 - combine manually checked discards with others, and filter completed.projects to remove all discards ----
# --------------------#
keeps <- c(keeps1, keeps2)
discards <- c(discards1, discards2)

# check that no projects appear in both keeps and discards 
keeps[keeps %in% discards]  # none
discards[discards %in% keeps]  # none

# remove discards from 'final' file
completed.projects.third.filter <- completed.projects.second.filter %>%
  filter(!x_id %in% discards)  # 4788 obs, removing 87

# write output
st_write(completed.projects.third.filter %>%
           # remove fields added during duplication removal process
           dplyr::select(-c(x_id, identifier, w_id)), 
         "../GIS/completedProjects.sqlite", delete_layer = TRUE)

