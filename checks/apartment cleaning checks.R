# apartment cleaning checks

# 1.  Check how to deal with completed developments appearing in 2 consecutive years (see 2.2 in 'apartments.R') ----
# -----------------------------------------------------------------------------#
# We've been told by DELWP that the  MRS_ files are compiled on the basis that
# they should contain the last 2 years of completed developments, so overlap is
# expected.  Scripts below check whether the overlap is complete

# 1.0 For checking completions, as required ----
# -------------------------------------#
# filter each 'standard_20xx' file to create a 'completed_20xx' file
years <- c(2004:2021)
for (i in 1:length(years)) {
  assign(paste0("completed_", as.character(years[i])),
         get(paste0("standard_", as.character(years[i]))) %>%
           filter(status == "Completed"))
}

## 1.1 Checking 2020/2021 ----
## -------------------------------------#
# checking overlaps based on project IDs
dup2021 <- completed_2021 %>%
  filter(project_id %in% completed_2020$project_id) # 198 observations

dup2020 <- completed_2020 %>%
  filter(project_id %in% completed_2021$project_id) # also 198 observations (as expected)
# so there are 198 that are in both; from inspection, they both have the same project names (where
# not NA), except that in a couple of cases one project name has an extra space so that they don't appear to align

# now check non-overlaps - 
extra2021 <- completed_2021 %>%
  filter(year_comp == 2020 & !project_id %in% completed_2020$project_id) # 32 observations
# so 32 more completed 2020 projects were found in 2021

# were any of those 32 in MRS_2020 at all?
extra2021_in2020 <- MRS_2020 %>%
  filter(PROJECTID %in% extra2021$project_id) 
# yes 23 of them appeared in MRS_2020, but their status there was 'under construction': new information in later year
# looking at one of the 9 others, as an example - Duke Riverside Townhouses, R15393 under construction in 2020,
# changes to R18092 completed 2020 (and with a different address, but same number of dwellings) in 2021

# in the other direction (dropped in the later year) - 
extra2020 <- completed_2020 %>%
  filter(year_comp == 2020 & !project_id %in% completed_2021$project_id) 
# so 3 completed projects were recorded in 2020 but omitted from 2021

# were any of those 3 in MRS_2021 at all?
extra2020_in2021 <- MRS_2021 %>%
  filter(PROJECTID %in% extra2020$project_id) # no, none, not by project ID

# did they appear under their project names?  checked; no.  They've just vanished, not sure why.
# Check them in Google Earth - 
extra2020 %>%
  dplyr::select(project_id, proj_name, street_num, street_name, street_type, suburb,
                detached, townhouses, apartments, unknown, total_dwel, max_storeys) %>%
  st_drop_geometry()
# project_id                proj_name street_num street_name street_type             suburb detached townhouses apartments unknown total_dwel max_storeys
# 1     R13342  Geelong Road Apartments    282-290     Geelong        Road     West Footscray        0          0         20       0         20           3
### Google Earth - completed in 2020 looks correct
# 2     R16644     Geelong rd dwellings        377     Geelong        Road         Kingsville        0         11          0       0         11           2
### Google Earth - completed in 2020 looks correct (nb looks unbuilt in Street View, but latest street view imagery is 2019)
# 3     R17791 MILLER STREET TOWNHOUSES       <NA>      Miller      Street HEIDELBERG HEIGHTS        0         10          0       0         10           2
### Working out the street number: half of it is shown in MRS_2016 as 32 Miller St, 10 att2/3 storey, under construction; then the whole is shown as completed
### in 2020.  Assuming it's no. 32, looks like it was completed in 2018 or 2019, but anyway MRS_2020 is correct in showing it as completed.

## Conclusion - the 3 had 'disappeared' in 2021 were all completed in 2020 or earlier.

# So - need to include 'Completed' from both files (rather than always just excluding the earlier year), 
# but exclude overlaps. Most will overlap.  Where they do, use the later (2021), as information is more likely to be update.
# Later file (2021) also contains a good number of new developments completed in the previous year,
# no doubt because of additional iniformation obtained.  Include these.
# Smaller number (3) of 2020 developments missing from 2021.  Include them anyway.


## 1.2 Repeating for 2017/2018 ----
## -------------------------------------#
# checking overlaps based on project IDs
dup2018 <- completed_2018 %>%
  filter(project_id %in% completed_2017$project_id) # 257 observations

dup2017 <- completed_2017 %>%
  filter(project_id %in% completed_2018$project_id) # also 257 observations (as expected)
# so there are 257 that are in both; from inspection, they both have the same project names (apart
# from one where the 2017 file adds 'double entry refer to project')

# now check non-overlaps - 
extra2018 <- completed_2018 %>%
  filter(year_comp == 2017 & !project_id %in% completed_2017$project_id) # 225 observations
# so 225 more completed 2017 projects were found in 2018

# were any of those 225 in MRS_2017 at all?
extra2018_in_2017 <- MRS_2017 %>%
  filter(PROJECTID %in% extra2018$project_id) 
# yes 218 of them appeared in MRS_2017, but their status there was 'firm', 'likely' or 'under construction': 
# new information in later year
# looking at one of the 7 others, as an example 
find <- extra2018 %>%
  filter(!project_id %in% extra2018_in_2017$PROJECTID) %>%
  .$project_id
find2 <- MRS_2018 %>%
  filter(PROJECTID %in% find)
# Amstel Development, R12023 under construction in 2017,
# changes to R16939 completed 2018 but with only 40 completed townhouses instead of 250 under construction

# in the other direction (dropped in the later year) - 
extra2017 <- completed_2017 %>%
  filter(year_comp == 2017 & !project_id %in% completed_2018$project_id) 
# so 4 completed projects were recorded in 2017 but omitted from 2018

# were any of those 4 in MRS_2018 at all?
extra2017_in2018 <- MRS_2018 %>%
  filter(PROJECTID %in% extra2017$project_id) # no, none, not by project ID

# did they appear under their project names/addresses?  checked; no.  They've just vanished, not sure why.

# Same conclusions as 1.1 apply


## 1.3 Repeating for 'crossover' years 2016/2017 ----
## -------------------------------------#
# checking overlaps based on project IDs
dup2017 <- completed_2017 %>%
  filter(project_id %in% completed_2016$project_id) # 495 observations

dup2016 <- completed_2016 %>%
  filter(project_id %in% completed_2017$project_id) # also 495 observations (as expected)
# so there are 495 that are in both; (project names not checked)

# now check non-overlaps - 
extra2017 <- completed_2017 %>%
  filter(year_comp == 2016 & !project_id %in% completed_2016$project_id) # 137 observations
# so 137 more completed 2017 projects were found in 2017

# were any of those 137 in MRS_2016 at all?
extra2017_in_2016 <- MRS_2016 %>%
  filter(Site_ID %in% extra2017$project_id) 
# yes 125 of them appeared in MRS_2016, but their status there was 'Under Construction': 
# new information in later year
# looking at one of the 12 others, as an example? 
find <- extra2017 %>%
  filter(!project_id %in% extra2017_in_2016$Site_ID) %>%
  .$project_id
find2 <- MRS_2017 %>%
  filter(PROJECTID %in% find)
# None of them have project names.  Searched for the first 4 street addresses, and none appeared in 2016
# Perhaps new?

# in the other direction (dropped in the later year) - 
extra2016 <- completed_2016 %>%
  filter(year_comp == 2016 & !project_id %in% completed_2017$project_id) 
# so 37 completed projects were recorded in 2016 but omitted from 2017

# were any of those 37 in MRS_2017 at all?
extra2016_in2017 <- MRS_2017 %>%
  filter(PROJECTID %in% extra2016$project_id) 
# yes, 8 of them, with the following project IDs:
extra2016_in2017$PROJECTID
# "R11598" "R11722" "R11961" "R05571" "R08958" "R11792" "R08909" "R05538"
# shown as status 'Possible', 'Firm' or 'Under Construction' in 2017
dropped_in_2017 <- MRS_2016 %>%
  filter(Site_ID %in% extra2016_in2017$PROJECTID)
# that is - these 8 are shown as 'completed 2016' in 2016, but then 'Possible', 'Firm' or 'Under Construction' in 2017

# what happens to them in 2018?
status_in_2018 <- MRS_2018 %>%
  filter(PROJECTID %in% extra2016_in2017$PROJECTID)
# four of them reappear.  Two (R11961 and R11598) are 'Completed 2018', R08958 is 'Firm' and R11792 is 'Possible'

# how do they appear in 'completed.combined' for 2017-21?
status_in_combined <- completed.combined %>%
  filter(project_id %in% extra2016_in2017$PROJECTID)
# only R11961 and R11598 appear, with their status as 'completed combined'

# check the 8 which are shown as 'completed' in MRS_2016, but then 'Possible', 'Firm' or 'Under Construction' in 2017
# in Google Earth
dropped_in_2017 %>%
  dplyr::select(Site_ID, project_name, street_number, street_name, street_type, suburb_name,
                detached, attOneS, attached2and3storey, FourSMD, Total_Dwellings) %>%
  st_drop_geometry()
# Site_ID                 project_name street_number street_name street_type     suburb_name detached attOneS attached2and3storey FourSMD Total_Dwellings
# 1  R11722 Rothschild Street Apartments            29  Rothschild      Street     Glen Huntly        0       0                  24       0              24
### Google Earth - in 2022 it's about 6 apartments, and has been since before 2000
# 2  R11598      Bedford Road Townhouses       167-169     Bedford        Road   Ringwood East        0       0                  10       0              10
### Google Earth - in 2022, it's about 10-12 apartments, less than 4 storey (unless underground carpark?).  Built 2016-17, looks completed 2018, as shown in MRS_2018
# 3  R05538                         <NA>         26-38       Merri      Parade       Northcote        0       0                   0      90              90
### Google Earth - in 2022, it's a large number of 4 storey apartments.  Looks like completed in 2014 rather than 2016.  This would match R07581 in MRS-2015, shown completed 2014.
# 4  R05571                    Trocadero       119-123     Hopkins      Street       Footscray        0       0                   0       0              16
### Google Earth - in 2022, it's a first floor above a shopping arcade, and has been since before 2000
# 5  R08909                         <NA>            30       Leeds      Street       Footscray        0       0                   0       0              10
### Google Earth - in 2022, it's two floors (could be apartments) above some shops, and has been since before 2000
# 6  R11792    Spencer Street Apartments           5/7     Spenser      Street        St Kilda        0       0                  14       0              14
### Google Earth - in 2022, it's a large old house (or perhaps semi-detached houses), and has been since before 2000
# 7  R11961   Mcnamara Street Townhouses         51-55    Mcnamara        Road         Macleod        0       0                  10       0              10
### Google Earth - actually 'McNamara Street'; 10 townhouses; not completed in 2016, but probably completed in 2017 (so MRS_2018 saying '2018' is not far off)
# 8  R08958                         <NA>           294        Bell      Street Heidelberg West        0       0                   0     135             135
### Google Earth - in 2022 it's a construction site, and has been since cleared of what looked like commercial buildings in 2017

# So, in all cases, correctiong the 2016 'completions' by later references that show them as not completed would yield better results 

# What about the 29 shown in 2016 but omitted altogether from 2017?  Check the first 3 in Google Earth:
extra2016 %>%
  dplyr::select(project_id, proj_name, street_num, street_name, street_type, suburb,
                detached, attOneS, attached2and3storey, FourSMD, total_dwel) %>%
  st_drop_geometry()

# project_id                                         proj_name  street_num  street_name street_type          suburb detached attOneS attached2and3storey FourSMD total_dwel
# 1      R11210                                              <NA>           7         Daws        Road  Doncaster East        0       0                  12       0         12
### Google Earth - looks more like completed 2017 than 2016, and looks more like 8 x 2 storey rather tahn 12 x 4 storey - but was completed about the right time.
# 2      R11722                      Rothschild Street Apartments          29   Rothschild      Street     Glen Huntly        0       0                  24       0         24
### [see above - this one reappears as incomplete in 2017]
# 3      R09164                                   Faversham House        9-13      Chatham        Road      Canterbury        0      34                   0       0         34
### Google Earth - completed 2016 (or perhaps 2015) looks right
# 4      R09072                               Park Crescent Units          22         Park    Crescent         Boronia        0       0                   9       0          9
### Google Earth - completed 2016 looks right



## 1.4  Further checks where project IDs are repeated ----
## -----------------------------------------------------------------------------#
# for each year's results, exclude completions in that year if they have also been reported in following year's results 
test <- rbind(completed_2021,
              completed_2020 %>% 
                filter(!(year_comp == 2020 & project_id %in% completed_2021$project_id)),
              completed_2019 %>% 
                filter(!(year_comp == 2019 & project_id %in% completed_2020$project_id)),
              completed_2018 %>% 
                filter(!(year_comp == 2018 & project_id %in% completed_2019$project_id)),
              completed_2017 %>% 
                filter(!(year_comp == 2017 & project_id %in% completed_2018$project_id)))  # 2279 observations

# check for duplicate project_ids
test2 <- test %>%
  group_by(project_id) %>%
  filter(n() > 1)
# produces 2 duplicates - R15189 and R08906
# R15189 is two separate projects, one in Wantirna and one in Sandringham.  Seems a genuine case of reusing numnber
# R08906 is a duplicate, 'The Buckley' in Footscray.  completed_2019 shows it as completed in 2018,
# while completed_2018 shows it as completed in 2017


# try again excluding any duplicate project_id in consecutive years, not just where it's the current year
test3 <- rbind(completed_2021,
               completed_2020 %>% 
                 filter(!project_id %in% completed_2021$project_id),
               completed_2019 %>% 
                 filter(!project_id %in% completed_2020$project_id),
               completed_2018 %>% 
                 filter(!project_id %in% completed_2019$project_id),
               completed_2017 %>% 
                 filter(!project_id %in% completed_2018$project_id))  # 2278 observations, one fewer than 'test'

# check again for duplicate project_ids
test4 <- test3 %>%
  group_by(project_id) %>%
  filter(n() > 1)
# only R15189 - which was not excluded, because both projects appear with that number in completed_2019
# (and one of them also appears in completed_2018, but is excluded there)


# are there other instances where project_id's are duplicated and don't represent same project?
# combine all, then find duplicate project id's
test5 <- rbind(completed_2021, completed_2020, completed_2019, completed_2018, completed_2017) %>%
  group_by(project_id) %>%
  filter(n() > 1) # 2156 observations

# find where different project id / proj name / total dwellings
test6 <- test5 %>%
  dplyr::select(project_id, proj_name, total_dwel) %>%
  st_drop_geometry() %>%
  distinct() # 1086 observations

# find remaining pairs of project IDs
test7 <- test6 %>%
  group_by(project_id) %>%
  filter(n() > 1) %>%
  .$project_id %>%
  unique()
test7
# "R15774" "R09281" "R16248" "R13341" "R11739" "R15189" "R13653" "R15068" "R10699"

# inspect those 7 projects - 
View(test5 %>% 
       filter(project_id %in% c("R09281", "R10699", "R11739", "R13341", "R13653", "R15068", "R15189", "R15774", "R16248")))
# explanations: R09281, R15774 & R16248 - spacing in proj_name
# R10699 - adding 'double entry refer to project' in proj_name
# R11739 - 1 extra townhouse; R13341 - 1 extra apartment; R13653 - 7 extra apartments
# R15068 - mistake in total_dwel in one occurrence (45 apts = 15 total_dwel) - fortunately correct figure is in later year's results
# R15189 - genuine re-used project number as above

# summary: the only re-used number is R15189; the approach of omitting project_id's where used in 2 consecutive files
# works, because (1) it doesn't exclude the genuine duplicate R15189 (as the duplicates appear in a single file), and
# (2) there aren't any other genuine duplicates that appear in consecutive years, and
# (3) excluding the duplicate from the earlier year file means that the later information (ie the duplicate as it 
# appears in the later year file) is used, which 'should' be the best information (R15068 is a good example of a later
# year file correcting an earlier year file mistake)


# 2.  Check whether area_ha is adequate   ----
# -----------------------------------------------------------------------------#
test <- completed.projects %>%  # created in 2.2 of 'apartments.R'(also saved as 'GIS/completedProjects.sqlite')
  mutate(geometry = GEOMETRY) %>% ## only if necessary
  mutate(test_area = as.numeric(st_area(geometry) / 10000),  # sq m to ha
         area_diff = abs(area_ha - test_area))
max(test$area_diff)  # 0.6920726

diff100 <- test %>%
  filter(area_diff >= 0.01) # difference > 100 sq m
nrow(diff100)  # 28 - that is, 28 projects where area is out by 100 sq m or more

# conclusion - area_ha will mostly be adequate; where it's 'wrong', we can't really be certain
# whether the error is in the calculation of the area, or the digitisation of the boundaries!


# 3. Check duplicate project id names  ----
# -----------------------------------------------------------------------------#
## 3.0 combine standard files into a single file ----
## -------------------------------------#
combined.files <- data.frame()

years <- c(2004:2021)
for (i in 1:length(years)) {
  combined.files <- rbind(combined.files,
                          get(paste0("standard_", as.character(years[i]))))
}
# 'combined.files' has 50925 observations


## 3.1 NAs ----
## -------------------------------------#
NAs <- combined.files %>%
  filter(is.na(project_id))
# produces 15, from 2006, 2008, 2010, 2012 and 2014 files
# but none of them are completed, so they can all be ignored
# [theoretically, a project could have been marked 'completed' in one year,
# then returned the following year with an uncompleted status and its number removed
# - but this seems unlikely]


## 3.2 duplicates ----
## -------------------------------------#
# find duplicate project id's where not same street address
# first test
combined.files.duplicatesX <- combined.files %>%
  group_by(project_id) %>%
  # keep only duplicate project id's
  filter(n() > 1) %>% # output at this point has 48751 observations
  # remove where same address (converting all to lower case)
  mutate(street_name = tolower(street_name), suburb = tolower(suburb)) %>%
  distinct(street_num, street_name, suburb) %>% # output at this point has 11277 observations
  # keep only remaining project id's
  filter(n() > 1)
# results in 2579 outputs.  From inspection, many are because of changes in street numbers

# second test
combined.files.duplicates <- combined.files %>%
  group_by(project_id) %>%
  # keep only duplicate project id's
  filter(n() > 1) %>% # output at this point has 48751 observations
  # remove where same address (converting all to lower case), EXCLUDING number
  mutate(street_name = tolower(street_name), suburb = tolower(suburb)) %>%
  distinct(street_name, suburb) %>% # output at this point has 10402 observations
  # keep only remaining project id's
  filter(n() > 1)
# results in 928 obesrvations.  From inspection, many still look like they should be the same,
# eg same street name and melbourne/docklands or richmond/burnley; or same suburb but different street name


# third test - spatial overlap??
# possible solutions (but neither works for me)
# see https://gis.stackexchange.com/questions/281078/r-can-dplyrgroup-by-work-with-sfst-intersects 
# (suggests grouping using 'split', but have not made it work correctly)
# https://stackoverflow.com/questions/69628734/find-intersection-of-multiple-polygons-by-group
# (uses 'map' which is in purrr - haven't pursued)

# so just use a loop instead...

# create a vector to hold project ID's
potential.duplicates <- c()

# loop through the project IDs
for (i in 1:length(projectIDs)) {
  # take rows which share the project ID
  id.group <- combined.files %>%
    filter(project_id == projectIDs[i])
  
  # create 'flag' which is a matrix of TRUE/FALSE results according to whether 
  # each element of the group intersects each other element
  flag <- st_intersects(id.group, sparse = FALSE)
  
  # if there is any non-intersection, then add the project ID to the list
  if (FALSE %in% flag) {
    potential.duplicates <- append(potential.duplicates, projectIDs[i])
  }
}

# extract the records that are in potential.duplicates
potential.duplicate.records <- combined.files %>%
  filter(project_id %in% potential.duplicates)
# produces 402 records

# filter to those where there is a completion (those without a completion don't matter)
potential.duplicate.IDs.completed <- potential.duplicate.records %>%
  filter(status == "Completed") %>%
  .$project_id %>%
  unique()  # 25 unique project IDs
potential.duplicate.records.completed <- potential.duplicate.records %>%
  filter(project_id %in% potential.duplicate.IDs.completed) %>%
  # reorder for easy manual review
  dplyr::select(project_id, file_year, status, year_comp, area_ha, proj_name, street_num, street_name, street_type, suburb,
                 detached, townhouses, apartments, unknown, att_1s, att_2_3s, att_4s, total_dwel, max_storeys)
# produces 173 observations

# manually reviewed the 25 projects (173 observations) to identify:-
# - any where there were two completions for different projects with same ID,
# - any where there was a completion, apparently overruled by a later non-completion for a different project but same ID

# The *only* apparently genuine duplicate is R15189 - obviously two different projects, both called 'The Reserve', one in 
# Sandringham (completed 2018) and one in Wantirna (completed 2019)

# Note also R11867, which appears to refer to different projects in Carlton and South Melbourne - 
# but not relevant, as the earlier is not completed; and the later is completed but then not overruled




# 4. Checking 'duplicates' and 'discards' in combined single file of completed projects ----
# -----------------------------------------------------------------------------#
# See the single file created in section 2.2 of 'apartments.R'

## 4.1 Duplicates ----
## -------------------------------------#
# There are 5003 projects in 'completed.projects', and 9768 duplicates in 'duplicates'
# This seems excessive, given that projects are not meant to be repeated for years before 2012
# [We later discovered that in fact many projects are repeated in earlier years
# - see section 10]

# (Note that 'duplictes' here means 'duplicte project_ids', not duplicate projects with
# different project IDs as discussed in sections 6-8)

# Here are the results - eg of 284 'completions' in 2005, 41 are duplicates of completions from 2004

# year completions duplicates
# 2005         284         41
# 2006         225        110
# 2007         351         97
# 2008         513        253
# 2009         756        505
# 2010         981        732
# 2011         660        464
# 2012         462        193
# 2013         551        267
# 2014         482        273
# 2015         387        200
# 2016         714        177
# 2017         891        495
# 2018         814        257
# 2019         625        332
# 2020         500        290
# 2021         526        198

# This is a very high volume of revised completion years, esp from before
# 2012 when (according to David Matthews) there weren't two years of completions. 
# I strongly suspect that in fact there were.  [Confirmed - see section 10]


## 4.2 Discards ----
## -------------------------------------#
#'Discards' file produced in section 2.2 of 'apartments.R' reviewed. 
# Contains 138 discards (69 pairs)

# Most show the expected pattern of 'Completed' then revised to an uncompleted status.  Exceptions - 

# R05225 - 2011 shows as 'completed', then 2012 shows as 'Construction 0-2 years' - 
# but still a completion year of 2011

# R10580, R13055, R05057, R13494 - 2015 shows as 'completed', then 2016 shows with
# various uncompleted statuses, but still completion year of 2015

### CHECK SOME OF THESE IN GOOGLE EARTH?


# 5. Densities ----
# -----------------------------------------------------------------------------#
## 5.1 Categories ----
## -------------------------------------#
# See 'apartments.R' section 3, which checks density of various grouings of apartments/detached, etc

# Note that there's an 'unallocated' category, calculated as follows - 
chkunalloc <- densities <- completed.projects %>%
  # calculate density (noting there are two area=0 which need to be fixed)
  mutate(area_ha = if_else(area_ha > 0, area_ha, as.numeric(st_area(geometry))),
         ## note - 'st_area' is wrong; it should be divided by 10000 to produce hectares
         ## but actually the two WILLS St are problematic duplications digitised as dots anyway
         density = total_dwel / area_ha,
         type = case_when(apartments > 0 & townhouses == 0 & detached == 0 & unknown == 0 ~ "apartments",
                          att_4s > 0 & (att_1s == 0 & att_2_3s == 0) & detached == 0 & unknown == 0 ~ "4+ storey attached (pre 2017)",
                          
                          apartments == 0 & townhouses > 0 & detached == 0 & unknown == 0 ~ "townhouses",
                          att_4s == 0 & (att_1s > 0 | att_2_3s > 0) & detached == 0 & unknown == 0 ~ "1-3 storey attached (pre 2017)",
                          
                          apartments > 0 & townhouses > 0 & detached == 0 & unknown == 0 ~ "mixed apartments/townhouses",
                          att_4s > 0 & (att_1s > 0 | att_2_3s > 0) & detached == 0 & unknown == 0 ~ "mixed attached (pre 2017)",
                          
                          apartments == 0 & townhouses == 0 & detached > 0 & unknown == 0 ~ "detached",
                          att_4s == 0 & (att_1s == 0 & att_2_3s == 0) & detached > 0 & unknown == 0 ~ "detached",
                          
                          apartments == 0 & townhouses == 0 & detached == 0 & unknown > 0 ~ "unknown",
                          att_4s == 0 & (att_1s == 0 & att_2_3s == 0) & detached == 0 & unknown > 0 ~ "unknown",
                          
                          (apartments > 0 | townhouses > 0)  & (detached > 0 | unknown > 0) ~ "mixed atached/detached",
                          (att_4s > 0 | att_1s > 0 | att_2_3s > 0) & (detached > 0 | unknown > 0) ~ "mixed attached/detached",
                          
                          TRUE ~ "unallocated")) %>%  ## should be 0!!
  st_drop_geometry() %>%
  filter(type == "unallocated")

unallocproj<- chkunalloc %>%
  arrange(project_id) %>%
  .$project_id


# Result:
# - one project where attached types are all zero but there is a mix of detached & unknown - R05011,
# - 28 anomalies where all dwelling types are zero (and, for these cases, total_dwel is also zero) - 
# "R00305" "R00687" "R03191" "R03296" "R03422" "R03494" "R03548" "R03585" "R0438" 
# "R0440"  "R05273" "R05292" "R05303" "R06932" "R07204" "R07316" "R07438"
# "R07944" "R07946" "R07953" "R07961" "R0884"  "R0885"  "R0973"  "R10333" "R10343"
# "R10460" "R10465"


# 6. Further duplicate review - checking for same project but different IDs ----
# -------------------------------------#
# checks below conducted on the version of 'completed projects' containing 5002 projects

# This section is the first attempt at reviewing for same project but different project_ids
# Later refined in sections 7 and 8, and then implemented in section 2.3 of apartments.R 

## 6.1 load data for checking ----
## -------------------------------------#
# save the version with 5002 projects, without PT stops/routes
# CP5002 <- st_read("../GIS/completedProjects(walking).sqlite") %>%
#   dplyr::select(-c(rail_stn, tram_bus_stop, tram_routes, bus_routes))
# 
# st_write(CP5002, "../GIS/completedProjects5002.sqlite", delete_layer = TRUE)

completed.projects <- st_read("../GIS/completedProjects5002.sqlite")


## 6.2 identifying potential duplicates through intersection ----
## -------------------------------------#
# check that finds intersecting projects and produces output in sequential rows 
# create empty vector to hold overlapping projects, with same variables as 'completed.projects'
overlapping.projects <- completed.projects %>% .[0, ]

# run through projects, and find pairs (or triplets, etc) of overlapping for checking
for (i in 1:nrow(completed.projects)) {
# for (i in 1:100) {
  project <- completed.projects[i, ]
  project.overlaps <- completed.projects[st_intersects(project, completed.projects, sparse = FALSE), ]
  # add the pair to the 'overlapping projects' file - will contain both 'project' 
  # and the projects that it overlaps with
  if (nrow(project.overlaps) > 1) {
    overlapping.projects <- rbind(overlapping.projects,
                                  project.overlaps)
  }
}

# produces 3813 observations (note - many are repeats, because each project
# is recorded both when it is 'project', and when it overlaps with another 'project')


## 6.3 first filter - 'padded' project_ids ----
## -------------------------------------#
# Find duplicates where the project is padded with an extra zero in later years, eg 'R0135' and 'R00135'.
# Note though that some padding does reflect different projects, eg 'R022' (Wantirna) and 'R00022' (Collingwood)
# Most of the 'problem' paddings are 2004 to 2006.

# run through projects, and find instances of duplicates with 'padded' numbers
testing.projects <- completed.projects %>%
  # remove initial character 'R' from project id  and convert to integer, eg R022 > 022 > 22
  mutate(new_proj_no = as.numeric(substring(project_id, 2)))
  
# create empty vector to hold overlapping projects, with same variables as 'testing.projects'
potential.duplicates <- testing.projects %>% .[0, ]

for (i in 1:nrow(testing.projects)) {
  # for (i in 1:100) {
  project <- testing.projects[i, ] 
  other.projects <- testing.projects[-i, ]
  # find other projects with the same new_proj_no as 'i'
  other.projects.same.new_proj_no <- other.projects %>%
    filter(new_proj_no == project$new_proj_no)
  # if there are any, then add the project and the potential duplicates to the potential duplicates file
  if (nrow(other.projects.same.new_proj_no) > 0) {
    potential.duplicates <- rbind(potential.duplicates,
                                  project,
                                  other.projects.same.new_proj_no)
  }
}

# produces 432 entries (which includes duplicates of each pair)
# examination of about first 150:
# - many pairs where address is clearly different (often 2004 and a much later year)
# - many pairs where area_ha, street_num, _street_name and total_dwel are the same, and the only difference is year
# - often  these are 2004 and 2006
# - some where dwelling numbers and/or area_ha have changed; in these cases, Google Earth suggests later more likely to be correct
# - some where street number isn't identical, though manual search looks clear enough

# revise above approach to search on a string of de-padded proj no + street name
# don't include street type - will pick up 'Road'/'Rd' as differences
testing.projects <- completed.projects %>%
# testing.projects <- completed.projects.with.duplicates %>%
  # remove initial character 'R' from project id  and convert to integer, eg R022 > 022 > 22; add street
  mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                            tolower(street_name),
                            tolower(suburb)  # optional extra - suburb - added for later testing
                            ))

# create empty vector to hold overlapping projects, with same variables as 'testing.projects'
potential.duplicates <- testing.projects %>% .[0, ]

for (i in 1:nrow(testing.projects)) {
  # for (i in 1:100) {
  project <- testing.projects[i, ] 
  other.projects <- testing.projects[-i, ]
  # find other projects with the same identifier as 'i'
  other.projects.same.identifier <- other.projects %>%
    filter(identifier == project$identifier)
  # if there are any, then add the project and the potential duplicates to the potential duplicates file
  if (nrow(other.projects.same.identifier) > 0) {
    potential.duplicates <- rbind(potential.duplicates,
                                  project,
                                  other.projects.same.identifier)
  }
}

# produces 404 entries (which includes duplicates of each pair) - so 101 pairs in total
# for all except 9 of the 101, the total dwellings are unchanged
# seems reasonable to treat all these as 'real' duplicates and retain only latest
# same result when adding suburb - feels 'safer' to do so

# testing filter
completed.projects.filtered1 <- completed.projects %>%
  # make identifier consisting of unpadded project_id + street_name + street_type
  mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                            tolower(street_name),
                            tolower(suburb))) %>%
  # keep only the latest file year for each identifier group
  group_by(identifier) %>%
  filter(file_year == max(file_year)) %>%
  ungroup()
# produces 4901 entries, thereby removing 101


## 6.4 second filter - duplicates with different project numbers ----
## -------------------------------------#
# Find duplicates where some or all of street, area and dwell details are the same
# begin with results fo first filter
testing.projects <- completed.projects.filtered1 %>%
  # make identifier consisting of combination of street_num + street_name + area_ha + total_dwel
  mutate(identifier = paste(street_num,
                            tolower(street_name),
                            area_ha,
                            total_dwel))

# create empty vector to hold overlapping projects, with same variables as 'testing.projects'
potential.duplicates <- testing.projects %>% .[0, ]

for (i in 1:nrow(testing.projects)) {
  # for (i in 1:100) {
  project <- testing.projects[i, ] 
  other.projects <- testing.projects[-i, ]
  # find other projects with the same new_proj_no as 'i'
  other.projects.same.identifier <- other.projects %>%
    filter(identifier == project$identifier)
  # if there are any, then add the project and the potential duplicates to the potential duplicates file
  if (nrow(other.projects.same.identifier) > 0) {
    potential.duplicates <- rbind(potential.duplicates,
                                  project,
                                  other.projects.same.identifier)
  }
}

# With identifier as street_name + street_type + area_ha + total_dwel
# Produces 146 entries (duplicated pairs, so 36? 37? total pairs (not sure why not even number))
# Quite often the project name and or street number don't exactly match
# There are some false matches; would need to add street number

# Tested again With identifier as street_name + street_type + total_dwel (ie area_ha omitted)
# Produces 1604 entries - many more false matches with different street numbers, but also many that look like
# genuine duplicates

# Tested again with street_num, street_name + street_type + total_dwell (ie add street_num, omit area_ha)
# produces 121 entries - but some are false matches where there is a missing street number

# Tested again with street_num + street_name + area_ha + total_dwell (ie all - except
# 'street_type' - omitted, to avoid missing 'road/rd' differences. 
# Produces only 113 entries, but they all look 'real' (though in some cases the older looks
# more correct...).  Some won't be eliminated as they have same file year.

# testing filter
completed.projects.filtered2 <- completed.projects.filtered1 %>%
  # make identifier consisting of street, area and total_dwel details
  mutate(identifier = paste(street_num,
                            tolower(street_name),
                            area_ha,
                            total_dwel)) %>%
  # keep only the latest file year for each identifier group
  group_by(identifier) %>%
  filter(file_year == max(file_year)) %>%
  ungroup()
# produces 4874 entries, thereby removing 27


## 6.5 third filter - duplicates with different project numbers but same file year ----
## -------------------------------------#
# These ones are not picked up in second issue because file year is the same
# So instead need to randomly select one or other - by highest project_id
# Really just a mop-up of second issue
# (Unless one has more unknowns than other)

# Find duplicates where some or all of street, area and dwell details are the same
# begin with results of second filter
testing.projects <- completed.projects.filtered2 %>%
  # make identifier consisting of combination of year, street, area and dwell details
  mutate(identifier = paste(file_year,
                            street_num,
                            tolower(street_name),
                            area_ha,
                            total_dwel))

# create empty vector to hold overlapping projects, with same variables as 'testing.projects'
potential.duplicates <- testing.projects %>% .[0, ]

for (i in 1:nrow(testing.projects)) {
  # for (i in 1:100) {
  project <- testing.projects[i, ] 
  other.projects <- testing.projects[-i, ]
  # find other projects with the same new_proj_no as 'i'
  other.projects.same.identifier <- other.projects %>%
    filter(identifier == project$identifier)
  # if there are any, then add the project and the potential duplicates to the potential duplicates file
  if (nrow(other.projects.same.identifier) > 0) {
    potential.duplicates <- rbind(potential.duplicates,
                                  project,
                                  other.projects.same.identifier)
  }
}

# Produces only 8 entries - just 1 developmentss (Wills St Melby).
# All details are the same except project id - no 'unknown' building types

# testing filter
completed.projects.filtered3 <- completed.projects.filtered2 %>%
  # make identifier consisting of year, street, area and dwell details
  mutate(identifier = paste(file_year,
                            street_num,
                            tolower(street_name),
                            area_ha,
                            total_dwel)) %>%
  # keep only the highest project ID for each identifier group
  group_by(identifier) %>%
  filter(project_id == max(project_id)) %>%
  ungroup()
# produces 4873 entries, thereby removing 1


## 6.6 re-check potential duplicates through st_overlap ----
## -------------------------------------#
# check that finds overlapping projects and produces output in sequential rows 
# use st_overlap - better than st_intersects - doesn't catch merely adjoining with shared boundary.
# create empty vector to hold overlapping projects, with same variables as 'completed.projects'
overlapping.projects <- completed.projects.filtered3 %>% .[0, ]
test <- completed.projects.filtered3 %>% as.data.frame() %>% st_as_sf()

# run through projects, and find pairs (or triplets, etc) of overlapping for checking - with st_overlaps instead
for (i in 1:nrow(test)) {
  # for (i in 1:100) {
  project <- test[i, ]
  project.overlaps <- test[st_overlaps(project, test, sparse = FALSE), ]
  # add the pair to the 'overlapping projects' file - will contain both 'project' 
  # and the projects that it overlaps with
  if (nrow(project.overlaps) > 0) { ## nb >0, cf >1 for st_intersescts
    overlapping.projects <- rbind(overlapping.projects2,
                                   project,
                                  project.overlaps)
  }
}

# produces 1580 entries

## 6.7 fourth filter - same street details and total dwel (but not area_ha) ----
## -------------------------------------#
# from comments in 5.2, would expect false matches where missing street no
# so,  use proj_name as an alt

# Find duplicates where some or all of project, street and dwell details (but not area) are the same
# begin with results of third filter
testing.projects <- completed.projects.filtered3 %>%
  # make identifiers consisting of combination of street no/proj no + street name + suburb +  dwell details
  mutate(identifier = paste(street_num,
                            tolower(street_name),
                            tolower(suburb),
                            total_dwel),
         identifier2 = paste(proj_name,
                             tolower(street_name),
                             tolower(suburb),
                             total_dwel))

# create empty vector to hold overlapping projects, with same variables as 'testing.projects'
potential.duplicates <- testing.projects %>% .[0, ]

for (i in 1:nrow(testing.projects)) {
  # for (i in 1:100) {
  project <- testing.projects[i, ] 
  other.projects <- testing.projects[-i, ]
  
  # find other projects with the same identifier as 'i', where
  ## street_num is present and the street_num identifier matches, or
  ## proj_name is present and the proj_name identifier matches
  other.projects.same.identifier <- other.projects %>%
    filter((!(is.na(street_num)) & identifier == project$identifier) |
              (is.na(street_num) & !(is.na(proj_name)) & identifier2 == project$identifier2))
             
  # if there are any, then add the project and the potential duplicates to the potential duplicates file
  if (nrow(other.projects.same.identifier) > 0) {
    potential.duplicates <- rbind(potential.duplicates,
                                  project,
                                  other.projects.same.identifier)
  }
}

# Produces 44 entries

# testing filter 
completed.projects.filtered4 <- completed.projects.filtered3 %>%
  # make identifier consisting of year, street, area and dwell details
  mutate(identifier = paste(street_num,
                            tolower(street_name),
                            tolower(suburb),
                            total_dwel),
         identifier2 = paste(proj_name,
                             tolower(street_name),
                             tolower(suburb),
                             total_dwel)) %>%
  # keep only the highest file_year for each identifier group
  # for conditional filtering see https://stackoverflow.com/questions/47624161/use-filter-in-dplyr-conditional-on-an-if-statement-in-r
  # note that require 'else' statement, which just says 'don't filter'
  group_by(identifier) %>%
  filter(
    if (!(is.na(street_num))) {
      file_year == max(file_year)
    } else {
      file_year == file_year
    }
  ) %>%
  ungroup() %>%
  group_by(identifier2) %>%
  filter(
    if (is.na(street_num) & !(is.na(proj_name))) {
      file_year == max(file_year)
    } else {
      file_year == file_year
    }
  ) %>%
  ungroup()
# produces 4862 entries, thereby removing 11


## 6.8 re-check overlaps... ----
## -------------------------------------#
# Run the same code as 5.5 - 
overlapping.projects <- completed.projects.filtered4 %>% .[0, ]
test <- completed.projects.filtered4 %>% as.data.frame() %>% st_as_sf()

# run through projects, and find pairs (or triplets, etc) of overlapping for checking - with st_overlaps instead
for (i in 1:nrow(test)) {
  # for (i in 1:100) {
  project <- test[i, ]
  project.overlaps <- test[st_overlaps(project, test, sparse = FALSE), ]
  # add the pair to the 'overlapping projects' file - will contain both 'project' 
  # and the projects that it overlaps with
  if (nrow(project.overlaps) > 0) { ## nb >0, cf >1 for st_intersescts
    overlapping.projects <- rbind(overlapping.projects,
                                  project,
                                  project.overlaps)
  }
}

# produces 1510 entries
st_write(overlapping.projects, "../GIS/overlapping.projects.temp.sqlite", delete_layer = TRUE) ## can delete this file after review



# 7. Duplicate elimination -  same project but different IDs ----
# -------------------------------------#
# checks below conducted on the version of 'completed projects' containing 5002 projects
# approach here is to first limit to projects which actually intersect, and 
# then run filters (including those from section 6)

# This section is the second attempt at reviewing for same project but different project_ids
# Begun in section 7 and refined in section 8, and then implemented in section 2.3 of apartments.R 

## 7.1 load data for checking ----
## -------------------------------------#
# save the version with 5002 projects, without PT stops/routes
# CP5002 <- st_read("../GIS/completedProjects(walking).sqlite") %>%
#   dplyr::select(-c(rail_stn, tram_bus_stop, tram_routes, bus_routes))
# 
# st_write(CP5002, "../GIS/completedProjects5002.sqlite", delete_layer = TRUE)

completed.projects.with.duplicates <- st_read("../GIS/completedProjects5002.sqlite") %>%
  # add a unique x_id field
  mutate(x_id = row_number())


## 7.2 collect groups of intersecting projects ----
## --------------------------------------#
# st_intersection produces a dataframe with two extra columns: 
# 'n.overlaps', which shows the number of overlaps for each record (and there is always at least one, itself;
# 'origins',  which is the row numbers of the intersecting projects

intersections <- st_intersection(completed.projects.with.duplicates) %>%
  # omit where there is only one intersetion (ie intersects itself)
  filter(n.overlaps > 1) %>%  # 1129 intersections
  # omit where not a polygon or collection (ie points and lines)
  filter(st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>% # 609 intersections
  # add column for intersection area
  mutate(isec_area_ha = as.numeric(st_area(GEOMETRY)) / 10000)
  # omit unless intersecting area is > 5% of particular feature's area ('area_ha is in ha, so * 10000; 0.05 is for 5%)
  # filter(as.numeric(st_area(GEOMETRY)) > (area_ha * 10000 * 0.05)) # 231 intersections

# omit unless intersecting area is >5% of each feature's area
# Note - doesn't work to just omit if >5% of the particular feature's area - fails to catch
# eg Wills St where two projects are tiny (or zero) areas within the other

# Later increased 5% threshold to 50%, after very extensive checking (recorded in working notes)


# vector to hold row numbers of small intersections
small.intersections <- c()

# loop through intersections, and find those where area is <= 5% of each area
for (i in 1:nrow(intersections)) {
  # find id's of projects in the group
  ids <- unlist(intersections[i, "origins"][[1]])
  # find project areas (in ha)
  project.areas_ha <- c()
  for (j in 1:length(ids)) {
    project.areas_ha <- c(project.areas_ha,
                        as.numeric(st_area(completed.projects.with.duplicates %>%
                                             ##nb won't work to filter on x_ids if they don't match row numbers
                                             filter(x_id == ids[j]) %>% 
                                            .$GEOMETRY)) / 10000)
  }
  # if isec.area is <= 5% of all project.areas, add to list of small intersections
  if (intersections[i, "isec_area_ha"][[1]] <= min(project.areas_ha) * 0.05) { 
    small.intersections <- c(small.intersections, i)
  } 
}

# filter intersections to those which aren't 'small'
intersections <- intersections %>%
  filter(!row_number() %in% small.intersections) # 262 intersections


# create empty frame to hold intersecting groups, with same variables as 'completed.projects.with.duplicates'
intersecting.groups <- completed.projects.with.duplicates %>%
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
  projects <- completed.projects.with.duplicates %>%
    ##nb won't work to filter on x_ids if they don't match row numbers
    filter(x_id %in% ids) %>%
    # add group_id
    mutate(group_id = i,
           isec_area_ha = isec_area_ha)
  # add the projects to 'intersecting groups'
  intersecting.groups <- rbind(intersecting.groups,
                               projects)
}
# 543 entries in 262 groups

## 7.3 filtering out projects - first pass - duplicate street/area/dwell details ----
## --------------------------------------#
# corresponds to filter in 6.4 above
# set up file with duplicates removed
completed.projects.duplicates.removed <- completed.projects.with.duplicates %>%
  filter(!x_id %in% intersecting.groups$x_id)


# filter duplicates where street_num + street_name + area_ha + total_dwel are same
# and choose highest file year (or, if same, highest project_id)
group_ids <- unique(intersecting.groups$group_id)

# dataframe to hold outputs
intersecting.groups.flagged <- intersecting.groups %>% .[0, ] %>%
  mutate(flag = "")

# loop thorugh intersecting groups, and flag as 'keep', 'discard' or 'further checking'
for (i in 1:length(group_ids)) {
  group <- intersecting.groups %>%
    # select the rows for the relevant group
    filter(group_id == group_ids[i]) %>%
    # add an identifier
    mutate(identifier = paste(street_num,
                              tolower(street_name),
                              area_ha,
                              total_dwel))
  # see whether or not there is more than one identifier  
  identifiers <- unique(group$identifier)
  
  # if only one (ie all have same identifier), then 
  # flag the highest file year and others to discard
  if (length(identifiers) == 1) {
    group <- group %>%
      mutate(flag = if_else(file_year == max(file_year), 
                            "keep",
                            "discard"))
    
    # second check where duplicate file years - keep highest project id
    if (nrow(group %>% filter(flag == "keep")) > 1) {
      print(paste("duplicate 'keep' for group no ", group_ids[i]))
      # find highest project id of the "keep"s
      highest_id <- max(group %>% filter(flag = "keep") %>% .$project_id)
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

# checking how many in each group
keep.discard.chk <- intersecting.groups.flagged %>%
  group_by(flag) %>%
  summarise(n = n()) %>%
  st_drop_geometry()
keep.discard.chk

# flag                 n
# * <chr>            <int>
#   1 discard            100
# 2 further checking   344
# 3 keep                99

# why more discards than keeps?
test <- intersecting.groups.flagged %>%
  group_by(group_id, flag) %>%
  summarise(n = n()) %>%
  st_drop_geometry()
test
# group_id 160 (37 Woolton Ave) has 3 identical, only one in highest file year

keeps <- intersecting.groups.flagged %>%
  filter(flag == "keep") %>%
  .$x_id

discards <- intersecting.groups.flagged %>%
  filter(flag == "discard") %>%
  .$x_id


intersecting.groups.tocheck <- intersecting.groups.flagged %>%
  filter(flag == "further checking")
length(unique(intersecting.groups.tocheck$group_id))  # 344 entries in 163 groups


## 7.4 filtering out projects - second pass - padded project_ids where street_name also the same ----
## and also eliminates duplicates where one has zero dwellings
# --------------------------------------#
# 'Padded project id's corresponds to filter in 6.3 above.  Zero's is not in section 6.
# Note 6.3 later revised so both street name and suburb are same - produces same result

# At this point, 100 duplicates have been removed
# But there are still 163 groups  in 'intersecting.groups.tocheck' to check

# Next filter removes 'padded' project IDs, eg R0314/R00314, where 
# street name also checks that they are the same project
# (see extensive notes in 'working notes.docx' which checked many of these)

# and also eliminated duplicates where one has zero dwellings

group_ids2 <- unique(intersecting.groups.tocheck$group_id)

# dataframe to hold outputs
intersecting.groups.flagged2 <- intersecting.groups.flagged %>% .[0, ]

# loop through intersecting groups, and flag as 'keep', 'discard' or 'further checking'
for (i in 1:length(group_ids)) {
  group <- intersecting.groups.tocheck %>%
    # select the rows for the relevant group
    filter(group_id == group_ids2[i]) %>%
    # add an identifier
    mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                              tolower(street_name)))
  # see whether or not there is more than one identifier  
  identifiers <- unique(group$identifier)
  
  # if only one (ie all have same identifier), then 
  # flag the highest file year and others to discard
  if (length(identifiers) == 1) {
    group <- group %>%
      mutate(flag = if_else(file_year == max(file_year), 
                            "keep",
                            "discard"))
    
    # second check where duplicate file years - keep highest project id
    # (though there should't be any for padded project_ids)
    if (nrow(group %>% filter(flag == "keep")) > 1) {
      print(paste("duplicate 'keep' for group no ", group_ids2[i]))
      # find highest project id of the "keep"s
      highest_id <- max(group %>% filter(flag = "keep") %>% .$project_id)
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
  
  # further pass to eliminate any zero dwelling entries
  # filter out zeroes - where only record has > 0  total dwellings
  if (nrow(group %>% filter(total_dwel > 0)) == 1) {
    group <- group %>%
      mutate(flag = if_else(total_dwel > 0, 
                            "keep",
                            "discard"))
    
  }
  
  # add the flagged group to 'intersecting groups flagged2'
  intersecting.groups.flagged2 <- rbind(intersecting.groups.flagged2,
                                       group)
}

keeps <- c(keeps,
           intersecting.groups.flagged2 %>%
             filter(flag == "keep") %>%
             .$x_id)

discards <- c(discards,
              intersecting.groups.flagged2 %>%
                filter(flag == "discard") %>%
                .$x_id)


intersecting.groups.tocheck2 <- intersecting.groups.flagged2 %>%
  filter(flag == "further checking")
length(unique(intersecting.groups.tocheck2$group_id))  # 294 entries in 138 groups


## 7.5 filtering out projects - third pass - same street, and aread & dwell within 20% +/- ----
## --------------------------------------#
# Still 138 groups  in 'intersecting.groups.tocheck2' to check

# Next filter removes those where street address is the same, but area and 
# dwellings are each within 20% of the mean for the group

# If successful, this could just be made part of the first pass

group_ids3 <- unique(intersecting.groups.tocheck2$group_id)

# dataframe to hold outputs
intersecting.groups.flagged3 <- intersecting.groups.flagged2 %>% .[0, ]


# loop thorugh intersecting groups, and flag as 'keep', 'discard' or 'further checking'
for (i in 1:length(group_ids3)) {
  group <- intersecting.groups.tocheck2 %>%
    # select the rows for the relevant group
    filter(group_id == group_ids3[i]) %>%
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
  
  
  # if only one (ie all have same identifier), AND
  # area_ha and total_dwell are within tolerances, then  
  # flag the highest file year and others to discard
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
      print(paste("duplicate 'keep' for group no ", group_ids3[i]))
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
  intersecting.groups.flagged3 <- rbind(intersecting.groups.flagged3,
                                       group)
  
}

# eliminates a further 11 projects.

keeps <- c(keeps,
           intersecting.groups.flagged3 %>%
             filter(flag == "keep") %>%
             .$x_id)

discards <- c(discards,
              intersecting.groups.flagged3 %>%
                filter(flag == "discard") %>%
                .$x_id)


intersecting.groups.tocheck3 <- intersecting.groups.flagged3 %>%
  filter(flag == "further checking")
length(unique(intersecting.groups.tocheck3$group_id))  # 273 entries in 128 groups


## 7.6 see whether higher intersection threshold than 5% would be better ----
## --------------------------------------#
# examine intersection percentage of total area 
intersecting.groups.flagged4 <- intersecting.groups.tocheck3 %>%
  # mutate(isec_pct = isec_area_ha / area_ha * 100)
  mutate(isec_pct = isec_area_ha / (as.numeric(st_area(GEOMETRY)) / 10000) * 100 )

# this sheet (273 entries in 128 groups) checked thoroughly from the perspective of isec_pct
# Found:
## - except as below, a threshold of 50% (instead of 5%) would not keep any duplicates that should be removed
## - indeed, except as below, a threshold of 60% would also not keep any duplicates
##   that should be removed, and would also eliminate a further 4 groups from 'project checks'
## - however, there are two instances (Sandown Park, Williamsons Rd) where groups of 3 include 
##   a 'padded id' duplicate which should removed, along with aseparate project that shoudl not
##   One of these (Sandown Park) woudl be picked up by the second  'padded id' filter, but the 
##   other (Williamsons Rd) would not, because there is no separate intersection of just the two 
##   genuine duplicates.

## Proposed solution:
## - run the 'padded id' (and zero) filter first
## - then run the intersections with a 50% threshold [or could use 60%, but 50% seems more defensible]
## - then run the 'identity' (with thresholds) filter
## - then manual review of whatever is left.

.


# 8. Duplicate elimination -  same project but different IDs ----
# -------------------------------------#
# checks below conducted on the version of 'completed projects' containing 5002 projects
# The approach here is as outlined at the end of section 7, drawing on 
# work from sections 6 and 7.

# This section is the third attempt at reviewing for same project but different project_ids
# Begun in sections and 6, refined here, and then implemented in section 2.3 of apartments.R 

# Note that some further changes have been made to section 2.3 of apartments following
# the discovery that completion years for pre-2012 projects should be assumed
# to be the earliest file dates rather than the latest file dates.  In particular,
# step 8.2 (remove zeros) does not appear in section 2.3, because it has already 
# been addressed in section 2.2.  
# The old code that reflected this section 8 is now in 
# discarded code/completed projects on last-is-best approach.R


## 8.1 load data for checking ----
## -------------------------------------#
# save the version with 5002 projects, without PT stops/routes
# CP5002 <- st_read("../GIS/completedProjects(walking).sqlite") %>%
#   dplyr::select(-c(rail_stn, tram_bus_stop, tram_routes, bus_routes))
# 
# st_write(CP5002, "../GIS/completedProjects5002.sqlite", delete_layer = TRUE)

completed.projects.with.duplicates <- st_read("../GIS/completedProjects5002.sqlite") %>%
  # add a unique x_id field
  mutate(x_id = row_number())


## 8.2 remove zero dwelling entries ----
## -------------------------------------#
# developed from section 7.4 
# check whether any zero total_dwel entries are failures to add dwel types
zeros <- completed.projects.with.duplicates %>%
  filter(total_dwel == 0)
## 28 entries  they are all zero (or NA) in all dwelling number fields

completed.projects.first.filter <- completed.projects.with.duplicates %>%
  filter(total_dwel > 0)
# produces 4974 entries - 28 removed


## 8.3 remove padded project id's ----
## -------------------------------------#
# developed from sections 6.3 and 7.4
# remove project IDs padded with zeros, eg R0302 / R00302,
# where same street_name and suburb
completed.projects.second.filter <- completed.projects.first.filter %>%
  # make identifier consisting of unpadded project_id + street_name + suburb
  mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                            tolower(street_name),
                            tolower(suburb))) %>%
  # keep only the latest file year for each identifier group
  group_by(identifier) %>%
  filter(file_year == max(file_year)) %>%
  ungroup()
# produces 4875 entries, removing 99
# this is 2 fewer than removed in section 6.3, because 2 of them were also zeros


## 8.4 collect groups of intersecting projects ----
## -------------------------------------#
# developed from section 7.2
# uses st_intersection produces a dataframe with two extra columns: 
# - 'n.overlaps', which shows the number of overlaps for each record (and there is always at least one, itself;
# - 'origins',  which is the row numbers of the intersecting projects

# note that there are fewer intersections than in section 7.2:
# - first, because zeros and padded id's are removed first
# - second, because of adoption of 50% threshold instead of 5% (after
#   very detailed checking, recorded in working notes)

# add w_id row numbers to completed.projects.second.filter, so that 'origins' can locate
completed.projects.second.filter <- completed.projects.second.filter %>%
  mutate(w_id = row_number())

intersections <- st_intersection(completed.projects.second.filter) %>%
  # omit where there is only one intersetion (ie intersects itself)
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
# 281 entries in 138 groups


## 8.5 remove where same street number & name, and area & dwel within 20% +/- ----
## --------------------------------------#
# Developed from section 6.4, 6.5,  7.3 and 7.5
# Identifies and removes intersecting groups where both street number
# and name are the same, and area and total_dwel are each within 20% of group mean
# See working notes for testing of 20% (only identifies one more property than 10%, and it's correct)
# Section 7.3 required area & dwel to be identical, while 7.5 adds concept of tolerance

group_ids <- unique(intersecting.groups$group_id)

# dataframe to hold outputs
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
  # within tolerances, then flag the highest file year and others to discard
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

# eliminates a further 37 projects.

# record 'keeps1' and 'discards1', to be combined with results of 
# manual checking of 'further checks'
keeps1 <- intersecting.groups.flagged %>%
  filter(flag == "keep") %>%
  .$x_id

discards1 <- intersecting.groups.flagged %>%
  filter(flag == "discard") %>%
  .$x_id

# extract 'further checking' for manual check
intersecting.groups.manual.to.check <- intersecting.groups.flagged %>%
  filter(flag == "further checking") %>%
  # add 'area_pct' column (concerting st_area to ha) to help identify extent of overlaps
  mutate(isec_pct = isec_area_ha / (as.numeric(st_area(GEOMETRY)) / 10000) * 100 ) %>%
  st_drop_geometry()
length(unique(intersecting.groups.manual.to.check$group_id))  # 209 entries in 103 groups

# save output for manual check
write.csv(intersecting.groups.manual.to.check, "./isecManualToCheck.csv", row.names = FALSE)


## 8.6 manual checking ----
## --------------------------------------#
# items flagged for manual check as above reviewed, and 'flag' column completed
# with keep/discard

# Note that in manual checking, where a project has to be kept or discarded in more than one 
# intersecting group, it's marked as 'kept' or 'discarded' in both.  
# Accordingly there can be duplications.  

# load manually checked file
intersecting.groups.manual.checked <- read.csv("./isecManualChecked.csv")

# note that Excel has corrupted the street_num column,  by reading some street numbers
# as dates, eg 6-8 >> 6-Aug.  So don't use the file for anything other than 
# keeps/discards as below

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

length(unique(discards2))  # 50
# removes a further 50 overlapping projects from manual checking



## 8.7 combine manually checked discards with others, and filter completed.projects to remove ----
## --------------------------------------#
keeps <- c(keeps1, keeps2)
discards <- c(discards1, discards2)

# checks on keeps/discards
# duplicates are permissible as long as filtering by removing discards
# ('filtering' by adding keeps could produce duplicates, if keeps contains duplicates)
# should be no projects in both keeps and discards 
length(keeps) # 191
length(unique(keeps))  # 173
length(discards)  # 90
length(unique(discards))  # 87

keeps[keeps %in% discards]  # none
discards[discards %in% keeps]  # none
# so all checks passed

# remove discards from 'final' file
completed.projects.third.filter <- completed.projects.second.filter %>%
  filter(!x_id %in% discards)
# produces 4788 entries, removing 87

# reconciliation
# ---------------#
# original completed.projects.with.duplicates - 5002
# section 8.2 - remove zero dwellings - 28
# section 8.3 - remove padded project id_s - 99
# section 8.4 - remove same street no & name, and
#   area and dwelling no within 20% of group mean - 37
# section 85 - remove overlapping duplicates 
#  identified by manual checking - 50
# 5002 - (28 + 99 + 37 + 50) = 4788


# 9. Identifying high density ----
# -----------------------------------------------------------------------------#
# general approach - include all apartments and att_4s, and all other projects
# where density is 100+

## 9.1 first pass - any apartments or att_4s ----
## ------------------------------------#
# find projects with 'apartments' or 'att_4s'
test <- st_read("../GIS/completedProjects_walking.sqlite") %>%
  mutate(dwel_ha = total_dwel / area_ha,
         hi_dens = case_when(apartments > 0 | att_4s > 0 ~ "yes",
                             TRUE ~ "undecided"))

# examine those with 'apartments' or 'att_4s' - how many, what else do they include
test2 <- test %>%
  filter(hi_dens == "yes") # 2207 obs (that is, the total of projects with att_4s or apartments)
# Of the att_4s:
## - 19 also have att_2_3s; of them, 11 have total density 100+
## - 1 also has att_1s, and 2 also have detached - some of which were also att_2_3s
# Of the apartments: 
## - 32 also have townhouses; of them, 19 have total density 100+
## - none have any other categories

# find number of projects and dwellings that aren't att_4s / apartments
# (these will be excluded from dwelling count)
test2A <- test2 %>%
  filter(detached > 0 | townhouses > 0 | unknown > 0 |
           att_1s > 0 | att_2_3s > 0) #52 obs - that is, 52 projects with dwellings to be excluded

test2B <- sum(test2A$detached, na.rm = TRUE)
test2B  # 156 detached
test2C <- sum(test2A$townhouses, na.rm = TRUE)
test2C  # 666 townhouses
test2D <- sum(test2A$unknown, na.rm = TRUE)
test2D  # 0 unknown
test2E <- sum(test2A$att_1s, na.rm = TRUE)
test2E  # 2 att_1s
test2F <- sum(test2A$att_2_3s, na.rm = TRUE)
test2F  # 1733 att_2_3s
test2G <- test2B + test2C + test2D + test2E + test2F
test2G  # 2557 dwellings in total

# so - will exclude 2557 dwellings in 52 projects


# find number that don't have 'apartments' or 'att4_s', but have density 100+, using '>= 100'
test3 <- test %>%
  filter(hi_dens == "undecided" &
           dwel_ha >= 100)
# 934 obs (that is, don't have any apartments/att4_s but density is 100+)

# check results of those '< 100'
test3A <- test %>%
  filter(hi_dens == "undecided" &
           dwel_ha < 100)
# Of these, 5 have dwel_ha of exactly 100 (eg 0.14 ha, 14 dwellings), but don't show up
# in the 'high' results, presumably because of a floating point issue. Four of these are
# att_2_3s, and the fifth is 14 att_1 dwellings.
# Two more would round to 100 - including 71 'unknown' in Caulfield North and 14 townhouses in Springvale
# Better to round to 100.

# address floating point issue arising that apparently excludes several with exactly 100
# first option - 'round'
test3B <- test %>%
  filter(hi_dens == "undecided" &
           round(dwel_ha) >= 100)
# 941 obs (when rounded to 100) - includes the 7 above

# second and better option - 'round' to 1 decimal
test3C <- test %>%
  filter(hi_dens == "undecided" &
           round(dwel_ha, 1) >= 100)
# 939 obs (when rounded to 100 at one decimal - includes the 5 'exactly 100')
# go with this. Best way of fixing the floating point problem, and while it would be
# 'nice' to include the 71 unknown (by the project name, it's a mix of apartments and townhouses), it is
# more consistent simply to follow the rule




## 9.2 second pass - adding density > 100+ ----
## ------------------------------------#
# find projects with 'apartments' or 'att_4s' and others with density 100+
test4 <- st_read("../GIS/completedProjects_walking.sqlite") %>%
  mutate(dwel_ha = total_dwel / area_ha,
         hi_dens = case_when(apartments > 0 | att_4s > 0 ~ "yes",
                             round(dwel_ha, 1) >= 100 ~ "yes", # rounding to avoid floating point exclusions of exactly 100
                             TRUE ~ "no"))

# check that the total number matches that expected from 9.1
test5 <- test4 %>%
  filter(hi_dens == "yes") # 3146 obs (that is, the total of high density projects)
# this is 939 more than the number with att_4s or apartments

# check that the number with densities < 100 (that is, from 'apartments'/'att_4s') is still as expected
test6 <- test5 %>%
  filter(dwel_ha < 100) # 174 obs (that is, out of the 3141, 176 have densities < 100)
# this comprises 169 which were att_4s or apartments < 100, and the extra 5 added by rounding

# check that the number that are not marked as high density is as expected
test7 <- test4 %>%
  filter(hi_dens == "no") # 1642 obs
# 1642 + 3146 = 4788, as expected


## 9.3 adding number of high density apartments ----
## ------------------------------------#
# Approach here is:
# - where there are 'att_4s' or 'apartments', include only those and omit other dwelling types
# - otherwise, where included because density 100+, include all dwelling types


test8 <- test4 %>%
  # group by row, so that 'sum' can be used
  rowwise() %>%
  mutate(hi_dens_dwel = case_when(apartments > 0 | att_4s > 0 ~ sum(apartments, att_4s, na.rm = TRUE),
                                  round(dwel_ha, 1) >= 100 ~ total_dwel)) %>%
  ungroup()

# note in the above that 'case_when' should operate like 'if_else' - that is, it shouldn't move
# to the second case if the first is satisfied (and so should not count all dwellings
# where there are 'apartments'/'att_4s' and also density 100+)

# check whether this is so 
# examine rows where hi_dens_dwel != total_dwel
test9 <- test8 %>%
  filter(!(is.na(hi_dens_dwel))) %>%
  filter(hi_dens_dwel != total_dwel)
# produces 52 obs - these are the expected ones from 'test2' in 9.1 above

## 9.3A alternative approaches of excluding  detached/unknown where no att-4s/apt but is 100+ ----
## ------------------------------------#
# identify 'detached' / 'unknown' which are included by the 100+ rule
detached <- test8 %>% 
  # find those with no att_4s or apartments and density 100+
  filter((att_4s == 0 | is.na(att_4s)) & 
           (apartments == 0 | is.na(apartments)) & 
           round(dwel_ha, 1) >= 100) %>%
  # find detached, and arrange in descending order
  filter(detached > 0) %>%
  arrange(desc(detached))

no.detached <- sum(detached$detached)
no.detached  # 656 detached dwellings

unknown <- test8 %>% 
  # find those with no att_4s or apartments and density 100+
  filter((att_4s == 0 | is.na(att_4s)) & 
           (apartments == 0 | is.na(apartments)) & 
           round(dwel_ha, 1) >= 100) %>%
  # find unknown, and arrange in descending order
  filter(unknown > 0) %>%
  arrange(desc(unknown))

no.unknown <- sum(unknown$unknown)
no.unknown  # 6050 unkwnown dwellings

write.csv(detached %>% st_drop_geometry(),
          "../Tables/detached_no_apt_or_4s_100+.csv")

write.csv(unknown %>% st_drop_geometry(),
          "../Tables/unknown_no_apt_or_4s_100+.csv")

# in discussion with Chris 27/10/22, we decided we would not exclude these
# detached/unknown 100+ projects


## 9.4 counting high density apartments ----
## ------------------------------------#
test10 <- sum(test8$hi_dens_dwel, na.rm = TRUE)
test10  # 214,149 dwellings


# 10. Checking duplicate completions in multiple years ----
# -----------------------------------------------------------------------------#
## 10.1 Duplicate completions ----
## ------------------------------------#
# This section identifies how many projects shown as completed in one year are
# also completed in a different year.  Only covers those with identical project
# id's - there are also 214 others, dealt with in section 2.3 of apartments.R [old
# version], where project id's are not the same. Also doesn’t deal with the 
# single example of a project_id being repeated for separate projects (see 
# section 2.2 [old version] of apartments.R)

# Run section 2.1 of apartments.R [old version] to load the standard files for 2004-2021

# Combine the standard files into a single file, and filter to completed projects
completed.MRS <- rbind(standard_2004, standard_2005, standard_2006, standard_2007,
                       standard_2008, standard_2009, standard_2010, standard_2011,
                       standard_2012, standard_2013, standard_2014, standard_2015,
                       standard_2016, standard_2017, standard_2018, standard_2019,
                       standard_2020, standard_2021) %>%
  filter(status == "Completed")  # 9956 projects

# find unique project_id / year of completion combinations (that is, omitting 
# the projects from 2012 where the project is intentionally repeated in 2 years
# and has the same project id)
completed.MRS.projects <- completed.MRS %>%
  distinct(project_id, year_comp)  # 7277 projects

# check how many distinct project ids
length(unique(completed.MRS.projects$project_id))  # 5028 distinct project_ids
# this is not the same as the 5002 projects produced by section 2.2 of 
# apartments.R [old version]; that's probably [but not checked] because that 
# section removes any projects shown as 'completed' one year, and then not \
# completed in a following year

# group by year, and find number of completions for each year, and those that are
# repeated with a different project year
repeated.completions <- completed.MRS.projects %>%
  
  # first year of completion for each project
  group_by(project_id) %>%
  mutate(first_year_comp = min(year_comp),
         n_years_per_project = n()) %>%
  ungroup() %>%
  
  # retain just project id, year of first completion and number of completion
  # years per project - one row per project
  distinct(project_id, first_year_comp, n_years_per_project) %>%
  
  # number of projects for each year of first completion, and number with
  # subsequent completions
  group_by(first_year_comp) %>%
  mutate(n_projects_per_first_year_comp = n(), 
         n_repeated_completions = sum(n_years_per_project > 1)) %>%
  ungroup() %>%
  
  # retain just year of first completion and completion numbers
  distinct(first_year_comp, 
           n_projects_per_first_year_comp, 
           n_repeated_completions) %>%
  
  # add percentage of subsequent completions
  mutate(repeated_completion_pct = 
           n_repeated_completions / n_projects_per_first_year_comp * 100)

# write output
write.csv(repeated.completions, 
          "../Tables/MRS repeated completions.csv",
          row.names = FALSE)

# checks
# gross no of projects shown as completed:
nrow(completed.MRS.projects)  # 7277

# net no of projects after completions removed:
length(unique(completed.MRS.projects$project_id))  # 5028 distinct project_ids

# no of repeated projects:
sum(repeated.completions$n_repeated_completions) # 1178 - which is less than 
# the difference between 7277 and 5028, but that's because some completions
# are repeated multiple times (to confirm this, run 'repeated.completions' above
# as far as the second 'ungroup' (that is, stop before the second 'distinct')),
# and run - 
sum(repeated.completions$n_years_per_project) # 7277


## 10.2 Duplicate completions with different dwelling no's ----
## ------------------------------------#
# Following section 10.1, David Matthews (formerly DELWP, now Dept of Transport
# and Planning) advised that for files before 2012 with duplicates, the earlier
# year of completion should be considered

# This section looks at how many times the dwelling numbers change in a repeated
# project

# It also checks whether there are repeated completions that span the 2012 year

# Run first part of section 10.1 to create completed.MRS

# Find multiple completions with different dwell details
completed.MRS.diff.dwell.project_ids <- completed.MRS %>%
  # distinct dwelling details for project
  distinct(project_id, detached, unknown, att_1s, att_2_3s, att_4s, total_dwel) %>%
  # find those with more than one entry
  group_by(project_id) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  # find unique project id's
  .$project_id %>%
  unique(.)
  
completed.MRS.diff.dwell <- completed.MRS %>%
  filter(project_id %in% completed.MRS.diff.dwell.project_ids) %>%
  st_drop_geometry()

length(unique(completed.MRS.diff.dwell$project_id))

# write output
write.csv(completed.MRS.diff.dwell, 
          "../Tables/MRS repeated completions diff dwell.csv",
          row.names = FALSE)


# Also check whether any of the different details span 2012, the year in which we
# switched to specified completion years
completed.projects.spanning.2012 <- completed.MRS.diff.dwell %>%
  group_by(project_id) %>%
  filter(min(file_year) <= 2011 & max(file_year) >= 2012)

# write output
write.csv(completed.projects.spanning.2012, 
          "../Tables/MRS repeated completions spanning 2012.csv",
          row.names = FALSE)

# There are 19 project that span 2012, but 8 of them are standard repetition of 
# projects in 2011 and 2012, with no disagreement on completion year but a 
# revision to project numbers.  The other 2 are: (1) R00506, in the 2005,
# 2011 and 2012 files, with the latter two agreeing on a completion date of 2011, 
# and (2) R03484, in the 2005, 2012 and 2013 files, with the latter two agreeing
# on a completion date of 2012.  The 2012 and 2013 dates are verified by Google
# Earth (that is, the later files that specify a completion date are correct)


# Should probably also look at how many are split before/after the 2012 change - 
# eg if year actually reconsidered in 2012 or later, go with later?


## 10.3 Checking zero dwelling projects - are they all duplicates? ----
## ------------------------------------#
# This section looks at projects with zero dwellings.  Aim is to avoid accidentally
# keeping them and deleting a 'duplicate' that actually has dwelling numbers.

# Compare section 2.3.2 of apartments.R [old version], which eliminated 28 zero
# dwelling records as part of removing duplicates with different project id's.

# Run first part of section 10.1 to create completed.MRS

# First, find projects with zero total dwellings - do they all have zero
# in all categories of dwellings as well, or are some just adding up errors?
completed.MRS.zero.check <- completed.MRS %>%
  filter(total_dwel == 0 | is.na(total_dwel))
# 60 results, and they all have either 0 or NA for all dwelling categories 
# that is - not just adding up errors
# Also, all 60 have 0 (rather than NA) for total_dwel

length(unique(completed.MRS.zero.check$project_id)) # covering 37 projects
# (that is, some projects have more than one zero entry)

# Find zero-dwelling projects in their project_id groups
completed.MRS.zeros <- completed.MRS %>%
  # find the projects containing zeros
  mutate(zero = ifelse(total_dwel == 0 | is.na(total_dwel), 0, 1)) %>%
  group_by(project_id) %>%
  filter(min(zero) == 0) %>%
  arrange(project_id)

nrow(completed.MRS.zeros)  # 72 entries
length(unique(completed.MRS.zeros$project_id)) # covering 37 projects
nrow(completed.MRS.zeros %>%
       filter(zero == 1)) # 12 entries that are not zero (that is, 72 entries,
# minus 60 that are zero, as above)
length(unique(completed.MRS.zeros %>% 
                filter(zero == 1) %>%
                .$project_id)) # 9 (that is, of the 37 project with zero id's
# details are available for 9 of them)

# Check how many of these projects would get eliminated anyway as duplicates
# with different project id's - 
# Run section 2.2 of apartments.R [old version] to create 'completed.projects'
# prior to elimination of duplicates with different project id's
completed.projects.deleted.with.zeros <- completed.projects %>%
  filter(total_dwel == 0 | is.na(total_dwel)) %>%
  .$project_id %>%
  unique(.)
# 28 projects - so 28 of the 37 projects would get deleted anyway as duplicates
# with different project id's

deleted.anyway <- completed.MRS.zeros %>%
  filter(project_id %in% completed.projects.deleted.with.zeros) # 50 entries
length(unique(deleted.anyway$project_id)) # covering 28 projects
# all except one (R00687) are in the group for which no details are available

# All we want to do is keep the 9 for which some details do exist
completed.MRS.zeros.with.information <- completed.MRS.zeros %>%
  # find the projects with some 1's ('zero' is coded 0 if total dwel is zero,
  # or 1 otherwise)
  group_by(project_id) %>%
  filter(max(zero) == 1)
# 24 entries, covering 9 projects
# Of those 9 projects, in all cases the file years are 2, 3 or 4 adjacent years, and:-
## - R00687, R07176 - pre-2012 (so no stated completion date); last file is zero 
##   dwel; can rely on first file for completion date but need to avoid taking 
##   last file's total dwel figure
## - R03425, R03429, R03539, R05279, R05664, R07319 - pre-2012 (so no stated 
##   completion date); first file is zero dwel; if delete first file then will 
##   lose its completion date; total dwel will be taken correctly from last file
## - R07264 - 2012/13, and files agree on completion year; first file is zero dwel; 
##   can rely on the last file for its total dwel and completion year 
## See further comments  in working notes doc; need to avoid deleting zero total
##   dwel lines until after they have been used to calculate the completion date


## 10.4 Reviewing file years for projects with padded project_ids ----
## ------------------------------------#
# This section checks projects with padded project_ids (eg R0302/R00302) - 
# generally, the latest file information shoudl be used, but the earliest completion
# date should be used if not specified in a post 2012 file

# Run section 2.3.1 of apartments.R to load completed.projects.with.duplicates

padded.check <- completed.projects.with.duplicates %>%
  # make identifier consisting of unpadded project_id + street_name + suburb
  mutate(identifier = paste(as.numeric(substring(project_id, 2)),
                            tolower(street_name),
                            tolower(suburb))) %>%
  # group by identifier, and find those with duplicates
  group_by(identifier) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# Completion year pairs for all are 2004/05, except for:
# - R00295, R00302 and R00480, which are 2004/07
# - R00321, which is 2004/14
# Two of these have been examined on Google Earth - R00302 (where 2007 appeared
# more correct) and R00321 (where 2014 was definitely correct)
# Treat on same basis as standard - assume earlier completion date, unless
# specified to contrary post 2012
         