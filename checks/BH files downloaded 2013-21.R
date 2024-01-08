# Review of BH files downloaded from https://www.planning.vic.gov.au/land-use-and-population-research/urban-development-program 
# for 2013-20, and 2021 files provided by DELWP

library(dplyr)
library(sf)
library(ggplot2)

# 1. Read in data ----
# -----------------------------------------------------------------------------#
## 1.1 helper function to read zipped GIS files ----
## -------------------------------------#
source("./functions/readZippedGIS.R")

## 1.2 read in BH files and convert to common CRS ----
## -------------------------------------#
# 'BH': broad hectaresite
# use CRS 7899 (GDA2020 / Vicgrid)
BH_2021 <- read_zipped_GIS("../Data/UDP2021_gpkg_out.zip",
                             file = "/UDP2021.gpkg",
                             layer = "UDP_BH2021") %>%
  st_transform(., crs = 7899)

BH_2020 <- read_zipped_GIS("../Data/UDP2020_BH.zip", 
                             subpath = "/ll_gda94/esrishape/whole_of_dataset/victoria/UDP") %>%
  st_transform(., crs = 7899)

BH_2019 <- read_zipped_GIS("../Data/UDP_2019_GIS_Data.zip",
                             file = "/UDP_BH2019.shp") %>%
  st_transform(., crs = 7899)

BH_2018 <- read_zipped_GIS("../Data/UDP_2018_GIS_Data_.zip",
                             subpath = "/UDP_2018_GIS_Data_",
                             file = "/BH2018.shp") %>%
  st_transform(., crs = 7899)

BH_2017 <- read_zipped_GIS("../Data/UDP2017_GIS_DATA.zip",
                             file = "/BH2017.shp") %>%
  st_transform(., crs = 7899)

BH_2016 <- read_zipped_GIS("../Data/UDP2016.zip",
                             file = "/BH2016.tab") %>%
  st_transform(., crs = 7899)

BH_2015 <- read_zipped_GIS("../Data/UDP-2015-MapInfo-Data.zip",
                             file = "/BH2015.tab") %>%
  st_transform(., crs = 7899)

BH_2014 <- read_zipped_GIS("../Data/UDP-2014-MapInfo-Data.zip",
                             subpath = "/UDP 2014 MapInfo Data",
                             file = "/BH2014.tab") %>%
  st_transform(., crs = 7899)

BH_2013 <- read_zipped_GIS("../Data/UDP-2013-Map-Info-Data.zip",
                             file = "/BH2013.tab") %>%
  st_transform(., crs = 7899)


# 2. Check development status and test against PPTN ----
# -----------------------------------------------------------------------------#
## 2.1 make table of 'developed' status used in each BH file ----
## -------------------------------------#
devStatuses <- data.frame()
years <- c(2013:2021)
devfields <- c("DevelopmentTiming", "Developmen", "DEV_STATUS", "DevelopmentStatus")

for (i in 1:length(years)) {
  year <- years[i]
  file <-get(paste0("BH_", as.character(year))) %>%
    st_drop_geometry()
  
  # pick the appropriate devfield name for the relevant year
  for (j in 1:length(devfields)) {
    if (devfields[j] %in% names(file)) {
      devfield <- devfields[j]
    }
  }
  
  # find the development statuses used in the relevant year
  devStatusList <- unique(file[, devfield])  # https://stackoverflow.com/questions/7979451/passing-a-string-as-a-data-frame-column-name
  
  # add year and development status to dataframe
  devStatuses <- rbind(devStatuses,
                       cbind(year = year,
                             devstat = paste(devStatusList, collapse = ", ")))  # https://stackoverflow.com/questions/13973116/convert-r-vector-to-string-vector-of-1-element
}

devStatuses
# year devstat
# 2013 3-5years, Developed, 6-10years, 11+ years, 1-2years, UGZ (PSP Required)
# 2014 3-5years, 6-10years, 11+ years, 1-2years, Developed, UGZ (PSP Required), Potential Residential
# 2015 3-5years, 11+ years, 1-2years, 6-10years, No Timing, Developed, Potential Residential, UGZ (PSP Required)
# 2016 1-2years, 3-5years, 6-10years, No Timing, Developed, Potential Residential, 11+ years, UGZ (PSP Required)
# 2017 3-5years, 1-2years, Developed, No Timing, 6-10years, 11+ years, Potential Residential, UGZ (PSP Required)
# 2018 Zoned englobo land, Unzoned englobo land requiring a PSP, Lots with a title, Proposed lots
# 2019 Zoned, Unzoned, Approved, Proposed
# 2020 Zoned, Unzoned, Proposed, Approved
# 2021 Proposed, Approved, Zoned, Unzoned

# Only 2013-2017 have 'Developed' status
# Email from D Matthews, DELWP, 21 Sep 22022: from 2018, instead of 'developed', lots are shown
# as 'approved with a title'
# But this section 2 considers only 'developed'


## 2.2 filter to developed ----
## -------------------------------------#
devyears <- c(2013:2017)

for (i in 1:length(devyears)) {
  year <- devyears[i]
  file <- get(paste0("BH_", as.character(year)))
  
  # create a new variable name for each year, and assign it the filtered developed projects
  # https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
  # Note that some projects with 'Developed' status may still be shown with 'RecentDevelopment' as 'Under Construction', 
  # so these need to be filtered out
  if ("DevelopmentTiming" %in% names(file)) {
    assign(paste0("BH_", as.character(year), "_developed"),
           file %>% 
             filter(DevelopmentTiming == "Developed" & !(RecentDevelopment == "Under Construction")))
  } else if ("Developmen" %in% names(file)) {
    assign(paste0("BH_", as.character(year), "_developed"),
           file %>% filter(Developmen == "Developed" & !(RecentDeve == "Under Construction")))
  }
}

# note that the remaining 'RecentDevelopment' column shows several year dates, so there is likely to be overlap between years


## 2.3 examine 'high' density ----
## -------------------------------------#
# only 2013-2015 have a 'Density' column

# 2013 - 
unique(BH_2013_developed$Density)  # "Normal" "Low" 
# metadata says "Determined by type of residential zoning, ie. LDRZ or R1Z."

# 2014 - 
unique(BH_2014_developed$Density)  # "Medium" "Convention" "High" "Low"
# metadata - 
# Number of lots anticipated/constructed per hectare:
# •	Low - 10 dwellings or less
# •	Conventional - 10 to 15 dwellings per hectare
# •	Medium - 16 to 30 dwellings per hectare
# •	High - above 30 dwellings per hectare
# Source: MPA, PSP Guidelines 2012.

# 2015 - 
unique(BH_2014_developed$Density)  # "Medium" "Convention" "High" "Low"
# metadata - same as 2014

# So - 'high' density is > 30 dwellings/ha (which is well short of ~100 for apartments)

## 2.4 filter to dwellings > 30 ----
## -------------------------------------#
devyears <- c(2013:2017)

for (i in 1:length(devyears)) {
  year <- devyears[i]
  file <- get(paste0("BH_", as.character(year), "_developed"))
  
  assign(paste0("BH_", as.character(year), "_developed_high"),
         file %>% filter(Lotsperha > 30))
}

# 2013 and 2014 have a 'ZonedResidential' field, so it would also be possible
# filter out any that are not - however, there are none in 2013, and only one  in 2014;
# and subsequent check for later years suggests 'commercial' zoning does not
# always preclude residential construction 


## 2.5 load PPTN and buffer to 800m ----
## -------------------------------------#
# 'PPTNlines' are bus and tram routes; 'PPTNpoints' are train stations
PPTNlines <- read_zipped_GIS("../Data/PPTN.zip",
                             subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/PPTN_2017",
                             file = "/PPTN_LINES.shp") %>%
  st_transform(., crs = 7899)

PPTNpoints <- read_zipped_GIS("../Data/PPTN.zip",
                             subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/PPTN_2017",
                             file = "/PPTN_POINTS.shp") %>%
  st_transform(., crs = 7899)

# buffer lines and points to 800 and merge
PPTN800 <- st_union(PPTNlines %>% st_buffer(., 800) %>% st_union(.), 
                    PPTNpoints %>% st_buffer(., 800) %>% st_union(.))

ggplot() + geom_sf(data = PPTN800)


## 2.6 intersect high density developments with PPTN800 ----
## --------------------------------------#
devyears <- c(2013:2017)

for (i in 1:length(devyears)) {
  year <- devyears[i]
  file <- get(paste0("BH_", as.character(year), "_developed_high"))
  
  assign(paste0("BH_", as.character(year), "_developed_high_PPTN"),
         file %>% 
           filter(st_intersects(file$geometry, PPTN800, sparse = FALSE)))
}

nrow(BH_2013_developed_high_PPTN) # 4
nrow(BH_2014_developed_high_PPTN) # 1
nrow(BH_2015_developed_high_PPTN) # 6
nrow(BH_2016_developed_high_PPTN) # 8
nrow(BH_2017_developed_high_PPTN) # 9


## 2.7 combine standardised files ----
## --------------------------------------#
BH_high_density_2013_17 <- rbind(BH_2013_developed_high_PPTN %>%
                                   mutate(file_year = 2013),
                                 BH_2014_developed_high_PPTN %>%
                                   mutate(file_year = 2014),
                                 BH_2015_developed_high_PPTN %>%
                                   mutate(ZonedResidential = "",
                                          EstateName = "",
                                          ZoneCode = "",
                                          ProjectCode = "",
                                          SiteID = "",
                                          file_year = 2015) %>%
                                   rename(suburb = ABSSuburb),
                                 BH_2016_developed_high_PPTN %>%
                                   mutate(Density = "",
                                          ZonedResidential = "",
                                          EstateName = "",
                                          ZoneCode = "",
                                          ProjectCode = "",
                                          SiteID = "",
                                          file_year = 2016) %>%
                                   rename(suburb = ABSSuburb),
                                 BH_2017_developed_high_PPTN %>%
                                   mutate(Density = "",
                                          ZonedResidential = "",
                                          EstateName = "",
                                          ZoneDescription = "",
                                          ProjectCode = "",
                                          SiteID = "",
                                          file_year = 2017) %>%
                                   rename(DevelopmentTiming = Developmen,
                                          RecentDevelopment = RecentDeve,
                                          suburb = ABSSuburb))

# write as GIS and table
st_write(BH_high_density_2013_17, "../GIS/BH_high_density_2013_17.sqlite", delete_layer = TRUE)
write.csv(BH_high_density_2013_17 %>% st_drop_geometry(),
          "../Tables/BH_high_density_2013_17.csv", row.names = FALSE)


# 3. Check developed/approved projects with developed/approved status ----
# -----------------------------------------------------------------------------#
# Email from D Matthews, DELWP, 21 Sep 2202: from 2018, instead of 'developed', lots are shown
# as 'approved with a title'
# See section 2.1 for development status.
# This section 3 considers both 'developed' (before 2018) and 'approved' (from 2018) 

## 3.1 Standardise and combine files, and filter to developed/approved ----
## -------------------------------------#
BH_combined <- rbind(BH_2013 %>%
                       mutate(PSPName = "",
                              file_year = 2013),
                     BH_2014 %>%
                       mutate(PSPName = "",
                              file_year = 2014),
                     BH_2015 %>%
                       mutate(ZonedResidential = "",
                              EstateName = "",
                              ZoneCode = "",
                              ProjectCode = "",
                              SiteID = "",
                              PSPName = "",
                              file_year = 2015) %>%
                       rename(suburb = ABSSuburb),
                     BH_2016 %>%
                       mutate(Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneCode = "",
                              ProjectCode = "",
                              SiteID = "",
                              PSPName = "",
                              file_year = 2016) %>%
                       rename(suburb = ABSSuburb),
                     BH_2017 %>%
                       mutate(Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneDescription = "",
                              ProjectCode = "",
                              SiteID = "",
                              PSPName = "",
                              file_year = 2017) %>%
                       rename(DevelopmentTiming = Developmen,
                              RecentDevelopment = RecentDeve,
                              suburb = ABSSuburb),
                     BH_2018 %>%
                       # Note 'OBJECTID' is just the row number, not an allocated project_id that applies across years
                       dplyr::select(-c(OBJECTID, Shape_Leng, Shape_Area)) %>%
                       mutate(RecentDevelopment = "",
                              Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneDescription = "",
                              ProjectCode = "",
                              SiteID = "",
                              file_year = 2018) %>%
                       rename(DevelopmentTiming = Developmen,
                              LGAName = LGA,
                              suburb = Suburb),
                     BH_2019 %>%
                       dplyr::select(-c(OBJECTID, Shape_Leng, Shape_Area)) %>%
                       mutate(RecentDevelopment = "",
                              Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneDescription = "",
                              ProjectCode = "",
                              SiteID = "",
                              file_year = 2019) %>%
                       rename(DevelopmentTiming = Developmen,
                              LGAName = LGA,
                              suburb = Suburb),
                     BH_2020 %>%
                       mutate(RecentDevelopment = "",
                              Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneDescription = "",
                              ProjectCode = "",
                              SiteID = "",
                              file_year = 2020) %>%
                       rename(DevelopmentTiming = DEV_STATUS,
                              Lotsperha = LOTSPERHA,
                              Areaha = AREAHA,
                              TotalLots = TOTALLOTS,
                              ZoneCode = ZONECODE,
                              Region = REGION,
                              LGAName = LGA,
                              suburb = SUBURB,
                              PSPName = PSPNAME),
                     BH_2021 %>%
                       mutate(RecentDevelopment = "",
                              Density = "",
                              ZonedResidential = "",
                              EstateName = "",
                              ZoneDescription = "",
                              ProjectCode = "",
                              SiteID = "",
                              file_year = 2021) %>%
                       rename(DevelopmentTiming = DevelopmentStatus,
                              LGAName = LGA,
                              suburb = Suburb,
                              geometry = Shape)
                     )

BH_dev_appr <- BH_combined %>%
  # See section 2.1 for development status; note that some projects with 'Developed' status 
  # may still be shown with 'RecentDevelopment' as 'Under Construction', so these need to be filtered out
  filter(DevelopmentTiming %in% c("Developed", "Lots with a title", "Approved") &
           !(RecentDevelopment == "Under Construction"))


## 3.2 Check adequacy of 'Areaha' field ----
## -------------------------------------#
test <- BH_dev_appr %>%  # created in 3.1 above
  mutate(test_area = as.numeric(st_area(geometry) / 10000),  # sq m to ha
         area_diff = abs(Areaha - test_area))
max(test$area_diff)  # 0.8671658

diff100 <- test %>%
  filter(area_diff >= 0.01) # difference > 100 sq m
nrow(diff100)  # 68 - that is, 28 projects (out of 85864) where area is out by 100 sq m or more

# conclusion - area_ha will mostly be adequate; where it's 'wrong', we can't really be certain
# whether the error is in the calculation of the area, or the digitisation of the boundaries!


## 3.3 Find developments with lots_ha 100+ ----
## -------------------------------------#
# checking - calculation based on DELWP's stated 'Areaha'
lots100plus1 <- BH_dev_appr %>%
  filter(TotalLots / Areaha >= 100)  
# returns 497 obs, of which all but 7 have 1 lot

# checked - calculation based on st_area
lots100plus_chk <- BH_dev_appr %>%
  filter(TotalLots / (as.numeric(st_area(geometry)) / 10000) >= 100) # sq m to ha
# also returns 497 obs, of which all but 7 have 1 lot
# accordingly, the stated 'Areaha' can be used

# calculation
lots100plus <- BH_dev_appr %>%
  filter(TotalLots / Areaha >= 100) %>%
  mutate(dwel_ha = TotalLots / Areaha,
         x_id = row_number())
  
st_write(lots100plus, "../GIS/BH_100dwel_ha_plus.sqlite", delete_layer = TRUE)


# comments on the 497
# those with > 1 lot, checked in Google Earth
# x_id 1 & 2 (duplicates - 82 lots) - commercial zone, but apartments/townhouses in Mill Park
# x_id 3 & 4 (duplicates - 69 lots) - apartments in Epping
# x_id 5 (49 lots) - apartments in South Morang
# x_id 51 (24 lots) - apartments in Epping
# x_id 171 (21 lots) - apartments in Craigieburn

# check the others for intersections
intersections <- st_intersection(lots100plus) %>%
  # omit where there is only one intersetion (ie intersects itself)
  filter(n.overlaps > 1) %>%  # 450 intersections
  # omit where not a polygon or collection (ie points and lines)
  filter(st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) # 2 intersections
# The 2 intersections are the duplicate pairs X_id 1&2 and 3&4
# Accordingly, all of the other 497 are indeed individual lots

# Some spot checks done of these; they all look like groups of apartments or 
# (more often) townhouses

# Accordingly, total 100+ are:
# x_id 1&2  82 lots
# x_id 3&4  69 lots
# x_id 5    49 lots
# x_id 51   24 lots
# x_id 171  21 lots
# others   490 lots
          #---------#
# total    735 lots

# Suburbs where they are located:
suburbs100plus <- lots100plus %>%
  .$suburb %>%
  str_to_title(.) %>%
  unique(.) %>%
  sort(.)
suburbs100plus  
# [1] "Clyde North"      "Craigieburn"      "Cranbourne South" "Cranbourne West"  "Diggers Rest"     "Doreen"          
# [7] "Epping"           "Keysborough"      "Mill Park"        "Officer"          "Pakenham"         "Point Cook"      
# [13] "Roxburgh Park"    "South Morang"     "Strathtulloh"     "Tarneit"          "Truganina"        "Werribee"        
# [19] "Wollert"          "Wyndham Vale"  


## 3.4 Find how many developments with lots_ha 100+ are within 800m of PPTN ----
## -------------------------------------#
# Run section 2.5 above to load PPTN and buffer to 800m to produce 'PPTN800'

lots100plus_PPTN <- lots100plus %>%
  filter(st_intersects(.$geometry, PPTN800, sparse = FALSE))

# produces 242 entries, comprising:
# x_id 1&2  82 lots
# x_id 5    49 lots
# others   239 lots
#---------#
# total    370 lots

# Suburbs where they are located:
suburbs100plus_PPTN <- lots100plus_PPTN %>%
  .$suburb %>%
  str_to_title(.) %>%
  unique(.) %>%
  sort(.)
suburbs100plus_PPTN
# [1] "Cranbourne South" "Cranbourne West"  "Mill Park"        "Officer"          "Pakenham"         "Point Cook"      
# [7] "South Morang"     "Tarneit"          "Wyndham Vale"    

