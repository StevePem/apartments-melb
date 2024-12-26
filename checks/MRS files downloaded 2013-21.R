# Review of MRS files downloaded from https://www.planning.vic.gov.au/land-use-and-population-research/urban-development-program 
# for 2013-20, and 2021 files provided by DELWP

# 1. Read in data
# -----------------------------------------------------------------------------
# 1.1 helper function to read zipped GIS files  
# -------------------------------------
# Extracts a zipped shapefile, mapinfo file, gpkg, etc, then reads uzing st_read
# 'subpath' is the string between the top zipped file and the ultimate file"
# 'file' not needed for files that don't have layers (eg shapefiles) if there is only one in the directory
# use 'file' (rather than 'layer') for shapefiles and mapinfo files; use both for gpkg [and sqlite?]
read_zipped_GIS <- function(zipfile, subpath = "", file = NULL, layer = NULL) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  if (is.null(layer)) {
    st_read(paste0(temp, subpath, file))
  } else {
    st_read(paste0(temp, subpath, file), layer)
  }
}


# 1.2 read in MRS files and convert to common CRS
# -------------------------------------
# 'MRS': major redevelopment site
# use CRS 7899 (GDA2020 / Vicgrid)
MRS_2021x <- read_zipped_GIS("../Data/UDP2021_gpkg_out.zip",
                            file = "/UDP2021.gpkg",
                            layer = "UDP_MRS2021") %>%
  st_transform(., crs = 7899)

MRS_2020x <- read_zipped_GIS("../Data/UDP2020_MRS.zip", 
                            subpath = "/ll_gda94/esrishape/whole_of_dataset/victoria/UDP") %>%
  st_transform(., crs = 7899)

MRS_2019x <- read_zipped_GIS("../Data/UDP_2019_GIS_Data.zip",
                            file = "/UDP_MRS2019.shp") %>%
  st_transform(., crs = 7899)

MRS_2018x <- read_zipped_GIS("../Data/UDP_2018_GIS_Data_.zip",
                            subpath = "/UDP_2018_GIS_Data_",
                            file = "/MRS2018.shp") %>%
  st_transform(., crs = 7899)

MRS_2017x <- read_zipped_GIS("../Data/UDP2017_GIS_DATA.zip",
                            file = "/MRS2017.shp") %>%
  st_transform(., crs = 7899)

MRS_2016x <- read_zipped_GIS("../Data/UDP2016.zip",
                            file = "/MRS2016.tab") %>%
  st_transform(., crs = 7899)

MRS_2015x <- read_zipped_GIS("../Data/UDP-2015-MapInfo-Data.zip",
                            file = "/MRS2015.tab") %>%
  st_transform(., crs = 7899)

MRS_2014x <- read_zipped_GIS("../Data/UDP-2014-MapInfo-Data.zip",
                            subpath = "/UDP 2014 MapInfo Data",
                            file = "/MRS2014.tab") %>%
  st_transform(., crs = 7899)

MRS_2013x <- read_zipped_GIS("../Data/UDP-2013-Map-Info-Data.zip",
                            file = "/MRS2013.tab") %>%
  st_transform(., crs = 7899)

# 1.3 compare content (observations/variables) of files
# -------------------------------------
# 'x' files (eg MRS_2020x) are the publicly-available files for 2013-2020, plus the 2021 file provided by DWELP
# files without x (eg MRS_2020) are the files for 2004-20 provided by DELWP - see #2.2 of 'apartments.R'

years <- c(2021:2013)

# make table to hold details
filedetails <- data.frame(year = numeric(),
                          xfile_obs = numeric(),
                          xfile_vars = numeric(),
                          file_obs = numeric(),
                          file_vars = numeric()
                          )

for (i in 1:length(years)) {
  # filenames for the year
  xfile <- get(paste0("MRS_", as.character(years[i]), "x"))
  file <- get(paste0("MRS_", as.character(years[i])))
  # add details to table
  filedetails <- rbind(filedetails,
                       cbind(year = years[i],
                             xfile_obs = nrow(xfile),
                             xfile_vars = length(xfile),
                             file_obs = nrow(file),
                             file_vars = length(file)))
}


# year xfile_obs xfile_vars file_obs file_vars
# 2021      2527         20     2527        21
# 2020      2701         20     2701        21
# 2019      3002         23     3002        21
# 2018      3444         22     3444        21
# 2017      3918         20     3918        21
# 2016      3967         13     3674        35
# 2015      3415         13     3415        35
# 2014      3123         13     2940        35
# 2013      2951         13     2859        36


# 2. Completed apartments
# -----------------------------------------------------------------------------
# 2.1 Filter to completed apartments, and standardise columns
# -------------------------------------
completed_2021x <- MRS_2021x %>%
  filter(Status == "Completed") %>%
  dplyr::select(project_id = ProjectID, status = Status, year_comp = YearCompl,
                area_ha = AreaHa, proj_name = ProjName, proj_part = ProjPart,
                street_num = StreetNum, street_name = StreetName, street_type = StreetType,
                add_misc = AddMisc, detached = Detached, townhouses = Townhouses,
                apartments = Apartments, unknown = Unknown, total_dwel = Total_Dwel,
                max_storeys = MaxStoreys, suburb = Suburb, lga = LGA, region = Region,
                geometry = Shape)

completed_2020x <- MRS_2020x %>%
  filter(STATUS == "Completed") %>%
  dplyr::select(project_id = PROJECTID, status = STATUS, year_comp = YEARCOMPL,
                area_ha = AREAHA, proj_name = PROJNAME, proj_part = PROJPART,
                street_num = STREETNUM, street_name = STREETNAME, street_type = STREETTYPE,
                add_misc = ADDMISC, detached = DETACHED, townhouses = TOWNHOUSES,
                apartments = APARTMENTS, unknown = UNKNOWN, total_dwel = TOTAL_DWEL,
                max_storeys = MAXSTOREYS, suburb = SUBURB, lga = LGA, region = REGION,
                geometry)

completed_2019x <- MRS_2019x %>%
  filter(Status == "Completed") %>%
  # omitting 'OBJECTID', 'Shape_Leng' and 'Shape_Area'
  dplyr::select(project_id = ProjectID, status = Status, year_comp = YearCompl,
                area_ha = AreaHa, proj_name = ProjName, proj_part = ProjPart,
                street_num = StreetNum, street_name = StreetName, street_type = StreetType,
                add_misc = AddMisc, detached = Detached, townhouses = Townhouses,
                apartments = Apartments, unknown = Unknown, total_dwel = Total_Dwel,
                max_storeys = MaxStoreys, suburb = Suburb, lga = LGA, region = Region,
                geometry)

completed_2018x <- MRS_2018x %>%
  filter(Status == "Completed") %>%
  # omitting 'Shape_Leng' and 'Shape_Area'
  dplyr::select(project_id = ProjectID, status = Status, year_comp = YearCompl,
                area_ha = AreaHa, proj_name = ProjName, proj_part = ProjPart,
                street_num = StreetNum, street_name = StreetName, street_type = StreetType,
                add_misc = AddMisc, detached = Detached, townhouses = Townhouses,
                apartments = Apartments, unknown = Unknown, total_dwel = Total_Dwel,
                max_storeys = MaxStoreys, suburb = Suburb, lga = LGA, region = Region,
                geometry)

completed_2017x <- MRS_2017x %>%
  filter(Status == "Completed") %>%
  dplyr::select(project_id = ProjectID, status = Status, year_comp = YearCompl,
                area_ha = AreaHa, proj_name = ProjName, proj_part = ProjPart,
                street_num = StreetNum, street_name = StreetName, street_type = StreetType,
                add_misc = AddMisc, detached = Detached, townhouses = Townhouses,
                apartments = Apartments, unknown = Unknown, total_dwel = Total_Dwel,
                max_storeys = MaxStoreys, suburb = Suburb, lga = LGA, region = Region,
                geometry)

completed_2016x <- MRS_2016x %>%
  filter(Status == "Completed") %>%
  # adding 'year_comp', 'proj_part', 'detached', 'townhouses', 'apartments', 'unknown', 'max_storeys'
  mutate(year_comp = NA, proj_part = NA, detached = NA, townhouses = NA, 
         apartments = NA, unknown = NA, max_storeys = NA) %>%
  dplyr::select(project_id = Site_ID, status = Status, year_comp,
                area_ha = Area_ha, proj_name = project_name, proj_part,
                street_num = street_number, street_name, street_type,
                add_misc = Address_other, detached, townhouses,
                apartments, unknown, total_dwel = Total_Dwellings,
                max_storeys, suburb = suburb_name, lga = Municipality, region = Region,
                geometry)

completed_2015x <- MRS_2015x %>%
  filter(Status == "Completed") %>%
  # adding 'year_comp', 'proj_part', 'detached', 'townhouses', 'apartments', 'unknown', 'max_storeys'
  mutate(year_comp = NA, proj_part = NA, detached = NA, townhouses = NA, 
         apartments = NA, unknown = NA, max_storeys = NA) %>%
  dplyr::select(project_id = Site_ID, status = Status, year_comp,
                area_ha = Area_ha, proj_name = project_name, proj_part,
                street_num = street_number, street_name, street_type,
                add_misc = Address_other, detached, townhouses,
                apartments, unknown, total_dwel = Total_Dwellings,
                max_storeys, suburb = suburb_name, lga = Municipality, region = Region,
                geometry)

completed_2014x <- MRS_2014x %>%
  filter(Status == "Completed") %>%
  # adding 'year_comp', 'proj_part', 'detached', 'townhouses', 'apartments', 'unknown', 'max_storeys'
  mutate(year_comp = NA, proj_part = NA, detached = NA, townhouses = NA, 
         apartments = NA, unknown = NA, max_storeys = NA) %>%
  dplyr::select(project_id = Site_ID, status = Status, year_comp,
                area_ha = Area_ha, proj_name = project_name, proj_part,
                street_num = street_number, street_name, street_type,
                add_misc = Address_other, detached, townhouses,
                apartments, unknown, total_dwel = Total_Dwellings,
                max_storeys, suburb = suburb_name, lga = Municipality, region = Region,
                geometry)

completed_2013x <- MRS_2013x %>%
  filter(Status == "Completed") %>%
  # adding 'year_comp', 'proj_part', 'detached', 'townhouses', 'apartments', 'unknown', 'max_storeys'
  mutate(year_comp = NA, proj_part = NA, detached = NA, townhouses = NA, 
         apartments = NA, unknown = NA, max_storeys = NA) %>%
  dplyr::select(project_id = Site_ID, status = Status, year_comp,
                area_ha = Area_ha, proj_name = project_name, proj_part,
                street_num = street_number, street_name, street_type,
                add_misc = Address_other, detached, townhouses,
                apartments, unknown, total_dwel = Total_Dwellings,
                max_storeys, suburb = suburb_name, lga = Municipality, region = Region,
                geometry)


# 2.2 compare projects in 'completed' files
# -------------------------------------
# for the 'completed' files tested in this section - 
# - 'x' files (eg completed_2020x) are from the publicly-available files for 2013-2020, plus the 2021 file provided by DWELP
# - files without x (eg completed_2020) are from the files for 2004-20 provided by DELWP - see #2.2 of 'apartments.R'

years <- c(2021:2013)

# make table to hold details
filedetails <- data.frame(year = numeric(),
                          xfile_extra_proj = character(),
                          file_extra_proj = character()
)

for (i in 1:length(years)) {
  # filenames for the year
  xfile <- get(paste0("completed_", as.character(years[i]), "x"))
  file <- get(paste0("completed_", as.character(years[i])))
  # add details to table
  filedetails <- rbind(filedetails,
                       cbind(year = years[i],
                             xfile_extra_proj = xfile %>%
                               filter(!project_id %in% file$project_id) %>%
                               .$project_id %>%
                               length(.),
                             file_extra_proj = xfile %>%
                               filter(!project_id %in% file$project_id) %>%
                               .$project_id %>%
                               length(.)))
}
# completed projects are the same for all of 2013-21

