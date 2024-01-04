# Checks of VBA building approvals for [yars...], downloaded from ...

# Used to check proportion of high density developments that are <10 dwellings
# (and so excluded from MRS)


# set up environment
library(dplyr)
library(readxlsb)
library(ggplot2)

# 1. Read in data
# -----------------------------------------------------------------------------
# note - reading in the .xlsb files is very slow (about 30 mins per year)
# these figures are building approvals for Victoria

# 1.1 read in
# -------------------------------------
zipfile <- "../Data/VBA.zip"
temp <- tempfile()
unzip(zipfile, exdir = temp)
 
years <- c(2009:2022)

for (i in 1:length(years)) {
  year <- years[i]
  
  # filename for the year
  if (year == 2022) {
    filename <- "20220496-Internal-TL-raw-data.xlsb"
  } else if (year == 2021) {
    filename <- "VBA-DataVic-Building-Permits-2021-Dec.xlsb"
  } else {
    filename <- paste0("VBA-DataVic-Building-Permits-", as.character(years[i]), ".xlsb")
  }
  
  # sheet containing data for the year
  if (year %in% c(2010:2014)) {
    sheetname <- "VBA_DataVic_Building_Permits_20"
  } else if (year == 2019) {
    sheetname <- "Data"
  } else if (year == 2022){
    sheetname <- "data"
  } else {
    sheetname <- "Sheet1"
  }
  # create a new variable name for each year, and assign it the relevant fil for the year
  # https://stackoverflow.com/questions/5510966/create-a-variable-name-with-paste-in-r
  assign(paste0("VBA_", as.character(years[i])),
         read_xlsb(paste0(temp, "/", filename), sheet = sheetname))

  print(paste(year, "done"))

}

# 1.2 saving and reloading
# -------------------------------------
# note - each file is about 25MB, so may not be permanently retained once checking done
write.csv(VBA_2009, "../Tables/VBA_2009.csv", row.names = FALSE)
write.csv(VBA_2010, "../Tables/VBA_2010.csv", row.names = FALSE)
write.csv(VBA_2011, "../Tables/VBA_2011.csv", row.names = FALSE)
write.csv(VBA_2012, "../Tables/VBA_2012.csv", row.names = FALSE)
write.csv(VBA_2013, "../Tables/VBA_2013.csv", row.names = FALSE)
write.csv(VBA_2014, "../Tables/VBA_2014.csv", row.names = FALSE)
write.csv(VBA_2015, "../Tables/VBA_2015.csv", row.names = FALSE)
write.csv(VBA_2016, "../Tables/VBA_2016.csv", row.names = FALSE)
write.csv(VBA_2017, "../Tables/VBA_2017.csv", row.names = FALSE)
write.csv(VBA_2018, "../Tables/VBA_2018.csv", row.names = FALSE)
write.csv(VBA_2019, "../Tables/VBA_2019.csv", row.names = FALSE)
write.csv(VBA_2020, "../Tables/VBA_2020.csv", row.names = FALSE)
write.csv(VBA_2021, "../Tables/VBA_2021.csv", row.names = FALSE)
write.csv(VBA_2022, "../Tables/VBA_2022.csv", row.names = FALSE)

VBA_2009 <- read.csv("../Tables/VBA_2009.csv")
VBA_2010 <- read.csv("../Tables/VBA_2010.csv")
VBA_2011 <- read.csv("../Tables/VBA_2011.csv")
VBA_2012 <- read.csv("../Tables/VBA_2012.csv")
VBA_2013 <- read.csv("../Tables/VBA_2013.csv")
VBA_2014 <- read.csv("../Tables/VBA_2014.csv")
VBA_2015 <- read.csv("../Tables/VBA_2015.csv")
VBA_2016 <- read.csv("../Tables/VBA_2016.csv")
VBA_2017 <- read.csv("../Tables/VBA_2017.csv")
VBA_2018 <- read.csv("../Tables/VBA_2018.csv")
VBA_2019 <- read.csv("../Tables/VBA_2019.csv")
VBA_2020 <- read.csv("../Tables/VBA_2020.csv")
VBA_2021 <- read.csv("../Tables/VBA_2021.csv")
VBA_2022 <- read.csv("../Tables/VBA_2022.csv")


# 1.3 combining
# -------------------------------------
VBA <- rbind(VBA_2009,
             VBA_2010,
             VBA_2011,
             VBA_2012,
             VBA_2013,
             VBA_2014,
             VBA_2015,
             VBA_2016,
             VBA_2017,
             VBA_2018,
             VBA_2019 %>% 
               rename(BASIS_Zone = BASIS_Building_Use),
             VBA_2020 %>%
               rename(Reported_Levy_amount = Original_Levy_Paid__c,
                      Site_street = site_street_name__c,
                      Site_suburb = site_town_suburb__c,
                      site_pcode = site_postcode__c,
                      Municipal.Name = Site_Municipality,
                      Allotment_Area = Allotment_Area__c,
                      Builder_suburb = Builder_Town_Suburb__c,
                      Builder_state = Builder_State__c,
                      Builder_pcode = Builder_Postcode__c,
                      Material_Code_Floor = Floor_Material__c,
                      Material_Code_Frame = Frame_Material__c,
                      Material_Code_Roof = Roof_Cladding_Material__c,
                      Material_Code_Walls = External_Wall_Material__c,
                      dwellings_before_work = Number_of_Existing_Dwellings__c,
                      dwellings_after_work = Number_of_New_Dwellings__c,
                      Number_of_storeys = Number_of_Storeys__c,
                      number_demolished = Number_of_Dwellings_Demolished__c,
                      Floor_area = Total_Floor_Area__c,
                      Permit_app_date = Building_Permit_Application_Date__c,
                      Calculated_levy_BACV = DBDRV.Levy,
                      solar_hot_water = Solar_Hot_Water_Indicator__c,
                      rainwater_tank = Rainwater_Tank_Indicator__c,
                      est_cost_project = Total_Estimated_Cost_of_Works__c,
                      BASIS_Zone = BASIS_Building_Use,
                      BASIS_BCA = BASIS_.BCA,
                      BASIS_OwnershipSector = BASIS_Ownership_Sector,
                      BASIS_OwnerBuilder = BASIS_Owner_Builder) %>%
               mutate(Calculated_Levy_amount = NA,
                      Multiple_Dwellings = NA,
                      cost_of_works_domestic = NA,
                      BACV_applicable_flag = NA),
             VBA_2021 %>%
               rename(Reported_Levy_amount = Original_Levy_Paid__c,
                      Site_street = site_street_name__c,
                      Site_suburb = site_town_suburb__c,
                      site_pcode = site_postcode__c,
                      Municipal.Name = Site_Municipality,
                      Allotment_Area = Allotment_Area__c,
                      Builder_suburb = Builder_Town_Suburb__c,
                      Builder_state = Builder_State__c,
                      Builder_pcode = Builder_Postcode__c,
                      Material_Code_Floor = Floor_Material__c,
                      Material_Code_Frame = Frame_Material__c,
                      Material_Code_Roof = Roof_Cladding_Material__c,
                      Material_Code_Walls = External_Wall_Material__c,
                      dwellings_before_work = Number_of_Existing_Dwellings__c,
                      dwellings_after_work = Number_of_New_Dwellings__c,
                      Number_of_storeys = Number_of_Storeys__c,
                      number_demolished = Number_of_Dwellings_Demolished__c,
                      Floor_area = Total_Floor_Area__c,
                      Permit_app_date = Building_Permit_Application_Date__c,
                      Calculated_levy_BACV = DBDRV.Levy,
                      solar_hot_water = Solar_Hot_Water_Indicator__c,
                      rainwater_tank = Rainwater_Tank_Indicator__c,
                      est_cost_project = Total_Estimated_Cost_of_Works__c,
                      BASIS_Zone = BASIS_Building_Use,
                      BASIS_OwnershipSector = BASIS_Ownership_Sector,
                      BASIS_OwnerBuilder = BASIS_Owner_Builder) %>%
               mutate(Calculated_Levy_amount = NA,
                      Multiple_Dwellings = NA,
                      cost_of_works_domestic = NA,
                      BACV_applicable_flag = NA),
             VBA_2022 %>%
               rename(Reported_Levy_amount = Original_Levy_Paid__c,
                      Site_street = site_street_name__c,
                      Site_suburb = site_town_suburb__c,
                      site_pcode = site_postcode__c,
                      Municipal.Name = Site_Municipality,
                      Allotment_Area = Allotment_Area__c,
                      Builder_suburb = Builder_Town_Suburb__c,
                      Builder_state = Builder_State__c,
                      Builder_pcode = Builder_Postcode__c,
                      Material_Code_Floor = Floor_Material__c,
                      Material_Code_Frame = Frame_Material__c,
                      Material_Code_Roof = Roof_Cladding_Material__c,
                      Material_Code_Walls = External_Wall_Material__c,
                      dwellings_before_work = Number_of_Existing_Dwellings__c,
                      dwellings_after_work = Number_of_New_Dwellings__c,
                      Number_of_storeys = Number_of_Storeys__c,
                      number_demolished = Number_of_Dwellings_Demolished__c,
                      Floor_area = Total_Floor_Area__c,
                      Permit_app_date = Building_Permit_Application_Date__c,
                      Calculated_levy_BACV = DBDRV.Levy,
                      solar_hot_water = Solar_Hot_Water_Indicator__c,
                      rainwater_tank = Rainwater_Tank_Indicator__c,
                      est_cost_project = Total_Estimated_Cost_of_Works__c,
                      BASIS_Zone = BASIS_Building_Use,
                      BASIS_OwnershipSector = BASIS_Ownership_Sector,
                      BASIS_OwnerBuilder = BASIS_Owner_Builder) %>%
               mutate(Calculated_Levy_amount = NA,
                      Multiple_Dwellings = NA,
                      cost_of_works_domestic = NA,
                      BACV_applicable_flag = NA)) 



# 2 Various checks
# -----------------------------------------------------------------------------
# 2.1 check status of LGAs covered by "Metropolitan"/"Rural" in 'Regional' field
# -------------------------------------
metrop <- VBA %>%
  filter(Region == "Metropolitan")
unique(metrop$Municipal.Name) %>% sort(.)

# [1] "1+X2"                 "Banyule"              "Bayside"              "Boroondara"           "Brimbank"             "Cardinia"            
# [7] "Casey"                "Darebin"              "Frankston"            "Glen Eira"            "Greater Dandenong"    "Hobsons Bay"         
# [13] "Hume"                 "Kingston"             "Knox"                 "Manningham"           "Maribyrnong"          "Maroondah"           
# [19] "Melbourne"            "Melton"               "Mildura"              "Monash"               "Moonee Valley"        "Moreland"            
# [25] "Mornington"           "Mornington Peninsula" "Nillumbik"            "Port Philip"          "Port Phillip"         "Stonnington"         
# [31] "Whitehorse"           "Whittlesea"           "Wyndham"              "Yarra"                "Yarra Ranges"  

# # so this is standard 31 Melbourne LGAs, not including extras with part in Melb GCCSA (eg Gisborne)
# but alsoalso include "1+X2", "Mildura", "Mornington" [typo for "Mornington Peninsula"], "Port Philip [typo for "Port Phillip"]
# checked "1+X2": project actually in  "Melbourne" (but it's an industrial development anyway))
# checked "Mildura": project actually in "Melbourne" (but it's a commercial development anyway)

rural <- VBA %>%
  filter(Region == "Rural")
unique(rural$Municipal.Name) %>% sort(.)
# [1] "Alpine"                      "Ararat"                      "Ballarat"                    "Bass Coast"                 
# [5] "Baw Baw"                     "Benalla"                     "Buloke"                      "Campaspe"                   
# [9] "Central Goldfield"           "Central Goldfields"          "City of Greater Geelong"     "Colac-Otway"                
# [13] "Colac Otway"                 "Corangamite"                 "East Gippsland"              "Falls Creek Alpine Resort"  
# [17] "Gannawarra"                  "Glenelg"                     "Golden Plains"               "Greater Bendigo"            
# [21] "Greater Geelong"             "Greater Shepparton"          "Hepburn"                     "Hindmarsh"                  
# [25] "Horsham"                     "Indigo"                      "La Trobe"                    "Lake Mountain Alpine Resort"
# [29] "Latrobe"                     "Loddon"                      "Macedon Ranges"              "Mansfield"                  
# [33] "Mildura"                     "Mitchell"                    "Moira"                       "Moorabool"                  
# [37] "Mount alexander"             "Mount Alexander"             "Moyne"                       "Mt Baw Baw Alpine Resort"   
# [41] "Mt Buller Alpine Resort"     "Mt Hotham Alpine Resort"     "Mt Sterling Alpine Resort"   "Mt Stirling Alpine Resort"  
# [45] "Murrindindi"                 "Northern Grampians"          "Pyrenees"                    "Queenscliff"                
# [49] "Queenscliff (B)"             "Queenscliffe"                "South Gippsland"             "Southern Grampians"         
# [53] "Strathbogie"                 "Surf Coast"                  "Swan Hill"                   "Towong"                     
# [57] "Wangaratta"                  "Warnambool"                  "Warrnambool"                 "Wellington"                 
# [61] "West Wimmera"                "Wodonga"                     "Yarriambiack"            

# these are all rural, and don't include any of the 31 metrop.

# so 'Metropolitan' is all 31 Melbourne LGAs; and none of them are in 'Rural'; 
# therefore 'Metropolitan' includes even the outer parts of (say) Yarra Ranges that 
# are not in Melb GCCSA, and doesn't include the GCCSA parts of (say) Gisborne

# decide to use 'Metropolitan', even though it does not exactly match Melb GCCSA


# 2.2 check status of development type
# -------------------------------------
unique(VBA$BASIS_Zone)
# [1] "Domestic"            "Public Buildings"    "Retail"              "Commercial"          "Hospital/Healthcare"
# [6] "Residential"         "Industrial"          "Public Building"     "public buildings"    "industrial"         
# [11] "public Buildings"    "commercial"          "domestic"

# will need Domestic, domestic and Residential

# 2.3 check how many developments lack allotment size or final dwelling number
# -------------------------------------
# note that this is before excluding multiple stages
test <- VBA %>%
  # filter to metropolitan projects
  filter(Region == "Metropolitan") %>%
  # filter to domestic/residential ### to check statuses for some other years
  filter(BASIS_Zone %in% c("Domestic", "domestic", "Residential")) %>%
  # filter to new developments only - codes are in metadata
  # (note that this excludes eg re-erection, extension, alteration)
  filter(BASIS_NOW == 1) # 479888 observations

# how many don't have allotment size
test2 <- test %>% 
  group_by(Allotment_Area) %>%
  summarise(n = n())
test2
# 139295 out of 479888 entries don't have an allotment area

# how many dwellings for those that lack allotment size
test3 <-test %>%
  filter(Allotment_Area == 0) %>%
  group_by(dwellings_after_work) %>%
  summarise(n = n())
test3
#   dwellings_after_work     n
# <dbl> <int>
# 1                    0 69652
# 2                    1 59849
# 3                    2  4731
# 4                    3  1768
# 5                    4   786
# 6                    5   318
# ...
# 145                555     6
# 146                612     1
# 147                700     5
# 148               1000     1
# 149               4954     1
# 150                 NA    45  

# So there are a lot - but most are also missing dwelling numbers, or 1 dwelling

# how many are missing dwelling numbers
test4 <- test %>%
  group_by(dwellings_after_work) %>%
  summarise(n = n())
test4
# 156943 0s plus 2383 NAs out of 479888 are missing dwelling numbers 
# (but, as noted above, there are 69652 0s that are missing both)

test5 <- test %>%
  filter(Allotment_Area == 0 | is.na(Allotment_Area) | 
           dwellings_after_work == 0 | is.na(dwellings_after_work))
# 229276 out of 479888 (48%) are missing one or the other or both


# 3  Compile metropolitan residential projects 
# -----------------------------------------------------------------------------
# 3.1 Filter to relevant projects
# -------------------------------------
residentialProjects <- VBA %>%
  # filter to metropolitan projects
  filter(Region == "Metropolitan") %>%
  
  # filter to domestic/residential
  filter(BASIS_Zone %in% c("Domestic", "domestic", "Residential")) %>%
  
  # filter to new developments only - codes are in metadata
  # (note that this excludes eg re-erection, extension, alteration)
  filter(BASIS_NOW == 1) %>% 
  
  # convert missing allotment area or final dwelling no to numeric
  # (this will coerce non-numeric, eg NULL, to NA)
  mutate(Allotment_Area = as.numeric(Allotment_Area),
         dwellings_after_work = as.numeric(dwellings_after_work)) %>%
  # filter to > 0 (this will exclude both 0s and NAs)
  filter(Allotment_Area > 0 & dwellings_after_work > 0) %>%
 
  ### filter out multiple permit numbers
  # approach is to use a 'sorting' field comprising address plus allotment area
  # not necessarily perfect, because allotment area could be recalculated
  # but other fields, such as floor area or storeys or dwellings, are more likely to 
  # change during the permitting process
  
  # add a sorting field
  mutate(sortfield = paste(Site_street, Site_suburb, Allotment_Area)) %>%
  # group by the sorting field, and find the row with the max permit number
  group_by(sortfield) %>%
  slice_max(permit_stage_number) %>%
  
  # calculate densities in dwellings/ha
  mutate(density = dwellings_after_work / Allotment_Area * 10000)

# examine densities
ggplot(data = residentialProjects, aes(x = density)) + 
  geom_histogram(binwidth = 10) + 
  scale_x_continuous(limits = c(0, 200))


# 3.2 Calculate dwelling numbers by density groups
# -----------------------------------------------------------------------------
residentialDensityGroupNumbers <- residentialProjects %>%
  # add column for density group
  mutate(densitygroup = case_when(density <= 30 ~ "dg1 <=30",
                                  density > 30 & density <= 60 ~ "dg2 >30 to <=60",
                                  density > 60 & density <= 80 ~ "dg3 >60 to <=80",
                                  density > 60 & density <= 100 ~ "dg4 >80 to <=100",
                                  density > 100 ~ "dg5 >100")) %>%
  group_by(densitygroup) %>%
  summarise(dwell_u10 = sum(dwellings_after_work[dwellings_after_work < 10]),
            dwell_10ormore = sum(dwellings_after_work[dwellings_after_work >= 10]),
            dwell_total = sum(dwellings_after_work),
            u10pct = dwell_u10 / dwell_total * 100)

write.csv(residentialDensityGroupNumbers, "../Tables/residentialDensityGroupNumbers.csv", row.names = FALSE)

# check percentage where density 100 dwell/ha or more (rather than more than 100)
residentialDensityGroupNumbers2 <- residentialProjects %>%
  # add column for density group
  mutate(densitygroup = case_when(density < 100 ~ "dgA <100",
                                  density >= 100 ~ "dgB >100")) %>%
  group_by(densitygroup) %>%
  summarise(dwell_u10 = sum(dwellings_after_work[dwellings_after_work < 10]),
            dwell_10ormore = sum(dwellings_after_work[dwellings_after_work >= 10]),
            dwell_total = sum(dwellings_after_work),
            u10pct = dwell_u10 / dwell_total * 100)

write.csv(residentialDensityGroupNumbers, "../Tables/residentialDensityGroupNumbers2.csv", row.names = FALSE)
## <10 pct for projects >= 100 dwell/ha: 6.23%

