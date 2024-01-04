# train cleaning checks

library(dplyr)
library(readxl)
library(tibble)  # column_to_rownames
library(tidyr)

source("./functions/station line details.R")  # in 1.1, for stationLineDetails() & see note at end of 1.3
source("./functions/stopList.R")  # in section 3, for reading station names from completed.projects


# 1. Investigate match between DoT station and line data ---- 
# -----------------------------------------------------------------------------#
# DoT's station volume figures only apply from 2016.  Aim is to investigate
# how well the line figures can be used to predict the station figures

## 1.1 Read in DoT data, and allocate lines ----
## -------------------------------------#
# Service volumes supplied by DoT, 23 Jan 2023, by station and line, monthly
line.vol <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                   sheet = "MetroMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Train)`) #%>%
  # # add date column (single so rows can be selected by > or <)
  # mutate(date = (as.Date(paste0("01", Month, Year), format = "%d%B%Y"))) #<<< MAY NOT NEED

station.vol <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx",
                                   sheet = "MetroByStationMonthly") %>%
  # station names are in first column - set as row names, then transpose
  # so they become column names
  column_to_rownames("...1") %>%
  t() %>%
  
  # convert to dataframe, with no rownames
  as.data.frame(., row.names = FALSE) %>%
  
  # remove blank month rows (which are totals) and Grand Total column
  rename(Month = `Row Labels`) %>%
  filter(!is.na(Month)) %>%
  dplyr::select(-`Grand Total`) %>%
  
  # add 'Year' column
  mutate(Year = c(rep("2016", 12), rep("2017", 12), rep("2018", 12),
                  rep("2019", 12), rep("2020", 12), rep("2021", 12),
                  rep("2022", 12)), .before = Month) %>%
  
  # convert all columns (except 'Month') to numeric
  mutate(across(.cols = -Month, ~ as.numeric(.))) %>%
  
  # rename Flinders St & Southern Cross, and combine the two Flinders St columns
  rename(`Flinders Street` = `Flinders Street Station`,
         `Southern Cross` = `Southern Cross Station`) %>%
  rowwise() %>%
  mutate(`Flinders Street` = sum(`Flinders Street`, `Flinders St City Circle`,
                                 na.rm = TRUE)) %>%
  dplyr::select(-`Flinders St City Circle`) %>%
  
  # confine to analysis period (to Jun 2022)
  mutate(date = (as.Date(paste0("01", Month, Year), format = "%d%B%Y"))) %>%
  filter(date < as.Date("2022-07-01")) %>%
  dplyr::select(-date)

  
# make table of relevant lines for each station
station.line.lookup <- stationLineDetails(station.vol)


# check that there are no typos in route names
route.names <- c()
for (i in 1:length(station.line.lookup$Routes)) {
  route.names <- c(route.names,
                     unlist(strsplit(station.line.lookup$Routes[i], ", ")))
}
route.names <- unique(route.names)
route.names %>% sort()  # list of 16 lines




## 1.2 Build table of station and line details by station and month ----
## -------------------------------------#
# load getLineVol, which finds  total line.vol for a given month, year and group of routes
# [could go into separate function file if ends up being used in main script]
source("./functions/getTrain.R")

# table of station and line volumes by year, month and station
station.data <- station.vol %>%
  # convert to table of Year, Month, Station, stn.vol
  pivot_longer(cols = -c(Year, Month),
               names_to = "Station",
               values_to = "stn.vol") %>%
  
  # join lines
  left_join(station.line.lookup,
            by = "Station") %>%
  
  # add line vol
  rowwise() %>%
  mutate(line.vol = getLineVol(line.vol, Year, Month, Routes))

# There are several NAs in the station.vols.  Some are stations not yet built - 
# Mernda line extension opened Aug 2018, Southland. opened Nov 2017.
# Others look like line closures (sometimes for LXR), eg Stony Point line Jul 
# 2016; several Frankston line stations in mid 2020; Mitcham, Heatherdale & 
# Nunawading in Jan 2017.


# These cast doubt on using the station volumes at all - maybe line volumes are
# better at smoothing over these closures (which would have been replaced by buses?)

# Line vol doesn't have NAs, but it sometimes has zeros for Stony Point line
# where closed - though sometimes (Jun 2020, Oct 2021) this correspinds with
# the stations actually reporting volumes.


# clean up data
station.data.cleaned <- station.data %>%
  # omit NAs and zeros
  filter(!is.na(stn.vol) & !is.na(line.vol) &
           stn.vol > 0 & line.vol > 0) %>%
  
  # calculate stn.vol as percentage of line.vol
  mutate(line.vol.pct = stn.vol / line.vol * 100)

# inspecting this, many cluster around 100%, as expected; but there are some
#  odd high and low results that often seem likely to be aligned to line closures

# summarise and find statistics for stn.vol/line.vol percentages
# Note - rounded for ease of review; may not want to round if used in final script
station.data.summaries <- station.data.cleaned %>%
  group_by(Station, Routes) %>%
  summarise(mean.line.vol = round(mean(line.vol)),
            mean.stn.vol = round(mean(stn.vol)),
            mean.pct = round(mean(line.vol.pct), 1),
            min.pct = round(min(line.vol.pct), 1),
            max.pct = round(max(line.vol.pct), 1),
            sd.pct = round(sd(line.vol.pct), 1),
            median.pct = round(median(line.vol.pct), 1)) %>%
  arrange(Routes)


## 1.3 Write output for review ----
## -------------------------------------#
write.csv(station.data.summaries, 
          "../Tables/station and line volumes 2016-22 for review.csv",
          row.names = FALSE)

# Note that the above was run and saved in March 2023 on the basis of an older
# version of 'station line details.R', which allocated East Richmond solely to
# Glen Waverley, and South Kensington solely to Werribee/Williamstown
# Subsequently (after looking at the results) the file was changed to allocate
# those stations to all lines passing through them (even though the other lines
# rarely stop)



# 2. Investigate Cth Games period volumes ---- 
# -----------------------------------------------------------------------------#
# DoT's train volumes are incomplete for March 2006.  DoT's comments:
# "For Metro Trains - No data from 15th Mar 2006 till 2nd Apr 2006 due to commonwealth games"
# (Note that same also applies to Yarra Trams, and this code is adapted from  
#  section 1 'tram cleaning checks.R', however the dates are different.)

# In March, that is a 17-day period, leaving 14 'normal' days

# In April, it is 2 missing days.

# Accordingly, for March, consider whether daily volumes from Feb can be applied.  
# Volumes from April may not be suitable, given that it includes Easter/school holidays
# (Easter day was Sun 15 Apr 2006)

# Feb/March volume comparisons from 2007 may also be relevant, noting Easter day
# was Sun 8 Apr 2007 (but possibly not 2005 - Easter day Sun 27 Mar 2005)

# For April, where the missing period is shorter, consider whether volumes from 
# April can be multiplied by 28/30 to produce reasonable volumes, by comparison
# with 2007 April volumes (noting that April contained Easter in both years).
# However, 1 & 2 April were Sat & Sun in 2006 and Sun & Mon in 2007, so 
# comparisons would not be exact, even if timetables had not changed.  Compare
# also to May 2006/2007 differences


## 2.1 Load DoT tram service volume figures ----
## ------------------------------------#
# Service volumes supplied by DoT, 23 Jan 2023
train.voltable <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                   sheet = "MetroMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Train)`)


## 2.2 March: daily volumes for Feb/Mar 2006 & 2007 ----
## ------------------------------------#
daily.volumes.mar <- train.voltable %>%
  # get the volumes for Feb/Mar 2006/07 by route
  filter(Year %in% c(2006, 2007) & Month %in% c("February", "March")) %>%
  pivot_wider(names_from = c(Month, Year),
              names_sep = "_",
              values_from = sched.vol) %>%
  # calculate daily volumes
  mutate(Feb_2006_daily_28 = February_2006 / 28,
         Mar_2006_daily_19 = March_2006 / 14,
         Feb_2007_daily_28 = February_2007 / 28,
         Mar_2007_daily_31 = March_2007 / 31) %>%
  # calculate percentage differences
  mutate(diff_feb_mar_2006 = Mar_2006_daily_19 / Feb_2006_daily_28 * 100,
         diff_feb_mar_2007 = Mar_2007_daily_31 / Feb_2007_daily_28 * 100)

mean(daily.volumes.mar$diff_feb_mar_2006, na.rm = TRUE)
sd(daily.volumes.mar$diff_feb_mar_2006, na.rm = TRUE)
mean(daily.volumes.mar$diff_feb_mar_2007, na.rm = TRUE)
sd(daily.volumes.mar$diff_feb_mar_2007, na.rm = TRUE)

# Results:
# In 2007, daily March volumes are within 1% of daily Feb volumes for all 
# lines except Pakenham (98.28%) [mean 99.995%, sd 0.75%].  This suggests that 
# applying Feb daily volumes to March is a reasonable approach.
# 
# The alternative method of dividing March 2006 volumes by 14 (for the 14 normal
# days that should have been captured by DoT figures excluding the 17 Cth Games
# days) in March results in all lines being between 96.03% and 100% of the
# daily Feb volumes.  So using March 2006 volumes divided by 14 would also be
# a workable approach, though not quite as close a fit as the Feb daily volumes.


## 2.3 April: daily volumes for April 2006 & 2007 ----
## ------------------------------------#
daily.volumes.apr <- train.voltable %>%
  # get the volumes for Feb/Mar 2006/07 by route
  filter(Year %in% c(2006, 2007) & Month %in% c("April", "May")) %>%
  pivot_wider(names_from = c(Month, Year),
              names_sep = "_",
              values_from = sched.vol) %>%
  # calculate daily volumes
  mutate(Apr_2006_daily_28 = April_2006 / 28,
         Apr_2007_daily_30 = April_2007 / 30,
         May_2006_daily_31 = May_2006 / 31,
         May_2007_daily_31 = May_2007 / 31) %>%
  # calculate percentage difference
  mutate(diff_apr_2006_07 = Apr_2006_daily_28 / Apr_2007_daily_30 * 100,
         diff_may_2006_07 = May_2006_daily_31 / May_2007_daily_31 * 100)

mean(daily.volumes.apr$diff_apr_2006_07, na.rm = TRUE)
sd(daily.volumes.apr$diff_apr_2006_07, na.rm = TRUE)
mean(daily.volumes.apr$diff_may_2006_07, na.rm = TRUE)
sd(daily.volumes.apr$diff_may_2006_07, na.rm = TRUE)

# Results:
# April 2006 daily volumes are between 98.86% (Sunbury) and 103.53% (Belgrave)
# of April 2007 daily volumes, apart from Pakenham which is 96.45% [mean 100.13%,
# sd 1.54%].   This suggests calculating Apr volumes as 30/28 * stated volumes 
# is a reasonable enough approach (in the absence of anything better), as long 
# as there were not in fact major changes in service volumes between 2006 and 2007.

# May 2007 daily volumes are between 94.98% (Alamein) and 100% (Williamstown), 
# apart from Stony Point (106.23%).  All but Alamein, Stony Point and Pakenham
# are between 98% and 100%.  Pakenham is 95.40%.  This suggests that it is 
# reasonable to assume there were no major changes in service volumes between
# 2006 and 2007, and also that there was in fact an increase in Pakenham volumes.
# Stony Point can be discounted as its low volumes would be heavily affected by
# number of weekend days in the month (and there were more in Apr 2006 than Apr
# 2007) – though in any case both months included holidays, so are atypical.


## 2.4 Join table and save results ----
## ------------------------------------#
daily.volumes <- daily.volumes.mar %>%
  left_join(daily.volumes.apr, by = "Route")

write.csv(daily.volumes, 
          "../Tables/train daily volumes Feb_Mar_Apr 2006_07 for Cth Games.csv",
          row.names = FALSE)


# 3. Check that GTFS and DOT station names are the same ---- 
# -----------------------------------------------------------------------------#
# The station names found for the apartments in section 2.4 of 'apartments.R'
# need to match the station names used by DoT in the station volumes

# stations for completed projects
completed.projects <- st_read("../GIS/completed_projects_with_stops.sqlite")
GTFS.stations <- stopList(completed.projects$rail_stn)

# DoT stations
DOT.stations <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx",
                                 sheet = "MetroByStationMonthly") %>%
  .[-1, 1] %>%
  unlist() %>% unname()


# GTFS stations missing from DoT
GTFS.stations[!GTFS.stations %in% DOT.stations]
# [1] "Bunyip"                "Flemington Racecourse" "Flinders Street"       "Geelong"              
# [5] "Jolimont-MCG"          "Showgrounds"           "Southern Cross" 

# Flinders Street, Southern Cross and Jolimont require manual alterations
# (to one set of data or the other).  Others don't matter.


# DoT stations not in GTFS
DOT.stations[!DOT.stations %in% GTFS.stations]
# [1] "Albion"                  "Belgrave"                "Berwick"                
# [4] "Coolaroo"                "Cranbourne"              "Crib Point"             
# [7] "Diggers Rest"            "Epping"                  "Flinders St City Circle"
# [10] "Flinders Street Station" "Hallam"                  "Hawkstowe"              
# [13] "Hoppers Crossing"        "Hurstbridge"             "Jolimont"               
# [16] "Kananook"                "Laverton"                "Merinda Park"           
# [19] "Mernda"                  "Mount Waverley"          "Narre Warren"           
# [22] "Officer"                 "Seaford"                 "Seaholme"               
# [25] "Southern Cross Station"  "Stony Point"             "Tecoma"                 
# [28] "Thomastown"              "Upfield"                 "Watsonia"               
# [31] "Wattle Glen"             "Westona"                 "Grand Total" 

# Flinders Street, Southern Cross and Jolimont require manual alterations
# (to one set of data or the other).  Others are likely to be stations with
# no apartments, so explicable, apart from Epping.

# Resolution: 
# - Epping manually added to the relevant project (R09066) in section 2.4.4 of
#   'apartments.R'
# - GTFS 'Jolimont-MCG' changed to 'Jolimont' in section 2.4.4 of 'apartments.R'
# - DoT 'Flinders St City Circle', 'Flinders Street Station' and 'Southern Cross
#   Station' changed to 'Flinders Street' and 'Southern Cross' in section 4.2
#   of 'trains.R'
