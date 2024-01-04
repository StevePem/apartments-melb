#==============================================================================#
#   Investigation of train data
#
#   Steve Pemberton, [November 2022]
#
# 
#   Organised as follows [review and expand as needed]
#   1  Review coverage of 2004 and 2008 data; calculate percentages of 3
#      car trains for 2004 and 2008 by timeband
#   2  Find weights for timebands (am peak, interpeak, pm peak, evening)
#      based on current timetable
#   3  Calculate capacity factors by year - weighted for 2004-2008; interpolated
#      or extrapolated for 2005-07 and 2009; assumed to be no 3 cars from 2010;
#      carriage capacity increase from 2017; HCMT in 2021
#   4  Calculate 'station percentage' for each station, which is the percentage
#      of trains on the lines serving the station that stop there
#
#==============================================================================#


# set up environment and basic data
library(dplyr)
library(readxl)
library(tidyr)  # for pivoting
library(tibble)  # column_to_rownames
library(tidytransit)  # for reading GTFS
library(stringr)

# load data supplied by Chris Loader, DOT, 30 Nov 2022
summary <- read_xlsx("../Data/Summary Load Standard Survey for 3 & 6 Car Loading.xlsx",
                     sheet = "Data")

timebands <- read_xlsx("../Data/Summary Load Standard Survey for 3 & 6 Car Loading.xlsx",
                       sheet = "Timeband Definitions")

data2004 <- read_xls("../Data/2004 Cleaned DOC 08 454299  2004 May Load Survey May 2004 Survey Data.xls",
                     sheet = "Sheet1")

data2008 <- read_xls("../Data/2008 Cleaned DOC 08 870089  2008 May Connex Load Standard Survey - results by service.xls",
                     sheet = "Sheet1") %>%
  # set row names, which were in row 2 of original sheet (and have now become row 1 of the dataframe)
  setNames(., as.character(.[1,])) %>%
  # omit row 1
  .[-1, ]

# destinations based on codes, ordered for display in lines, final terminus first 
# see https://vicsig.net/index.php?page=infrastructure&section=codes&sort=n for station codes
dest_table <- data.frame(rbind(c(1, "EPP", "Epping"),
                               c(2, "HBE", "Hurstbridge"),
                               c(3, "ELT", "Eltham"),
                               c(4, "GRN", "Greensborough"),
                               c(5, "MCD", "Macleod"),
                               c(6, "GWY", "Glen Waverley"),
                               c(7, "ALM", "Alamein"),
                               c(8, "RIV", "Riversdale"),
                               c(9, "BEG", "Belgrave"),
                               c(10, "UFG", "Upper Ferntree Gully"),
                               c(11, "LIL", "Lilydale"),
                               c(12, "MLK", "Mooroolbark"),
                               c(13, "RWD", "Ringwood"),
                               c(14, "BBN", "Blackburn"),
                               c(15, "PKM", "Pakenham"),
                               c(16, "CBE", "Cranbourne"),
                               c(17, "DNG", "Dandenong"),
                               c(18, "WTL", "Westall"),
                               c(19, "OAK", "Oakleigh"),
                               c(20, "FKN", "Frankston"),
                               c(21, "CAR", "Carrum"),
                               c(22, "MOR", "Mordialloc"),
                               c(23, "CTM", "Cheltenham"),
                               c(24, "MRN", "Moorabbin"),
                               c(25, "SHM", "Sandringham"),
                               c(26, "UFD", "Upfield"),
                               c(27, "CGB", "Craigieburn"),
                               c(28, "BMS", "Broadmeadows"),
                               c(29, "SUN", "Sunbury"),
                               c(30, "SDM", "Watergardens"),
                               c(31, "SAB", "St Albans"),
                               c(32, "WER", "Werribee"),
                               c(33, "WIL", "Williamstown")))

# zip folder for PTV GTFS data (for timeband weights)
# from https://transitfeeds.com/p/ptv/497 , downloaded 3 Oct 2022
GTFS <- "../Data/gtfs_20220929.zip"




# 1. Prevalence of 3-car trains ----
# -----------------------------------------------------------------------------#
## 1.1 check routes, observation points etc (investigation) ----
## -------------------------------------#
### data2004 ----
# unique routes in data2004
routes2004 <- data2004 %>% 
  .$OPR_Route_Code %>%
  unique(.)
routes2004
# [1] "FSSEPP" "FSSGRN" "FSSHBE" "FSSELT" "FSSMCD" "FSSRIV" "FSSALM" "FSSGWY" "FSSBEG" "FSSBBN" "FSSRWD" "FSSUFG" "FSSLIL"
# [14] "FSSMLK" "FSSPKM" "FSSDNG" "FSSWTL" "FSSCBE" "FSSFKN" "FSSCAR" "FSSMOR" "FSSMRN" "FSSUFD" "FSSBMS" "FSSWIL" "FSSSDM"
# [27] "FSSWER" "FSSSHM"

# and origin-dest code
OD2004 <- data2004 %>%
  .$Orig_Dest %>%
  unique(.)
OD2004
# [1] "EPP" "GRN" "HBE" "ELT" "MCD" "RIV" "ALM" "GWY" "BEG" "BBN" "RWD" "UFG" "LIL" "MLK" "PKM" "DNG" "WTL" "CBE" "FKN" "CAR" "MOR"
# [22] "MRN" "UFD" "BMS" "WIL" "SDM" "WER" "SHM"

# do orig and dest always form part of the route code?
test <- data2004 %>%
  filter(!((Orig == substr(OPR_Route_Code, 1, 3) & Dest == substr(OPR_Route_Code, 4, 6)) |
           (Orig == substr(OPR_Route_Code, 4, 6) & Dest == substr(OPR_Route_Code, 1, 3)))) 
# zero obs - so yes, they always do

# unique observation points, by route
obs2004 <- data2004 %>%
  .$Stn %>%
  unique(.)
obs2004
# [1] "Jolimont"         "Clifton Hill"     "West Richmond"    "Richmond"         "East Richmond"    "Burnley"         
# [7] "Darling"          "Glenferrie"       "Camberwell"       "Box Hill"         "South Yarra"      "North Melbourne" 
# [13] "Macaulay"         "Kensington"       "South Kensington" "Footscray"       

# notes - 
# 1 There are several cases where a single train is checked at multiple points
# 2 Observations are for 4, 5 and 6 May (see Date).  
# 3 There are cases where a train has
#   different no. of carriages on different days, see eg TD == 1850 (Eltham).

# Approach - aggregate all the results for each line, within timebands, noting
# that the timebands are tested at intermediate points (not the ends) and some
# trains will be duplicated

# add timeband (note that 'time' will have a date as well as time, which will be today's date when
# run - but the mutation for 'timeband' compares to the same strptime format, which
# will have the same date, so the inclusion of the date is irrelevant)
data2004 <- data2004 %>%
  mutate(time = strptime(`Sched Time`, "%I:%M:%S %p")) %>%
  mutate(timeband = case_when(
    time <= strptime("09:00:00 AM", "%I:%M:%S %p") ~ "AMP",
    time <  strptime("03:30:00 PM", "%I:%M:%S %p") ~ "INP",
    time <= strptime("06:30:00 PM", "%I:%M:%S %p") ~ "PMP",
    TRUE                                           ~ "POP"
  ))

min(data2004$time) # "2022-12-05 07:00:00 AEDT" [nb date doesn't matter]
max(data2004$time)  # "2022-12-05 19:58:00 AEDT"
# ie 7am to 8pm

# check for missing details on cars
obs2004 <- nrow(data2004)
obs2004  # 1920

nuls2004 <- nrow(data2004 %>% filter(is.na(`No of Cars`)))
nuls2004 # 86
# So 86 of 1920 observations (4.5%) are missing

### data2008 ----
# repeating above checks for 2008
# does not show routes or direction, just 'Orig.Dest.'
routes2008 <- data2008 %>% 
  .$`Orig./Dest.` %>%
  unique(.)
routes2008
# [1] "EPP" "GRN" "HBE" "ELT" "MCD" "GWY" "RIV" "ALM" "BEG" "BBN" "RWD" "UFG" "LIL" "MLK" "DNG" "PKM" "OAK" "WTL" "CBE"
# [20] "FKN" "CAR" "CTM" "MRN" "CGB" "UFD" "BMS" "WIL" "WER" "SDM" "SUN" "SAB" "SHM"

# unique observation points are not specified - can't really tell whether 
# a train is sampled multiple times (but probably is, given ~800 obs of 4 days each, whereas 
# 2004 had 1920 obs for 3 days, ie about 650 obs per day)

# note - here are cases where a train has different no. of carriages on different days

# Approach - aggregate all the results for each line, within timebands, noting
# that the timebands are tested at intermediate points (not the ends) and some
# trains will be duplicated

# add timeband (note that 'time' will have a date as well as time, which will be today's date when
# run - but the mutation for 'timeband' compares to the same strptime format, which
# will have the same date, so the inclusion of the date is irrelevant)
data2008 <- data2008 %>%
  # fix time, which read_xls has converted to a decimal between 0 and 1
  # convert to seconds: decimal time * 24 hours * 3600 seconds in hour
  mutate(seconds = as.numeric(`Sched. Time at Cordon`) * 24 * 3600) %>%
  # convert seconds to h:m (note - all seconds are 0 in original)
  mutate(time_as_char = paste0(floor(seconds / 3600), ":",  # hours - seconds divided by 3600, rounded down
                               round((seconds / 60) %% 60))) %>%  # mins - seconds divided by 60, remainder from 60, rounded (needed for floating point)
  mutate(time = strptime(time_as_char, "%H:%M")) %>%
  mutate(timeband = case_when(
    time <= strptime("09:00:00 AM", "%I:%M:%S %p") ~ "AMP",
    time <  strptime("03:30:00 PM", "%I:%M:%S %p") ~ "INP",
    time <= strptime("06:30:00 PM", "%I:%M:%S %p") ~ "PMP",
    TRUE                                           ~ "POP"
  ))

min(data2008$time) # "2022-12-05 07:00:00 AEDT" [nb date doesn't matter]
max(data2008$time)  # "2022-12-05 21:48:00 AEDT"
# ie 7am to 10pm

# check for missing details on cars
obs2008 <- nrow(data2008) * 4 # (because 4 obs per row)
obs2008  # 3176

nuls2008 <- nrow(data2008 %>% filter(Num_Cars_Day_1 == "0")) +
  nrow(data2008 %>% filter(Num_Cars_Day_2 == "0")) +
  nrow(data2008 %>% filter(Num_Cars_Day_3 == "0")) +
  nrow(data2008 %>% filter(Num_Cars_Day_4 == "0"))
nuls2008 # 52
# So 52 of 3176 observations (1.6%) are missing



## 1.2 make table of 3 car percentages (outputs) ----
## -------------------------------------#
### data2004 ----

cars2004 <- data2004 %>%
  
  # add timebands (same as in section 1.1)
  mutate(time = strptime(`Sched Time`, "%I:%M:%S %p")) %>%
  mutate(timeband = case_when(
    time <= strptime("09:00:00 AM", "%I:%M:%S %p") ~ "AMP",
    time <  strptime("03:30:00 PM", "%I:%M:%S %p") ~ "INP",
    time <= strptime("06:30:00 PM", "%I:%M:%S %p") ~ "PMP",
    TRUE                                           ~ "POP"
   )) %>%
  
  # filter out na.s
  filter(!is.na(`No of Cars`)) %>%
  
  # group by endpoint, timeband and no of cars, and count
  rename(endpoint_code = Orig_Dest) %>%
  group_by(endpoint_code, timeband, `No of Cars`) %>%
  summarise(n = n()) %>%
  
  # convert to one row per endpoint/timeband
  pivot_wider(names_from = `No of Cars`,
              values_from = n,
              values_fill = 0) %>%
  
  # calculate total obs and %
  mutate(obs = `3` + `6`,
         pct_3_car = `3` / obs * 100) %>%
  
  # convert to one row per endpoint
  dplyr::select(endpoint_code, timeband, obs, pct_3_car) %>%
  pivot_wider(names_from = timeband,
              values_from = c(obs, pct_3_car),
              values_fill = 0) %>%
  
  # join station names and arrange
  left_join(., dest_table, by = c("endpoint_code" = "X2")) %>%
  arrange(as.numeric(X1)) %>%
  dplyr::select(orig_dest = X3, code = endpoint_code,
                AMP_obs = obs_AMP, AMP_3_car_pct = pct_3_car_AMP,
                INP_obs = obs_INP, INP_3_car_pct = pct_3_car_INP,
                PMP_obs = obs_PMP, PMP_3_car_pct = pct_3_car_PMP,
                POP_obs = obs_POP, POP_3_car_pct = pct_3_car_POP)

# save output
write.csv(cars2004, "../Tables/trains_2004_3_car.csv", row.names = FALSE)

  
### data2008 ----

cars2008 <- data2008 %>%
  
  # add timebands (same as in section 1.1)
  # fix time, which read_xls has converted to a decimal between 0 and 1
  # convert to seconds: decimal time * 24 hours * 3600 seconds in hour
  mutate(seconds = as.numeric(`Sched. Time at Cordon`) * 24 * 3600) %>%
  # convert seconds to h:m (note - all seconds are 0 in original)
  mutate(time_as_char = paste0(floor(seconds / 3600), ":",  # hours - seconds divided by 3600, rounded down
                               round((seconds / 60) %% 60))) %>%  # mins - seconds divided by 60, remainder from 60, rounded (needed for floating point)
  mutate(time = strptime(time_as_char, "%H:%M")) %>%
  mutate(timeband = case_when(
    time <= strptime("09:00:00 AM", "%I:%M:%S %p") ~ "AMP",
    time <  strptime("03:30:00 PM", "%I:%M:%S %p") ~ "INP",
    time <= strptime("06:30:00 PM", "%I:%M:%S %p") ~ "PMP",
    TRUE                                           ~ "POP"
  )) %>%
  
  # make each observation day a separate row
  dplyr::select(endpoint_code = `Orig./Dest.`, 
                Num_Cars_Day_1, Num_Cars_Day_2,
                Num_Cars_Day_3, Num_Cars_Day_4,
                timeband) %>%
  pivot_longer(cols = c("Num_Cars_Day_1", "Num_Cars_Day_2",
                        "Num_Cars_Day_3", "Num_Cars_Day_4"),
                        names_to = "day", values_to = "cars") %>%
  
  # filter out zero.s
  filter(!(cars == "0")) %>%
  
  # group by endpoint, timeband and no of cars, and count
  group_by(endpoint_code, timeband, cars) %>%
  summarise(n = n()) %>%
  
  # convert to one row per endpoint/timeband
  pivot_wider(names_from = cars,
              values_from = n,
              values_fill = 0) %>%
  
  # calculate total obs and %
  mutate(obs = `3` + `6`,
         pct_3_car = `3` / obs * 100) %>%
  
  # convert to one row per endpoint
  dplyr::select(endpoint_code, timeband, obs, pct_3_car) %>%
  pivot_wider(names_from = timeband,
              values_from = c(obs, pct_3_car),
              values_fill = 0) %>%
  
  # join station names and arrange
  left_join(., dest_table, by = c("endpoint_code" = "X2")) %>%
  arrange(as.numeric(X1)) %>%
  dplyr::select(orig_dest = X3, code = endpoint_code,
                AMP_obs = obs_AMP, AMP_3_car_pct = pct_3_car_AMP,
                INP_obs = obs_INP, INP_3_car_pct = pct_3_car_INP,
                PMP_obs = obs_PMP, PMP_3_car_pct = pct_3_car_PMP,
                POP_obs = obs_POP, POP_3_car_pct = pct_3_car_POP)

#save output
write.csv(cars2008, "../Tables/trains_2008_3_car.csv", row.names = FALSE)


# 2. Timeband weights based on current timetable ----
# -----------------------------------------------------------------------------#
# To calculate an overall factor, weights are needed for peak and offpeak timebands
# Based on analysis of services in current timetable

## 2.1 read in GTFS ----
## -------------------------------------#
gtfs <- read_gtfs(GTFS) %>%
  gtfs_as_sf(., crs = 4326)

stop_times <- gtfs$stop_times
trips <- gtfs$trips
calendar <- gtfs$calendar
calendar_dates <- gtfs$calendar_dates

# stops <- gtfs$stops %>%   
#   st_transform(7899)
# shapes <- gtfs$shapes %>%
#   st_transform(7899)
# routes <- gtfs$routes
# agency <- gtfs$agency


## 2.2 select and check date ----
## -------------------------------------#
# To be non-school-holiday Wednesday without disruptions

# Select Wed 23 Nov 2022 and check
# (Note this was in week before State election, when there
# were few transport disruptions)
test_date <- calendar_dates %>%
  filter(date == "2022-11-23")
test_date
# service_id date       exception_type
# <chr>      <date>              <int>
# 4_TL+dw100 2022-11-23              2

# So the only exception is 4_TL+dw100, which is a bus timetable.


## 2.3 find service IDs for selected date ----
## -------------------------------------#
# [see gtfs frequencies and hours.R in BusVic project for more extensive code]

# selected date
DAY <- "wednesday"
DATE <- "20221123"

# create list of services for selected dates
services <- calendar %>%
  as.data.frame(.) %>%
  filter(get(DAY) == "1") %>%
  filter(strptime(start_date, format = "%Y-%m-%d") <= strptime(DATE, format = "%Y%m%d")) %>%
  filter(strptime(end_date, format = "%Y-%m-%d") >= strptime(DATE, format = "%Y%m%d")) %>%
  .$service_id

# update service list for exceptions (but, as per section 2.2, only bus so doesn't matter)
for (i in 1:nrow(calendar_dates)) {
  if (strptime(calendar_dates[i, "date"][[1]], format = "%Y-%m-%d") == strptime(DATE, format = "%Y%m%d")) {
    if (calendar_dates[i, "exception_type"] == "1") {
      services <- c(services, as.character(calendar_dates[i, 1]))
    }
    if (calendar_dates[i, "exception_type"] == "2") {
      services <- services[!services %in% calendar_dates[i, 1]]
    }
  }
}


## 2.3 find train trips for selected date, from services  ----
## -------------------------------------#
# filter services to train services (which start with 2)
train_services <- services[grep("^2", services)]


# find trip_ids in 'trips'
selected_trip_ids <- trips %>%
  filter(service_id %in% train_services) %>%
  .$trip_id

# find selected trip id's in 'stop_times', take first stop, and allocate time code
selected_origins <- stop_times %>%
  
  # selected trip id's
  filter(trip_id %in% selected_trip_ids) %>%
  
  # first stop only
  filter(stop_sequence == 1) %>%
  
  # allocate time code
  # note times after 24:00:00 are 'NA', but they'll end up as 'POP' which is fine
  mutate(time = strptime(departure_time, format = "%H:%M:%S"),
         timeband = case_when(
           time <= strptime("09:00:00 AM", "%I:%M:%S %p") ~ "AMP",
           time <  strptime("03:30:00 PM", "%I:%M:%S %p") ~ "INP",
           time <= strptime("06:30:00 PM", "%I:%M:%S %p") ~ "PMP",
           TRUE                                           ~ "POP"  # will also catch NAs (after 24:00:00) as intended
         ))


## 2.4 calculate weights ----
## -------------------------------------#

weights <- selected_origins %>%
  # number of services per timeband
  group_by(timeband) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  # percentage of services per timeband
  mutate(pct = n / sum(n) * 100)


# write output
write.csv(weights, "../Tables/train timeband weights.csv", row.names = FALSE)


# 3. Capacity factors by lines, weighted for timebands ----
# -----------------------------------------------------------------------------#
## 3.1 read in 2004/2008 details and timeband weights ----
## -------------------------------------#
cars2004 <- read.csv("../Tables/trains_2004_3_car.csv")  # created in section 1.2
cars2008 <- read.csv("../Tables/trains_2008_3_car.csv")  # created in section 1.2

weights <- read.csv("../Tables/train timeband weights.csv") %>% # created in section 2.4
# round to nearest 10
  mutate(weight = round(pct/10) / 10)  # gives 30, 30, 20, 20


## 3.2 combine 2004/2008 results by line, and weight ----
## -------------------------------------#
# function to group by line and weight
mean3car <- function(cars) {
  
  # add field for line
  lines <- cars %>%
    mutate(line = case_when(
      code %in% c("EPP")                                    ~ "Epping",
      code %in% c("HBE", "ELT", "GRN", "MCD")               ~ "Hurstbridge",
      code %in% c("GWY")                                    ~ "Glen Waverley",
      code %in% c("ALM", "RIV")                             ~ "Alamein",
      code %in% c("BEG", "UFG", "LIL", "MLK", "RWD", "BBN") ~ "Belgrave/Lilydale",
      code %in% c("PKM", "CBE", "DNG", "WTL", "OAK")        ~ "Pakenham/Cranbourne",
      code %in% c("FKN", "CAR", "CTM", "MOR", "MRN")        ~ "Frankston",
      code %in% c("SHM")                                    ~ "Sandringham",
      code %in% c("UFD")                                    ~ "Upfield",
      code %in% c("CGB", "BMS")                             ~ "Craigieburn",
      code %in% c("SUN", "SDM", "SAB")                      ~ "Sunbury",
      code %in% c("WER")                                    ~ "Werribee",
      code %in% c("WIL")                                    ~ "Williamstown"
    )) %>%
    
    # calculate weighted mean of 3car% for each line
    group_by(line) %>%
    summarise(AMP_3_car_pct_mean = weighted.mean(AMP_3_car_pct, AMP_obs),
              INP_3_car_pct_mean = weighted.mean(INP_3_car_pct, INP_obs),
              PMP_3_car_pct_mean = weighted.mean(PMP_3_car_pct, PMP_obs),
              POP_3_car_pct_mean = weighted.mean(POP_3_car_pct, POP_obs)) %>%
    ungroup() %>%
    
    # weight by timeband
    mutate(mean_3_car_pct = 
             (AMP_3_car_pct_mean * weights[weights$timeband == "AMP", "weight"] / 100) +
             (INP_3_car_pct_mean * weights[weights$timeband == "INP", "weight"] / 100) +
             (PMP_3_car_pct_mean * weights[weights$timeband == "PMP", "weight"] / 100) +
             (POP_3_car_pct_mean * weights[weights$timeband == "POP", "weight"] / 100)) %>%
    
    # select required output
    dplyr::select(line, mean_3_car_pct)
}

mean3car2004 <- mean3car(cars2004)

mean3car2008 <- mean3car(cars2008)


## 3.3 linear inter/extrapolation for 2005-07 and 2009 ----
## -------------------------------------#
# Concept: 3 car observations for 2005-07 and 2009 are only provided
# on a all/some/none basis.  If 'some' is reported, then calculate
# the percentage by linear interpolation / extrapolation from 2004 & 2008

# prepare table to hold results, with 2005-07 and 2009 inter/extrapolated
# (but some will need to be converted to zero later)
mean3car <- mean3car2004 %>%
  rename(mean_3_car_pct_2004 = mean_3_car_pct) %>%
  # add column for 2008
  left_join(mean3car2008, by = "line") %>%
  rename(mean_3_car_pct_2008 = mean_3_car_pct) %>%
  # add columns for 2005-07 and 2009
  ## interpolated years are 2004 plus 25% of difference 2008-2004 per year;
  mutate(mean_3_car_pct_2005 = mean_3_car_pct_2004 + 
           ((mean_3_car_pct_2008 - mean_3_car_pct_2004) * 0.25),
         mean_3_car_pct_2006 = mean_3_car_pct_2004 + 
           ((mean_3_car_pct_2008 - mean_3_car_pct_2004) * 0.5),
         mean_3_car_pct_2007 = mean_3_car_pct_2004 + 
           ((mean_3_car_pct_2008 - mean_3_car_pct_2004) * 0.75)) %>%
  # extrapolated year is 2008 plus 25% of difference 2008-2004,
  # but if that would be below zero, the 50% of 2008 figure instead
  mutate(mean_3_car_pct_2009 = mean_3_car_pct_2008 +
           ((mean_3_car_pct_2008 - mean_3_car_pct_2004) * 0.25)) %>%
  mutate(mean_3_car_pct_2009 = if_else(mean_3_car_pct_2009 < 0,
                                       mean_3_car_pct_2008 / 2, 
                                       mean_3_car_pct_2009)) %>%
  # reorder columns
  dplyr::select(line, mean_3_car_pct_2004, mean_3_car_pct_2005, mean_3_car_pct_2006,
                mean_3_car_pct_2007, mean_3_car_pct_2008, mean_3_car_pct_2009)


# extract each year's figures from summary (read in under 'basic data' above)
summary2005 <- summary[, c(1, 3:7)] %>%
  setNames(c("group", "OD", "AMP", "INP", "PMP", "POP"))

summary2006 <- summary[, c(1, 9:13)] %>%
  setNames(c("group", "OD", "AMP", "INP", "PMP", "POP"))

summary2007 <- summary[, c(1, 15:19)] %>%
  setNames(c("group", "OD", "AMP", "INP", "PMP", "POP"))

summary2009 <- summary[, c(1, 21:25)] %>%
  setNames(c("group", "OD", "AMP", "INP", "PMP", "POP"))


# set mean3car columns to zero where required based on summary
# (while coded in full below, only applies to Caulfield group)
years <- c(2005:2007, 2009)

for (i in 1:length(years)) {
  # get colum and summary for the year
  yearcol <- paste0("mean_3_car_pct_", as.character(years[i]))
  yearsum <- get(paste0("summary", as.character(years[i])))
  
  # for each line, if no orig/dest on the line has 3 car in any timeband, set to zero
  if (any(as.matrix(yearsum %>% filter(OD %in% c("EPP"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Epping", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("HBE", "ELT", "GRN", "MCD"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Hurstbridge", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("GWY"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Glen Waverley", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("ALM", "RIV"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Alamein", yearcol] <- 0
  }
    
  if (any(as.matrix(yearsum %>% filter(OD %in% c("BEG", "UFG", "LIL", "MLK", "RWD", "BBN"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Belgrave/Lilydale", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(group == "Caulfield")) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Pakenham/Cranbourne", yearcol] <- 0
    mean3car[mean3car$line == "Frankston", yearcol] <- 0
    mean3car[mean3car$line == "Sandringham", yearcol] <- 0
  }

  if (any(as.matrix(yearsum %>% filter(OD %in% c("UFD"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Upfield", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("CGB", "BMS"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Craigieburn", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("SUN", "SDM", "SAB"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Sunbury", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("WER"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Werribee", yearcol] <- 0
  }
  
  if (any(as.matrix(yearsum %>% filter(OD %in% c("WIL"))) 
          %in% c("All 3", "Some 3")) == FALSE) {
    mean3car[mean3car$line == "Williamstown", yearcol] <- 0
  }
}
  
## 3.4 capacity factors ----
## -------------------------------------#
# for 2004-09, capacity factor is 1 - (3_car_pct/2)
# the 3_car_pct needs to be divided by 2, so that if all trains
# were 3 car instead of 6 car, the capacity factor would  be 0.5

train_cap_factors <- mean3car %>%
  # transpose
  t() %>%
  as.data.frame() %>%
  # set names equal to first row
  setNames(.[1, ]) %>%
  # and remove first row (which is line names)
  .[-1, ]

# convert row names to years
rownames(train_cap_factors) <- paste0("FY_", str_sub(rownames(train_cap_factors), -4, -1))
  
# calculate capacity factors for 2004-09
train_cap_factors <- train_cap_factors %>% 
  mutate(across(.cols = everything(), ~ 1 - (as.numeric(.x)/2)))


# add capacity factors for 2010-2020 (assumed to be 1 for 2010-16,
# then increased in 2017 with increase in carriage capacity - see
# https://transport.vic.gov.au/-/media/tfv-documents/mi-metropolitan-train-load-standards-may-2017.pdf? p5
train_cap_factors <- train_cap_factors %>%
  rbind(FY_2010 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2011 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2012 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2013 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2014 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2015 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2016 = rep(1, ncol(train_cap_factors))) %>%
  rbind(FY_2017 = rep(900/798, ncol(train_cap_factors))) %>%
  rbind(FY_2018 = rep(900/798, ncol(train_cap_factors))) %>%
  rbind(FY_2019 = rep(900/798, ncol(train_cap_factors))) %>%
  rbind(FY_2020 = rep(900/798, ncol(train_cap_factors)))


# convert row names to first column
train_cap_factors <- cbind(
  year = row.names(train_cap_factors),
  train_cap_factors
)
rownames(train_cap_factors) <- NULL

# add monthly capacity factors from Jul 2020, which are:
# - Jul to Dec 2020: same as Jun 2020
# - from Jan 2021 for all lines except Dandenong - same as Jun 2020
# - from Jan 2021 for Dandenong lines, where HCMT came into operation - 
#   ramping up from approx 0% HCMT to 100% Feb 2023, so say 4% per month, adding
#   200 capacity per train for the extra percentage

# for months Jul-Dec 2020 (no change)
fixed.months <- sprintf("2020-%02d", 7:12)
for (i in 1:length(fixed.months)) {
  train_cap_factors <- train_cap_factors %>%
    rbind(c(year = fixed.months[i], 
            train_cap_factors[train_cap_factors$year == "FY_2020", -1]))
}

# for months Jan 2021-Jun 2022 (no change except for Dandenong, 4% HCMT per month)
HCMT.months <- c(sprintf("2021-%02d", 1:12), sprintf("2022-%02d", 1:6))
HCMT.base <- train_cap_factors[train_cap_factors$year == "FY_2020",
                               "Pakenham/Cranbourne"] # capacity for FY_2020
for (i in 1:length(HCMT.months)) {
  HCMT.pct <- i * 0.04  # 4% per month
  HCMT.cap <- (HCMT.base * (1 - HCMT.pct)) + (HCMT.base * HCMT.pct * 1100/900)
  train_cap_factors <- train_cap_factors %>%
    rbind(c(year = HCMT.months[i],
            train_cap_factors[train_cap_factors$year == "FY_2020", -1] %>%
              mutate(`Pakenham/Cranbourne` = HCMT.cap)))
}



## 3.5 add details for shared line stations ----
## -------------------------------------#
train_cap_factors <- train_cap_factors %>%
  rowwise() %>%
  # each station is the mean of the relevant lines
  # but Hawksburn_Armadale is wholly Frankston from Feb 2021
  mutate(`Stn City` = mean(c(`Alamein`, `Belgrave/Lilydale`, `Craigieburn`, `Epping`,
                           `Frankston`, `Glen Waverley`, `Hurstbridge`, `Pakenham/Cranbourne`,
                           `Sandringham`, `Sunbury`, `Upfield`, `Werribee`, 
                           `Williamstown`)),
         `Stn Jolimont_Clifton Hill` = mean(c(`Epping`, `Hurstbridge`)),
         `Stn Richmond` = mean(c(`Alamein`, `Belgrave/Lilydale`, `Frankston`, 
                               `Glen Waverley`, `Pakenham/Cranbourne`, `Sandringham`)),
         `Stn Burnley` = mean(c(`Alamein`, `Belgrave/Lilydale`, `Glen Waverley`)),
         `Stn Hawthorn_Camberwell` = mean(c(`Alamein`, `Belgrave/Lilydale`)),
         `Stn South Yarra` = mean(c(`Pakenham/Cranbourne`, `Frankston`, `Sandringham`)),
         `Stn Hawksburn_Armadale` = if_else(year %in% c("2021-02", "2021-03", "2021-04", "2021-05",
                                                        "2021-06", "2021-07", "2021-08", "2021-09",
                                                        "2021-10", "2021-11", "2021-12", "2022-01",
                                                        "2022-02", "2022-03", "2022-04", "2022-05",
                                                        "2022-06"),
                                            `Frankston`,
                                            mean(c(`Pakenham/Cranbourne`, `Frankston`))),
         `Stn Malvern_Caulfield` = mean(c(`Pakenham/Cranbourne`, `Frankston`)),
         `Stn North Melbourne` = mean(c(`Craigieburn`, `Sunbury`, `Upfield`, `Werribee`, 
                                        `Williamstown`)),
         `Stn South Kensington` = mean(c(`Werribee`, `Williamstown`)),
         `Stn Footscray` = mean(c(`Sunbury`, `Werribee`, `Williamstown`)),
         `Stn Seddon_Newport` = mean(c(`Werribee`, `Williamstown`)))


## 3.6 add Stony Point (assumed to be 1) ----
## -------------------------------------#
train_cap_factors <- train_cap_factors %>%
  mutate(`Stony Point` = 1)

## 3.7 rebase so first year is 1 ----
## -------------------------------------#
# factors for first year
year1 <- train_cap_factors[1, ]

# for each row in the table ...
for (i in 1:nrow(train_cap_factors)) {
  # ... and for each column (other than the first, 'year') ...
  for (j in 2:ncol(train_cap_factors)) {
    # current figure divided by first year figure
    train_cap_factors[i, j] <- train_cap_factors[i, j] / year1[j]
  }
}
  

## 3.8 write output ----
## -------------------------------------#
write.csv(train_cap_factors, "../Tables/train capacity factors.csv", row.names = FALSE)


# 4. Station volumes as percentage of line volumes ----
# -----------------------------------------------------------------------------#
# DoT have supplied train line volumes for all years, but station volumes for 2016-22
# only.  Determines, for each station, the median of the station.vol as a percentage
# of the line.vol.  An expanded version also appears in section 1 of 
# 'train cleaning checks.R'

## 4.1 Read in volumes ----
## -------------------------------------## 
# Service volumes supplied by DoT, 23 Jan 2023
line.vol <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                              sheet = "MetroMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Train)`)

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


## 4.2 Calculate station percentages ----
## -------------------------------------## 
# make table of relevant lines for each station
station.line.lookup <- stationLineDetails(station.vol)

# table of median of station.vol as percentage of line.vol, by station
# (see section 1 of 'train cleaning checks.R' for more details)
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
  mutate(line.vol = getLineVol(line.vol, Year, Month, Routes)) %>%
  # omit NAs and zeros
  filter(!is.na(stn.vol) & !is.na(line.vol) &
           stn.vol > 0 & line.vol > 0) %>%
  # calculate stn.vol as percentage of line.vol
  mutate(line.vol.pct = stn.vol / line.vol * 100) %>%
  # summarise and find median percentage by station
  group_by(Station, Routes) %>%
  summarise(station.pct = median(line.vol.pct)) %>%
  arrange(Routes)


## 4.3 Write output ----
## -------------------------------------## 
write.csv(station.data, "../Tables/station percentages.csv", row.names = FALSE)
