# Checks of ABS building approvals for 2011-16 and 2016-21, downloaded from https://explore.data.abs.gov.au/, search 
# 'building approvals by SA2'(but that may only find 2016-21 - can dig for 2011-16 in https://www.abs.gov.au/statistics/industry/building-and-construction/building-approvals-australia/latest-release)

# Used to check differences between approval numbers by half year, to inform 
# decision on whether reasonable to apportion a year's results equally between
# the two half years that comprise the full year


# set up environment
library(dplyr)

# 1. Read in data
# -----------------------------------------------------------------------------
# these figures are building approvals for all SA4s within Greater Melbourne GCCSA
BA2011_16 <- read.csv('../Data/ABS 2011-16 BA.csv')
BA2016_21 <- read.csv('../Data/ABS 2016-21 BA.csv')

BA <- rbind(BA2011_16, BA2016_21)


# 2. Compile dwellings approved in half years
# -----------------------------------------------------------------------------
# calculate total dwellings approved for each half year
approvalsByHalfYear <- BA %>%
  # add financial year and type
  mutate(month = as.numeric(substr(TIME_PERIOD..Time.Period, 6, 7)),  # last 2 characters, eg "2011-07" >> 07
         year = as.numeric(substr(TIME_PERIOD..Time.Period, 1, 4)), # first 4 characters, eg "2011-07" >> 2011
         # for Jan-Jun (month <= 6), fin year is same as year; for Jul-Dec, fin year is following year (eg Jul 2011 is fin year 2012)
         finyear = if_else(month <= 6, year, year + 1),
         # types: [1] "120: Semi-detached, row or terrace houses, townhouses - Total"             
         # "130: Flats units or apartments - Total including those attached to a house" OR "130: Apartments - Total including those attached to a house"  
         type = substr(BUILDING_TYPE..Type.of.building, 1, 3)) %>%   
  # exclude incomplete 2023 year
  filter(finyear <= 2022) %>%
   # group by financial year
  group_by(finyear) %>%
  # calculate no of dwellings approved ('obs_value') for each half year, by apartment and townhouse
  summarise(tot.dwell.apt = sum(OBS_VALUE[type == "130"]),
            jul_dec.dwell.apt = sum(OBS_VALUE[month > 6 & type == "130"]),
            jul_dec.pct.apt = jul_dec.dwell.apt / tot.dwell.apt * 100,
            jan_jun.dwell.apt = sum(OBS_VALUE[month <= 6 & type == "130"]),
            jan_jun.pct.apt = jan_jun.dwell.apt / tot.dwell.apt * 100,
            tot.dwell.town = sum(OBS_VALUE[type == "120"]),
            jul_dec.dwell.town = sum(OBS_VALUE[month > 6 & type == "120"]),
            jul_dec.pct.town = jul_dec.dwell.town / tot.dwell.town * 100,
            jan_jun.dwell.town = sum(OBS_VALUE[month <= 6 & type == "120"]),
            jan_jun.pct.town = jan_jun.dwell.town / tot.dwell.town * 100,
            tot.dwell = sum(OBS_VALUE),
            jul_dec.dwell = sum(OBS_VALUE[month > 6]),
            jul_dec.pct = jul_dec.dwell / tot.dwell * 100,
            jan_jun.dwell = sum(OBS_VALUE[month <= 6]),
            jan_jun.pct = jan_jun.dwell / tot.dwell * 100) %>%
  ungroup() %>%
  mutate(finyear = as.character(finyear))

# calculate a total row 
totals <- data.frame(finyear = "2012-2022",
                     tot.dwell.apt = sum(approvalsByHalfYear$tot.dwell.apt),
                     jul_dec.dwell.apt = sum(approvalsByHalfYear$jul_dec.dwell.apt),
                     jul_dec.pct.apt = sum(approvalsByHalfYear$jul_dec.dwell.apt) / sum(approvalsByHalfYear$tot.dwell.apt) * 100,
                     jan_jun.dwell.apt = sum(approvalsByHalfYear$jan_jun.dwell.apt),
                     jan_jun.pct.apt = sum(approvalsByHalfYear$jan_jun.dwell.apt) / sum(approvalsByHalfYear$tot.dwell.apt) * 100,
                     tot.dwell.town = sum(approvalsByHalfYear$tot.dwell.town),
                     jul_dec.dwell.town = sum(approvalsByHalfYear$jul_dec.dwell.town),
                     jul_dec.pct.town = sum(approvalsByHalfYear$jul_dec.dwell.town) / sum(approvalsByHalfYear$tot.dwell.town) * 100,
                     jan_jun.dwell.town = sum(approvalsByHalfYear$jan_jun.dwell.town),
                     jan_jun.pct.town = sum(approvalsByHalfYear$jan_jun.dwell.town) / sum(approvalsByHalfYear$tot.dwell.town) * 100,
                     tot.dwell = sum(approvalsByHalfYear$tot.dwell),
                     jul_dec.dwell = sum(approvalsByHalfYear$jul_dec.dwell),
                     jul_dec.pct = sum(approvalsByHalfYear$jul_dec.dwell) / sum(approvalsByHalfYear$tot.dwell) * 100,
                     jan_jun.dwell = sum(approvalsByHalfYear$jan_jun.dwell),
                     jan_jun.pct = sum(approvalsByHalfYear$jan_jun.dwell) / sum(approvalsByHalfYear$tot.dwell) * 100)

# bind the totals from the complete years to the apprvals table
approvalsByHalfYear <- rbind(approvalsByHalfYear, totals)

write.csv(approvalsByHalfYear, "../Tables/approvalsByHalfYear.csv", row.names = FALSE)
