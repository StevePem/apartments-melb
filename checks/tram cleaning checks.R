# tram cleaning checks

# set up environment
library(dplyr)
library(openxlsx)
library(tidyr)  # for pivoting


# 1. Investigate Cth Games period volumes ---- 
# -----------------------------------------------------------------------------#
# DoT's tram volumes are incomplete for March 2006.  DoT's comments:
# "For Yarra Trams - No data from 15th Mar 2006 till 26th Mar 2006 due to commonwealth games"
# (Note that same also applies to Metro Trains, and a version of this code is  
# also used in section 2 of 'train cleaning checks.R'))

# That is a 12-day inclusive period, leaving 19 'normal' days

# Accordingly, consider whether daily volumes from Feb can be applied.  
# Volumes from April may not be suitable, given that it includes Easter/school holidays
# (Easter day was Sun 15 Apr 2006)

# Feb/March volume comparisons from 2007 may also be relevant, noting Easter day
# was Sun 8 Apr 2007 (but possibly not 2005 - Easter day Sun 27 Mar 2005)
# Note also no tram route changes in Feb/Mar 2006 (except Cth Games itself) or
# Feb/Mar 2007

## 1.1 Load DoT tram service volume figures ----
## ------------------------------------#
# Service volumes supplied by DoT, 23 Jan 2023
tram.voltable <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                   sheet = "YarraMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Tram)`)


## 1.2 Daily volumes for Feb/Mar 2006 & 2007 ----
## ------------------------------------#
daily.volumes <- tram.voltable %>%
  # get the volumes for Feb/Mar 2006/07 by route
  filter(Year %in% c(2006, 2007) & Month %in% c("February", "March")) %>%
  pivot_wider(names_from = c(Month, Year),
              names_sep = "_",
              values_from = sched.vol) %>%
  # calculate daily volumes
  mutate(Feb_2006_daily_28 = February_2006 / 28,
         Mar_2006_daily_19 = March_2006 / 19,
         Feb_2007_daily_28 = February_2007 / 28,
         Mar_2007_daily_31 = March_2007 / 31) %>%
  # calculate percentage differences
  mutate(diff_2006 = Mar_2006_daily_19 / Feb_2006_daily_28 * 100,
         diff_2007 = Mar_2007_daily_31 / Feb_2007_daily_28 * 100)

mean(daily.volumes$diff_2006, na.rm = TRUE)
sd(daily.volumes$diff_2006, na.rm = TRUE)
mean(daily.volumes$diff_2007, na.rm = TRUE)
sd(daily.volumes$diff_2007, na.rm = TRUE)

# Results:
# In 2007, daily March volumes are within 1% of daily Feb volumes for all 
# routes except routes 6 (101.04%), 80 (102.51%) and city shuttle (94.34%) [mean 
# 100.20%, sd 1.41%].  This suggests that applying Feb daily volumes to March
# is a reasonable approach.
# 
# The alternative method of dividing March 2006 volumes by 19 (for the 19 normal
# days that should have been captured by DoT figures excluding the 12 Cth Games
# days) results in all but one route where the daily March volumes differ by
# more than 1% from the Feb daily volumes, with 5 routes differing by more than 
# 5% (of which 4 are more than 10% and 1 is more than 20%)[mean 101.12%, sd 6.95%].
# However, three of those routes are 3, 5 and 55, where the gap would be made up by
# a separately-reported ' route 3/5/55' figure. So using March 2006 volumes divided
# by 19 would be a workable approach, if a further adjustment waws made for 
# routes 3, 5 and 55 - but the Feb volumes seem more reliable.

## 1.3 Save results ----
## ------------------------------------#
write.csv(daily.volumes, 
          "../Tables/tram daily volumes Feb_Mar 2006_07 for Cth Games.csv",
          row.names = FALSE)


# 2. Check that tram capacity factors cover all months with DoT volumes ---- 
# -----------------------------------------------------------------------------#
# Run sections 3.2 and 3.3 of analysis.R to load:
# - 'tram.voltable' - DoT volume figures, with required adjustments [note - 
#   need to run whole of 3.2], and
# - 'tram.captable'- monthly capacity factors as calculated in section 1
#   of trams.R

# limit tram.voltable to analysis period
tram.voltable.converted <- tram.voltable %>%
  mutate(date = as.Date(paste0(Year, "-", Month, "-01"), format = "%Y-%B-%d")) %>%
  filter(date >= as.Date("2003-07-01") & date <= as.Date("2022-06-30")) %>%
  # select required columns
  select(-date)

# convert tram.captable to table in same form of tram.voltable
tram.captable.converted <- tram.captable %>%
  # split month into month and year
  mutate(month = as.Date(paste0(month, "-01")),
         Year = as.numeric(strftime(month, format = "%Y")),
         Month = strftime(month, format = "%B")) %>%
  # pivot into Route and cap_factor columns
  pivot_longer(cols = starts_with("route_"),
               names_to = "Route",
               values_to = "cap_factor") %>%
  # remove 'route_' from Route column, leaving numbers
  mutate(Route = gsub("route_", "", Route)) %>%
  # select required columns
  select(Year, Month, Route, cap_factor)


# join tables
checktable <- tram.voltable.converted %>%
  left_join(tram.captable.converted,
            by = c("Year", "Month", "Route"))

# there should be no missing cap.factors (NAs or zeros) in checktable - that
# is, every month for which a route has a volume should also have a cap.factor
checktable2 <- checktable %>%
  filter(!(cap_factor > 0 | is.na(cap_factor)))
# zero obs, which is the expected result!  


# 3. Compare tram 2005 and 2007 volumes ---- 
# -----------------------------------------------------------------------------#
# Compares annual tram volumes by route between 2005 and 2007

# Tram volumes drop in 2005, but this is because of the amalgamations of
# routes 22 & 8 and 16 & 69.  The 2006 drop also needs to be considered. 
# However, more useful to compare 2005 to 2007, to avoid the distortions
# from the omission of the 2006 Commonwealth Games period.

comparison <- readxl::read_xlsx("../Data/DOT Service Volumes.xlsx", 
                                   sheet = "YarraMonthly") %>%
  # select relevant columns
  dplyr::select(Year, Month, Route, sched.vol = `# scheduled (Tram)`) %>%
  
  # add fin year column
  mutate(FY = ifelse(Month %in% c("July", "August", "September", 
                                  "October", "November", "December"),
                     paste0("FY_", Year + 1),
                     paste0("FY_", Year))) %>%
  
  # group and sum by year and route
  group_by(Route, FY) %>%
  summarise(vol = sum(sched.vol)) %>%
  ungroup() %>%
  
  # retain only 2005 and 2007, and pivot
  filter(FY %in% c("FY_2005", "FY_2007")) %>%
  pivot_wider(names_from = FY, values_from = vol) %>%
  
  # arrange the route numbers numerically (ignore the warning here)
  arrange(as.numeric(Route))

# add a total row
total_row <- comparison %>%
  summarise(Route = 'Total',
            FY_2005 = sum(FY_2005, na.rm = TRUE),
            FY_2007 = sum(FY_2007, na.rm = TRUE))

comparison <- bind_rows(comparison, total_row) %>%
  
  # and calculate % changes
  mutate(change_pct = round((FY_2007 - FY_2005) / FY_2005 * 100, 1))

# write output
write.csv(comparison, "../Tables/tram 2005 to 2007 service drop.csv", row.names = FALSE)

             
              


  
