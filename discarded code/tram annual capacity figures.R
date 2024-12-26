# was in trams.R, for annual capacity factors, before changed to monthly
# (and before a timetable starting 1-Jul-2003 was added - adding this rendered 
# 'if year starts and ends before first timetable' unnecessary, though harmless)


## 1.6 make table of capacity factors for each year 
## -------------------------------------#
# add and complete relevant date columns 
capacity.factors.dates <- capacity.factors %>%
  # add end_date, one day before next start_date
  mutate(end_date = lead(start_date) - 1)

# relevant years
years <- c(2004:2022)

# data frame to hold outputs
capacity.factors.years <- data.frame()

# identify timetables for each year and number of days for which they are in effect 
for (i in 1:length(years)) {
  # find start and end dates of financial year
  year.start <- as.Date(paste0(years[i]-1, "-07-01"))
  year.end <- as.Date(paste0(years[i], "-06-30"))
  
  # if year starts and ends before first timetable, use first timetable
  if (year.end < capacity.factors.dates[1, "start_date"]) {
    timetables <- capacity.factors.dates[1,] %>%
      # and 'days' will be full year
      mutate(days = year.end - year.start + 1)  # if you don't add 1, it only records 364 days
  } else {
    # otherwise, select the relevant capacity factor timetables
    timetables <- capacity.factors.dates %>%
      filter(
        # starts within year
        start_date >= year.start & start_date <= year.end |
          # ends within year
          end_date >= year.start & end_date <= year.end |
          # covers entire year
          start_date < year.start & end_date > year.end
      ) %>%
      # complete end_date as year.end if NA
      mutate(end_date = case_when(
        is.na(end_date) ~ year.end,
        TRUE            ~ end_date
      ))
    
    # change first start_date to year.start if first timetable starts during year
    if (timetables[1, "start_date"] > year.start) {
      timetables[1, "start_date"] <- year.start
    } 
    
    # add number of days that relevant timetable is in effect
    timetables <- timetables %>%
      mutate(days = case_when(
        # starts before year and ends within year
        start_date < year.start & end_date <= year.end  ~ end_date - year.start + 1,
        # starts and ends within year
        start_date >= year.start & end_date <= year.end ~ end_date - start_date + 1,
        # starts within year and ends after year
        start_date >= year.start & end_date > year.end  ~ year.end - start_date + 1,
        # covers entire year
        start_date < year.start & end_date > year.end   ~ year.end - year.start + 1
      ))
  }
  
  # calculate weighted average route capacity factors
  cap.factors <- timetables %>%
    summarise(across(any_of(route.cols), ~ weighted.mean(.x, as.numeric(days), na.rm = TRUE)))
  
  # add financial year and organise
  cap.factors <- cap.factors %>%
    mutate(fin_year = years[i]) %>%
    dplyr::select(fin_year, any_of(route.cols))
  
  # add the result to the output
  capacity.factors.years <- rbind(capacity.factors.years,
                                  cap.factors)
}

# remove 'NaN' cells
capacity.factors.years[capacity.factors.years == "NaN"] <- NA




# function from section 3.4 of analysis.R, get monthly volumes
#-------------------------------------#
get.tram.cap <- function(month, route) {
  year <- strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%Y")
  
  # ADJUST this to match eventual cap table month/year format - maybe captable
  # could just have '2003-07' format and then it would be easier
  # month.year <- strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%b_%Y")
  
  route_col <- paste0("route_", route)
  
  # route column is present, get capacity from tram.captable, otherwise 1
  if (route_col %in% colnames(tram.captable)) {
    cap <- tram.captable[tram.captable$fin_year == year, ## match to month/year
                         route_col][[1]]
  } else {
    cap <- 1
  }
  
  # if route column is present but no capacity provided, 'cap' will be NA or empty: convert to 1
  if (length(cap) == 0) {
    cap <- 1
  } else if (exists("cap") & is.na(cap)) {
    cap <- 1
  }
  
  return(cap)
}


