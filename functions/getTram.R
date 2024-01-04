# functions to look up tram volume and capacity by month

# function to look up monthly volume in tram.voltable, for month in format
# "2003-07" (tram.voltable has "2003" and "July"), and route
get.tram.vol <- function(month, route) {
  year = strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%Y")
  month = strftime(as.Date(paste0(month, "-01"), "%Y-%m-%d"), format = "%B")
  
  # get volume from tram.voltable
  vol <- tram.voltable[tram.voltable$Year == year &
                         tram.voltable$Month == month &
                         tram.voltable$Route == route, 
                       "sched.vol"][[1]] 
  
  # if no volume is provided, 'vol' will be numeric(0): convert to ordinary 0
  if (identical(vol, numeric(0))) {vol <- 0}
  
  return(vol)
}


# function to look up monthly capacity in tram.captable, for month in format
# "2003-07" and route
get.tram.cap <- function(month, route) {
  route_col <- paste0("route_", route)
  
  # get capacity from tram.captable [would be NA if no volume present - this
  # should never happen as capacities should cover all routes where volume data
  # provided - see section 2 of checks/tram cleaning checks.R for confirmation]
  cap <- tram.captable[tram.captable$month == month, ## match to month
                       route_col][[1]]
  
  return(cap)
}
