# function to adjust station volumes for new stations opened (or added to metro
# network by being electrified) during analysis period

# see Tables/train new stations.xlsx for station opening dates

newStations <- function(mthly.vol, months) {
  
  # for each station:
  # - volume for months before the station opened is 0
  # - volume in the month the station opened is multiplied by a day fraction
  
  # Coolaroo opened 6 Jun 2010
  station <- "Coolaroo"
  change.month <- "Jun_2010"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 25/30
  
  # Roxburgh Park electrified 30 Sep 2007
  station <- "Roxburgh Park"
  change.month <- "Sep_2007"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 1/30
  
  # Craigieburn electrified 30 Sep 2007
  station <- "Craigieburn"
  change.month <- "Sep_2007"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 1/30
  
  # Lynbrook opened 22 Apr 2012
  station <- "Lynbrook"
  change.month <- "Apr_2012"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 9/30
  
  # Southland opened 26 Nov 2017
  station <- "Southland"
  change.month <- "Nov_2017"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 5/30
  
  # South Morang opened 22 Apr 2012
  station <- "South Morang"
  change.month <- "Apr_2012"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 9/30
  
  # Middle Gorge opened 26 Aug 2018
  station <- "Middle Gorge"
  change.month <- "Aug_2018"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 6/31
  
  # Hawkstowe opened 26 Aug 2018
  station <- "Hawkstowe"
  change.month <- "Aug_2018"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 6/31
  
  # Mernda opened 26 Aug 2018
  station <- "Mernda"
  change.month <- "Aug_2018"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 6/31
  
  # Cardinia Road opened 22 Apr 2012
  station <- "Cardinia Road"
  change.month <- "Apr_2012"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 9/30
  
  # Diggers Rest electrified 18 Nov 2012
  station <- "Diggers Rest"
  change.month <- "Nov_2012"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 13/30
  
  # Sunbury electrified 18 Nov 2012
  station <- "Sunbury"
  change.month <- "Nov_2012"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 13/30
  
  # Williams Landing opened 28 Apr 2013
  station <- "Williams Landing"
  change.month <- "Apr_2013"
  mthly.vol[mthly.vol$Station == station,
            !(names(mthly.vol)) %in% c("Station", "Routes", "station.pct") &
              to_date(names(mthly.vol)) < to_date(change.month)] <- 0
  mthly.vol[mthly.vol$Station == station, names(mthly.vol) == change.month] <- 
    mthly.vol[mthly.vol$Station == station, 
              names(mthly.vol) == change.month] * 3/30
  
  return(mthly.vol)
}

